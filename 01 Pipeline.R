# =============================================================================
# 01_PIPELINE.R
# =============================================================================
# Data import -> recode -> collapse -> survey design -> bivariate ->
# multivariable -> sensitivity -> vulnerable groups -> multilevel ->
# G-computation -> supplementary checks.
#
# Run this file ONCE. It ends by saving every object to
# analysis_workspace.RData. 02_tables.R, 03_figures.R and 04_maps.R all
# start with load() on that file, so none of them need to re-run any model.
#
# This file computes ONLY. All docx/html/png rendering happens in 02-04.
# =============================================================================
rm(list = ls())
setwd("/Users/macuser/IMMS Datasets")

library(haven)
library(dplyr)
library(tidyr)
library(forcats)
library(gtsummary)
library(survey)
library(srvyr)
library(broom)
library(purrr)
library(lme4)
library(MASS)   # mvrnorm; loads after dplyr so select() masking is intentional
library(car)    # GVIF
library(pwr)

options(survey.lonely.psu = "adjust")
n_sim <- 1000
set.seed(20260527)

cat("===== 01_PIPELINE.R starting =====\n")

# =============================================================================
# SECTION 1 — IMPORT
# =============================================================================
df <- read_sav("IMMS_Wave1_27052026.sav")

ng_raw <- df %>%
  filter(ADM1 == 13) %>%
  mutate(geotype_raw = as.numeric(S13), psu = as.factor(PSU))

us_raw <- df %>%
  filter(ADM1 == 22) %>%
  mutate(geotype_raw = as.numeric(S13))

cat("Loaded: NG n =", nrow(ng_raw), " US n =", nrow(us_raw), "\n")

# =============================================================================
# SECTION 2 — RECODE
# =============================================================================
recode_all <- function(d) {
  yn <- function(x, na_code = 99, yes = 2)
    factor(ifelse(x == na_code, NA, x == yes), c(FALSE, TRUE), c("No", "Yes"))
  
  d <- d %>% mutate(
    confident       = as.numeric(B1 == 4 & B2 == 4),
    confident_top2  = case_when(B1 == 99 | B2 == 99 ~ NA_real_,
                                B1 %in% c(3, 4) & B2 %in% c(3, 4) ~ 1, TRUE ~ 0),
    conf_importance = case_when(B1 == 99 ~ NA_real_, B1 == 4 ~ 1, TRUE ~ 0),
    conf_safety     = case_when(B2 == 99 ~ NA_real_, B2 == 4 ~ 1, TRUE ~ 0),
    
    geotype = relevel(factor(geotype_raw, 1:3, c("Urban", "Peri-urban", "Rural")), ref = "Urban"),
    
    age_cat     = factor(ifelse(S3_RECODE %in% c(6, 99), NA, S3_RECODE), 1:5,
                         c("18-24", "25-34", "35-44", "45-54", "55+")),
    education   = factor(ifelse(S4 %in% c(97, 98, 99), NA, S4), 1:6,
                         c("None", "Primary", "Secondary", "Vocational", "University", "Masters/PhD")),
    ses         = factor(ifelse(S9 %in% c(6, 99), NA, S9), 1:5,
                         c("Basics unaffordable", "Struggle utilities", "Struggle goods",
                           "Struggle luxury", "Comfortable")),
    ease_access = factor(ifelse(B14 %in% c(97, 98, 99), NA, B14), 1:4,
                         c("Not at all", "A little", "Moderately", "Very")),
    employment  = droplevels(factor(ifelse(S5 == 99, NA, S5), 1:6,
                                    c("Employed", "Student employed", "Student not employed",
                                      "Not employed", "Home/family", "Retired"))),
    gender      = factor(ifelse(GENDER == 3, NA, GENDER), 1:2, c("Male", "Female")),
    child_age_cat = cut(CHILD_AGE_1, c(-Inf, 4, 11, 17),
                        c("Pre-school (0-4)", "Primary (5-11)", "Secondary (12-17)")),
    
    know_where  = yn(B11), recall = yn(B9), turned_away = yn(B13),
    hcw_rec     = yn(B3, na_code = 98, yes = 1),
    peer_norm   = yn(B5), family_norm = yn(B6),
    relig_norm  = yn(B7), comm_norm   = yn(B8),
    
    barrier_count = rowSums(across(A_B16_2:A_B16_11, ~ .x == 2), na.rm = TRUE),
    
    n_children = C0, migrant = yn(S12), child_disab = yn(C8_1, 98, 1),
    wt = WEIGHT
  )
  
  rel <- fct_recode(as_factor(d$religion_RECODE),
                    "No religion" = "Do not have a religion",
                    NULL          = "Prefer not to say")
  d$religion <- factor(as.character(rel),
                       levels = c("Christian", "Muslim", "No religion", "Hindu",
                                  "Sikh", "Jewish", "Buddhist", "Indigenous", "Other"))
  d
}

ng <- recode_all(ng_raw)
us <- recode_all(us_raw)

cat("Recoded. Confidence — NG:", round(100 * mean(ng$confident, na.rm = TRUE), 1),
    "% US:", round(100 * mean(us$confident, na.rm = TRUE), 1), "%\n")

# =============================================================================
# SECTION 3 — COLLAPSE COVARIATES
# =============================================================================
collapse_covars <- function(d) {
  d %>% mutate(
    education3 = fct_collapse(education,
                              "Primary or less" = c("None", "Primary"),
                              "Secondary"       = c("Secondary", "Vocational")),
    ses3 = fct_collapse(ses,
                        "Struggling"  = c("Basics unaffordable", "Struggle utilities"),
                        "Managing"    = c("Struggle goods", "Struggle luxury"),
                        "Comfortable" = "Comfortable"),
    employment3 = fct_collapse(employment,
                               "Employed"    = c("Employed", "Student employed"),
                               "Student"     = "Student not employed",
                               "Not working" = c("Not employed", "Home/family", "Retired")),
    religion3 = fct_collapse(religion,
                             "Christian" = "Christian", "Muslim" = "Muslim",
                             "No religion" = "No religion", "Jewish" = "Jewish",
                             other_level = "Other")
  ) %>% mutate(religion3 = droplevels(religion3))
}

ng <- collapse_covars(ng)
us <- collapse_covars(us)

# =============================================================================
# SECTION 4 — SURVEY DESIGN, STATE, REGION
# =============================================================================
ng <- ng %>% mutate(wt_r = wt * n() / sum(wt),
                    state = droplevels(haven::as_factor(ADM2)))
us <- us %>% mutate(wt_r = wt * n() / sum(wt),
                    state = droplevels(haven::as_factor(REGION_US_STATES)))

ng_zones <- list(
  "North Central" = c("BENUE","KOGI","KWARA","NASARAWA","NIGER","PLATEAU","FCT ABUJA"),
  "North East"    = c("ADAMAWA","BAUCHI","BORNO","GOMBE","TARABA","YOBE"),
  "North West"    = c("JIGAWA","KADUNA","KANO","KATSINA","KEBBI","SOKOTO","ZAMFARA"),
  "South East"    = c("ABIA","ANAMBRA","EBONYI","ENUGU","IMO"),
  "South South"   = c("AKWA IBOM","BAYELSA","CROSS RIVER","DELTA","EDO","RIVERS"),
  "South West"    = c("EKITI","LAGOS","OGUN","ONDO","OSUN","OYO")
)

us_regions <- list(
  "Northeast" = c("CONNECTICUT","MAINE","MASSACHUSETTS","NEW HAMPSHIRE","RHODE ISLAND",
                  "VERMONT","NEW JERSEY","NEW YORK","PENNSYLVANIA"),
  "Midwest"   = c("ILLINOIS","INDIANA","MICHIGAN","OHIO","WISCONSIN","IOWA","KANSAS",
                  "MINNESOTA","MISSOURI","NEBRASKA","NORTH DAKOTA","SOUTH DAKOTA"),
  "South"     = c("DELAWARE","FLORIDA","GEORGIA","MARYLAND","NORTH CAROLINA","SOUTH CAROLINA",
                  "VIRGINIA","WEST VIRGINIA","DISTRICT OF COLUMBIA","ALABAMA","KENTUCKY",
                  "MISSISSIPPI","TENNESSEE","ARKANSAS","LOUISIANA","OKLAHOMA","TEXAS"),
  "West"      = c("ARIZONA","COLORADO","IDAHO","MONTANA","NEVADA","NEW MEXICO","UTAH",
                  "WYOMING","ALASKA","CALIFORNIA","HAWAII","OREGON","WASHINGTON")
)

to_region <- function(x, lk) {
  s <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(s))
  for (r in names(lk)) out[s %in% lk[[r]]] <- r
  factor(out, levels = names(lk))
}

ng$region <- to_region(ng$state, ng_zones)
us$region <- to_region(us$state, us_regions)

ng_svy <- ng %>% as_survey_design(ids = psu, weights = wt_r)
us_svy <- us %>% as_survey_design(ids = 1,   weights = wt_r)

state_tbl <- function(d, design, lab, has_psu = TRUE) {
  base <- svyby(~confident, ~state, design, svymean, na.rm = TRUE, vartype = "ci") %>%
    as_tibble() %>%
    transmute(country = lab, state,
              pct = round(100 * confident, 1), lci = round(100 * ci_l, 1),
              uci = round(100 * ci_u, 1), width = round(100 * (ci_u - ci_l), 1))
  
  n_tbl <- d %>% filter(!is.na(confident)) %>% group_by(state) %>%
    summarise(n = n(),
              n_psu = if (has_psu) n_distinct(psu) else NA_integer_,
              .groups = "drop")
  if (!has_psu) n_tbl$n_psu <- NULL
  
  base %>% left_join(n_tbl, by = "state") %>% arrange(desc(width))
}

ng_states <- state_tbl(ng, ng_svy, "Nigeria", has_psu = TRUE)
us_states <- state_tbl(us, us_svy, "USA",     has_psu = FALSE)

cat("Survey design set. Region and state assigned.\n")

# =============================================================================
# SECTION 5 — SAMPLE CHARACTERISTICS (Table 1 object)
# =============================================================================
tbl1_data <- bind_rows(
  ng %>% mutate(country = "Nigeria"),
  us %>% mutate(country = "USA")
) %>%
  mutate(confident_f = factor(confident, 0:1, c("Not confident", "Confident"))) %>%
  dplyr::select(country, geotype, age_cat, child_age_cat, gender,
                education3, employment3, religion3, ses3,
                migrant, child_disab, confident_f)

tbl1 <- tbl1_data %>%
  tbl_summary(by = country,
              label = list(
                geotype ~ "Geotype", age_cat ~ "Caregiver age group",
                child_age_cat ~ "Age of index child", gender ~ "Gender",
                education3 ~ "Highest education", employment3 ~ "Employment status",
                religion3 ~ "Religion", ses3 ~ "Socioeconomic status",
                migrant ~ "Migrant status", child_disab ~ "Child disability",
                confident_f ~ "Vaccine confident (strongly agree important + strongly agree safe)"
              ),
              statistic = all_categorical() ~ "{n} ({p}%)",
              digits = all_categorical() ~ c(0, 1), missing = "no") %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. Characteristics of the study population**") %>%
  bold_labels()

# =============================================================================
# SECTION 6 — MISSING DATA AUDIT
# =============================================================================
vars <- c("confident","geotype","age_cat","child_age_cat","gender",
          "education","employment","religion","ses","ease_access","know_where",
          "hcw_rec","recall","turned_away","barrier_count","peer_norm",
          "family_norm","relig_norm","comm_norm","migrant","child_disab")

miss_tab <- tibble(
  Variable    = vars,
  Nigeria     = sapply(vars, function(v) sum(is.na(ng[[v]]))),
  Nigeria_pct = round(100 * sapply(vars, function(v) mean(is.na(ng[[v]]))), 1),
  USA         = sapply(vars, function(v) sum(is.na(us[[v]]))),
  USA_pct     = round(100 * sapply(vars, function(v) mean(is.na(us[[v]]))), 1)
)

m3c_vars <- c("confident","geotype","region","age_cat","child_age_cat","gender",
              "education3","employment3","religion3","ses3","ease_access",
              "know_where","hcw_rec","recall","turned_away","barrier_count",
              "peer_norm","family_norm","relig_norm","comm_norm")

cc_ng <- ng %>% dplyr::select(all_of(m3c_vars)) %>% drop_na() %>% nrow()
cc_us <- us %>% dplyr::select(all_of(m3c_vars)) %>% drop_na() %>% nrow()

cat("Complete cases (M3c vars) — NG:", cc_ng, "/", nrow(ng),
    " US:", cc_us, "/", nrow(us), "\n")

# =============================================================================
# SECTION 7 — BIVARIATE MODELS
# =============================================================================
all_predictors <- c("geotype","age_cat","child_age_cat","gender","education3",
                    "employment3","religion3","ses3","ease_access","know_where",
                    "hcw_rec","recall","turned_away","barrier_count",
                    "peer_norm","family_norm","relig_norm","comm_norm")

fit_bivar <- function(design, predictor, lab) {
  x <- design$variables[[predictor]]
  if (is.factor(x) && nlevels(droplevels(x[!is.na(x)])) < 2)
    return(tibble(country = lab, predictor = predictor, term = "(skipped)",
                  OR = NA, lci = NA, uci = NA, p = NA))
  m <- tryCatch(svyglm(as.formula(paste("confident ~", predictor)),
                       design = design, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m))
    return(tibble(country = lab, predictor = predictor, term = "(failed)",
                  OR = NA, lci = NA, uci = NA, p = NA))
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(country = lab, predictor, term, OR = round(estimate, 2),
              lci = round(conf.low, 2), uci = round(conf.high, 2),
              p = signif(p.value, 3))
}

ng_bv <- map_dfr(all_predictors, ~ fit_bivar(ng_svy, .x, "Nigeria"))
us_bv <- map_dfr(all_predictors, ~ fit_bivar(us_svy, .x, "USA"))

bv <- bind_rows(ng_bv, us_bv) %>%
  mutate(sig = ifelse(!is.na(p) & p <= 0.05, "*", ""))

side <- ng_bv %>%
  rename(NG_OR = OR, NG_lci = lci, NG_uci = uci, NG_p = p) %>%
  full_join(us_bv %>% rename(US_OR = OR, US_lci = lci, US_uci = uci, US_p = p),
            by = c("predictor","term")) %>%
  mutate(NG_sig = ifelse(!is.na(NG_p) & NG_p <= 0.05, "*", ""),
         US_sig = ifelse(!is.na(US_p) & US_p <= 0.05, "*", ""))

# Covariate vs geotype (confounder verification)
covars <- setdiff(all_predictors, "geotype")
exp_test <- function(d, predictor, lab) {
  x <- d[[predictor]]; g <- d$geotype
  ok <- !is.na(x) & !is.na(g); x <- x[ok]; g <- g[ok]
  tt <- if (is.factor(x) || is.character(x))
    chisq.test(table(x, g), simulate.p.value = TRUE, B = 2000)
  else kruskal.test(as.numeric(x) ~ g)
  tibble(country = lab, predictor = predictor,
         statistic = round(tt$statistic, 1), p = signif(tt$p.value, 3))
}

exp_assoc <- bind_rows(
  map_dfr(covars, ~ exp_test(ng, .x, "Nigeria")),
  map_dfr(covars, ~ exp_test(us, .x, "USA"))
)

cat("Bivariate models fitted.\n")

# =============================================================================
# SECTION 8 — MULTIVARIABLE MODELS
# =============================================================================
core_conf <- "age_cat + child_age_cat + gender + education3 + employment3 + religion3 + ses3"
access    <- "ease_access + know_where + hcw_rec + recall + turned_away + barrier_count"
norms     <- "peer_norm + family_norm + relig_norm + comm_norm"

f_m1      <- "confident ~ geotype"
f_m1r     <- "confident ~ geotype + region"
f_m2      <- paste("confident ~ geotype + region +", core_conf)
f_m3a     <- paste("confident ~ geotype + region +", core_conf, "+", access)
f_m3b     <- paste("confident ~ geotype + region +", core_conf, "+", norms)
f_m3c     <- paste("confident ~ geotype + region +", core_conf, "+", access, "+", norms)
f_m3c_ext <- paste(f_m3c, "+ migrant + child_disab")

formulas <- list("M1 crude" = f_m1, "M1 + region" = f_m1r, "M2 confounders" = f_m2,
                 "M3a + access" = f_m3a, "M3b + norms" = f_m3b, "M3c combined" = f_m3c)

fit_all <- function(design) {
  out <- list()
  for (nm in names(formulas)) {
    m <- tryCatch(svyglm(as.formula(formulas[[nm]]), design = design,
                         family = quasibinomial()), error = function(e) NULL)
    if (!is.null(m)) out[[nm]] <- m
  }
  out
}

ng_fits <- fit_all(ng_svy)
us_fits <- fit_all(us_svy)

extract_geo <- function(fit_list, lab) {
  imap_dfr(fit_list, function(m, nm) {
    tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(grepl("geotype", term)) %>%
      transmute(country = lab, model = nm, term, OR = round(estimate, 2),
                lci = round(conf.low, 2), uci = round(conf.high, 2),
                p = signif(p.value, 3), sig = ifelse(p.value <= 0.05, "*", ""))
  })
}

geo_across <- bind_rows(extract_geo(ng_fits, "Nigeria"), extract_geo(us_fits, "USA"))

audit_row <- function(fit_list, lab) {
  imap_dfr(fit_list, function(m, nm) {
    aic <- tryCatch(AIC(m)[["AIC"]], error = function(e) NA)
    tibble(country = lab, model = nm, df = m$df.residual,
           dev = round(m$deviance, 1), null_dev = round(m$null.deviance, 1),
           AIC = round(aic, 1))
  })
}

audit_tbl <- bind_rows(audit_row(ng_fits, "Nigeria"), audit_row(us_fits, "USA"))

gvif_ng <- vif(ng_fits[["M3c combined"]])
gvif_us <- vif(us_fits[["M3c combined"]])

fit_interaction <- function(design, base_formula, interaction, lab) {
  f_int <- paste(base_formula, "+", interaction)
  m <- tryCatch(svyglm(as.formula(f_int), design = design, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m))
    return(tibble(country = lab, interaction = interaction, term = "(failed)", OR = NA, p = NA))
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(":", term)) %>%
    transmute(country = lab, interaction, term, OR = round(estimate, 2),
              lci = round(conf.low, 2), uci = round(conf.high, 2),
              p = signif(p.value, 3), sig = ifelse(p.value <= 0.05, "*", ""))
}

interaction_results <- bind_rows(
  fit_interaction(ng_svy, f_m2, "geotype:ses3",      "Nigeria"),
  fit_interaction(ng_svy, f_m2, "geotype:religion3", "Nigeria"),
  fit_interaction(us_svy, f_m2, "geotype:ses3",      "USA"),
  fit_interaction(us_svy, f_m2, "geotype:religion3", "USA")
)

peri_contrast <- function(design, lab) {
  d <- design$variables
  d$geotype_p <- relevel(d$geotype, ref = "Peri-urban")
  design_p <- if (lab == "Nigeria")
    as_survey_design(d, ids = psu, weights = wt_r)
  else as_survey_design(d, ids = 1, weights = wt_r)
  m <- svyglm(as.formula(gsub("geotype", "geotype_p", f_m2)),
              design = design_p, family = quasibinomial())
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("geotype_p", term)) %>%
    transmute(country = lab, term, OR = round(estimate, 2),
              lci = round(conf.low, 2), uci = round(conf.high, 2),
              p = signif(p.value, 3))
}

peri_results <- bind_rows(peri_contrast(ng_svy, "Nigeria"), peri_contrast(us_svy, "USA"))

extract_all <- function(fit_list, model_name, lab) {
  m <- fit_list[[model_name]]
  if (is.null(m)) return(NULL)
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(country = lab, model = model_name, term, OR = round(estimate, 2),
              lci = round(conf.low, 2), uci = round(conf.high, 2),
              p = signif(p.value, 3), sig = ifelse(p.value <= 0.05, "*", ""))
}

full_m2  <- bind_rows(extract_all(ng_fits, "M2 confounders", "Nigeria"),
                      extract_all(us_fits, "M2 confounders", "USA"))
full_m3c <- bind_rows(extract_all(ng_fits, "M3c combined",   "Nigeria"),
                      extract_all(us_fits, "M3c combined",   "USA"))

cat("Multivariable models fitted.\n")

# =============================================================================
# SECTION 9 — SENSITIVITY ANALYSES
# =============================================================================
fit_sensitivity <- function(dat, outcome_var, formula_str, model_name, lab,
                            sensitivity_name, design_ids, weights = TRUE) {
  dat$confident <- dat[[outcome_var]]
  dsn <- if (weights) {
    if (design_ids == "psu") as_survey_design(dat, ids = psu, weights = wt_r)
    else as_survey_design(dat, ids = 1, weights = wt_r)
  } else as_survey_design(dat, ids = 1, weights = NULL)
  
  m <- tryCatch(svyglm(as.formula(formula_str), design = dsn, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m))
    return(tibble(country = lab, sensitivity = sensitivity_name, model = model_name,
                  term = "(failed)", n = nrow(dat), OR = NA, lci = NA, uci = NA, p = NA))
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("geotype", term)) %>%
    transmute(country = lab, sensitivity = sensitivity_name, model = model_name, term,
              n = length(m$y), OR = round(estimate, 2), lci = round(conf.low, 2),
              uci = round(conf.high, 2), p = signif(p.value, 3))
}

run_pair <- function(dat, outcome_var, lab, sensitivity_name, design_ids, weights = TRUE) {
  bind_rows(
    fit_sensitivity(dat, outcome_var, f_m3a, "M3a", lab, sensitivity_name, design_ids, weights),
    fit_sensitivity(dat, outcome_var, f_m3c, "M3c", lab, sensitivity_name, design_ids, weights)
  )
}

# Country-specific sensitivity block (same 11 specs, differ only in
# small-state threshold and design_ids)
build_sens <- function(dat, lab, design_ids, small_state_filter) {
  bind_rows(
    run_pair(dat, "confident", lab, "Primary", design_ids),
    run_pair(dat %>% drop_na(all_of(m3c_vars)), "confident", lab, "S1. Complete cases", design_ids),
    run_pair(dat, "confident_top2",  lab, "S2. Top-2-box",       design_ids),
    run_pair(dat, "conf_importance", lab, "S3. Importance only", design_ids),
    run_pair(dat, "conf_safety",     lab, "S4. Safety only",     design_ids),
    run_pair(dat %>% filter(migrant == "No" | is.na(migrant)),
             "confident", lab, "S5. Excl. migrants", design_ids),
    run_pair(dat %>% filter(child_disab == "No" | is.na(child_disab)),
             "confident", lab, "S6. Excl. child disability", design_ids),
    run_pair(dat, "confident", lab, "S7. Unweighted", design_ids, weights = FALSE),
    run_pair(dat %>% filter(state %in% small_state_filter),
             "confident", lab, "S8. Small-sample states excluded", design_ids),
    run_pair(dat %>% filter(n_children == 1), "confident", lab, "S9. 1 child only", design_ids),
    run_pair(dat %>% filter(n_children >= 2), "confident", lab, "S10. 2+ children", design_ids),
    run_pair(dat %>% filter(religion3 != "No religion" | is.na(religion3)),
             "confident", lab, "S11. Excl. No religion", design_ids)
  )
}

ng_keep_states <- ng %>% group_by(state) %>%
  summarise(n_psu = n_distinct(psu), .groups = "drop") %>%
  filter(n_psu >= 15) %>% pull(state)

us_keep_states <- us %>% group_by(state) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 50) %>% pull(state)

ng_sens <- build_sens(ng, "Nigeria", "psu",  ng_keep_states)
us_sens <- build_sens(us, "USA",     "none", us_keep_states)
sens_all <- bind_rows(ng_sens, us_sens)

primary_or <- sens_all %>% filter(sensitivity == "Primary") %>%
  dplyr::select(country, model, term, primary_OR = OR)

sens_flagged <- sens_all %>%
  left_join(primary_or, by = c("country","model","term")) %>%
  mutate(direction_match = sign(log(OR)) == sign(log(primary_OR)),
         within_range    = abs(OR - primary_OR) <= 0.15,
         consistent      = ifelse(direction_match & within_range, "OK",
                                  ifelse(!direction_match, "FLIP", "SHIFT")))

# Pooled country x geotype interaction
pool <- bind_rows(
  ng %>% mutate(country = "Nigeria") %>%
    dplyr::select(country, confident, geotype, region, age_cat, child_age_cat,
                  gender, education3, employment3, religion3, ses3),
  us %>% mutate(country = "USA") %>%
    dplyr::select(country, confident, geotype, region, age_cat, child_age_cat,
                  gender, education3, employment3, religion3, ses3)
) %>% mutate(country = factor(country, levels = c("Nigeria","USA")))

m_main <- glm(confident ~ country + geotype + age_cat + child_age_cat + gender +
                education3 + employment3 + religion3 + ses3,
              data = pool, family = binomial())
m_int  <- glm(confident ~ country * geotype + age_cat + child_age_cat + gender +
                education3 + employment3 + religion3 + ses3,
              data = pool, family = binomial())

lrt_country_geotype <- anova(m_main, m_int, test = "LRT")
int_terms <- tidy(m_int, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl(":", term)) %>%
  transmute(term, OR = round(estimate, 2), lci = round(conf.low, 2),
            uci = round(conf.high, 2), p = signif(p.value, 3),
            sig = ifelse(p.value <= 0.05, "*", ""))

# Bonferroni + FDR on the four primary outcome definitions
n_tests <- 16
primary_tests <- sens_flagged %>%
  filter(model == "M3c",
         sensitivity %in% c("Primary","S2. Top-2-box","S3. Importance only","S4. Safety only")) %>%
  mutate(p_bonf   = pmin(p * n_tests, 1),
         sig_bonf = ifelse(p_bonf <= 0.05, "*", ""),
         sig_raw  = ifelse(p <= 0.05, "*", ""),
         p_fdr    = p.adjust(p, method = "BH"),
         sig_fdr  = ifelse(p_fdr <= 0.05, "*", "")) %>%
  dplyr::select(country, sensitivity, term, OR, lci, uci, p,
                sig_raw, p_bonf, sig_bonf, p_fdr, sig_fdr)

cat("Sensitivity analyses complete.\n")

# =============================================================================
# SECTION 10 — VULNERABLE GROUPS
# =============================================================================
desc_by_group <- function(design, group_var, lab) {
  f <- as.formula(paste("~", group_var))
  svyby(~confident, f, design, svymean, na.rm = TRUE, vartype = "ci") %>%
    as_tibble() %>%
    left_join(design$variables %>% group_by(across(all_of(group_var))) %>%
                summarise(n = n(), .groups = "drop"), by = group_var) %>%
    transmute(country = lab, group = group_var, status = .data[[group_var]], n = n,
              pct = round(100 * confident, 1), lci = round(100 * ci_l, 1),
              uci = round(100 * ci_u, 1))
}

desc_tbl <- bind_rows(
  desc_by_group(ng_svy, "migrant",     "Nigeria"),
  desc_by_group(ng_svy, "child_disab", "Nigeria"),
  desc_by_group(us_svy, "migrant",     "USA"),
  desc_by_group(us_svy, "child_disab", "USA")
)

fit_extended <- function(design, lab) {
  m <- svyglm(as.formula(f_m3c_ext), design = design, family = quasibinomial())
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("migrant|child_disab", term)) %>%
    transmute(country = lab, term, OR = round(estimate, 2),
              lci = round(conf.low, 2), uci = round(conf.high, 2),
              p = signif(p.value, 3), sig = ifelse(p.value <= 0.05, "*", ""))
}

extended_results <- bind_rows(fit_extended(ng_svy, "Nigeria"), fit_extended(us_svy, "USA"))

test_interaction <- function(design, interaction_term, lab) {
  f_int <- paste(f_m3c, "+", interaction_term)
  m <- tryCatch(svyglm(as.formula(f_int), design = design, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(":", term)) %>%
    transmute(country = lab, interaction = interaction_term, term,
              OR = round(estimate, 2), lci = round(conf.low, 2),
              uci = round(conf.high, 2), p = signif(p.value, 3),
              sig = ifelse(p.value <= 0.05, "*", ""))
}

int_results <- bind_rows(
  test_interaction(ng_svy, "geotype:migrant",     "Nigeria"),
  test_interaction(ng_svy, "geotype:child_disab", "Nigeria"),
  test_interaction(us_svy, "geotype:migrant",     "USA"),
  test_interaction(us_svy, "geotype:child_disab", "USA")
)

fit_subgroup <- function(dat, lab, subgroup_name, design_ids) {
  n_sub <- nrow(dat)
  formula_use <- if (n_sub < 200) f_m2 else f_m3c
  model_used  <- if (n_sub < 200) "M2 (small n)" else "M3c"
  dsn <- if (design_ids == "psu") as_survey_design(dat, ids = psu, weights = wt_r)
  else as_survey_design(dat, ids = 1, weights = wt_r)
  m <- tryCatch(svyglm(as.formula(formula_use), design = dsn, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("geotype", term)) %>%
    transmute(country = lab, subgroup = subgroup_name, n = n_sub, model = model_used, term,
              OR = round(estimate, 2), lci = round(conf.low, 2),
              uci = round(conf.high, 2), p = signif(p.value, 3))
}

subgroup_results <- bind_rows(
  fit_subgroup(ng %>% filter(migrant == "Yes"),     "Nigeria", "Migrants",             "psu"),
  fit_subgroup(ng %>% filter(migrant == "No"),      "Nigeria", "Non-migrants",         "psu"),
  fit_subgroup(ng %>% filter(child_disab == "Yes"), "Nigeria", "Child disability",     "psu"),
  fit_subgroup(ng %>% filter(child_disab == "No"),  "Nigeria", "No child disability",  "psu"),
  fit_subgroup(us %>% filter(migrant == "Yes"),     "USA",     "Migrants",             "none"),
  fit_subgroup(us %>% filter(migrant == "No"),      "USA",     "Non-migrants",         "none"),
  fit_subgroup(us %>% filter(child_disab == "Yes"), "USA",     "Child disability",     "none"),
  fit_subgroup(us %>% filter(child_disab == "No"),  "USA",     "No child disability",  "none")
) %>%
  mutate(diverged = abs(log(OR)) > 15) %>%
  { if (any(.$diverged))
    cat("WARNING: dropping diverged subgroup rows:\n",
        paste(capture.output(print(filter(., diverged))), collapse = "\n"), "\n"); . } %>%
  filter(!diverged) %>%
  dplyr::select(-diverged)

compare_side <- subgroup_results %>%
  mutate(vuln = case_when(
    grepl("^Migrants", subgroup) ~ "vulnerable",
    grepl("Non-migrants", subgroup) ~ "reference",
    grepl("^Child disability", subgroup) ~ "vulnerable",
    grepl("^No child disability", subgroup) ~ "reference"
  ), group = case_when(
    grepl("migrant", subgroup, ignore.case = TRUE) ~ "Migration",
    grepl("disab", subgroup, ignore.case = TRUE) ~ "Disability"
  )) %>%
  dplyr::select(country, group, vuln, term, OR, lci, uci, p, n)

table8 <- desc_tbl %>%
  transmute(country, group, status, n, pct, lci, uci) %>%
  left_join(
    extended_results %>%
      mutate(group = ifelse(grepl("migrant", term), "migrant", "child_disab")) %>%
      dplyr::select(country, group, adj_OR = OR, adj_lci = lci, adj_uci = uci, p),
    by = c("country", "group")
  ) %>%
  mutate(across(c(adj_OR, adj_lci, adj_uci, p), ~ ifelse(status == "No", NA, .)))

cat("Vulnerable group analyses complete.\n")

# =============================================================================
# SECTION 11 — MULTILEVEL MODELS + G-COMPUTATION
# =============================================================================
add_state_re <- function(formula_str) paste(formula_str, "+ (1 | state)")

fit_ml <- function(f_str, data) {
  tryCatch(glmer(as.formula(add_state_re(f_str)), data = data, family = binomial(),
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 100000))),
           error = function(e) NULL)
}

fit_multilevel_set <- function(data) {
  fs <- list("M2 confounders" = f_m2, "M3a + access" = f_m3a,
             "M3b + norms"    = f_m3b, "M3c combined" = f_m3c)
  Filter(Negate(is.null), lapply(fs, fit_ml, data = data))
}

ng_ml <- fit_multilevel_set(ng)
us_ml <- fit_multilevel_set(us)

ng_ml_ext <- fit_ml(f_m3c_ext, ng)
us_ml_ext <- fit_ml(f_m3c_ext, us)

variance_row <- function(fit, model_name, lab) {
  vc <- VarCorr(fit)
  state_var <- as.numeric(vc$state[1])
  icc <- state_var / (state_var + (pi^2 / 3))
  tibble(country = lab, model = model_name,
         state_sd = round(sqrt(state_var), 3),
         state_var = round(state_var, 3),
         ICC = round(icc, 3),
         ICC_pct = paste0(round(100 * icc, 1), "%"))
}

variance_tbl <- bind_rows(imap_dfr(ng_ml, ~ variance_row(.x, .y, "Nigeria")),
                          imap_dfr(us_ml, ~ variance_row(.x, .y, "USA")))

extract_geo_ml <- function(fit, model_name, lab) {
  s <- summary(fit)$coefficients
  keep <- grep("geotype", rownames(s))
  tibble(country = lab, model = model_name, term = rownames(s)[keep],
         OR  = round(exp(s[keep, 1]), 2),
         lci = round(exp(s[keep, 1] - 1.96 * s[keep, 2]), 2),
         uci = round(exp(s[keep, 1] + 1.96 * s[keep, 2]), 2),
         p   = signif(s[keep, 4], 3))
}

geo_ml <- bind_rows(imap_dfr(ng_ml, ~ extract_geo_ml(.x, .y, "Nigeria")),
                    imap_dfr(us_ml, ~ extract_geo_ml(.x, .y, "USA")))

# G-computation
apply_cf <- function(data_cc, cf_change) {
  data_cf <- data_cc
  for (v in names(cf_change)) if (v %in% names(data_cf)) {
    data_cf[[v]] <- if (is.factor(data_cf[[v]]))
      factor(cf_change[[v]], levels = levels(data_cf[[v]])) else cf_change[[v]]
  }
  data_cf
}

g_compute <- function(fit, data, cf_change, scenario_name, lab) {
  vars_needed <- all.vars(formula(fit))
  data_cc <- data %>% dplyr::select(any_of(vars_needed)) %>% na.omit()
  data_cf <- apply_cf(data_cc, cf_change)
  p_actual <- predict(fit, newdata = data_cc, type = "response",
                      allow.new.levels = TRUE, re.form = NA)
  p_cf     <- predict(fit, newdata = data_cf, type = "response",
                      allow.new.levels = TRUE, re.form = NA)
  tibble(country = lab, scenario = scenario_name, n_used = length(p_actual),
         mean_actual = round(100 * mean(p_actual), 1),
         mean_cf     = round(100 * mean(p_cf), 1),
         diff_pp     = round(100 * mean(p_cf - p_actual), 2))
}

# Scenario list — country label attached at call site
scenarios <- list(
  list(cf = list(hcw_rec     = "Yes"),   name = "Universal HCW recommendation", ext = FALSE),
  list(cf = list(ease_access = "Very"),  name = "Universal easy access",        ext = FALSE),
  list(cf = list(turned_away = "No"),    name = "No one turned away",           ext = FALSE),
  list(cf = list(geotype     = "Urban"), name = "Everyone urban residence",     ext = FALSE),
  list(cf = list(child_disab = "No"),    name = "Eliminate child disability",   ext = TRUE),
  list(cf = list(migrant     = "No"),    name = "Eliminate migrant status",     ext = TRUE)
)

run_gcomp <- function(fit_std, fit_ext, data, lab) {
  map_dfr(scenarios, function(s)
    g_compute(if (s$ext) fit_ext else fit_std, data, s$cf, s$name, lab))
}

gcomp_results <- bind_rows(
  run_gcomp(ng_ml[["M3c combined"]], ng_ml_ext, ng, "Nigeria"),
  run_gcomp(us_ml[["M3c combined"]], us_ml_ext, us, "USA")
) %>% arrange(country, scenario)

# Parametric bootstrap CIs
param_boot_gcomp <- function(fit, data, cf_change, scenario_name, lab) {
  vars_needed <- all.vars(formula(fit))
  data_cc <- data %>% dplyr::select(any_of(vars_needed)) %>% na.omit()
  data_cf <- apply_cf(data_cc, cf_change)
  X_actual <- model.matrix(delete.response(terms(fit)), data_cc)
  X_cf     <- model.matrix(delete.response(terms(fit)), data_cf)
  beta_sim <- MASS::mvrnorm(n_sim, mu = fixef(fit), Sigma = as.matrix(vcov(fit)))
  diffs <- sapply(seq_len(n_sim), function(b)
    100 * mean(plogis(X_cf %*% beta_sim[b, ]) - plogis(X_actual %*% beta_sim[b, ])))
  tibble(country = lab, scenario = scenario_name,
         diff_pp = round(mean(diffs), 2),
         lci     = round(quantile(diffs, 0.025), 2),
         uci     = round(quantile(diffs, 0.975), 2))
}

run_boot <- function(fit_std, fit_ext, data, lab) {
  map_dfr(scenarios, function(s)
    param_boot_gcomp(if (s$ext) fit_ext else fit_std, data, s$cf, s$name, lab))
}

boot_results <- bind_rows(
  run_boot(ng_ml[["M3c combined"]], ng_ml_ext, ng, "Nigeria"),
  run_boot(us_ml[["M3c combined"]], us_ml_ext, us, "USA")
) %>% arrange(country, scenario)

extract_state_re <- function(fit, lab) {
  re <- ranef(fit, condVar = TRUE)$state
  se <- sqrt(attr(re, "postVar")[1, 1, ])
  tibble(country = lab, state = rownames(re),
         intercept = round(re[, 1], 3), se = round(se, 3),
         lci = round(re[, 1] - 1.96 * se, 3),
         uci = round(re[, 1] + 1.96 * se, 3))
}

state_re <- bind_rows(extract_state_re(ng_ml[["M3c combined"]], "Nigeria"),
                      extract_state_re(us_ml[["M3c combined"]], "USA"))

# Robustness check: does USA ICC appear without region as fixed effect?
us_ml_no_region <- glmer(paste(gsub(" \\+ region", "", f_m3c), "+ (1 | state)"),
                         data = us, family = binomial(),
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 100000)))
us_icc_no_region_singular   <- isSingular(us_ml_no_region)
us_icc_with_region_singular <- isSingular(us_ml[["M3c combined"]])

cat("Multilevel + G-computation complete. USA singular (no region):",
    us_icc_no_region_singular, " (with region):", us_icc_with_region_singular, "\n")

# =============================================================================
# SECTION 12 — SUPPLEMENTARY: POWER, CORRELATION, MEDIATED PROPORTION,
#              NORM CONCORDANCE
# =============================================================================
# --- Power ---
ng_ns <- ng %>% count(geotype) %>% pull(n, name = geotype)
us_ns <- us %>% count(geotype) %>% pull(n, name = geotype)
p_urban_ng <- mean(ng$confident[ng$geotype == "Urban"], na.rm = TRUE)
p_urban_us <- mean(us$confident[us$geotype == "Urban"], na.rm = TRUE)

min_detectable_or <- function(n1, n2, p_baseline, power_target = 0.80) {
  target_or_search <- seq(0.5, 2.0, by = 0.01)
  powers <- sapply(target_or_search, function(or) {
    odds_baseline <- p_baseline / (1 - p_baseline)
    p_alt <- (or * odds_baseline) / (1 + or * odds_baseline)
    h <- ES.h(p_alt, p_baseline)
    tryCatch(pwr.2p2n.test(h = h, n1 = n1, n2 = n2, sig.level = 0.05)$power,
             error = function(e) NA)
  })
  above1 <- target_or_search[target_or_search > 1 & powers >= power_target]
  below1 <- target_or_search[target_or_search < 1 & powers >= power_target]
  list(min_or_above = if (length(above1) == 0) NA else min(above1),
       max_or_below = if (length(below1) == 0) NA else max(below1))
}

power_tbl <- bind_rows(lapply(c(0.80, 0.90), function(pt) {
  ng_res <- min_detectable_or(ng_ns["Rural"], ng_ns["Urban"], p_urban_ng, pt)
  us_res <- min_detectable_or(us_ns["Rural"], us_ns["Urban"], p_urban_us, pt)
  tibble(power_target = pt,
         ng_or_below = ng_res$max_or_below, ng_or_above = ng_res$min_or_above,
         us_or_below = us_res$max_or_below, us_or_above = us_res$min_or_above)
}))

# --- Correlation matrix ---
pred_vars <- c("age_cat","child_age_cat","gender","education3","employment3","religion3",
               "ses3","ease_access","know_where","hcw_rec","recall","turned_away",
               "barrier_count","peer_norm","family_norm","relig_norm","comm_norm")

make_numeric_df <- function(d, vars)
  d %>% dplyr::select(all_of(vars)) %>% mutate(across(everything(), ~ as.numeric(.)))

cor_ng <- cor(make_numeric_df(ng, pred_vars), use = "pairwise.complete.obs", method = "spearman")
cor_us <- cor(make_numeric_df(us, pred_vars), use = "pairwise.complete.obs", method = "spearman")

# --- Mediated proportion (uses ML fits) ---
extract_log_or <- function(fit, term) {
  s <- summary(fit)$coefficients
  row <- grep(paste0("^", term, "$"), rownames(s))
  if (length(row) == 0) return(NA)
  s[row, 1]
}

# pm() suppresses when M2 total is too close to null (|log-OR| < 0.15) and
# flags |pct| > 100 as unstable — known limitation of difference-in-coefficients.
pm <- function(total, direct) {
  if (is.na(total) | is.na(direct)) return(list(val = NA, flag = "\u2014"))
  if (abs(total) < 0.15)
    return(list(val = NA, flag = "Total effect \u2248 0 (not interpretable)"))
  pct <- round(100 * (total - direct) / total, 1)
  if (abs(pct) > 100) return(list(val = pct, flag = "Unstable"))
  list(val = pct, flag = "")
}

mediation_table <- function(fit_list, lab) {
  get <- function(mod, term) extract_log_or(fit_list[[mod]], term)
  contrasts <- list(
    "Peri-urban vs Urban" = "geotypePeri-urban",
    "Rural vs Urban"      = "geotypeRural"
  )
  via_map <- list("Access only (M2\u2192M3a)" = "M3a + access",
                  "Norms only (M2\u2192M3b)"  = "M3b + norms",
                  "Both (M2\u2192M3c)"        = "M3c combined")
  map_dfr(names(contrasts), function(ct) {
    total <- get("M2 confounders", contrasts[[ct]])
    map_dfr(names(via_map), function(via) {
      r <- pm(total, get(via_map[[via]], contrasts[[ct]]))
      tibble(country = lab, contrast = ct, via = via,
             pct_mediated = r$val, flag = r$flag)
    })
  })
}

med_tbl <- bind_rows(mediation_table(ng_ml, "Nigeria"),
                     mediation_table(us_ml, "USA"))

# --- Community x religious norm concordance ---
agree_ng <- mean(ng$relig_norm == ng$comm_norm, na.rm = TRUE)
agree_us <- mean(us$relig_norm == us$comm_norm, na.rm = TRUE)

f_m3c_no_comm  <- gsub(" \\+ comm_norm", "", f_m3c)
m_ng_no_comm   <- svyglm(as.formula(f_m3c_no_comm), design = ng_svy, family = quasibinomial())
m_us_no_comm   <- svyglm(as.formula(f_m3c_no_comm), design = us_svy, family = quasibinomial())

comm_norm_check <- tibble(
  country               = c("Nigeria", "USA"),
  agreement_pct         = round(100 * c(agree_ng, agree_us), 1),
  rural_OR_without_comm = round(exp(c(coef(m_ng_no_comm)["geotypeRural"],
                                      coef(m_us_no_comm)["geotypeRural"])), 2),
  rural_OR_with_comm    = round(exp(c(coef(ng_fits[["M3c combined"]])["geotypeRural"],
                                      coef(us_fits[["M3c combined"]])["geotypeRural"])), 2)
)

# --- B7 skip pattern (non-religious respondents) ---
b7_skip_check <- bind_rows(
  ng %>% group_by(religion3) %>%
    summarise(n = n(), na_relig_norm = sum(is.na(relig_norm)),
              pct_na = round(100 * mean(is.na(relig_norm)), 1), .groups = "drop") %>%
    mutate(country = "Nigeria"),
  us %>% group_by(religion3) %>%
    summarise(n = n(), na_relig_norm = sum(is.na(relig_norm)),
              pct_na = round(100 * mean(is.na(relig_norm)), 1), .groups = "drop") %>%
    mutate(country = "USA")
)

cat("Supplementary checks complete.\n")

# =============================================================================
# SAVE WORKSPACE
# =============================================================================
save.image("analysis_workspace.RData")
cat("\n===== 01_PIPELINE.R complete. Saved analysis_workspace.RData =====\n")
#END PHASE 1