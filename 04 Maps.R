# =============================================================================
# 04_MAPS.R
# =============================================================================
# Loads analysis_workspace.RData and builds all GIS maps. Separate from
# 03_figures.R because GADM shapefile downloads are the slowest and most
# failure-prone step (network-dependent).
#
# Outputs:
#   phase6_ng_residual_confidence_map.png/.pdf   — Fig 6, adjusted Nigeria
#   phase6_ng_raw_confidence_map.png/.pdf        — Fig 6b, raw Nigeria
#   figure_ng_rural_distribution_map.png         — Fig 7
#   phase6b_us_raw_confidence_map.png            — Fig 8, raw USA
#   phase6b_us_ci_width_map.png                  — Fig 9, USA uncertainty
#   figure_ng_regional_geotype_facets.png/.pdf   — Fig 10, crude NG regional
#   figure_us_regional_geotype_facets.png/.pdf   — Fig 11, crude USA regional
#   figure_ng_regional_geotype_adjusted.png      — adjusted NG regional
#   figure_us_regional_geotype_adjusted.png      — adjusted USA regional
#   figure_ng_regional_model_sequence.png        — NG crude/M2/M3c row
#   figure_us_regional_model_sequence.png        — USA crude/M2/M3c row
#   figure_ng_regional_gcomp_access.png          — NG "very easy access" gain
#   figure_us_regional_gcomp_access.png          — USA "very easy access" gain
#
# USA gets no residual map: ICC ~ 0 in every multilevel spec means there
# is no residual state-level variation left to plot after adjustment.
# =============================================================================
rm(list = ls())
setwd("/Users/macuser/IMMS Datasets")

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sf)
library(geodata)
library(survey)
library(srvyr)

load("analysis_workspace.RData")

# =============================================================================
# HELPERS
# =============================================================================
save_gg <- function(p, fname, width, height, dpi = 300, also_pdf = FALSE) {
  ggsave(paste0(fname, ".png"), p, width = width, height = height, dpi = dpi, bg = "white")
  if (also_pdf) ggsave(paste0(fname, ".pdf"), p, width = width, height = height, bg = "white")
}

# Persistent-cache GADM helper with retry (so a successful download is
# reused across sessions rather than re-fetched every run).
gadm_cache_dir <- file.path(getwd(), "gadm_cache")
if (!dir.exists(gadm_cache_dir)) dir.create(gadm_cache_dir, recursive = TRUE)

get_gadm <- function(country_code, level = 1, cache_path = gadm_cache_dir,
                     max_attempts = 4, wait_seconds = 10) {
  for (attempt in seq_len(max_attempts)) {
    shp <- tryCatch(
      geodata::gadm(country = country_code, level = level, path = cache_path),
      error = function(e) {
        cat("  Attempt", attempt, "errored:", conditionMessage(e), "\n"); NULL
      }
    )
    if (!is.null(shp)) return(sf::st_as_sf(shp))
    cat("  Attempt", attempt, "returned no data.\n")
    if (attempt < max_attempts) {
      cat("  Retrying in", wait_seconds, "seconds...\n")
      Sys.sleep(wait_seconds)
    }
  }
  stop("\nCould not download '", country_code, "' level-", level, " shapefile from GADM after ",
       max_attempts, " attempts. Cache: '", cache_path, "'. Re-run later once server recovers, ",
       "or manually place the '_", level, "_sf.rds' file in the cache dir.", call. = FALSE)
}

# Shared white-background void theme for every regional map
map_theme_regional <- theme_void(base_size = 11) +
  theme(plot.background   = element_rect(fill = "white", colour = NA),
        panel.background  = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key        = element_rect(fill = "white", colour = NA),
        strip.background  = element_rect(fill = "white", colour = NA),
        panel.spacing     = unit(1.2, "lines"),
        plot.title        = element_text(face = "bold", size = 14),
        strip.text        = element_text(face = "bold", size = 12),
        legend.position   = "bottom")

# Fix Nigerian state names for FCT (shapefile uses long form)
fix_ng_state <- function(x) {
  ifelse(toupper(x) == "FEDERAL CAPITAL TERRITORY", "FCT ABUJA", toupper(x))
}

# =============================================================================
# NIGERIA SHAPEFILE
# =============================================================================
cat("Loading Nigeria state shapefile...\n")
ng_shape <- get_gadm("NGA", level = 1)

# =============================================================================
# FIGURE 6 — NIGERIA RESIDUAL CONFIDENCE (adjusted, Model 3c)
# =============================================================================
ng_intercepts <- state_re %>%
  filter(country == "Nigeria") %>%
  mutate(state_upper = fix_ng_state(state))

ng_shape_data <- ng_shape %>%
  mutate(NAME_1_upper = toupper(NAME_1)) %>%
  left_join(ng_intercepts, by = c("NAME_1_upper" = "state_upper"))

missing <- ng_shape_data %>% filter(is.na(intercept)) %>% pull(NAME_1)
if (length(missing) > 0) { cat("WARNING: unmatched GADM states:\n"); print(missing) }

map_intercepts <- ggplot(ng_shape_data) +
  geom_sf(aes(fill = intercept), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    name = "Residual\nconfidence\n(log-odds)",
    low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
    midpoint = 0, na.value = "grey85"
  ) +
  labs(title = "Nigerian state-level residual vaccine confidence") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

save_gg(map_intercepts, "phase6_ng_residual_confidence_map", 8, 7, also_pdf = TRUE)

# =============================================================================
# FIGURE 6b — NIGERIA RAW CONFIDENCE (unadjusted)
# =============================================================================
ng_raw_map_data <- ng_shape %>%
  mutate(NAME_1_upper = toupper(NAME_1)) %>%
  left_join(ng_states %>% mutate(state_upper = fix_ng_state(state)),
            by = c("NAME_1_upper" = "state_upper"))

map_raw <- ggplot(ng_raw_map_data) +
  geom_sf(aes(fill = pct), color = "white", linewidth = 0.2) +
  scale_fill_gradient(name = "% confident\n(weighted)", low = "#FEE5D9", high = "#A50F15",
                      na.value = "grey85", limits = c(30, 90)) +
  labs(title = "Nigerian state-level vaccine confidence (unadjusted)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14))

save_gg(map_raw, "phase6_ng_raw_confidence_map", 8, 7, also_pdf = TRUE)

# =============================================================================
# FIGURE 7 — NIGERIA RURAL-RESIDENCE DISTRIBUTION
# =============================================================================
ng_rural_pct <- ng %>%
  group_by(state) %>%
  summarise(pct_rural = round(100 * mean(geotype == "Rural", na.rm = TRUE), 1),
            .groups = "drop")

ng_rural_map_data <- ng_shape %>%
  mutate(NAME_1_upper = toupper(NAME_1)) %>%
  left_join(ng_rural_pct %>% mutate(state_upper = fix_ng_state(state)),
            by = c("NAME_1_upper" = "state_upper"))

p_rural_map <- ggplot(ng_rural_map_data) +
  geom_sf(aes(fill = pct_rural), color = "white", linewidth = 0.2) +
  scale_fill_gradient(name = "% rural", low = "#FFF7EC", high = "#7F2704", na.value = "grey85") +
  labs(title = "Nigeria \u2014 Distribution of rural residence by state") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14))

save_gg(p_rural_map, "figure_ng_rural_distribution_map", 8, 7)

# =============================================================================
# USA SHAPEFILE
# =============================================================================
cat("Loading USA state shapefile...\n")
us_shape <- get_gadm("USA", level = 1) %>%
  filter(NAME_1 %in% c(state.name, "District of Columbia"))

us_map_data <- us_shape %>%
  mutate(state_upper = toupper(NAME_1)) %>%
  left_join(us_states %>% mutate(state_upper = toupper(as.character(state))),
            by = "state_upper") %>%
  mutate(ci_width = uci - lci)

missing <- us_map_data %>% filter(is.na(pct)) %>% pull(NAME_1)
if (length(missing) > 0) {
  cat("WARNING: unmatched states (expected: DC has no IMMS data):\n"); print(missing)
}

# =============================================================================
# FIGURE 8 — USA RAW CONFIDENCE (unadjusted, descriptive)
# =============================================================================
# Descriptive only. ICC ~ 0 in every multilevel spec means all state-level
# variation shown here is compositional, not contextual.
map_us_raw <- ggplot(us_map_data) +
  geom_sf(aes(fill = pct), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(name = "% confident\n(weighted)", option = "plasma",
                       direction = -1, na.value = "grey85") +
  coord_sf(crs = 5070) +
  labs(title = "US state-level vaccine confidence (unadjusted)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "right")

save_gg(map_us_raw, "phase6b_us_raw_confidence_map", 10, 6.5)

# =============================================================================
# FIGURE 9 — USA CI-WIDTH MAP
# =============================================================================
map_us_ci <- ggplot(us_map_data) +
  geom_sf(aes(fill = ci_width), color = "white", linewidth = 0.15) +
  scale_fill_gradient(name = "95% CI width\n(percentage points)",
                      low = "#2166AC", high = "#F4A582", na.value = "grey85") +
  coord_sf(crs = 5070) +
  labs(title = "US state estimate uncertainty") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14))

save_gg(map_us_ci, "phase6b_us_ci_width_map", 10, 6.5)

# =============================================================================
# REGIONAL GEOMETRIES (dissolve state polygons -> region polygons)
# =============================================================================
ng_shape_region <- ng_shape %>%
  mutate(region = to_region(fix_ng_state(NAME_1), ng_zones)) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>% summarise(.groups = "drop")

us_shape_region <- us_shape %>%
  mutate(region = to_region(NAME_1, us_regions)) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>% summarise(.groups = "drop")

# =============================================================================
# REGIONAL CRUDE CONFIDENCE (weighted svyby)
# =============================================================================
region_geotype_tbl <- function(design, lab) {
  by_geo <- svyby(~confident, ~region + geotype, design, svymean,
                  na.rm = TRUE, vartype = "ci") %>%
    as_tibble() %>%
    transmute(country = lab, region, panel = as.character(geotype),
              pct = round(100 * confident, 1),
              lci = round(100 * ci_l, 1), uci = round(100 * ci_u, 1))
  overall <- svyby(~confident, ~region, design, svymean,
                   na.rm = TRUE, vartype = "ci") %>%
    as_tibble() %>%
    transmute(country = lab, region, panel = "Overall",
              pct = round(100 * confident, 1),
              lci = round(100 * ci_l, 1), uci = round(100 * ci_u, 1))
  n_tbl <- design$variables %>%
    filter(!is.na(confident)) %>%
    count(region, geotype, name = "n") %>%
    mutate(panel = as.character(geotype)) %>%
    dplyr::select(region, panel, n)
  bind_rows(overall, by_geo) %>%
    left_join(n_tbl, by = c("region", "panel")) %>%
    mutate(panel = factor(panel, levels = c("Overall","Urban","Peri-urban","Rural")))
}

ng_region_geotype <- region_geotype_tbl(ng_svy, "Nigeria")
us_region_geotype <- region_geotype_tbl(us_svy, "USA")

# Flag small cells (< n=30) so a reader can spot low-precision estimates
for (df in list(list(d = ng_region_geotype, lab = "Nigeria"),
                list(d = us_region_geotype, lab = "USA"))) {
  small <- df$d %>% filter(panel != "Overall", n < 30)
  if (nrow(small) > 0) {
    cat("WARNING:", df$lab, "region x geotype cells with n < 30:\n")
    print(as.data.frame(small), row.names = FALSE)
  }
}

# =============================================================================
# REGIONAL ADJUSTED CONFIDENCE (Model 3c standardized predictions)
# =============================================================================
# Standardizes covariates to observed distribution while forcing region
# and geotype to each cell — same G-computation logic as intervention
# scenarios, applied across a two-factor grid.
adj_region_geotype_tbl <- function(fit, data, m3c_vars, lab) {
  data_cc <- data %>% dplyr::select(all_of(c(m3c_vars, "wt_r"))) %>% na.omit()
  region_levels  <- levels(data_cc$region)
  geotype_levels <- levels(data_cc$geotype)
  
  out <- list()
  for (r in region_levels) {
    # Overall panel: fix region, leave geotype at observed distribution
    d_ov <- data_cc; d_ov$region <- factor(r, levels = region_levels)
    p_ov <- predict(fit, newdata = d_ov, type = "response")
    out[[length(out) + 1]] <- tibble(country = lab, region = r, panel = "Overall",
                                     pct = round(100 * weighted.mean(p_ov, d_ov$wt_r, na.rm = TRUE), 1))
    # Geotype-specific
    for (g in geotype_levels) {
      d_g <- data_cc
      d_g$region  <- factor(r, levels = region_levels)
      d_g$geotype <- factor(g, levels = geotype_levels)
      p_g <- predict(fit, newdata = d_g, type = "response")
      out[[length(out) + 1]] <- tibble(country = lab, region = r, panel = g,
                                       pct = round(100 * weighted.mean(p_g, d_g$wt_r, na.rm = TRUE), 1))
    }
  }
  bind_rows(out) %>%
    mutate(panel = factor(panel, levels = c("Overall","Urban","Peri-urban","Rural")))
}

ng_region_geotype_adj <- adj_region_geotype_tbl(ng_fits[["M3c combined"]], ng, m3c_vars, "Nigeria")
us_region_geotype_adj <- adj_region_geotype_tbl(us_fits[["M3c combined"]], us, m3c_vars, "USA")

# =============================================================================
# FACETED REGIONAL MAPS (crude + adjusted, both countries)
# =============================================================================
# Diverging red-blue scale centered on each map's OWN midpoint (not fixed
# 0-100), so panel-to-panel contrast isn't washed out.
make_regional_facet_map <- function(map_data, title_country, legend_name, crs = NULL) {
  rng <- range(map_data$pct, na.rm = TRUE)
  lo  <- floor(rng[1] / 5) * 5
  hi  <- ceiling(rng[2] / 5) * 5
  mid <- (lo + hi) / 2
  
  p <- ggplot(map_data) +
    geom_sf(aes(fill = pct), color = "white", linewidth = 0.3) +
    facet_wrap(~ panel, nrow = 1) +
    scale_fill_gradient2(name = legend_name,
                         low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                         midpoint = mid, limits = c(lo, hi), na.value = "grey90") +
    labs(title = paste(title_country, "-", legend_name)) +
    map_theme_regional
  if (!is.null(crs)) p <- p + coord_sf(crs = crs, datum = NA)
  p
}

# Crude (3-panel: Urban / Peri-urban / Rural — Overall dropped)
save_gg(make_regional_facet_map(
  ng_shape_region %>%
    left_join(ng_region_geotype %>% filter(panel != "Overall"), by = "region"),
  "Nigeria", "% confident (weighted)") + labs(title = NULL),
  "figure_ng_regional_geotype_facets", 14, 6, also_pdf = TRUE)

save_gg(make_regional_facet_map(
  us_shape_region %>%
    left_join(us_region_geotype %>% filter(panel != "Overall"), by = "region"),
  "USA", "% confident (weighted)", crs = 5070) + labs(title = NULL),
  "figure_us_regional_geotype_facets", 14, 5, also_pdf = TRUE)

# Adjusted
save_gg(make_regional_facet_map(
  ng_shape_region %>%
    left_join(ng_region_geotype_adj %>% filter(panel != "Overall"), by = "region"),
  "Nigeria", "% confident (adjusted)"),
  "figure_ng_regional_geotype_adjusted", 14, 6)

save_gg(make_regional_facet_map(
  us_shape_region %>%
    left_join(us_region_geotype_adj %>% filter(panel != "Overall"), by = "region"),
  "USA", "% confident (adjusted)", crs = 5070),
  "figure_us_regional_geotype_adjusted", 14, 5)

# =============================================================================
# REGIONAL MODEL-SEQUENCE MAPS (Crude vs M2 vs M3c, Overall panel only)
# =============================================================================
adj_region_overall_tbl <- function(fit, data, m3c_vars, lab, model_name) {
  data_cc <- data %>% dplyr::select(all_of(c(m3c_vars, "wt_r"))) %>% na.omit()
  region_levels <- levels(data_cc$region)
  map_dfr(region_levels, function(r) {
    d <- data_cc; d$region <- factor(r, levels = region_levels)
    p <- predict(fit, newdata = d, type = "response")
    tibble(country = lab, region = r, panel = model_name,
           pct = round(100 * weighted.mean(p, d$wt_r, na.rm = TRUE), 1))
  })
}

build_model_sequence <- function(crude_tbl, fit_list, data, lab) {
  crude <- crude_tbl %>% filter(panel == "Overall") %>%
    transmute(country, region, panel = "Crude", pct)
  m2  <- adj_region_overall_tbl(fit_list[["M2 confounders"]], data, m3c_vars, lab, "M2 (confounders)")
  m3c <- adj_region_overall_tbl(fit_list[["M3c combined"]],   data, m3c_vars, lab, "M3c (fully adjusted)")
  bind_rows(crude, m2, m3c) %>%
    mutate(panel = factor(panel, levels = c("Crude","M2 (confounders)","M3c (fully adjusted)")))
}

ng_model_sequence <- build_model_sequence(ng_region_geotype, ng_fits, ng, "Nigeria")
us_model_sequence <- build_model_sequence(us_region_geotype, us_fits, us, "USA")

make_model_seq_map <- function(map_data, title_country, crs = NULL) {
  rng <- range(map_data$pct, na.rm = TRUE)
  lo  <- floor(rng[1] / 5) * 5
  hi  <- ceiling(rng[2] / 5) * 5
  mid <- (lo + hi) / 2
  p <- ggplot(map_data) +
    geom_sf(aes(fill = pct), color = "white", linewidth = 0.3) +
    facet_wrap(~ panel, nrow = 1) +
    scale_fill_gradient2(name = "% confident",
                         low = "#B2182B", mid = "#F7F7F7", high = "#2166AC",
                         midpoint = mid, limits = c(lo, hi), na.value = "grey90") +
    labs(title = paste(title_country, "- Regional confidence across the model sequence")) +
    map_theme_regional
  if (!is.null(crs)) p <- p + coord_sf(crs = crs, datum = NA)
  p
}

save_gg(make_model_seq_map(ng_shape_region %>% left_join(ng_model_sequence, by = "region"),
                           "Nigeria"),
        "figure_ng_regional_model_sequence", 13, 5.5)

save_gg(make_model_seq_map(us_shape_region %>% left_join(us_model_sequence, by = "region"),
                           "USA", crs = 5070),
        "figure_us_regional_model_sequence", 13, 4.5)

# =============================================================================
# REGIONAL G-COMPUTATION MAPS (universal easy access)
# =============================================================================
# Extends national G-computation ("universal easy access" is largest lever
# in both countries) to show WHERE that gain would concentrate.
region_g_compute <- function(fit, data, cf_change, scenario_name, lab, min_n = 30) {
  vars_needed <- all.vars(formula(fit))
  data_cc <- data %>% dplyr::select(any_of(unique(c(vars_needed, "region")))) %>% na.omit()
  map_dfr(levels(data_cc$region), function(r) {
    d_r <- data_cc %>% filter(region == r)
    if (nrow(d_r) == 0) return(NULL)
    d_cf <- d_r
    for (v in names(cf_change)) if (v %in% names(d_cf)) {
      d_cf[[v]] <- if (is.factor(d_cf[[v]]))
        factor(cf_change[[v]], levels = levels(d_cf[[v]])) else cf_change[[v]]
    }
    p_actual <- predict(fit, newdata = d_r,  type = "response", allow.new.levels = TRUE, re.form = NA)
    p_cf     <- predict(fit, newdata = d_cf, type = "response", allow.new.levels = TRUE, re.form = NA)
    tibble(country = lab, region = r, scenario = scenario_name, n = nrow(d_r),
           mean_actual = round(100 * mean(p_actual), 1),
           mean_cf     = round(100 * mean(p_cf), 1),
           diff_pp     = round(100 * mean(p_cf - p_actual), 2),
           small_n     = nrow(d_r) < min_n)
  })
}

ng_region_gcomp <- region_g_compute(ng_ml[["M3c combined"]], ng,
                                    list(ease_access = "Very"),
                                    "Universal easy access", "Nigeria")
us_region_gcomp <- region_g_compute(us_ml[["M3c combined"]], us,
                                    list(ease_access = "Very"),
                                    "Universal easy access", "USA")

# Sequential amber-to-green (NOT diverging) — diff_pp is expected positive
# everywhere; diverging would falsely imply some regions lose confidence.
make_gcomp_map <- function(map_data, title_country, crs = NULL) {
  rng <- range(map_data$diff_pp, na.rm = TRUE)
  lo  <- max(0, floor(rng[1])); hi <- ceiling(rng[2])
  p <- ggplot(map_data) +
    geom_sf(aes(fill = diff_pp), color = "white", linewidth = 0.3) +
    scale_fill_gradient(name = "Predicted gain\n(percentage points)",
                        low = "#FFF7BC", high = "#238443",
                        limits = c(lo, hi), na.value = "grey90") +
    labs(title = paste(title_country, "- Predicted gain from universal easy access, by region")) +
    theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "right")
  if (!is.null(crs)) p <- p + coord_sf(crs = crs, datum = NA)
  p
}

save_gg(make_gcomp_map(ng_shape_region %>% left_join(ng_region_gcomp, by = "region"),
                       "Nigeria"),
        "figure_ng_regional_gcomp_access", 8, 7)

save_gg(make_gcomp_map(us_shape_region %>% left_join(us_region_gcomp, by = "region"),
                       "USA", crs = 5070),
        "figure_us_regional_gcomp_access", 10, 6.5)

cat("===== 04_MAPS.R complete =====\n")
##COMPLETE PHASE 4