# =============================================================================
# 02_TABLES.R
# =============================================================================
# Loads analysis_workspace.RData (from 01_pipeline.R) and renders every
# table used in the paper as .docx / .html. No modelling happens here.
#
# Outputs:
#   Table 1                 — sample characteristics (from gtsummary tbl1)
#   Table 6                 — mediated proportion (styled gt)
#   Table 10                — sensitivity ORs across 11 specifications
#   Table (flagged)         — sensitivity specs inconsistent with primary
#   Tables X1-X9            — supplementary, one .docx/.html per table
#   supplementary_tables_X1_X9.docx — combined supplementary doc
# =============================================================================
rm(list = ls())
setwd("/Users/macuser/IMMS Datasets")

library(dplyr)
library(tidyr)
library(gt)
library(gtsummary)
library(officer)
library(flextable)

load("analysis_workspace.RData")

# =============================================================================
# HELPERS
# =============================================================================
academic_gt <- function(data, table_num, title, note, groups = NULL) {
  tbl <- if (!is.null(groups)) gt(data, groupname_col = groups) else gt(data)
  tbl %>%
    tab_header(title = html(paste0("<i><b>Table ", table_num, ":</b></i> ", title))) %>%
    tab_options(
      table.border.top.style = "solid", table.border.top.width = px(2),
      table.border.top.color = "black",
      table.border.bottom.style = "solid", table.border.bottom.width = px(2),
      table.border.bottom.color = "black",
      heading.border.bottom.style = "none",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1.5),
      column_labels.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = px(1),
      table_body.border.bottom.color = "black",
      row_group.border.top.style = "solid", row_group.border.top.width = px(1),
      row_group.border.bottom.style = "none",
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      heading.title.font.size = px(14),
      heading.align = "left",
      table.font.size = px(11),
      data_row.padding = px(6)
    ) %>%
    cols_align(align = "left",   columns = 1) %>%
    cols_align(align = "center", columns = -1) %>%
    tab_source_note(source_note = html(paste0("<i>Note. ", note, "</i>")))
}

save_pair <- function(gt_obj, fname) {
  gtsave(gt_obj, paste0(fname, ".docx"))
  gtsave(gt_obj, paste0(fname, ".html"))
}

fmt_or <- function(OR, lci, uci)
  paste0(sprintf("%.2f", OR), " (", sprintf("%.2f", lci), "\u2013", sprintf("%.2f", uci), ")")

# =============================================================================
# TABLE 1 — Sample characteristics
# =============================================================================
gtsave(as_gt(tbl1), "table1_sample_characteristics.docx")
gtsave(as_gt(tbl1), "table1_sample_characteristics.html")

# =============================================================================
# TABLE 6 — Mediated proportion
# =============================================================================
x6_mediation <- med_tbl %>%
  mutate(
    display_pct = case_when(
      is.na(pct_mediated) ~ flag,
      flag == "Unstable"  ~ paste0(sprintf("%.1f", pct_mediated), "% (unstable)"),
      TRUE                ~ paste0(sprintf("%.1f", pct_mediated), "%")
    )
  ) %>%
  dplyr::select(country, contrast, via, display_pct) %>%
  academic_gt(
    "6", "Mediated proportion of the geotype effect via access barriers and social norms",
    paste0(
      "Percent mediated = 100 \u00d7 (log-OR at M2 \u2212 log-OR at indicated model) / log-OR at M2, ",
      "using the weighted multilevel M3c model sequence. \u201cTotal effect \u2248 0\u201d indicates the ",
      "M2 (confounder-adjusted) geotype estimate was too close to the null (|log-OR| < 0.15) for a ",
      "mediated percentage to be meaningfully interpreted. \u201cUnstable\u201d flags values outside ",
      "\u00b1100%, a known limitation of the difference-in-coefficients method when the total effect ",
      "is small relative to the mediator-induced shift \u2014 these should be read as directional ",
      "only, not as precise estimates."
    ),
    groups = "country"
  ) %>%
  cols_label(contrast = "Contrast", via = "Mediator block", display_pct = "% mediated")

save_pair(x6_mediation, "table6_mediated_proportion")

# =============================================================================
# TABLE 10 — Sensitivity ORs across 11 specifications (M3c)
# =============================================================================
sens_order <- c(
  "Primary", "S1. Complete cases", "S2. Top-2-box", "S3. Importance only",
  "S4. Safety only", "S5. Excl. migrants", "S6. Excl. child disability",
  "S7. Unweighted", "S8. Small-sample states excluded",
  "S9. 1 child only", "S10. 2+ children", "S11. Excl. No religion"
)

sensitivity_or_table <- sens_all %>%
  filter(model == "M3c") %>%
  mutate(
    contrast = ifelse(grepl("Peri", term), "Peri-urban vs Urban", "Rural vs Urban"),
    `OR (95% CI)` = fmt_or(OR, lci, uci),
    # Merge country-specific S8 labels into one — same conceptual check
    sensitivity_label = case_when(
      sensitivity %in% c("S8. States n_psu>=15", "S8. States n>=50",
                         "S8. Small-sample states excluded") ~ "S8. Small-sample states excluded",
      TRUE ~ sensitivity
    )
  ) %>%
  dplyr::select(country, sensitivity_label, contrast, n, `OR (95% CI)`, p) %>%
  pivot_wider(
    id_cols = sensitivity_label,
    names_from = c(country, contrast),
    values_from = c(`OR (95% CI)`, n, p),
    names_sep = " | "
  ) %>%
  mutate(sensitivity_label = factor(sensitivity_label, levels = sens_order)) %>%
  arrange(sensitivity_label) %>%
  rename(Specification = sensitivity_label)

x10_sensitivity <- sensitivity_or_table %>%
  dplyr::select(
    Specification,
    `OR (95% CI) | Nigeria | Peri-urban vs Urban`,
    `OR (95% CI) | Nigeria | Rural vs Urban`,
    `OR (95% CI) | USA | Peri-urban vs Urban`,
    `OR (95% CI) | USA | Rural vs Urban`
  ) %>%
  setNames(c(
    "Specification",
    paste("Nigeria", "\u2014", "Peri-urban vs Urban"),
    paste("Nigeria", "\u2014", "Rural vs Urban"),
    paste("USA",     "\u2014", "Peri-urban vs Urban"),
    paste("USA",     "\u2014", "Rural vs Urban")
  )) %>%
  academic_gt(
    "10", "Geotype odds ratios across sensitivity analysis specifications (Model 3c)",
    paste0(
      "OR (95% CI) for each geotype contrast under 11 pre-specified alternative specifications, ",
      "fully adjusted Model 3c. \u201cPrimary\u201d is the main analytic sample and specification ",
      "reported throughout Results."
    )
  )

save_pair(x10_sensitivity, "table_X10_sensitivity_or")

# =============================================================================
# TABLE — Sensitivity specifications flagged as inconsistent
# =============================================================================
sensitivity_flagged_table <- sens_flagged %>%
  filter(consistent != "OK") %>%
  mutate(
    term  = ifelse(grepl("Peri", term), "Peri-urban vs Urban", "Rural vs Urban"),
    OR_CI = fmt_or(OR, lci, uci)
  ) %>%
  transmute(
    Country = country, Model = model, Sensitivity = sensitivity,
    Contrast = term, n = n, `OR (95% CI)` = OR_CI, p = signif(p, 3),
    `Primary OR` = sprintf("%.2f", primary_OR)
  ) %>%
  arrange(Country, Model, Sensitivity)

flagged_gt <- sensitivity_flagged_table %>%
  gt(groupname_col = "Country") %>%
  tab_header(title = html(
    "<i><b>Table:</b></i> Sensitivity specifications flagged as inconsistent with the primary estimate"
  )) %>%
  tab_source_note(source_note = html(paste0(
    "<i>Note. SHIFT = same direction, deviation greater than 0.15 OR from primary estimate. ",
    "FLIP = direction reversed relative to the null. All other specification x model x country ",
    "combinations were consistent with the primary estimate (Appendix 7).</i>"
  )))

gtsave(flagged_gt, "table_sensitivity_flagged.docx")

# =============================================================================
# X1-X9 — DERIVED OBJECTS
# =============================================================================
gvif_to_df <- function(gvif_mat, lab) {
  as.data.frame(gvif_mat) %>%
    tibble::rownames_to_column("Predictor") %>%
    mutate(country = lab) %>%
    rename(GVIF = GVIF, Df = Df, `GVIF^(1/2Df)` = `GVIF^(1/(2*Df))`)
}
gvif_combined <- bind_rows(gvif_to_df(gvif_ng, "Nigeria"), gvif_to_df(gvif_us, "USA"))

sig_sum <- bv %>%
  filter(!is.na(p)) %>%
  group_by(country, predictor) %>%
  summarise(any_sig = any(p <= 0.05), .groups = "drop")

outcome_sig  <- sig_sum   %>% filter(any_sig)     %>% dplyr::select(country, predictor)
exposure_sig <- exp_assoc %>% filter(p <= 0.05)  %>% dplyr::select(country, predictor)

confounder_status <- bind_rows(
  inner_join(outcome_sig, exposure_sig, by = c("country","predictor")) %>%
    mutate(status = "Confirmed confounder (sig. with outcome AND geotype)"),
  anti_join(outcome_sig, exposure_sig, by = c("country","predictor")) %>%
    mutate(status = "Outcome-associated only (likely mediator, not confounder)"),
  anti_join(exposure_sig, outcome_sig, by = c("country","predictor")) %>%
    mutate(status = "Geotype-associated only (not retained)")
) %>% arrange(country, status, predictor)

sens_summary_tbl <- sens_flagged %>%
  filter(sensitivity != "Primary") %>%
  group_by(country, model, term, consistent) %>%
  summarise(n_sens = n(), .groups = "drop") %>%
  pivot_wider(names_from = consistent, values_from = n_sens, values_fill = 0) %>%
  mutate(term = case_when(
    term == "geotypePeri-urban" ~ "Peri-urban vs Urban",
    term == "geotypeRural"      ~ "Rural vs Urban",
    TRUE ~ term
  ))

# =============================================================================
# X1-X9 — INDIVIDUAL STYLED gt TABLES
# =============================================================================
x1 <- primary_tests %>%
  mutate(term = ifelse(grepl("Peri", term), "Peri-urban vs Urban", "Rural vs Urban"),
         `OR (95% CI)` = fmt_or(OR, lci, uci)) %>%
  dplyr::select(country, sensitivity, term, `OR (95% CI)`, p, sig_raw, p_bonf, sig_bonf) %>%
  academic_gt("X1", "Multiple testing correction on primary geotype contrasts",
              "Bonferroni threshold: \u03b1 = 0.05/16 = 0.003. * indicates significance at the given threshold.",
              groups = "country") %>%
  fmt_scientific(columns = c(p, p_bonf), decimals = 2) %>%
  cols_label(sensitivity = "Outcome definition", term = "Contrast",
             sig_raw = "Sig. (raw)", p_bonf = "p (Bonferroni)",
             sig_bonf = "Sig. (Bonferroni)")
save_pair(x1, "table_X1")

x2 <- peri_results %>%
  mutate(term = ifelse(grepl("Urban$", term), "Urban vs Peri-urban", "Rural vs Peri-urban"),
         `OR (95% CI)` = fmt_or(OR, lci, uci)) %>%
  dplyr::select(country, term, `OR (95% CI)`, p) %>%
  academic_gt("X2", "Geotype association with peri-urban as reference category",
              paste0("Model 2 (confounder-adjusted), re-referenced to peri-urban. ",
                     "Peri-urban functions as the lowest-confidence category in Nigeria, ",
                     "with both urban and rural above it."),
              groups = "country") %>%
  fmt_scientific(columns = p, decimals = 2)
save_pair(x2, "table_X2")

x3 <- gvif_combined %>%
  dplyr::select(country, Predictor, GVIF, Df, `GVIF^(1/2Df)`) %>%
  academic_gt("X3", "Generalized variance inflation factors, Model 3c",
              paste0("GVIF^(1/2Df) values below ~1.5 indicate no meaningful multicollinearity ",
                     "concern (max observed: 1.25 Nigeria, 1.18 USA)."),
              groups = "country") %>%
  fmt_number(columns = c(GVIF, `GVIF^(1/2Df)`), decimals = 2)
save_pair(x3, "table_X3")

x4 <- confounder_status %>%
  academic_gt("X4", "Verification of a priori DAG-derived confounder status",
              paste0("hcw_rec and know_where are outcome-associated but not geotype-associated ",
                     "in Nigeria, consistent with their inclusion in the access mediator block ",
                     "rather than the confounder block."),
              groups = "country") %>%
  cols_label(predictor = "Predictor", status = "Classification")
save_pair(x4, "table_X4")

x5 <- comm_norm_check %>%
  academic_gt("X5", "Community and religious leader norm concordance",
              paste0("High agreement (Nigeria 92.8%, USA 82.6%) reflects overlapping religious ",
                     "and community leadership structures. Excluding comm_norm does not ",
                     "materially change the rural geotype estimate.")) %>%
  fmt_number(columns = c(rural_OR_without_comm, rural_OR_with_comm), decimals = 2) %>%
  cols_label(country = "Country", agreement_pct = "Agreement (%)",
             rural_OR_without_comm = "Rural OR (comm_norm excluded)",
             rural_OR_with_comm    = "Rural OR (comm_norm included)")
save_pair(x5, "table_X5")

x6_power <- power_tbl %>%
  academic_gt("X6", "Minimum detectable odds ratio, rural vs urban contrast",
              paste0("Both countries have >90% power to detect the observed geotype effect ",
                     "sizes at the stated sample sizes.")) %>%
  fmt_percent(columns = power_target, decimals = 0) %>%
  fmt_number(columns = c(ng_or_below, ng_or_above, us_or_below, us_or_above), decimals = 2) %>%
  cols_label(power_target = "Target power", ng_or_below = "Nigeria (OR < 1)",
             ng_or_above = "Nigeria (OR > 1)", us_or_below = "USA (OR < 1)",
             us_or_above = "USA (OR > 1)")
save_pair(x6_power, "table_X6")

x7 <- sens_summary_tbl %>%
  academic_gt("X7", "Sensitivity analysis consistency summary",
              paste0("OK = same direction and within 0.15 OR of primary estimate. SHIFT = same ",
                     "direction, larger deviation. FLIP = direction reversed. Only 4 of 88 checks ",
                     "(all Nigeria, small-subsample specifications) were flagged."),
              groups = "country") %>%
  cols_label(model = "Model", term = "Contrast")
save_pair(x7, "table_X7")

x8 <- audit_tbl %>%
  academic_gt("X8", "Model fit across the nested model sequence",
              paste0("Declining deviance and AIC across the model sequence indicate improved fit ",
                     "with each added block, most pronounced after the access mediator block (M3a) ",
                     "in both countries."),
              groups = "country") %>%
  fmt_number(columns = c(dev, null_dev, AIC), decimals = 1) %>%
  cols_label(model = "Model", df = "Residual df", dev = "Deviance",
             null_dev = "Null deviance", AIC = "AIC")
save_pair(x8, "table_X8")

x9 <- b7_skip_check %>%
  dplyr::select(country, religion3, n, na_relig_norm, pct_na) %>%
  academic_gt("X9", "Missingness on religious leader norm (B7) by religion",
              paste0("Nigeria's 'No religion' category (n=29) has both a very small sample and ",
                     "elevated B7 missingness (10.3%), consistent with the unstable geotype x ",
                     "religion interaction estimates reported for this subgroup."),
              groups = "country") %>%
  fmt_number(columns = pct_na, decimals = 1) %>%
  cols_label(religion3 = "Religion", n = "N", na_relig_norm = "Missing (n)",
             pct_na = "Missing (%)")
save_pair(x9, "table_X9")

# =============================================================================
# COMBINED SUPPLEMENTARY DOC (X1-X9 in one .docx via flextable/officer)
# =============================================================================
set_flextable_defaults(font.size = 10, padding = 4, theme_fun = "theme_booktabs")

style_ft <- function(ft, table_num, title) {
  ft %>%
    bold(part = "header") %>%
    hline_top(part = "all",  border = fp_border(color = "black", width = 2)) %>%
    hline_bottom(part = "body", border = fp_border(color = "black", width = 1.5)) %>%
    border_inner_h(border = fp_border(color = "white", width = 0), part = "body") %>%
    border_inner_v(border = fp_border(color = "white", width = 0)) %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    set_caption(caption = as_paragraph(
      as_chunk(paste0("Table ", table_num, ": "), props = fp_text(bold = TRUE, italic = TRUE)),
      as_chunk(title)
    ), autonum = NULL)
}

add_table_to_doc <- function(doc, ft, note = NULL) {
  doc <- doc %>% body_add_flextable(ft)
  if (!is.null(note)) doc <- doc %>% body_add_par(paste0("Note. ", note), style = "Normal")
  doc %>% body_add_par("", style = "Normal")
}

doc <- read_docx() %>% body_add_par("Supplementary Tables", style = "heading 1")

fx1 <- primary_tests %>%
  mutate(term = ifelse(grepl("Peri", term), "Peri-urban vs Urban", "Rural vs Urban"),
         est  = fmt_or(OR, lci, uci)) %>%
  dplyr::select(country, sensitivity, term, est, p, sig_raw, p_bonf, sig_bonf) %>%
  flextable() %>% autofit() %>%
  style_ft("X1", "Multiple testing correction on primary geotype contrasts")
doc <- add_table_to_doc(doc, fx1, "Bonferroni threshold: \u03b1 = 0.05/16 = 0.003.")

fx2 <- peri_results %>%
  mutate(term = ifelse(grepl("Urban$", term), "Urban vs Peri-urban", "Rural vs Peri-urban"),
         est  = fmt_or(OR, lci, uci)) %>%
  dplyr::select(country, term, est, p) %>%
  flextable() %>% autofit() %>%
  style_ft("X2", "Geotype association with peri-urban as reference category")
doc <- add_table_to_doc(doc, fx2,
                        paste0("Model 2, re-referenced to peri-urban. Peri-urban functions as the lowest-confidence ",
                               "category in Nigeria, with both urban and rural above it."))

fx3 <- gvif_combined %>%
  dplyr::select(country, Predictor, GVIF, Df, `GVIF^(1/2Df)`) %>%
  flextable() %>% autofit() %>%
  style_ft("X3", "Generalized variance inflation factors, Model 3c")
doc <- add_table_to_doc(doc, fx3,
                        "Max GVIF^(1/2Df): 1.25 Nigeria, 1.18 USA \u2014 no meaningful multicollinearity.")

fx4 <- confounder_status %>%
  flextable() %>% autofit() %>%
  style_ft("X4", "Verification of a priori DAG-derived confounder status")
doc <- add_table_to_doc(doc, fx4,
                        paste0("hcw_rec and know_where are outcome-associated but not geotype-associated in Nigeria, ",
                               "consistent with their inclusion in the access mediator block rather than the confounder block."))

fx5 <- comm_norm_check %>%
  flextable() %>% autofit() %>%
  style_ft("X5", "Community and religious leader norm concordance")
doc <- add_table_to_doc(doc, fx5,
                        paste0("High agreement (Nigeria 92.8%, USA 82.6%). Excluding comm_norm does not materially ",
                               "change the rural geotype estimate."))

fx6 <- power_tbl %>%
  flextable() %>% autofit() %>%
  style_ft("X6", "Minimum detectable odds ratio, rural vs urban contrast")
doc <- add_table_to_doc(doc, fx6,
                        "Both countries have >90% power to detect the observed effect sizes.")

fx7 <- sens_summary_tbl %>%
  flextable() %>% autofit() %>%
  style_ft("X7", "Sensitivity analysis consistency summary")
doc <- add_table_to_doc(doc, fx7,
                        paste0("OK = consistent with primary estimate. SHIFT = same direction, larger deviation. ",
                               "FLIP = direction reversed. 4 of 88 checks flagged."))

fx8 <- audit_tbl %>%
  flextable() %>% autofit() %>%
  style_ft("X8", "Model fit across the nested model sequence")
doc <- add_table_to_doc(doc, fx8,
                        "Declining deviance/AIC indicates improved fit with each added block.")

fx9 <- b7_skip_check %>%
  dplyr::select(country, religion3, n, na_relig_norm, pct_na) %>%
  flextable() %>% autofit() %>%
  style_ft("X9", "Missingness on religious leader norm (B7) by religion")
doc <- add_table_to_doc(doc, fx9,
                        paste0("Nigeria's 'No religion' category (n=29) has elevated B7 missingness (10.3%), ",
                               "consistent with unstable interaction estimates for this subgroup."))

print(doc, target = "supplementary_tables_X1_X9.docx")

cat("===== 02_TABLES.R complete =====\n")
#END PHASE 2