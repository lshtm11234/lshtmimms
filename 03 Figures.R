# =============================================================================
# 03_FIGURES.R
# =============================================================================
# Loads analysis_workspace.RData and builds every figure that does not
# need shapefiles. Maps live in 04_maps.R.
#
# Outputs:
#   conceptual_framework.png/.pdf   — Figure 0
#   figure_forest_combined.png      — Figure 1 (both countries in one panel)
#   figure_forest_nigeria.png       — Figure 1a (Nigeria alone)
#   figure_forest_usa.png           — Figure 1b (USA alone)
#   figure_likert_distribution.png  — Figure 2
#   figure_or_attenuation.png       — Figure 3
#   figure_gcomputation_bars.png    — Figure 4
#   figure_correlation_nigeria.png  — Figure 5a
#   figure_correlation_usa.png      — Figure 5b
#   figure_icc_comparison.png       — Figure 6
#   dag_confounders_only.png/.pdf   — Figure A1
# =============================================================================
rm(list = ls())
setwd("/Users/macuser/IMMS Datasets")

library(ggplot2)
library(ggforce)      # geom_circle for Figure 0
library(dplyr)
library(tidyr)
library(forcats)
library(ggcorrplot)
library(scales)

load("analysis_workspace.RData")

# =============================================================================
# HELPERS
# =============================================================================
save_gg <- function(p, fname, width, height, dpi = 300, also_pdf = FALSE) {
  ggsave(paste0(fname, ".png"), p, width = width, height = height, dpi = dpi, bg = "white")
  if (also_pdf) ggsave(paste0(fname, ".pdf"), p, width = width, height = height, device = cairo_pdf)
}

# =============================================================================
# FIGURE 0 — CONCEPTUAL FRAMEWORK
# =============================================================================
p_framework <- ggplot() +
  geom_circle(aes(x0 = 6, y0 = 7,   r = 2.2), fill = "#EDF2F7", colour = "#90A4AE", linewidth = 0.6) +
  annotate("text", x = 6, y = 9.0, label = "Health system", size = 4, fontface = "bold", colour = "#455A64") +
  geom_circle(aes(x0 = 6, y0 = 6.7, r = 1.5), fill = "#E8F0E8", colour = "#90A890", linewidth = 0.6) +
  annotate("text", x = 6, y = 7.85, label = "Community", size = 3.5, fontface = "bold", colour = "#455A64") +
  geom_circle(aes(x0 = 6, y0 = 6.5, r = 0.8), fill = "#FFF8E1", colour = "#C8B880", linewidth = 0.6) +
  annotate("text", x = 6, y = 6.5, label = "Household", size = 3, fontface = "bold", colour = "#455A64") +
  
  annotate("rect", xmin = 0.3, xmax = 3.3, ymin = 7.4, ymax = 8.4,
           fill = "#E8EAF6", colour = "#7986CB", linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = 1.8, y = 8.05, label = "Residence (S13)", size = 3.2, fontface = "bold", colour = "#37474F") +
  annotate("text", x = 1.8, y = 7.65, label = "Urban / peri-urban / rural", size = 2.4, colour = "#546E7A") +
  
  annotate("rect", xmin = 0.3, xmax = 3.3, ymin = 4.2, ymax = 6.0,
           fill = "#E8EAF6", colour = "#7986CB", linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = 1.8, y = 5.65, label = "Sociodemographic", size = 3.2, fontface = "bold", colour = "#37474F") +
  annotate("text", x = 1.8, y = 5.0,
           label = "Region, caregiver age,\ngender, education,\nemployment, religion, SES",
           size = 2.4, colour = "#546E7A") +
  
  annotate("rect", xmin = 1.2, xmax = 6.0, ymin = 1.4, ymax = 3.6,
           fill = "#E0F2F1", colour = "#4DB6AC", linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = 3.6, y = 3.2, label = "Model 3a \u2014 Structural barriers",
           size = 3, fontface = "bold", colour = "#37474F") +
  annotate("text", x = 3.6, y = 2.55,
           label = "Ease of access, knows where,\nHCW rec, recall,\nturned away, barrier count",
           size = 2.3, colour = "#546E7A") +
  annotate("text", x = 3.6, y = 1.8, label = "(B14, B11, B3, B9, B13, B16)",
           size = 2.1, colour = "#78909C") +
  
  annotate("rect", xmin = 6.5, xmax = 11.0, ymin = 1.4, ymax = 3.6,
           fill = "#FFF3E0", colour = "#FFB74D", linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = 8.75, y = 3.2, label = "Model 3b \u2014 Social perceptions",
           size = 3, fontface = "bold", colour = "#37474F") +
  annotate("text", x = 8.75, y = 2.6,
           label = "Peer, family, religious,\ncommunity leader norms",
           size = 2.3, colour = "#546E7A") +
  annotate("text", x = 8.75, y = 1.8, label = "(B5, B6, B7, B8)",
           size = 2.1, colour = "#78909C") +
  
  annotate("rect", xmin = 2.5, xmax = 9.5, ymin = -1.0, ymax = 0.6,
           fill = "#F3E5F5", colour = "#AB47BC", linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = 6, y = 0.25, label = "Caregiver vaccine confidence",
           size = 3.5, fontface = "bold", colour = "#37474F") +
  annotate("text", x = 6, y = -0.2, label = "Composite of B1 (importance) & B2 (safety)",
           size = 2.4, colour = "#546E7A") +
  annotate("text", x = 6, y = -0.6, label = "Binary: confident vs not",
           size = 2.2, colour = "#78909C", fontface = "italic") +
  
  # Arrows
  annotate("curve", x = 3.3, xend = 4.0, y = 7.9, yend = 7.8,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B", curvature = -0.15) +
  annotate("curve", x = 3.3, xend = 5.2, y = 5.3, yend = 6.5,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B", curvature = -0.25) +
  annotate("segment", x = 4.5, xend = 3.6, y = 5.0, yend = 3.6,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B") +
  annotate("segment", x = 7.5, xend = 8.75, y = 5.0, yend = 3.6,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B") +
  annotate("segment", x = 3.6, xend = 4.8, y = 1.4, yend = 0.6,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B") +
  annotate("segment", x = 8.75, xend = 7.2, y = 1.4, yend = 0.6,
           arrow = arrow(length = unit(0.08, "in"), type = "closed"),
           linewidth = 0.5, colour = "#607D8B") +
  
  annotate("text", x = 10.2, y = 9.5, label = "Models fitted separately: Nigeria | USA",
           size = 2.8, colour = "#90A4AE", fontface = "italic") +
  
  coord_fixed(ratio = 1, xlim = c(-0.5, 12), ylim = c(-1.8, 10)) +
  theme_void(base_size = 11) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

save_gg(p_framework, "conceptual_framework", 11, 10, also_pdf = TRUE)

# =============================================================================
# FIGURE 1 — BIVARIATE FOREST PLOTS
# =============================================================================
group_order   <- c("Residence","Demographics","Socioeconomic","Religion","Access barriers","Social norms")
group_colours <- c("Residence"       = "#1B7837",
                   "Demographics"    = "#5E3C99",
                   "Socioeconomic"   = "#B35806",
                   "Religion"        = "#B8860B",
                   "Access barriers" = "#08306B",
                   "Social norms"    = "#8B0A50")

# Human-readable term labels
term_lookup <- c(
  "geotypePeri-urban"              = "Peri-urban vs Urban",
  "geotypeRural"                   = "Rural vs Urban",
  "age_cat25-34"                   = "Age 25-34 vs 18-24",
  "age_cat35-44"                   = "Age 35-44",
  "age_cat45-54"                   = "Age 45-54",
  "age_cat55+"                     = "Age 55+",
  "child_age_catPrimary (5-11)"    = "Child: Primary (5-11)",
  "child_age_catSecondary (12-17)" = "Child: Secondary (12-17)",
  "genderFemale"                   = "Female vs Male",
  "education3Secondary"            = "Secondary vs Primary/None",
  "education3University"           = "University",
  "education3Masters/PhD"          = "Masters/PhD",
  "employment3Student"             = "Student vs Working",
  "employment3Not working"         = "Not working",
  "religion3Muslim"                = "Muslim vs Christian",
  "religion3No religion"           = "No religion",
  "religion3Jewish"                = "Jewish",
  "religion3Other"                 = "Other religion",
  "ses3Managing"                   = "Managing vs Struggling",
  "ses3Comfortable"                = "Comfortable",
  "ease_accessA little"            = "Access: A little vs Not at all",
  "ease_accessModerately"          = "Access: Moderately",
  "ease_accessVery"                = "Access: Very easy",
  "know_whereYes"                  = "Knows where: Yes",
  "hcw_recYes"                     = "HCW recommended: Yes",
  "recallYes"                      = "Recall contact: Yes",
  "turned_awayYes"                 = "Turned away: Yes",
  "barrier_count"                  = "Barrier count (per unit)",
  "peer_normYes"                   = "Peer norm: Yes",
  "family_normYes"                 = "Family norm: Yes",
  "relig_normYes"                  = "Religious norm: Yes",
  "comm_normYes"                   = "Community norm: Yes"
)

forest_data <- bind_rows(ng_bv, us_bv) %>%
  filter(!is.na(OR)) %>%
  mutate(
    log_or  = log(OR), log_lci = log(lci), log_uci = log(uci),
    sig     = ifelse(p <= 0.05, "sig", "ns"),
    group = case_when(
      predictor == "geotype" ~ "Residence",
      predictor %in% c("age_cat","child_age_cat","gender") ~ "Demographics",
      predictor %in% c("education3","employment3","ses3")  ~ "Socioeconomic",
      predictor == "religion3" ~ "Religion",
      predictor %in% c("ease_access","know_where","hcw_rec","recall",
                       "turned_away","barrier_count") ~ "Access barriers",
      predictor %in% c("peer_norm","family_norm","relig_norm","comm_norm") ~ "Social norms",
      TRUE ~ "Other"
    ),
    term_label = ifelse(term %in% names(term_lookup), term_lookup[term], term)
  ) %>%
  mutate(group = factor(group, levels = group_order)) %>%
  arrange(group, desc(log_or)) %>%
  mutate(term_label = fct_inorder(term_label),
         sig_label  = factor(sig, levels = c("sig", "ns"),
                             labels = c("p \u2264 0.05", "p > 0.05")))

# Some USA CIs (e.g. "Access: Very easy", OR ~12) extend past the default
# limit; widen so bars aren't silently clipped.
axis_limit <- max(2.5, ceiling(max(abs(forest_data$log_lci), abs(forest_data$log_uci),
                                   na.rm = TRUE) * 10) / 10)

forest_base <- function() {
  list(
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4),
    geom_linerange(aes(xmin = log_lci, xmax = log_uci), linewidth = 0.6, alpha = 0.85),
    geom_point(aes(shape = sig_label), size = 3, stroke = 1.3, fill = "white"),
    scale_shape_manual(name = "Significance",
                       values = c("p \u2264 0.05" = 16, "p > 0.05" = 21)),
    scale_colour_manual(values = group_colours, guide = "none"),
    scale_x_continuous(name = "Log odds ratio", limits = c(-axis_limit, axis_limit)),
    theme_minimal(base_size = 11),
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 9),
          strip.text.y.left = element_text(face = "bold", size = 10, angle = 0, hjust = 1),
          strip.placement = "outside",
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
  )
}

# Combined
p_forest_combined <- ggplot(
  forest_data %>% mutate(country = factor(country, levels = c("Nigeria", "USA"))),
  aes(x = log_or, y = term_label, colour = group)
) +
  forest_base() +
  facet_grid(group ~ country, scales = "free_y", space = "free_y", switch = "y") +
  labs(title = "Bivariate Associations with Vaccine Confidence \u2014 Nigeria and USA") +
  theme(strip.text.x = element_text(face = "bold", size = 16))

save_gg(p_forest_combined, "figure_forest_combined", 14, 12)

# Separate (one file per country)
make_forest <- function(country_name) {
  ggplot(forest_data %>% filter(country == country_name),
         aes(x = log_or, y = term_label, colour = group)) +
    forest_base() +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(title = paste(country_name, "\u2014 Bivariate Associations with Vaccine Confidence"))
}

save_gg(make_forest("Nigeria"), "figure_forest_nigeria", 10, 12)
save_gg(make_forest("USA"),     "figure_forest_usa",     10, 12)

# Figure captions (for the document, not rendered in image)
cat("\nFigure captions (for document body, not rendered on plot):\n")
cat("Figure 1. Bivariate Associations with Vaccine Confidence.\n")
cat("Univariate weighted log odds ratios; filled points indicate statistical\n")
cat("significance (p \u2264 0.05). Reference: Urban, 18-24, Male, Primary/None,\n")
cat("Working, Christian, Struggling, Not at all easy access.\n")

# =============================================================================
# FIGURE 2 — LIKERT DISTRIBUTION (B1/B2)
# =============================================================================
likert_data <- bind_rows(
  ng %>% transmute(country = "Nigeria", item = "B1 Important", response = factor(B1)),
  ng %>% transmute(country = "Nigeria", item = "B2 Safe",       response = factor(B2)),
  us %>% transmute(country = "USA",     item = "B1 Important", response = factor(B1)),
  us %>% transmute(country = "USA",     item = "B2 Safe",       response = factor(B2))
) %>%
  filter(response != 99) %>%
  mutate(response = factor(response, levels = 1:4,
                           labels = c("Strongly disagree","Disagree","Agree","Strongly agree")))

p_likert <- ggplot(likert_data, aes(x = item, fill = response)) +
  geom_bar(position = "fill", width = 0.7) +
  facet_wrap(~ country) +
  scale_fill_manual(values = c("#D73027","#FC8D59","#91BFDB","#4575B4"), name = "Response") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Distribution of vaccine confidence responses",
       subtitle = "B1 (importance) and B2 (safety) by country",
       x = NULL, y = "Proportion of respondents") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

save_gg(p_likert, "figure_likert_distribution", 10, 5)

# =============================================================================
# FIGURE 3 — GEOTYPE OR ATTENUATION ACROSS NESTED MODELS
# =============================================================================
attenuation_data <- geo_across %>%
  mutate(model = factor(model, levels = c("M1 crude","M1 + region","M2 confounders",
                                          "M3a + access","M3b + norms","M3c combined")))

p_attenuation <- ggplot(attenuation_data,
                        aes(x = model, y = OR, colour = term, group = term)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.15, linewidth = 0.4) +
  facet_wrap(~ country, ncol = 1) +
  scale_colour_manual(values = c("geotypePeri-urban" = "#E69F00",
                                 "geotypeRural"      = "#0072B2"),
                      labels = c("Peri-urban vs Urban", "Rural vs Urban"),
                      name = "Contrast") +
  labs(title = "Geotype effect attenuation across nested models",
       subtitle = "Odds ratio with 95% CI \u2014 how mediators change the geotype effect",
       x = NULL, y = "Odds ratio") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12))

save_gg(p_attenuation, "figure_or_attenuation", 10, 8)

# =============================================================================
# FIGURE 4 — G-COMPUTATION BAR CHART WITH CIs
# =============================================================================
p_gcomp <- ggplot(boot_results,
                  aes(x = reorder(scenario, diff_pp), y = diff_pp, fill = country)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("Nigeria" = "#2166AC", "USA" = "#B2182B"), name = "Country") +
  labs(title = "Population-level impact of hypothetical interventions",
       subtitle = "G-computation: predicted change in % confident (with 95% CI)",
       x = NULL, y = "Change in confidence (percentage points)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

save_gg(p_gcomp, "figure_gcomputation_bars", 10, 7)

# =============================================================================
# FIGURE 5 — CORRELATION MATRICES
# =============================================================================
make_corr <- function(m, country) {
  ggcorrplot(m, type = "lower", lab = TRUE, lab_size = 2.5,
             colors = c("#B2182B", "white", "#2166AC"),
             title = paste(country, "\u2014 Spearman correlation matrix of predictors"))
}

save_gg(make_corr(cor_ng, "Nigeria"), "figure_correlation_nigeria", 9, 9)
save_gg(make_corr(cor_us, "USA"),     "figure_correlation_usa",     9, 9)

# =============================================================================
# FIGURE 6 — ICC COMPARISON BAR
# =============================================================================
icc_ng <- variance_tbl %>% filter(country == "Nigeria", model == "M3c combined") %>% pull(ICC) * 100
icc_us <- variance_tbl %>% filter(country == "USA",     model == "M3c combined") %>% pull(ICC) * 100

icc_plot_data <- data.frame(
  country = c("Nigeria", "USA"),
  ICC     = c(icc_ng, icc_us),
  label   = c(paste0(round(icc_ng, 1), "%"),
              ifelse(icc_us < 0.1, "\u2248 0%", paste0(round(icc_us, 1), "%")))
)

p_icc <- ggplot(icc_plot_data, aes(x = country, y = ICC, fill = country)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Nigeria" = "#2166AC", "USA" = "#B2182B"), guide = "none") +
  scale_y_continuous(limits = c(0, max(icc_ng, 16) * 1.15), breaks = scales::pretty_breaks()) +
  labs(title = "Between-state variance in vaccine confidence",
       subtitle = "Intraclass correlation coefficient (Model 3c)",
       x = NULL, y = "ICC (%)") +
  theme_minimal(base_size = 13)

save_gg(p_icc, "figure_icc_comparison", 6, 5)

# =============================================================================
# FIGURE A1 — FORMAL DAG (confounders only) — Appendix A
# =============================================================================
# Coordinates for the seven confounder boxes so the code below reads as
# data rather than repeated annotate() calls.
confounders <- tibble::tribble(
  ~name,           ~x1,  ~x2,  ~y1,  ~y2,  ~tx,  ~ty,  ~size,
  "Region",        0.4,  2.0,  3.7,  4.3,  1.2,  4.0,  3,
  "Caregiver Age", 2.4,  4.6,  3.7,  4.3,  3.5,  4.0,  3,
  "Gender",        5.0,  6.4,  3.7,  4.3,  5.7,  4.0,  3,
  "Education",     6.8,  8.6,  3.7,  4.3,  7.7,  4.0,  3,
  "Religion",      1.6,  3.2,  5.2,  5.8,  2.4,  5.5,  3,
  "SES",           4.0,  5.4,  5.2,  5.8,  4.7,  5.5,  3,
  "Employment",    6.0,  8.0,  5.2,  5.8,  7.0,  5.5,  3
)

dag_arrows_to_geotype <- tibble::tribble(
  ~x,   ~xend, ~y,   ~yend,
  1.6,  0.5,   3.7,  0.35,
  3.7,  0.6,   3.7,  0.35,
  5.8,  0.7,   3.7,  0.35,
  7.9,  0.8,   3.7,  0.35,
  2.5,  0.4,   5.2,  0.35,
  4.8,  0.6,   5.2,  0.35,
  7.2,  0.8,   5.2,  0.35
)

dag_arrows_to_outcome <- tibble::tribble(
  ~x,   ~xend, ~y,   ~yend,
  1.6,  9.3,   3.7,  0.45,
  3.7,  9.5,   3.7,  0.45,
  5.8,  9.7,   3.7,  0.45,
  7.9,  9.9,   3.7,  0.45,
  2.5,  9.4,   5.2,  0.45,
  4.8,  9.6,   5.2,  0.45,
  7.2,  9.9,   5.2,  0.45
)

dag_arrows_between <- tibble::tribble(
  ~x,   ~xend, ~y,   ~yend,
  7.9,  4.8,   4.3,  5.2,
  7.9,  7.2,   4.3,  5.2,
  2.5,  1.6,   5.2,  4.3
)

p_dag <- ggplot() +
  # Exposure and outcome boxes
  annotate("rect", xmin = -1.0, xmax = 1.0, ymin = -0.4, ymax = 0.4,
           fill = "#D6E4F5", colour = "#5B85BC", linewidth = 1) +
  annotate("text", x = 0, y = 0, label = "Geotype\n(S13)", size = 4, fontface = "bold") +
  annotate("rect", xmin = 9.0, xmax = 11.2, ymin = -0.4, ymax = 0.4,
           fill = "#D8E8D8", colour = "#7DAB7D", linewidth = 1) +
  annotate("text", x = 10.1, y = 0, label = "Vaccine\nConfidence", size = 3.5, fontface = "bold") +
  
  # Confounder boxes and labels (from tibble)
  geom_rect(data = confounders, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
            fill = "#E0DEF0", colour = "#8B7FBF", linewidth = 0.6) +
  geom_text(data = confounders, aes(x = tx, y = ty, label = name, size = size)) +
  scale_size_identity() +
  
  # Direct effect
  annotate("segment", x = 0.9, xend = 9.1, y = 0, yend = 0,
           arrow = arrow(length = unit(0.12, "in"), type = "closed"),
           linewidth = 1, colour = "#2166AC", linetype = "dashed") +
  annotate("text", x = 5, y = -0.5, label = "Direct effect (tested)",
           size = 3, colour = "#2166AC", fontface = "italic") +
  
  # Confounder arrows to geotype
  geom_segment(data = dag_arrows_to_geotype, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.07, "in"), type = "closed"),
               linewidth = 0.4, colour = "grey50") +
  
  # Confounder arrows to outcome
  geom_segment(data = dag_arrows_to_outcome, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.07, "in"), type = "closed"),
               linewidth = 0.4, colour = "grey50") +
  
  # Between-confounder arrows
  geom_segment(data = dag_arrows_between, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.06, "in"), type = "closed"),
               linewidth = 0.3, colour = "grey50", linetype = "dotted") +
  
  # Legend
  annotate("rect", xmin = 2,   xmax = 2.4, ymin = -2.0, ymax = -1.7,
           fill = "#D6E4F5", colour = "#5B85BC", linewidth = 0.5) +
  annotate("text", x = 2.6, y = -1.85, label = "Exposure", size = 2.8, hjust = 0) +
  annotate("rect", xmin = 4,   xmax = 4.4, ymin = -2.0, ymax = -1.7,
           fill = "#D8E8D8", colour = "#7DAB7D", linewidth = 0.5) +
  annotate("text", x = 4.6, y = -1.85, label = "Outcome", size = 2.8, hjust = 0) +
  annotate("rect", xmin = 6,   xmax = 6.4, ymin = -2.0, ymax = -1.7,
           fill = "#E0DEF0", colour = "#8B7FBF", linewidth = 0.5) +
  annotate("text", x = 6.6, y = -1.85, label = "Confounder", size = 2.8, hjust = 0) +
  annotate("segment", x = 2, xend = 3, y = -2.5, yend = -2.5,
           linewidth = 0.5, colour = "#2166AC", linetype = "dashed",
           arrow = arrow(length = unit(0.06, "in"), type = "closed")) +
  annotate("text", x = 3.2, y = -2.5, label = "Direct effect", size = 2.5, hjust = 0) +
  annotate("segment", x = 5, xend = 6, y = -2.5, yend = -2.5,
           linewidth = 0.4, colour = "grey50",
           arrow = arrow(length = unit(0.06, "in"), type = "closed")) +
  annotate("text", x = 6.2, y = -2.5, label = "Confounding pathway", size = 2.5, hjust = 0) +
  annotate("segment", x = 8, xend = 9, y = -2.5, yend = -2.5,
           linewidth = 0.3, colour = "grey50", linetype = "dotted",
           arrow = arrow(length = unit(0.05, "in"), type = "closed")) +
  annotate("text", x = 9.2, y = -2.5, label = "Between confounders", size = 2.5, hjust = 0) +
  
  labs(title = "Directed Acyclic Graph: Geotype and Vaccine Confidence",
       subtitle = paste0(
         "Confounders selected a priori (Balogun et al., 2024; de Figueiredo et al., 2022).\n",
         "Mediator and independent-effect pathways described in Methods (Section 3.4)."
       )) +
  coord_cartesian(xlim = c(-1.5, 12), ylim = c(-3.2, 6.8)) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, colour = "grey30", hjust = 0.5, lineheight = 1.3),
        plot.margin = margin(15, 15, 15, 15))

save_gg(p_dag, "dag_confounders_only", 12, 8, also_pdf = TRUE)

cat("===== 03_FIGURES.R complete =====\n")
#END PHASE 3