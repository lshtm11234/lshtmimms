# ============================================================
# Conceptual framework: socioecological model of caregiver
# vaccine confidence
#
# FINAL VERSION v10 (locked)
# ============================================================
library(ggplot2)
library(ggforce)

# ---------------------------------------------------------
# 1. Palette
# ---------------------------------------------------------
tol_indigo   <- "#332288"
tol_teal     <- "#44AA99"
tol_cinnamon <- "#A5665A"
tol_blush    <- "#D4A29C"
tol_wine     <- "#882255"
txt_black    <- "#000000"

tint <- function(hex, amount = 0.82) {
  rgb_vals <- col2rgb(hex) / 255
  mix <- rgb_vals * (1 - amount) + amount
  rgb(mix[1], mix[2], mix[3])
}

col_ring_health  <- tint(tol_indigo,   0.90); str_health  <- tol_indigo
col_ring_comm    <- tint(tol_teal,     0.88); str_comm    <- tol_teal
col_ring_house   <- tint(tol_blush,    0.80); str_house   <- tol_cinnamon

str_residence <- tol_teal
str_sociodem  <- tol_cinnamon
str_3a        <- tol_indigo
str_3b        <- tol_teal
str_outcome   <- tol_wine
str_vuln      <- tol_cinnamon

# ---------------------------------------------------------
# 2. Font
# ---------------------------------------------------------
fam <- "serif"

# ---------------------------------------------------------
# 3. Rounded rectangle helper
# ---------------------------------------------------------
rounded_rect <- function(xmin, xmax, ymin, ymax, r = 0.6, n = 20) {
  r <- min(r, (xmax - xmin) / 2, (ymax - ymin) / 2)
  corner <- function(cx, cy, a0, a1) {
    a <- seq(a0, a1, length.out = n)
    data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
  }
  rbind(
    corner(xmax - r, ymax - r, 0, pi / 2),
    corner(xmin + r, ymax - r, pi / 2, pi),
    corner(xmin + r, ymin + r, pi, 3 * pi / 2),
    corner(xmax - r, ymin + r, 3 * pi / 2, 2 * pi)
  )
}
box_df <- function(id, xmin, xmax, ymin, ymax, r = 1.6) {
  d <- rounded_rect(xmin, xmax, ymin, ymax, r); d$id <- id; d
}

# ---------------------------------------------------------
# 4. Box coordinates
#    Vulnerable box pushed down 6 units for arrow breathing room
# ---------------------------------------------------------
boxes <- list(
  residence = c(xmin = 34, xmax = 86,  ymin = 110, ymax = 134),
  sociodem  = c(xmin = 96, xmax = 128, ymin = 60,  ymax = 90),
  m3a       = c(xmin = 4,  xmax = 56,  ymin = 2,   ymax = 42),
  m3b       = c(xmin = 64, xmax = 116, ymin = 2,   ymax = 42),
  outcome   = c(xmin = 30, xmax = 90,  ymin = -24, ymax = -4),
  vuln      = c(xmin = 34, xmax = 86,  ymin = -46, ymax = -32)
)
ctr <- function(b) c(x = mean(c(b["xmin"], b["xmax"])),
                     y = mean(c(b["ymin"], b["ymax"])))
box_polys <- do.call(rbind, lapply(names(boxes), function(nm) {
  box_df(nm, boxes[[nm]]["xmin"], boxes[[nm]]["xmax"],
         boxes[[nm]]["ymin"], boxes[[nm]]["ymax"])
}))

# ---------------------------------------------------------
# 5. Nested circles
# ---------------------------------------------------------
circle_cx <- 60; circle_cy <- 74
r_health <- 28; r_comm <- 19; r_house <- 9.5

# ---------------------------------------------------------
# 6. Build the plot
# ---------------------------------------------------------
p <- ggplot() +
  
  # ===== NESTED CIRCLES =====================================================
geom_circle(aes(x0 = circle_cx, y0 = circle_cy, r = r_health),
            fill = col_ring_health, color = str_health, linewidth = 2.8) +
  geom_circle(aes(x0 = circle_cx, y0 = circle_cy, r = r_comm),
              fill = col_ring_comm, color = str_comm, linewidth = 2.8) +
  geom_circle(aes(x0 = circle_cx, y0 = circle_cy, r = r_house),
              fill = col_ring_house, color = str_house, linewidth = 2.8) +
  
  annotate("text", x = circle_cx, y = circle_cy + r_health - 5,
           label = "Health System",
           fontface = "bold", size = 10, color = txt_black, family = fam) +
  annotate("text", x = circle_cx, y = circle_cy + r_comm - 4.5,
           label = "Community",
           fontface = "bold", size = 8.5, color = txt_black, family = fam) +
  annotate("text", x = circle_cx, y = circle_cy,
           label = "Caregiver",
           fontface = "bold", size = 8.0, color = txt_black, family = fam) +
  
  # ===== EXPOSURE BOX ======================================================
geom_polygon(data = subset(box_polys, id == "residence"), aes(x, y),
             fill = "white", color = str_residence, linewidth = 2.8) +
  annotate("text", x = ctr(boxes$residence)["x"], y = 128,
           label = "Exposure",
           fontface = "bold", size = 11.5, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$residence)["x"], y = 121,
           label = "Geotype",
           fontface = "bold", size = 10, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$residence)["x"], y = 114.5,
           label = "Urban, peri-urban, rural",
           size = 8.5, color = txt_black, family = fam) +
  
  # ===== SOCIODEMOGRAPHIC BOX ==============================================
geom_polygon(data = subset(box_polys, id == "sociodem"), aes(x, y),
             fill = "white", color = str_sociodem, linewidth = 2.8) +
  annotate("text", x = ctr(boxes$sociodem)["x"], y = 86,
           label = "Sociodemographic",
           fontface = "bold", size = 7.8, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$sociodem)["x"], y = 82,
           label = "Confounders",
           fontface = "bold", size = 7.8, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$sociodem)["x"], y = 73,
           label = "Caregiver age, gender,\neducation, employment,\nSES, religion, region",
           size = 6.8, color = txt_black, lineheight = 1.15, family = fam) +
  
  # ===== MODEL 3a — STRUCTURAL BARRIERS ===================================
geom_polygon(data = subset(box_polys, id == "m3a"), aes(x, y),
             fill = "white", color = str_3a, linewidth = 2.8) +
  annotate("text", x = ctr(boxes$m3a)["x"], y = 38,
           label = "Structural Barriers",
           fontface = "bold", size = 8.5, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$m3a)["x"], y = 32.5,
           label = "Model 3a",
           fontface = "italic", size = 7.2, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$m3a)["x"], y = 16,
           label = "Ease of access\nKnows where to vaccinate\nHCW recommendation\nHealth system recall\nEver turned away\nBarrier count",
           size = 7.0, color = txt_black, lineheight = 1.15, family = fam) +
  
  # ===== MODEL 3b — SOCIAL NORMS ==========================================
geom_polygon(data = subset(box_polys, id == "m3b"), aes(x, y),
             fill = "white", color = str_3b, linewidth = 2.8) +
  annotate("text", x = ctr(boxes$m3b)["x"], y = 38,
           label = "Social Norms",
           fontface = "bold", size = 8.5, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$m3b)["x"], y = 32.5,
           label = "Model 3b",
           fontface = "italic", size = 7.2, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$m3b)["x"], y = 18,
           label = "Peer norms\nFamily norms\nReligious leader norms\nCommunity leader norms",
           size = 7.0, color = txt_black, lineheight = 1.15, family = fam) +
  
  # ===== OUTCOME BOX ======================================================
geom_polygon(data = subset(box_polys, id == "outcome"), aes(x, y),
             fill = "white", color = str_outcome, linewidth = 2.8) +
  annotate("text", x = 60, y = -8,
           label = "Outcome",
           fontface = "bold", size = 11.5, color = txt_black, family = fam) +
  annotate("text", x = 60, y = -14.5,
           label = "Caregiver Vaccine Confidence",
           fontface = "bold", size = 10, color = txt_black, family = fam) +
  annotate("text", x = 60, y = -20,
           label = "Importance and safety",
           size = 8.5, color = txt_black, family = fam) +
  
  # ===== VULNERABLE SUBGROUPS BOX =========================================
geom_polygon(data = subset(box_polys, id == "vuln"), aes(x, y),
             fill = "white", color = str_vuln,
             linewidth = 2.6, linetype = "dashed") +
  annotate("text", x = ctr(boxes$vuln)["x"], y = -37,
           label = "Vulnerable Subgroups",
           fontface = "bold", size = 8.0, color = txt_black, family = fam) +
  annotate("text", x = ctr(boxes$vuln)["x"], y = -42,
           label = "Migrant status, child disability",
           size = 7.0, color = txt_black, family = fam) +
  
  # ===== ARROWS ============================================================
# 1. Exposure --> Health system
annotate("segment", x = 60, xend = 60, y = 110, yend = 103,
         arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
         linewidth = 2.4, colour = str_residence) +
  
  # 2. Sociodemographic --> Caregiver circle
  annotate("segment", x = 96, xend = 71, y = 74, yend = 74,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.4, colour = str_sociodem) +
  
  # 3. Health system --> Structural Barriers
  annotate("segment", x = 46, xend = 31, y = 52, yend = 43,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.4, colour = str_3a) +
  
  # 4. Community --> Social Norms
  annotate("segment", x = 70, xend = 89, y = 62, yend = 43,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.4, colour = str_3b) +
  
  # 5. Structural Barriers --> Outcome
  annotate("segment", x = 38, xend = 45, y = 2, yend = -3.5,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.4, colour = str_outcome) +
  
  # 6. Social Norms --> Outcome
  annotate("segment", x = 82, xend = 75, y = 2, yend = -3.5,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.4, colour = str_outcome) +
  
  # 7. Vulnerable Subgroups --> Outcome
  annotate("segment", x = 60, xend = 60, y = -32, yend = -25,
           arrow = arrow(length = unit(4.5, "mm"), type = "closed"),
           linewidth = 2.2, colour = str_vuln, linetype = "dashed") +
  annotate("text", x = 65, y = -28.5, label = "moderates",
           fontface = "italic", size = 6.5, colour = txt_black,
           hjust = 0, family = fam) +
  
  # ===== CANVAS ============================================================
coord_fixed(xlim = c(0, 130), ylim = c(-48, 138), expand = FALSE) +
  theme_void(base_family = fam) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(14, 14, 14, 14))

# ---------------------------------------------------------
# 7. Save
# ---------------------------------------------------------
ggsave("conceptual_framework.png", p,
       width = 16, height = 19, dpi = 500, bg = "white")

cat("Saved: conceptual_framework.png\n")
# =============================================================================
# BOTTOM-BOX OUTCOME BY STATE — active hesitancy/refusal, mirror of top-box
# =============================================================================

# Bottom-box = strongly disagree on BOTH importance (B1) and safety (B2)
# Recall: confident = as.numeric(B1 == 4 & B2 == 4) used 4 = "strongly agree"
# so 1 = "strongly disagree" on the same 4-point scale.

ng <- ng %>%
  mutate(bottom_box = case_when(
    B1 == 99 | B2 == 99 ~ NA_real_,
    B1 == 1 & B2 == 1   ~ 1,
    TRUE                ~ 0
  ))

us <- us %>%
  mutate(bottom_box = case_when(
    B1 == 99 | B2 == 99 ~ NA_real_,
    B1 == 1 & B2 == 1   ~ 1,
    TRUE                ~ 0
  ))

cat("Bottom-box prevalence — NG:", round(100*mean(ng$bottom_box, na.rm=TRUE),1),
    "%  US:", round(100*mean(us$bottom_box, na.rm=TRUE),1), "%\n")

# Rebuild survey designs to include the new variable
ng_svy_bb <- ng %>% as_survey_design(ids = psu, weights = wt_r)
us_svy_bb <- us %>% as_survey_design(ids = 1,   weights = wt_r)

# State-level weighted % bottom-box, same structure as ng_states/us_states
bottom_box_state_tbl <- function(design, has_psu = TRUE) {
  base <- svyby(~bottom_box, ~state, design, svymean, na.rm = TRUE, vartype = "ci") %>%
    as_tibble() %>%
    transmute(state,
              pct = round(100*bottom_box,1), lci = round(100*ci_l,1),
              uci = round(100*ci_u,1), width = round(100*(ci_u-ci_l),1))
  base %>% arrange(desc(pct))
}

ng_bottom_states <- bottom_box_state_tbl(ng_svy_bb)
us_bottom_states <- bottom_box_state_tbl(us_svy_bb)

cat("\n===== NIGERIA — BOTTOM-BOX (ACTIVE HESITANCY) BY STATE =====\n")
print(as.data.frame(ng_bottom_states), row.names = FALSE)

cat("\n===== USA — BOTTOM-BOX (ACTIVE HESITANCY) BY STATE =====\n")
print(as.data.frame(us_bottom_states), row.names = FALSE)