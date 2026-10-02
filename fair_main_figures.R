pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, MASS)

# source("useful_functions.R")
# source("generate_hyp.R")
hyp = readRDS("data/data_updated.rds")
################################################################################

#-------------------------------------------------------------------------------
#             FIGURE: FEMA grant dist and recommended aid heterogeneity        |           
#-------------------------------------------------------------------------------

table(hyp$info_arm, hyp$second_home)      # the 6 cells

# #Percent recommended aid outcome
# full_model1 <- feols(
#   percent_aid ~ second_home * prior_info * adaptive_measures +
#     DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
#     Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
#   data = hyp,
#   vcov = ~ResponseID
# )
# summary(full_model)
full_model <- feols(
  percent_aid ~ info_arm*second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
summary(full_model)
# Robustness removing "Missing" govtrustbin and gap_quartile

#test = feols(percent_aid ~ second_home * prior_info * adaptive_measures +
#         DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
#         Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
#       data = hyp %>% filter(GovTrustBin != "Missing", gap_quartile != "Missing"),
#       vcov = ~ResponseID)
# ── 2) Base case (intercept) ────────────────────────────────────────────────
base_case_predicted <- coef(full_model)["(Intercept)"]

# ── 3) Build plot_data_full (demographic covariates only; centered on intercept) ──
model_results <- tidy(full_model) %>%
  filter(term != "(Intercept)") %>%
  # exclude treatment terms & Gender from the heterogeneity figure
  filter(!str_detect(term, "second_home|prior_info|adaptive_measures|Gender"))

plot_data <- model_results %>%
  mutate(
    covariate = case_when(
      str_detect(term, "RiskAversion_bin")            ~ "RiskAversion_bin",
      str_detect(term, "Party")                 ~ "Party",
      str_detect(term, "GovTrustBin")           ~ "GovTrustBin",
      str_detect(term, "gap_quartile")          ~ "gap_quartile",
      term == "DisasterExperience"              ~ "DisasterExperience",
      str_detect(term, "AnnualIncome_grouped")  ~ "AnnualIncome_grouped",
      TRUE ~ NA_character_
    ),
    term_clean = term %>%
      str_remove("^RiskAversion_bin") %>%
      str_remove("^Party") %>%
      str_remove("^GovTrustBin") %>%
      str_remove("^gap_quartile") %>%
      str_remove("^AnnualIncome_grouped") %>%
      str_replace("^DisasterExperience$", "Has disaster experience"),
    estimate_centered = base_case_predicted + estimate,
    lower_centered    = estimate_centered - 1.96 * std.error,
    upper_centered    = estimate_centered + 1.96 * std.error,
    covariate_label = case_when(
      covariate == "RiskAversion_bin"           ~ "Risk Preference",
      covariate == "Party"                ~ "Political Party",
      covariate == "GovTrustBin"          ~ "Government Trust",
      covariate == "gap_quartile"         ~ "Tax Progressivity",
      covariate == "DisasterExperience"   ~ "Disaster Experience",
      covariate == "AnnualIncome_grouped" ~ "Annual Income",
      TRUE ~ covariate
    )
  ) %>%
  filter(!is.na(covariate))
baseline_groups <- tibble(
  term = c("Risk neutral", "Independent", "High government trust",
           "Mid-range tax progressive", "No disaster experience",
           "Annual Income $100,000 to $249,999"),
  covariate = c("RiskAversion_bin", "Party", "GovTrustBin",
                "gap_quartile", "DisasterExperience",
                "AnnualIncome_grouped"),
  term_clean = c("Risk neutral", "Independent", "High government trust",
                 "Mid-range tax progressive", "No disaster experience",
                 "Annual Income $100,000 to $249,999"),
  estimate = 0,
  std.error = 0,
  estimate_centered = base_case_predicted,
  lower_centered    = base_case_predicted,
  upper_centered    = base_case_predicted,
  covariate_label = c("Risk Preference", "Political Party", "Government Trust",
                      "Tax Progressivity", "Disaster Experience", "Annual Income"),
  p.value = NA_real_
)

desired_levels <- c(
  # Annual Income
  "Annual Income > $250,000",
  "Annual Income $100,000 to $249,999",
  "Annual Income < $100,000",
  # Disaster Experience
  "Has disaster experience", "No disaster experience",
  # Tax Progressivity
  "Most tax progressive", "Mid-range tax progressive", "Least tax progressive",
  # Government Trust
  "Low government trust", "High government trust",
  # Political Party
  "Democrat", "Independent", "Republican",
  # Risk Preference
  "Risk tolerant", "Risk neutral", "Risk averse"
)

plot_data_full <- bind_rows(plot_data, baseline_groups) %>%
  filter(term_clean != "Other") %>%
  mutate(
    covariate_label = factor(
      covariate_label,
      levels = c(
        "Annual Income", "Disaster Experience",
        "Tax Progressivity", "Government Trust",
        "Political Party", "Risk Preference"
      )
    ),
    term_clean = factor(term_clean, levels = desired_levels)
  )

# ── 4) Color palette for covariate facets ───────────────────────────────────
palette2 <- c("goldenrod", "purple2", "forestgreen", "#e7298a", "#d95f02", "blue4")
covariate_colors <- c(
  "Annual Income"        = palette2[1],
  "Disaster Experience"  = palette2[2],
  "Tax Progressivity"    = palette2[3],
  "Government Trust"     = palette2[4],
  "Political Party"      = palette2[5],
  "Risk Preference"      = palette2[6]
)
library(data.table)
# === IA registrations: HA (% of verified damage), CPI-adjusted to 2024 ===
ia_raw = data.table::fread("C:\\Users\\indumati\\Box\\FEMA DATA\\Individual Assistance\\IndividualsAndHouseholdsProgramValidRegistrationsV2_2026.csv")

library(lubridate)   # build_owners_noRA() calls year() unprefixed

# Annual CPI, 2024 base
getSymbols("CPIAUCSL", src = "FRED", warnings = FALSE, quiet = TRUE)
cpi_year <- data.frame(date = index(CPIAUCSL), cpi = as.numeric(CPIAUCSL)) |>
  mutate(year_decl = lubridate::year(date)) |>
  group_by(year_decl) |>
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
cpi_2024 <- cpi_year$cpi[cpi_year$year_decl == 2024]

na_award_as_zero <- TRUE
ha_cap           <- 43600
large_loss       <- 40000   # 2024 dollars

build_owners_noRA <- function(ho = c(0,1), fl = c(0,1)) {
  ia_raw |>
    filter(!is.na(rpfvl) | !is.na(ppfvl), ownRent == "O",
           homeOwnersInsurance %in% ho, floodInsurance %in% fl) |>
    mutate(
      verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0),
      haAmount      = if (na_award_as_zero) coalesce(haAmount, 0) else haAmount,
      loss_award    = haAmount - coalesce(rentalAssistanceAmount, 0),
      year_decl     = year(as.Date(sub("T.*$", "", declarationDate)))
    ) |>
    filter(verified_loss > 0, is.na(haAmount) | haAmount <= ha_cap) |>
    left_join(cpi_year, by = "year_decl") |>
    mutate(verified_loss_2024 = verified_loss * cpi_2024 / cpi,
           comp_rate          = loss_award / verified_loss) |>
    filter(is.finite(comp_rate), comp_rate >= 0, !is.na(verified_loss_2024))
}

summarise_large <- function(d) {
  d |> filter(verified_loss_2024 >= large_loss) |>
    summarise(n           = n(),
              mean_pct    = mean(comp_rate) * 100,
              median_pct  = median(comp_rate) * 100,
              share_lt_10 = mean(comp_rate < 0.10) * 100)
}

# Uninsured owners (no HO, no flood), HA net of rental assistance
ia <- build_owners_noRA(ho = c(0,1), fl = c(0,1)) |>
  mutate(ha_pct_of_damage = pmin(comp_rate * 100, 100))   # same 0–100 cap as before #199877 rows

summarise_large(ia)

# --------------------------------------------------------------------------------------------
# ── 5) Heterogeneity side: keep estimates in percent (no $ conversion) ──────
baseline_terms <- c(
  "Risk neutral","Independent","High government trust",
  "Mid-range tax progressive","No disaster experience",
  "Annual Income $100,000 to $249,999"
)

plot_data_nobase_pct <- plot_data_full %>%
  filter(!(term_clean %in% baseline_terms)) %>%
  mutate(
    sig_level = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

plot_data_numeric_pct <- plot_data_nobase_pct %>%
  mutate(
    y_numeric = as.numeric(factor(
      paste(covariate_label, term_clean),
      levels = rev(unique(paste(covariate_label, term_clean)))
    ))
  )

# ── 6) BINNED IA densities ────────
ia <- ia %>%
  mutate(
    damage_bin = if_else(verified_loss_2024 >= 40000, "≥ $40k", "< $40k")
  )

# Add density for ALL damages
dens_all <- density(
  ia$ha_pct_of_damage,
  adjust = 1.5, from = 0, to = 100, na.rm = TRUE
)

dens_ge40 <- density(
  ia$ha_pct_of_damage[ia$damage_bin == "≥ $40k"],
  adjust = 1.5, from = 0, to = 100, na.rm = TRUE
)

ia_ha_density_2 <- bind_rows(
  tibble(x = dens_all$x, dens = dens_all$y, damage_bin = "All damages"),
  tibble(x = dens_ge40$x, dens = dens_ge40$y, damage_bin = "≥ $40k")
)

ia_ha_mean_2 <- ia %>%
  group_by(damage_bin) %>%
  summarise(mean_ha_pct = mean(ha_pct_of_damage, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    tibble(
      damage_bin = "All damages",
      mean_ha_pct = mean(ia$ha_pct_of_damage, na.rm = TRUE)
    )
  )

# ── 7) y scaling to match heterogeneity rows ─────────────────────────────────

y_max <- max(plot_data_numeric_pct$y_numeric, na.rm = TRUE)
dens_max <- max(ia_ha_density_2$dens, na.rm = TRUE)
scale_factor <- (y_max * 0.9) / dens_max

ia_ha_pct_density_2 <- ia_ha_density_2 %>%
  mutate(dens_scaled = dens * scale_factor)

left_breaks_raw <- scales::breaks_pretty(n = 5)(c(0, dens_max))
left_breaks_pos <- left_breaks_raw * scale_factor

# ── 8) x range (percent) ────────────────────────────────────────────────────

x_max_pct <- max(
  ia_ha_pct_density_2$x,
  plot_data_numeric_pct$estimate_centered,
  plot_data_numeric_pct$upper_centered,
  base_case_predicted,
  ia_ha_mean_2$mean_ha_pct,
  na.rm = TRUE
)

x_end <- 110
# ── 9) Build combined plot ──────────────────────────────────────────────────
ihp_max       <- 43600
ihp_loss      <- 250000
ihp_pct_line  <- ihp_max / ihp_loss * 100   # ~17.4%



ref_lines <- data.frame(
  x      = c(base_case_predicted, ihp_pct_line),
  label  = c("Baseline recommended aid",
             "IHP max coverage\nfor $250,000 loss"),
  colour = c("gray40", "steelblue4")
)
ref_lines <- ref_lines[order(ref_lines$x), ]
ref_lines$hjust <- c(1.04, -0.04)          # left line -> label to its left; right line -> to its right
ref_lines$y     <- y_max * c(0.99, 0.86)   # stagger heights as a second safeguard



combined_plot_pct <- ggplot() +
  geom_line(
    data = ia_ha_pct_density_2,
    aes(x = x, y = dens_scaled, linetype = damage_bin),
    colour = "red4",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = base_case_predicted,
    colour = "gray35",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = ihp_pct_line,
    colour = "gray35",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_errorbar(
    data = plot_data_numeric_pct,
    aes(
      x = estimate_centered,
      y = y_numeric,
      xmin = lower_centered,
      xmax = upper_centered,
      colour = covariate_label
    ),
    width = 0.25,
    linewidth = 1.6
  ) +
  geom_point(
    data = plot_data_numeric_pct,
    aes(x = estimate_centered, y = y_numeric, colour = covariate_label),
    size = 4.2
  ) +
  geom_text(
    data = plot_data_numeric_pct,
    aes(x = estimate_centered, y = y_numeric, label = sig_level),
    vjust = -0.2,
    size = 8,
    colour = "black",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = covariate_colors, guide = "none") +
  scale_linetype_manual(
    values = c("All damages" = "dashed", "≥ $40k" = "solid"),
    name = "Damage Assessed"
  ) + 
  scale_x_continuous(
    breaks = seq(0, x_end, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0),
    guide  = guide_axis(n.dodge = 1)
  ) +
  coord_cartesian(xlim = c(0, x_end)) +
  scale_y_continuous(
    limits = c(0, y_max * 1.08),
    expand = c(0, 0),
    breaks = left_breaks_pos,
    labels = function(b) scales::number(b / scale_factor, accuracy = 0.01),
    name   = "Density",
    sec.axis = sec_axis(
      ~ .,
      breaks = plot_data_numeric_pct$y_numeric,
      labels = plot_data_numeric_pct$term_clean,
      name   = NULL
    )
  ) +
  labs(
    x = "Aid Award as Percent of Disaster Damages",
    y = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position    = "bottom",
    legend.title = element_text(size = 12),
    legend.key.width   = grid::unit(2.5, "cm"),
    axis.text.y.right  = element_text(hjust = 0, margin = margin(l = 6)),
    text               = element_text(size = 16),
    axis.text.x        = element_text(size = 14),
    axis.text.y        = element_text(size = 14),
    axis.title.x       = element_text(size = 16),
    axis.title.y       = element_text(size = 16),
    panel.spacing      = grid::unit(1, "lines"),
    axis.line.x        = element_line(color = "black", linewidth = 0.6),
    axis.line.y.left   = element_line(color = "black", linewidth = 0.6),
    axis.line.y.right  = element_line(color = "black", linewidth = 0.6),
    panel.border       = element_blank()
  ) +
  guides(
    linetype = guide_legend(
      keywidth = grid::unit(1.2, "cm")
    )
  ) +  annotate(
    "text",
    x = base_case_predicted,
    y = y_max * 1.04,
    label = "Baseline\nrecommended aid",
    hjust = 1.05, vjust = 1,
    size = 4.8, colour = "gray40",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = ihp_pct_line,
    y = y_max * 1.04,
    label = "IHP \nmax coverage\nfor $250K loss",
    hjust = 1.05, vjust = 1,
    size = 4.8, colour = "gray35",
    lineheight = 0.9
  )

# Extract the y-axis labels and their corresponding colors
axis_label_colors <- plot_data_numeric_pct %>%
  arrange(desc(y_numeric)) %>%
  pull(covariate_label) %>%
  as.character() %>%
  sapply(function(x) covariate_colors[x])

# Apply colored labels to the right y-axis
combined_plot_pct <- combined_plot_pct +
  theme(
    axis.text.y.right = element_text(
      hjust = 0, 
      margin = margin(l = 6),
      colour = axis_label_colors
    )
  )

combined_plot_pct

ggsave("Figures\\dist_plot_homeowners_noRA.png",
       plot = combined_plot_pct, width = 12, height = 7, dpi = 300)
ggsave("Figures\\dist_plot_7.30.png",
       plot = combined_plot_pct, width = 12, height = 7, dpi = 300)

# ggsave("L:\\Wetland Flood Mitigation\\Disaster aid survey\\EDA Figures\\Fairness paper figures\\fig1.png", plot = combined_plot_pct, width = 12, height = 7, dpi = 300)


#-------------------------------------------------------------------------------
#             FIGURE: MODELS for RESP and PERCENT AID                          |           
#-------------------------------------------------------------------------------
hyp <- hyp %>%
  mutate(
    RiskAversion_bin = factor(RiskAversion_bin),
    GovTrustBin      = factor(GovTrustBin)
  )

hyp <- hyp %>%
  mutate(
    RiskAversion_bin = relevel(RiskAversion_bin, ref = "Risk neutral"),
    GovTrustBin      = relevel(GovTrustBin, ref = "High government trust") # pick yours
  )

plot_scenario_effects <- function(model,
                                  data,
                                  cluster_var = "ResponseID",
                                  outcome_label = "Predicted Responsibility (1–10)",
                                  save_path = NULL,
                                  width = 12, height = 6, dpi = 300,
                                  include_second_home = FALSE) {
  
  # ---------------------------
  # Scenario grid at baseline controls
  # ---------------------------
  grid <- data %>%
    dplyr::ungroup() %>%
    dplyr::distinct(second_home, prior_info, adaptive_measures) %>%
    dplyr::arrange(second_home, prior_info, adaptive_measures)
  
  # baseline value of second_home, whether factor ("primary") or numeric (0)
  sh_base <- if (is.factor(data$second_home)) levels(data$second_home)[1] else 0
  # the include_second_home filter
  if (!include_second_home) {
    grid <- grid %>% dplyr::filter(second_home == sh_base)
  }
  # helper to safely grab first level
  first_level <- function(x) {
    if (is.factor(x)) levels(x)[1] else sort(unique(x))[1]
  }
  
  # set controls to baseline (keep factors as factors)
  baseline_vals <- list(
    DisasterExperience = 0,
    Gender        = first_level(data$Gender),
    AgeGroup      = first_level(data$AgeGroup),
    AnnualIncome_grouped  = first_level(data$AnnualIncome_grouped),
    Party         = first_level(data$Party),
    Race2         = first_level(data$Race2),
    RiskAversion_bin = first_level(data$RiskAversion_bin),
    GovTrustBin      = first_level(data$GovTrustBin),
    gap_quartile  = first_level(data$gap_quartile)
  )
  
  for (nm in names(baseline_vals)) {
    grid[[nm]] <- baseline_vals[[nm]]
  }
  
  grid$base_noadapt <- 0
  
  # ---------------------------
  # Predictions with clustered SEs
  # ---------------------------
  vcv_cl <- vcov(model, cluster = cluster_var)
  
  pred <- predict(
    model,
    newdata = grid,
    se.fit  = TRUE,
    vcov    = vcv_cl
  )
  
  grid <- grid %>%
    dplyr::mutate(
      est  = as.numeric(pred$fit),
      se   = as.numeric(pred$se.fit),
      low  = est - 1.96 * se,
      high = est + 1.96 * se,
      Scenario = dplyr::case_when(
        prior_info == 0 & adaptive_measures == 0 ~
          "No prior info, no adaptation \n(base case)",
        prior_info == 1 & adaptive_measures == 0 ~
          "Prior info, no adaptation",
        prior_info == 1 & adaptive_measures == 1 ~
          "Prior info, adaptation",
        TRUE ~ "Other"
      ),
      Home_Type = dplyr::if_else(second_home == sh_base, "Primary Residence", "Second Home"),
      is_base_case = (second_home == sh_base & prior_info == 0 & adaptive_measures == 0),
      point_color  = ifelse(is_base_case, "black", Home_Type)
    )
  
  # ---------------------------
  # Stars vs baseline
  # ---------------------------
  X <- model.matrix(model, data = grid)
  
  common <- intersect(colnames(X), names(coef(model)))
  X <- X[, common, drop = FALSE]
  b <- coef(model)[common]
  V <- vcv_cl[common, common]
  
  # Base row: primary residence + no prior info + no adaptation
  base_row <- which(grid$is_base_case)
  if (length(base_row) != 1) {
    stop("Could not uniquely identify the baseline scenario row. Check grid construction.")
  }
  
  x0 <- X[base_row, , drop = FALSE]
  Xdiff <- sweep(X, 2, x0)
  
  diff_est <- as.numeric(Xdiff %*% b)
  diff_se  <- sqrt(diag(Xdiff %*% V %*% t(Xdiff)))
  p_val    <- 2 * pnorm(-abs(diff_est / diff_se))
  
  grid <- grid %>%
    dplyr::mutate(
      stars = dplyr::case_when(
        p_val < .001 ~ "***",
        p_val < .01  ~ "**",
        p_val < .05  ~ "*",
        p_val < .1   ~ ".",
        TRUE         ~ ""
      )
    )
  
  # Scenario order (base in the middle)
  grid <- grid %>%
    dplyr::mutate(
      Scenario = factor(
        Scenario,
        levels = c("Prior info, no adaptation",
                   "No prior info, no adaptation \n(base case)",
                   "Prior info, adaptation")
      )
    )
  
  # stars below lower whisker
  y_gap <- diff(range(c(grid$low, grid$high), na.rm = TRUE)) * 0.008
  grid <- grid %>% dplyr::mutate(y_star = low - y_gap)
  
  pd <- position_dodge(width = 0.35)
  n_home <- dplyr::n_distinct(grid$Home_Type)
  
  if (n_home == 1) {
    p <- ggplot(grid, aes(x = Scenario, y = est)) +
      # draw dashed line FIRST so points sit on top
      geom_hline(yintercept = grid$est[grid$is_base_case],
                 linetype = "dashed", colour = "black", linewidth = 1) +
      geom_errorbar(aes(ymin = low, ymax = high),
                    width = 0, size = 1.1) +
      geom_point(aes(shape = is_base_case),
                 colour = "#D55E00",
                 size = 4.5) +
      geom_text(aes(y = y_star, label = stars),
                vjust = 1.1, size = 6, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      expand_limits(y = min(grid$y_star, na.rm = TRUE) - y_gap * 0.5) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), guide = "none") +
      labs(x = NULL, y = outcome_label) +
      theme_minimal(base_size = 18) +
      theme(
        axis.title.y       = element_text(size = 20, margin = margin(r = 12)),
        axis.text.x        = element_text(size = 16),
        axis.text.y        = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        plot.margin        = margin(t = 10, r = 10, b = 25, l = 55)
      )
    
  } else {
    grid <- grid %>%
      dplyr::mutate(
        Home_Type   = factor(Home_Type, levels = c("Second Home", "Primary Residence")),
        point_color = factor(point_color, levels = c("black", "Second Home", "Primary Residence"))
      )
    
    p <- ggplot(grid, aes(x = Scenario, y = est, group = Home_Type)) +
      # draw dashed line FIRST so points (triangle) sit on top
      geom_hline(yintercept = grid$est[grid$is_base_case],
                 linetype = "dashed", colour = "black", linewidth = 1) +
      geom_errorbar(aes(ymin = low, ymax = high, colour = Home_Type),
                    width = 0, size = 1.1, position = pd) +
      geom_point(aes(shape = is_base_case, colour = point_color),
                 size = 4.5, position = pd) +
      geom_text(aes(y = y_star, label = stars),
                vjust = 1.1, position = pd, size = 6, show.legend = FALSE) +
      coord_cartesian(clip = "off") +
      expand_limits(y = min(grid$y_star, na.rm = TRUE) - y_gap * 0.5) +
      scale_colour_manual(
        values = c(
          "Primary Residence" = "#D55E00",
          "Second Home"       = "#0072B2",
          "black"             = "black"
        ),
        breaks = c("Primary Residence", "Second Home"),
        labels = c("Primary Residence", "Second Home")
      ) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), guide = "none") +
      labs(x = NULL, y = outcome_label, colour = "Home Type") +
      theme_minimal(base_size = 18) +
      theme(
        axis.title.y       = element_text(size = 20, margin = margin(r = 12)),
        axis.text.x        = element_text(size = 16),
        axis.text.y        = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position    = "bottom",
        legend.title       = element_text(size = 16),
        legend.text        = element_text(size = 14),
        plot.margin        = margin(t = 100, r = 10, b = 25, l = 55)
      )
  }
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = width, height = height, dpi = dpi)
  }
  
  return(p)
}


### MODELS ###
hyp$resp = as.numeric(hyp$resp)
m_resp <- feols(
  resp ~ second_home * prior_info * adaptive_measures +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)

m_percaid <- feols(
  percent_aid ~ second_home * prior_info * adaptive_measures + 
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)


p_resp <- plot_scenario_effects(
  model = m_resp,
  data  = hyp,
  outcome_label = "Predicted Responsibility (1–10)",
  include_second_home = TRUE,
  save_path = "Figures/resp_plot_9.2.png"
)
p_resp
p_aid <- plot_scenario_effects(
  model = m_percaid,
  data  = hyp,
  outcome_label = "Predicted Recommended Government Aid\n(% of Loss)",
  include_second_home = TRUE,
  save_path = "Figures/aid_plot_9.2.png")

p_aid

#-----------------------------------------------------------------------------------------
#   FIGURE:  Ridge plot of perceived responsibility against recommended government aid   |           
#-----------------------------------------------------------------------------------------

library(ggridges)

plot_ridge_resp_aid <- function(hyp) {
  # ensure numeric response and bounded aid
  hyp <- hyp %>%
    mutate(
      resp = as.numeric(resp),
      percent_aid = as.numeric(percent_aid)
    )
  
  # tidy factors and scenario label (kept in case you want to facet later)
  hyp_plot <- hyp %>%
    mutate(
      second_home_f = if_else(second_home == 1, "Second home", "Primary home"),
      prior_info_f  = if_else(prior_info  == 1, "Had prior info", "No prior info"),
      adaptive_f    = if_else(adaptive_measures == 1, "Adapted", "Did not adapt"),
      scenario      = interaction(second_home_f, prior_info_f, adaptive_f, sep = " | "),
      # ordered levels 0..10 even if some are missing in data
      resp_factor   = factor(resp, levels = 0:10)
    )
  
  # per-level means (drops NAs safely)
  aid_means <- hyp_plot %>%
    group_by(resp_factor) %>%
    summarize(mean_aid = mean(percent_aid, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(resp_factor))
  
  # global linear fit and predictions at integer resp 0..10
  lm_fit <- lm(percent_aid ~ resp, data = hyp_plot)
  pred_df <- data.frame(resp = 0:10)
  pred_df$pred_aid <- predict(lm_fit, newdata = pred_df)
  pred_df$resp_factor <- factor(pred_df$resp, levels = 0:10)
  
  # plot
  p_left <- ggplot(hyp_plot, aes(x = percent_aid, y = resp_factor)) +
    geom_density_ridges(
      scale = 2,
      rel_min_height = 0.01,
      fill = "grey85",
      color = "grey40",
      alpha = 0.9,
      size = 0.3
    ) +
    # mean points (x = mean aid for each responsibility level)
    geom_point(
      data = aid_means,
      aes(x = mean_aid, y = resp_factor),
      inherit.aes = FALSE,
      size = 2.2
    ) +
    # best-fit line across responsibility levels
    geom_line(
      data = pred_df,
      aes(x = pred_aid, y = resp_factor, group = 1),
      inherit.aes = FALSE,
      linewidth = 0.9
    ) +
    scale_x_continuous(
      name = "Recommended government aid (% of loss)",
      limits = c(0, 100)
    ) +
    scale_y_discrete(
      name = "Perceived responsibility"
    ) +
    coord_flip() +
    theme_minimal(base_size = 18) +
    theme(
      axis.title.x       = element_text(size = 20),
      axis.title.y       = element_text(size = 20),
      axis.text.x        = element_text(size = 16),
      axis.text.y        = element_text(size = 16),
      panel.grid.minor   = element_blank(),
      legend.position    = "none"
    )
  
  p_left
}

# usage
p_ridge <- plot_ridge_resp_aid(hyp)
p_ridge
# # Fit the linear model
lm_fit <- feols(percent_aid ~ resp, data = hyp, vcov = ~ResponseID )
summary(lm_fit)

ggsave("Figures/ridge_plot_9.21.png",
        p_ridge, width = 12, height = 7, dpi = 300) # ADD R2 IN MSPAINT




#-----------------------------------------------------------------------------------------
#                            FIGURE:  MEDIATION                                          |           
#-----------------------------------------------------------------------------------------
library(dplyr)
library(mediation)
library(fixest)

hyp <- readRDS("data/data_updated.rds")

# ---- 1. Exposures ---------------------------------------------------

table(hyp$info_arm, hyp$second_home)      # the 6 cells


# ---- 2. Estimation sample -------------------------------------------
# Subset ONCE so the mediator and outcome models are fit on identical rows.

W_all <- c("Gender", "AgeGroup", "AnnualIncome_grouped", "Race2", "DisasterExperience", "Party", "GovTrustBin", "gap_quartile", "RiskAversion_bin", "hazard", "AnnualIncome", "RiskAversion")

est <- hyp %>%
  dplyr::select(all_of(c("percent_aid", "resp", "info_arm", "second_home",
                         "hazard", W_all, "ResponseID"))) %>%
  filter(complete.cases(.)) %>%
  as.data.frame()

nrow(est); n_distinct(est$ResponseID)
est$resp <- as.numeric(est$resp)

W = c("Gender", "AgeGroup", "AnnualIncome_grouped", "Race2", "DisasterExperience", "Party", "GovTrustBin", "gap_quartile", "RiskAversion_bin", "hazard")



# ---- 3. The two models ----------------------------------------------

rhs <- paste(W, collapse = " + ")

m_M <- lm(as.formula(paste("resp ~ info_arm + second_home +", rhs)), est)
m_Y <- lm(as.formula(paste("percent_aid ~ info_arm * resp + second_home * resp +",
                           rhs)), est)

# # No covariates -- treatment and mediator only
# m_M0 <- lm(resp ~ info_arm + second_home, est)
# m_Y0 <- lm(percent_aid ~ info_arm * resp + second_home * resp, est)

summary(m_M)   # X -> M
summary(m_Y)   # M -> Y, the direct X -> Y edges (the dashed arrows), and X:M


# ---- 4. ACME / ADE / total, one contrast at a time -------------------
# The same two fitted models serve all three contrasts -- mediate() just
# changes which exposure it perturbs. Resampling is by respondent, since
# each contributes two vignettes.

run_med <- function(treat, from, to, sims = 1000) {
  set.seed(2026)
  mediation::mediate(m_M, m_Y, treat = treat, mediator = "resp",
                     control.value = from, treat.value = to,
                     sims = sims, cluster = est$ResponseID)
}

med_X1 <- run_med("info_arm",    "none",    "info")         # info vs none
med_X2 <- run_med("info_arm",    "info",    "info_adapt")   # adapt | info
med_X3 <- run_med("second_home", "primary", "second")       # second home

summary(med_X1); summary(med_X2); summary(med_X3)

tidy_med <- function(x, label) {
  data.frame(
    contrast = label,
    ACME  = x$d.avg,    ACME_lo = x$d.avg.ci[1], ACME_hi = x$d.avg.ci[2],
    ADE   = x$z.avg,    ADE_lo  = x$z.avg.ci[1], ADE_hi  = x$z.avg.ci[2],
    Total = x$tau.coef, Tot_lo  = x$tau.ci[1],   Tot_hi  = x$tau.ci[2],
    PropMed = x$n.avg,  row.names = NULL)
}

results <- bind_rows(
  tidy_med(med_X1, "X1: prior info vs. none"),
  tidy_med(med_X2, "X2: adaptation vs. prior info only"),
  tidy_med(med_X3, "X3: second home vs. primary"))

print(results, digits = 3)


# ---- 5. Sensitivity to U (the grey box) ------------------------------
# medsens needs the no-interaction outcome model, so fit a parallel pair.
# It reports the M-Y error correlation rho at which the ACME hits zero.

m_Y_ni <- lm(as.formula(paste("percent_aid ~ info_arm + second_home + resp +",
                              rhs)), est)

set.seed(2026)
med_X3_ni <- mediate(m_M, m_Y_ni, treat = "info_arm", mediator = "resp",
                     control.value = "primary", treat.value = "second",
                     sims = 1000)

sens <- medsens(med_X3_ni, rho.by = 0.05, effect.type = "indirect")
summary(sens)
plot(sens, sens.par = "rho")


# ---- 6. Respondent fixed effects, as within-pair differences ---------
# Two vignettes per respondent, so demeaning within ResponseID removes any U
# that shifts a respondent's LEVEL of responsibility and aid. W drops out --
# that is the point of the spec, not an omission.

dw <- est %>%
  group_by(ResponseID) %>% filter(n() == 2) %>%
  mutate(resp_w = resp - mean(resp),
         aid_w  = percent_aid - mean(percent_aid)) %>%
  ungroup() %>% as.data.frame()

# Note: hazard varies WITHIN respondent, so demeaning does not absorb it.
# Leaving it out here puts the fire-vs-flood contrast into the residual.
m_M_fe <- lm(resp_w ~ info_arm + second_home, dw)
m_Y_fe <- lm(aid_w ~ info_arm * resp_w + second_home * resp_w, dw)

set.seed(2026)
med_X3_fe <- mediate(m_M_fe, m_Y_fe, treat = "second_home", mediator = "resp_w",
                     control.value = "primary", treat.value = "second",
                     sims = 1000, cluster = dw$ResponseID)
summary(med_X3_fe) # THIS TABLE IS IN SI


# ---- 7. Are the post-treatment covariates unmoved by treatment? ------
# Party / GovTrust / tax progressivity are asked after the vignettes. Putting
# them in W is fine only if treatment did not move them -- test it before
# claiming it, then re-run sections 3-4 with W extended.
#
# Two separate tests. The vignette cells vary WITHIN respondent, so they are
# an unlikely source of trouble. The real exposure is `Treated`, the
# between-respondent information arm, which precedes the end-of-survey block
# and could plausibly move stated trust in government or tax preferences.

bal <- function(v, rhs) {
  f <- summary(lm(as.formula(paste0("as.numeric(as.factor(", v, ")) ~ ", rhs)),
                  data = hyp))$fstatistic
  data.frame(covariate = v, on = rhs,
             p_joint = pf(f[1], f[2], f[3], lower.tail = FALSE),
             row.names = NULL)
}

vars <- c("Party", "GovTrustBin", "gap_quartile", "RiskAversion_bin")

bind_rows(
  lapply(vars, bal, rhs = "info_arm + second_home"),   # vignette cells
  lapply(vars, bal, rhs = "Treated")                   # information arm
)

# W <- c(W, "Party", "RiskAversion_bin", "GovTrustBin", "gap_quartile")

# FIGURES --------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sandwich)

LAB <- c(X1 = "X1 Prior info only\nvs. baseline",
         X2 = "X2  Prior info + adaptation\nvs. prior info only",
         X3 = "X3  Second home\nvs. primary residence")



# ---- Fig 1: decomposition -------------------------------------------
# One row per quantity, three panels. The eye should land on the fact
# that ACME dominates for X1/X2 and ADE dominates for X3.

pull_long <- function(x, label) {
  data.frame(
    contrast = label,
    quantity = c("Total effect", "Direct (ADE)", "Mediated (ACME)"),
    est = c(x$tau.coef,  x$z.avg,       x$d.avg),
    lo  = c(x$tau.ci[1], x$z.avg.ci[1], x$d.avg.ci[1]),
    hi  = c(x$tau.ci[2], x$z.avg.ci[2], x$d.avg.ci[2]),
    pm  = c(NA,          NA,            x$n.avg),
    row.names = NULL)
}

dec <- bind_rows(pull_long(med_X1, LAB["X1"]),
                 pull_long(med_X2, LAB["X2"]),
                 pull_long(med_X3, LAB["X3"]))


strip_lab <- dec %>%
  filter(!is.na(pm)) %>%
  transmute(contrast,
            strip = sprintf("%s   \u00b7   %.0f%% via responsibility",
                            gsub("\n", " ", contrast), 100 * pm))

dec <- dec %>%
  left_join(strip_lab, by = "contrast") %>%
  mutate(strip = factor(strip, levels = strip_lab$strip),
         quantity = factor(quantity,
                           levels = c("Total effect", "Direct (ADE)", "Mediated (ACME)")))

PAL <- c("Mediated (ACME)" = "#2C5F8A",   # the channel the paper is about
         "Direct (ADE)"    = "#9AA5AE",   # everything else
         "Total effect"    = "#1A1A1A")


fig_med <- ggplot(dec, aes(x = est, y = quantity, colour = quantity)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey55") +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.1,
                 alpha = 0.85, show.legend = FALSE) +
  geom_point(size = 2.9, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+.1f", est)),
            vjust = -1.25, size = 3.1, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = PAL) +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_discrete(expand = expansion(add = 0.75)) +
  facet_wrap(~ strip, ncol = 1) +
  labs(x = "Effect on recommended aid",
       y = NULL,
       caption = paste("Points are posterior means with 95% quasi-Bayesian",
                       "intervals; 1,000 draws, n = 3,922 vignettes.",
                       "\nMediated + direct sum to the total effect.")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.spacing      = unit(1.1, "lines"),
    strip.text         = element_text(hjust = 0, face = "bold", size = 10.5,
                                      margin = margin(b = 6)),
    axis.text.y        = element_text(colour = "grey20", size = 9.8),
    axis.title.x       = element_text(margin = margin(t = 10), size = 9.8),
    plot.caption       = element_text(hjust = 0, colour = "grey45", size = 8,
                                      margin = margin(t = 12)),
    plot.margin        = margin(12, 16, 10, 12)
  )

fig_med
# ggsave("Figures/Mediation/mediation.png", fig1,
#        width = 6.8, height = 6.4, dpi = 300, bg = "white")
