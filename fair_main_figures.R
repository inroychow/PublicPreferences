setwd("C:/Users/indumati/Box/Disaster aid survey/disaster_survey_github")
pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, MASS)

source("src/Fairness_main_analyses/useful_functions.R")
source("src/Fairness_main_analyses/generate_hyp.R")

################################################################################

#-------------------------------------------------------------------------------
#             FIGURE: FEMA grant dist and recommended aid heterogeneity        |           
#-------------------------------------------------------------------------------
#Percent recommended aid outcome
full_model <- feols(
  percent_aid ~ second_home * prior_info * adaptive_measures +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
summary(full_model)

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
  mutate(
    covariate_label = factor(
      covariate_label,
      levels = c("Annual Income", "Disaster Experience",
                 "Tax Progressivity", "Government Trust",
                 "Political Party", "Risk Preference")
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
ia = data.table::fread("C:\\Users\\indumati\\Box\\FEMA DATA\\Individual Assistance\\IndividualsAndHouseholdsProgramValidRegistrationsV2.csv")
# keep inspected with any verified loss, and positive verified loss
ia <- ia %>%
  filter(!is.na(rpfvl) | !is.na(ppfvl)) %>%
  mutate(verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0)) %>%
  filter(verified_loss > 0)

# CPI (annual averages) anchored to 2024
getSymbols("CPIAUCSL", src = "FRED", warnings = FALSE, quiet = TRUE)
cpi_year <- data.frame(date = index(CPIAUCSL), cpi = as.numeric(CPIAUCSL)) %>%
  mutate(year_decl = lubridate::year(date)) %>%
  group_by(year_decl) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
cpi_2024 <- cpi_year$cpi[cpi_year$year_decl == 2024]

# join event year from IA & adjust both verified loss and HA grants
ia <- ia %>%
  mutate(
    decl_date = suppressWarnings(as.Date(sub("T.*$", "", declarationDate))),
    year_decl = lubridate::year(decl_date)
  ) %>%
  left_join(cpi_year, by = "year_decl") %>%
  mutate(
    cpi_factor          = cpi_2024 / cpi,
    verified_loss_2024  = verified_loss * cpi_factor,
    ha_amount_2024      = haAmount   * cpi_factor,
    ha_pct_of_damage    = 100 * (ha_amount_2024 / verified_loss_2024)
  ) %>%
  filter(is.finite(ha_pct_of_damage)) %>%
  mutate(ha_pct_of_damage = pmin(pmax(ha_pct_of_damage, 0), 100))  # cap to 0–100

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
  adjust = 1.5
)

dens_ge40 <- density(
  ia$ha_pct_of_damage[ia$damage_bin == "≥ $40k"],
  adjust = 1.5
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

x_end <- max(100, ceiling(x_max_pct / 10) * 10)

# ── 9) Build combined plot ──────────────────────────────────────────────────

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
    limits = c(0, x_end),
    breaks = seq(0, x_end, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0),
    guide  = guide_axis(n.dodge = 1)
  ) +
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
  ) +
  annotate(
    "text",
    x = base_case_predicted,
    y = y_max * 1.02,
    label = "Baseline recommended aid",
    hjust = 1.05, size = 4.8, colour = "gray40"
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


ggsave("L:\\Wetland Flood Mitigation\\Disaster aid survey\\EDA Figures\\Fairness paper figures\\dist_plot_2.12.png",
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
                                  include_second_home = TRUE) {
  
  # ---------------------------
  # Scenario grid at baseline controls
  # ---------------------------
  grid <- data %>%
    distinct(second_home, prior_info, adaptive_measures) %>%
    arrange(second_home, prior_info, adaptive_measures)
  
  # Option: exclude second homes (keep only primary residence)
  if (!include_second_home) {
    grid <- grid %>% dplyr::filter(second_home == 0)
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
      Home_Type    = dplyr::if_else(second_home == 0, "Primary Residence", "Second Home"),
      is_base_case = (second_home == 0 & prior_info == 0 & adaptive_measures == 0),
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
  save_path = "C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/fig_resp_sec.png"
)

p_aid <- plot_scenario_effects(
  model = m_percaid,
  data  = hyp,
  outcome_label = "Predicted Recommended Government Aid\n(% of Loss)",
  include_second_home = TRUE,
  save_path = "C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/fig_recaid_sec.png")

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

ggsave("C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/fig_ridge_vert.png",
        p_ridge, width = 12, height = 7, dpi = 300)

#-------------------------------------------------------------------------------
#                  FIGURE:  % distributions of policy questions                |        
#-------------------------------------------------------------------------------

# select just the variables of interest
vars <- c("GovRole", "GovRoleA", "GovRoleB",
          "PostDisasterGovAllocate", "PostDisasterGovUse",
          "GovInsur", "GovInsurA", "GovInsurB", "GovInsurC", "GovInsurD")

percents <- hyp %>%
  dplyr::select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "response") %>%
  group_by(variable, response) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(
    percent = 100 * n / sum(n),
  ) %>%
  arrange(variable, response)
create_overall_heatmap <- function(data, likert_vars) {
  
  heatmap_df <- data |>
    dplyr::select(all_of(likert_vars)) |>
    tidyr::pivot_longer(everything(),
                        names_to  = "Question",
                        values_to = "Response") |>
    filter(!is.na(Response)) |>
    count(Question, Response, name = "n") |>
    group_by(Question) |>
    mutate(
      prop           = n / sum(n),
      Question_Label = question_labels[Question],
      Category       = question_categories[Question]
    ) |>
    ungroup() |>
    mutate(
      Response = recode(Response,
                        "Neither agreenor disagree" = "Neither agree nor disagree") |>
        factor(levels = c("Strongly disagree", "Disagree",
                          "Neither agree nor disagree", "Agree",
                          "Strongly agree")),
      Category = factor(Category, 
                        levels = c("Government\nMandates", "Market\nMechanisms", "Public\nSubsidy"))
    )
  
  question_order <- c(
    "Restrict development in high-risk areas",
    "Require disaster-resistant construction",
    "Mandatory disaster insurance",
    "Higher insurance cost in riskier areas",
    "Public disaster insurance option",
    "Home buyouts",
    "Taxes make insurance affordable for all",
    "Taxes make insurance affordable for low-income"
  )
  
  heatmap_df <- heatmap_df |>
    dplyr::mutate(Question_Label = factor(Question_Label, levels = rev(question_order)))
  
  ggplot2::ggplot(heatmap_df, ggplot2::aes(Response, Question_Label, fill = prop)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(prop, accuracy = 1)),
      colour = "white", size = 3, fontface = "bold"
    ) +
    ggplot2::facet_grid(Category ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_fill_viridis_c(
      option = "viridis",
      name   = "Proportion",
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(size = 13, face = "bold"),
      legend.key.width  = grid::unit(1, "cm"),
      legend.key.height = grid::unit(0.5, "cm"),
      axis.text.y       = ggplot2::element_text(size = 12, hjust = 1),
      axis.text.x       = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
      strip.placement   = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 90, face = "bold", size = 11, hjust = 0.5),
      strip.background  = ggplot2::element_rect(fill = "grey90", colour = "white"),
      panel.spacing     = grid::unit(1, "lines")
    )
}
question_categories <- c(
  "GovRole"   = "Government\nMandates",
  "GovRoleA"  = "Government\nMandates",
  "GovInsur"  = "Market\nMechanisms",
  "GovInsurB" = "Market\nMechanisms",
  "GovInsurD" = "Public\nSubsidy",
  "GovInsurC" = "Public\nSubsidy",
  "GovRoleB"  = "Public\nSubsidy",
  "GovInsurA" = "Government\nMandates"
)
likert_vars <- c("GovRole", "GovRoleA", "GovInsurA",
                 "GovInsur", "GovInsurB",
                 "GovRoleB", "GovInsurC", "GovInsurD")

create_overall_heatmap(hyp, likert_vars)

# ggsave("C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/question_dist.png", width = 12, height = 7, dpi = 300)


#-------------------------------------------------------------------------------
#                  FIGURE:  Ordinal regression for policy support              |        
#-------------------------------------------------------------------------------
# Helper function to run ordinal reg
model_variables <- c("DisasterExperience", "Party", "RiskAversion_bin", "GovTrustBin", "gap_quartile")

# Function to run ordinal regression for Likert-scale variables
run_ordinal_regression <- function(dv, data, predictors = model_variables) {
  
  # Create formula string from predictor list
  formula_str <- paste(dv, "~", paste(predictors, collapse = " + "))
  
  cat("Using formula:", formula_str, "\n")
  
  tryCatch({
    # Convert the dep var to ordered factor with proper levels
    # Define the correct order from lowest to highest agreement
    likert_levels <- c("Strongly disagree", "Disagree", "Neither agreenor disagree", 
                       "Agree", "Strongly agree")
    
    # Ensure the DV is properly ordered 
    if (!is.ordered(data[[dv]])) {
      data[[dv]] <- factor(data[[dv]], levels = likert_levels, ordered = TRUE)
      cat("Converting", dv, "to ordered factor\n")
    }
    
    # Check that all predictors exist in the data
    missing_vars <- predictors[!predictors %in% names(data)]
    if(length(missing_vars) > 0) {
      cat("ERROR: Missing predictor variables:", paste(missing_vars, collapse = ", "), "\n")
      return(NULL)
    }
    
    model <- polr(as.formula(formula_str), data = data, Hess = TRUE)
    
    # Get coefficients with CIs
    coef_table <- summary(model)$coefficients
    
    # Calculate CIs
    tryCatch({
      ci <- confint(model, level = 0.95)
      has_ci <- TRUE
    }, error = function(e) {
      cat("Warning: Could not calculate confidence intervals, using SE approximation\n")
      has_ci <<- FALSE
      ci <<- NULL
    })
    
    # Combine results
    if (has_ci && !is.null(ci) && nrow(ci) == nrow(coef_table)) {
      results <- data.frame(
        Variable = rownames(coef_table),
        Coefficient = coef_table[, "Value"],
        SE = coef_table[, "Std. Error"],
        t_value = coef_table[, "t value"],
        OR = exp(coef_table[, "Value"]),
        CI_lower = exp(ci[, 1]),
        CI_upper = exp(ci[, 2]),
        stringsAsFactors = FALSE
      )
    } else {
      # Use SE approximation for CI
      results <- data.frame(
        Variable = rownames(coef_table),
        Coefficient = coef_table[, "Value"],
        SE = coef_table[, "Std. Error"],
        t_value = coef_table[, "t value"],
        OR = exp(coef_table[, "Value"]),
        CI_lower = exp(coef_table[, "Value"] - 1.96 * coef_table[, "Std. Error"]),
        CI_upper = exp(coef_table[, "Value"] + 1.96 * coef_table[, "Std. Error"]),
        stringsAsFactors = FALSE
      )
    }
    
    # Calc p-values 
    results$p_value <- 2 * (1 - pnorm(abs(results$t_value)))
    
    # Add sigstars
    results$sig <- case_when(
      results$p_value < 0.001 ~ "***",
      results$p_value < 0.01 ~ "**",
      results$p_value < 0.05 ~ "*",
      results$p_value < 0.1 ~ ".",
      TRUE ~ ""
    )
    
    return(list(model = model, results = results))
    
  }, error = function(e) {
    cat("Error in ordinal regression for", dv, ":", e$message, "\n")
    return(NULL)
  })
}


question_labels <- c(
  "GovInsur"  = "Higher insurance cost in riskier areas",
  "GovInsurA" = "Mandatory disaster insurance",
  "GovInsurB" = "Public disaster insurance option",
  "GovInsurC" = "Taxes make insurance affordable for all",
  "GovInsurD" = "Taxes make insurance affordable for low-income",
  "GovRole"   = "Restrict development in high-risk areas",
  "GovRoleA"  = "Require disaster-resistant construction",
  "GovRoleB"  = "Home buyouts",
  "PostDisasterGovAllocate" = "Preferred way government should allocate post-disaster aid",
  "PostDisasterGovUse"      = "Preferred way households should use post-disaster aid"
)

nature_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", colour = "black"),
      plot.title    = element_text(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, colour = "grey40"),
      axis.title    = element_text(size = 10, face = "bold"),
      axis.text     = element_text(size = 9,  colour = "black"),
      legend.title  = element_text(size = 10, face = "bold"),
      legend.text   = element_text(size = 9),
      panel.grid.major = element_line(colour = "grey90", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "black", fill = NA, size = 0.5),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      plot.margin      = margin(10, 10, 10, 10)
    )
}
nature_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A", "#B15928","pink2")

regression_data <- hyp %>%
  mutate(
    # set reference levels
    Party              = fct_relevel(factor(Party), "Independent"),
    GovTrustBin        = fct_relevel(factor(GovTrustBin), "Low government trust"),
    RiskAversion_bin         = fct_relevel(factor(RiskAversion_bin), "Risk neutral"),
    DisasterExperience = fct_relevel(factor(DisasterExperience), "0"),
    gap_quartile = fct_relevel(
      factor(gap_quartile,
             levels = c("Mid-range tax progressive",
                        "Least tax progressive",
                        "Most tax progressive")),
      "Mid-range tax progressive"
    )
  ) %>%
  filter(
    !is.na(Party),
    !is.na(gap_quartile),
    !is.na(DisasterExperience),
    !is.na(RiskAversion_bin),
    !is.na(GovTrustBin)
  )


# Separate Likert-scale from categorical choice variables
likert_vars <- c("GovRole", "GovRoleA", "GovRoleB", "GovInsur", "GovInsurA", 
                 "GovInsurB", "GovInsurC", "GovInsurD")
choice_vars <- c("PostDisasterGovAllocate", "PostDisasterGovUse")

existing_likert <- likert_vars[likert_vars %in% names(regression_data)]
existing_choice <- choice_vars[choice_vars %in% names(regression_data)]

# Convert Likert-scale variables to ordered factors
likert_levels <- c("Strongly disagree", "Disagree", "Neither agreenor disagree", 
                   "Agree", "Strongly agree")

for (var in existing_likert) {
  if (is.character(regression_data[[var]])) {
    regression_data[[var]] <- factor(regression_data[[var]], 
                                     levels = likert_levels, 
                                     ordered = TRUE)
    cat("Converted", var, "to ordered factor\n")
  }
}

cat("Likert-scale variables to analyze:", paste(existing_likert, collapse = ", "), "\n")
cat("Choice variables to analyze:", paste(existing_choice, collapse = ", "), "\n\n")

# Run ordinal regression

regression_data <- regression_data %>%
  mutate(
    Party = fct_relevel(Party, "Independent"),   
    DisasterExperience = fct_relevel(DisasterExperience, "0"),
    GovTrustBin = fct_relevel(GovTrustBin, "Low government trust"),
    RiskAversion_bin = fct_relevel(RiskAversion_bin, "Risk neutral"),
    gap_quartile = fct_relevel(gap_quartile, "Mid-range tax progressive")
  )


cat("ORDINAL LOGISTIC REGRESSION RESULTS\n")
cat(rep("=", 80), "\n")

ordinal_results <- list()
for (var in existing_likert) {
  cat("\n=== ANALYZING:", var, "===\n")
  
  # Check if variable exists and has data
  if(!var %in% names(regression_data)) {
    cat("ERROR: Variable", var, "not found in data\n")
    next
  }
  
  # Check variable class and values
  cat("Variable class:", class(regression_data[[var]]), "\n")
  cat("Is factor:", is.factor(regression_data[[var]]), "\n")
  cat("Is ordered:", is.ordered(regression_data[[var]]), "\n")
  
  var_table <- table(regression_data[[var]], useNA = "ifany")
  cat("Response distribution:\n")
  print(var_table)
  
  if (length(var_table) < 3) {
    cat("SKIPPING - insufficient variation\n")
    next
  }
  cat("Running regression...\n")
  result <- run_ordinal_regression(var, regression_data)
  
  if (!is.null(result)) {
    ordinal_results[[var]] <- result
    cat("SUCCESS - model created\n")
  } else {
    cat("FAILED - see error above\n")
  }
}

# Function to create figure 
create_policy_plot <- function(ordinal_results, question_labels,
                               groups_to_plot = c("RiskAversion_bin", "gap_quartile", "DisasterExperience")) {
  library(dplyr); library(stringr); library(ggplot2)
  
  coef_data <- data.frame()
  
  # Map model coefficients back to human-readable “Group” labels
  group_map <- c(
    "RiskAversion_bin"     = "Risk\ntolerance",
    "gap_quartile"         = "Tax\nprogressivity",
    "DisasterExperience"   = "Disaster\nexperience",
    "GovTrustBin"          = "Government\ntrust",
    "Party"                = "Party"
  )
  
  # prefixes we want to keep in the coefficient table
  var_prefixes <- c("Party", "DisasterExperience", "GovTrustBin", "RiskAversion_bin", "gap_quartile")
  
  model_categories <- c(
    "GovRole"   = "Government\nMandates",
    "GovRoleA"  = "Government\nMandates",
    "GovInsurA" = "Government\nMandates",
    "GovInsur"  = "Market\nMechanisms",
    "GovInsurB" = "Market\nMechanisms",
    "GovRoleB"  = "Public\nSubsidy",
    "GovInsurC" = "Public\nSubsidy",
    "GovInsurD" = "Public\nSubsidy"
  )
  
  for (m in names(ordinal_results)) {
    res <- ordinal_results[[m]]$results
    if (is.null(res) || !nrow(res)) next
    
    keep <- grepl(paste(var_prefixes, collapse = "|"), res$Variable)
    res  <- res[keep, , drop = FALSE]
    if (!nrow(res)) next
    
    res$Question   <- question_labels[[m]]
    res$Model_Name <- m
    res$Category   <- unname(model_categories[m])
    
    res$Predictor_Clean <- dplyr::recode(
      res$Variable,
      PartyRepublican                        = "Republican",
      PartyDemocrat                          = "Democrat",
      DisasterExperience1                    = "Disaster experience",
      `GovTrustBinHigh government trust`     = "High gov trust",
      `RiskAversion_binRisk tolerant`        = "Risk tolerant",
      `RiskAversion_binRisk averse`          = "Risk averse",
      `gap_quartileMost tax progressive`     = "Most tax progressive",
      `gap_quartileLeast tax progressive`    = "Least tax progressive",
      .default = res$Variable
    )
    
    coef_data <- dplyr::bind_rows(coef_data, res)
  }
  
  # Order questions within categories
  model_order <- c("GovRole","GovRoleA","GovInsurA","GovInsur","GovInsurB","GovRoleB","GovInsurC","GovInsurD")
  ordered_labels <- sapply(model_order, function(x) question_labels[[x]])
  coef_data$Question_Wrapped <- stringr::str_wrap(coef_data$Question, width = 20)
  ordered_labels_wrapped     <- stringr::str_wrap(ordered_labels, width = 20)
  coef_data$Question_Wrapped <- factor(coef_data$Question_Wrapped, levels = ordered_labels_wrapped)
  
  coef_data$Category <- factor(
    coef_data$Category,
    levels = c("Government\nMandates", "Market\nMechanisms", "Public\nSubsidy")
  )
  
  # Assign each coefficient to a “Group” based on its variable prefix
  coef_data <- coef_data %>%
    mutate(
      Group = case_when(
        grepl("^RiskAversion_bin", Variable)     ~ group_map["RiskAversion_bin"],
        grepl("^gap_quartile", Variable)         ~ group_map["gap_quartile"],
        grepl("^DisasterExperience", Variable)   ~ group_map["DisasterExperience"],
        grepl("^GovTrustBin", Variable)          ~ group_map["GovTrustBin"],
        grepl("^Party", Variable)                ~ group_map["Party"],
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Group)) %>%
    filter(Group %in% unname(group_map[groups_to_plot]))
  
  # Palette: keep your existing colors + add party & trust
  pal <- c(
    # Risk
    "Risk neutral"           = "blue2",
    "Risk tolerant"          = "#6baed6",
    "Risk averse"            = "blue4",
    # Tax
    "Least tax progressive"  = "chartreuse3",
    "Mid-range tax progressive" = "#74c476",
    "Most tax progressive"   = "forestgreen",
    # Disaster
    "No disaster experience" = "#dadaeb",
    "Disaster experience"    = "purple",
    # Gov trust
    "High gov trust"         = "#e7298a",
    # Party
    "Democrat"               = "#d95f02",
    "Republican"             = "#fdae6b"
  )
  
  # Ensure predictor levels don’t drop unpredictably
  coef_data <- coef_data %>%
    mutate(Predictor_Clean = factor(Predictor_Clean, levels = names(pal)))
  
  ggplot(coef_data,
         aes(x = Question_Wrapped, y = Coefficient,
             colour = Predictor_Clean, group = Predictor_Clean)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .7) +
    geom_line(size = 1.5, alpha = .8) +
    geom_point(size = 4, alpha = .95) +
    geom_errorbar(aes(ymin = Coefficient - 1.96*SE,
                      ymax = Coefficient + 1.96*SE),
                  width = .3, alpha = .75) +
    facet_grid(Group ~ Category, scales = "free_x", space = "free_x") +
    scale_color_manual(values = pal, drop = TRUE) +
    labs(x = "",
         y = "Log-odds coefficient",
         caption = "Note: Coefficient of 0 = no effect; >0 = increased odds of higher support; <0 = decreased odds") +
    nature_theme() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.y     = element_text(size = 16),
      axis.text.y      = element_text(size = 16),
      strip.text       = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "grey90", colour = "white"),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 16),
      legend.key.size  = unit(1.2, "lines"),
      plot.caption     = element_text(hjust = 0, size = 14, color = "gray30", margin = margin(t = 10))
    )
}

policy_plot<- create_policy_plot(
  ordinal_results = ordinal_results,
  question_labels = question_labels,
  groups_to_plot  = c("Party", "GovTrustBin")
)

print(policy_plot)

ggsave("C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/fig_policies.png",  plot = policy_plot, width = 12, height = 8, dpi = 300)
