setwd("C:/Users/indumati/Box/Disaster aid survey/disaster_survey_github")
pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, splines, readr, ggridges, forcats, stringi, purr, viridis)

source("src/Fairness_main_analyses/useful_functions.R")
source("src/Fairness_main_analyses/generate_hyp.R")

################################################################################

#-------------------------------------------------------------------------------
#             FIGURE: DIST PLOT WITH UNINSURED DAMAGES VALUES                  |           
#-------------------------------------------------------------------------------

# === IA registrations: HA (% of verified damage), CPI-adjusted to 2024 ===
ia <- data.table::fread("L:/Wetland Flood Mitigation/Paper_NFIP/IndividualsAndHouseholdsProgramValidRegistrationsV2.csv")

# keep inspected with any verified loss, and positive verified loss
ia <- ia %>%
  filter(!is.na(rpfvl) | !is.na(ppfvl)) %>%
  mutate(verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0)) %>%
  filter(verified_loss > 0)

# FILTER TO JUST UNINSURED LOSSES
ia <- ia %>%
  filter(floodInsurance == 0)

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
# All records density
dens_all <- density(
  ia$ha_pct_of_damage,
  adjust = 1.5
)

# ≥ $40k only
dens_ge40 <- density(
  ia$ha_pct_of_damage[ia$verified_loss_2024 >= 40000],
  adjust = 1.5
)

ia_ha_density_2 <- bind_rows(
  tibble(x = dens_all$x,   dens = dens_all$y,   damage_bin = "All damages"),
  tibble(x = dens_ge40$x,  dens = dens_ge40$y,  damage_bin = "≥ $40k damage")
)

ia_ha_mean_2 <- ia %>%
  mutate(damage_bin = if_else(verified_loss_2024 >= 40000, "≥ $40k damage", "All damages")) %>%
  group_by(damage_bin) %>%
  summarise(mean_ha_pct = mean(ha_pct_of_damage, na.rm = TRUE), .groups = "drop")

# ── 7) y scaling ─────────────────────────────────────────────────────────────
y_max    <- max(plot_data_numeric_pct$y_numeric, na.rm = TRUE)
dens_max <- max(ia_ha_density_2$dens, na.rm = TRUE)

scale_factor <- (y_max * 0.9) / dens_max

ia_ha_pct_density_2 <- ia_ha_density_2 %>%
  mutate(dens_scaled = dens * scale_factor)

left_breaks_raw <- scales::breaks_pretty(n = 5)(c(0, dens_max))
left_breaks_pos <- left_breaks_raw * scale_factor

# ── 8) x range ───────────────────────────────────────────────────────────────
x_max_pct <- max(
  ia_ha_pct_density_2$x,
  plot_data_numeric_pct$estimate_centered,
  plot_data_numeric_pct$upper_centered,
  base_case_predicted,
  ia_ha_mean_2$mean_ha_pct,
  na.rm = TRUE
)
x_end <- max(110, ceiling(x_max_pct / 10) * 10)


# ── 9) Build combined plot ────────────────────────────────────────────────────
combined_plot_pct <- ggplot() +
  geom_line(
    data = ia_ha_pct_density_2,
    aes(x = x, y = dens_scaled, linetype = damage_bin),
    colour = "red4", linewidth = 1
  ) +
  geom_vline(
    xintercept = base_case_predicted,
    colour = "gray35", linetype = "dashed", linewidth = 1.2
  ) +
  geom_errorbar(
    data = plot_data_numeric_pct,
    aes(
      x = estimate_centered, y = y_numeric,
      xmin = lower_centered, xmax = upper_centered,
      colour = covariate_label
    ),
    width = 0.25, linewidth = 1.6
  ) +
  geom_point(
    data = plot_data_numeric_pct,
    aes(x = estimate_centered, y = y_numeric, colour = covariate_label),
    size = 4.2
  ) +
  geom_text(
    data = plot_data_numeric_pct,
    aes(x = estimate_centered, y = y_numeric, label = sig_level),
    vjust = -0.2, size = 8, colour = "black", show.legend = FALSE
  ) +
  scale_colour_manual(values = covariate_colors, guide = "none") +
  scale_linetype_manual(
    values = c("All damages" = "dashed", "≥ $40k damage" = "solid"),
    name   = "Damage Assessed (uninsured losses only)"
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
    legend.title       = element_text(size = 12),
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
    linetype = guide_legend(keywidth = grid::unit(1.2, "cm"))
  ) +
  annotate(
    "text",
    x     = base_case_predicted,
    y     = y_max * 1.02,
    label = "Baseline recommended aid",
    hjust = 1.05, size = 4.8, colour = "gray40"
  )

# ── Colored right-axis labels ─────────────────────────────────────────────────
axis_label_colors <- plot_data_numeric_pct %>%
  arrange(desc(y_numeric)) %>%
  pull(covariate_label) %>%
  as.character() %>%
  sapply(function(x) covariate_colors[x])

combined_plot_pct <- combined_plot_pct +
  theme(
    axis.text.y.right = element_text(
      hjust  = 0,
      margin = margin(l = 6),
      colour = axis_label_colors
    )
  )

combined_plot_pct
ggsave("L:\\Wetland Flood Mitigation\\Disaster aid survey\\EDA Figures\\Fairness paper figures\\dist_uninsured.png",
       plot = combined_plot_pct, width = 12, height = 7, dpi = 300)

#-------------------------------------------------------------------------------
#             FIGURE: RESP / AID SPLINE                                        |           
#-------------------------------------------------------------------------------

plot_ridge_resp_aid_spline <- function(hyp, spline_df = 4) {
  
  hyp_plot <- hyp %>%
    mutate(
      resp = parse_number(as.character(resp)),
      percent_aid = parse_number(as.character(percent_aid))
    ) %>%
    filter(!is.na(resp), !is.na(percent_aid)) %>%
    mutate(
      second_home_f = if_else(second_home == 1, "Second home", "Primary home"),
      prior_info_f  = if_else(prior_info  == 1, "Had prior info", "No prior info"),
      adaptive_f    = if_else(adaptive_measures == 1, "Adapted", "Did not adapt"),
      scenario      = interaction(second_home_f, prior_info_f, adaptive_f, sep = " | "),
      resp_factor   = factor(resp, levels = 0:10)
    )
  
  aid_means <- hyp_plot %>%
    group_by(resp_factor) %>%
    summarize(mean_aid = mean(percent_aid, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(resp_factor))
  
  spl_fit <- feols(percent_aid ~ ns(resp, df = spline_df), data = hyp_plot, vcov = ~ResponseID)
  
  pred_df <- data.frame(resp = 0:10)
  pred_df$pred_aid <- predict(spl_fit, newdata = pred_df)
  pred_df$resp_factor <- factor(pred_df$resp, levels = 0:10)
  
  ggplot(hyp_plot, aes(x = percent_aid, y = resp_factor)) +
    geom_density_ridges(
      scale = 2,
      rel_min_height = 0.01,
      fill = "grey85",
      color = "grey40",
      alpha = 0.9,
      size = 0.3
    ) +
    geom_point(
      data = aid_means,
      aes(x = mean_aid, y = resp_factor),
      inherit.aes = FALSE,
      size = 2.2
    ) +
    geom_line(
      data = pred_df,
      aes(x = pred_aid, y = resp_factor, group = 1),
      inherit.aes = FALSE,
      linewidth = 1.0
    ) +
    scale_x_continuous(
      name = "Recommended government aid (% of loss)",
      limits = c(0, 100)
    ) +
    scale_y_discrete(name = "Perceived responsibility") +
    coord_flip() +
    theme_minimal(base_size = 18) +
    theme(
      axis.title.x      = element_text(size = 20, margin = margin(t = 12)),
      axis.title.y      = element_text(size = 20),
      axis.text.x       = element_text(size = 16),
      axis.text.y       = element_text(size = 16),
      panel.grid.minor  = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position   = "none",
      plot.margin       = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

p_ridge_spline <- plot_ridge_resp_aid_spline(hyp, spline_df = 4)
p_ridge_spline

# ggsave("C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/fig_spline.png",
#        plot = p_ridge_spline , width = 15, height = 7, dpi = 300)

####################################################

#-------------------------------------------------------------------------------
#             TABLE and FIGURE : REASONS FOR RECOMMENDING NO AID ACROSS SCENARIOS          
#-------------------------------------------------------------------------------

# ==============================================================================
# 1. TEXT NORMALIZATION FUNCTION
# ==============================================================================

normalize_reason <- function(x) {
  x %>%
    str_squish() %>%
    str_to_lower() %>%
    stri_trans_general("Any-Latin; Latin-ASCII") %>%
    str_replace_all("['`']", "'")
}

# ==============================================================================
# 2. REASON CLASSIFICATION FUNCTION
# ==============================================================================

classify_reason_vec <- function(reason_raw) {
  reason <- normalize_reason(reason_raw)
  cat <- rep(NA_character_, length(reason))
  
  detect <- function(pat) {
    str_detect(reason, regex(pat, ignore_case = TRUE, comments = TRUE))
  }
  
  # Category rules (most specific first)
  
  # Should have bought insurance
  pick <- is.na(cat) & detect("\\binsur[a-z]{2,}\\b|\\b(no|should.?ve|should have|didn.?t|could.?ve).*insur")
  cat[pick] <- "Should have bought insurance"
  
  # Personal responsibility / fault
  pick <- is.na(cat) & detect("\\b(personal|home(owner|own)|responsib|fault|own risk|own bills|accountable|
                                gambled|poor planning|his loss|bear(s)? .*responsib|majority.*incident|
                                risk that .* (chose|choose)|chose not to|could(|n'?t) have|should(|n'?t) have)")
  cat[pick] <- "Personal responsibility / fault"
  
  # Lack of preparedness / mitigation
  pick <- is.na(cat) & detect("\\b(prepar|prevent|protect|precaut|mitigat|preemptive|clear(ing)?|clean(ing)?|
                                brush|shrub|unsafe|safety aware|hazard|roof|update|maintenance|plan|planning|
                                follow(ed)? .* (rule|rules|guidelines|safety)|move out)")
  cat[pick] <- "Lack of preparedness / mitigation"
  
  # Taxpayer fairness / cost burden
  pick <- is.na(cat) & detect("\\b(my|our) tax( payer| )?dollars|\\btax(payer|es)?\\b|
                                \\b(can'?t|cannot|can not|couldn'?t) afford|\\bburden|cost|
                                give away[, ]?\\$?[0-9,]+|give away money")
  cat[pick] <- "Taxpayer fairness / cost burden"
  
  # Government role illegitimate
  pick <- is.na(cat) & detect("\\b(not for them to do|they shouldn'?t|law[s]? are in place|not charity)\\b|
                               \\b(gov('?t|ernment)|federal).{0,60}(not|shouldn'?t|isn'?t|ain'?t|use .*funds?)|
                               (job is|job isn't|not their job|should not be to hand out money)")
  cat[pick] <- "Government role illegitimate"
  
  # Low risk argument
  pick <- is.na(cat) & detect("\\b(odds|chance|risk).{0,50}(low|small|remote|rare)")
  cat[pick] <- "Low risk argument"
  
  # Unclear / no reason given
  pick <- is.na(cat) & detect("\\b(i (don'?t|do not) know|not sure|no particular reason|
                                don'?t care[.!? ]?|dont care[.!? ]?|only two answers)\\b")
  cat[pick] <- "Unclear / no reason given"
  
  # Fallback
  cat[is.na(cat)] <- "Other"
  
  fct_drop(factor(cat))
}

# ==============================================================================
# 3. MANUAL CORRECTIONS FOR EDGE CASES
# ==============================================================================

manual_map <- tibble(
  reason_raw_norm = c(
    "i don't care",
    "not for them to do",
    "there were only two answers to choose from",
    "laws are in place",
    "he knew what he needed to do and chose not to do it.",
    "they should use federal funds",
    "he didn't move out improve",
    "no pass a bill and a set amy",
    "it is the risk that alex choose",
    "they shouldn't",
    "i would be ok with a government grant but t o just give away 250,000 because he had damage is not right.",
    "because they should have another fund for that",
    "it is his loss",
    "no particular reason",
    "owner could have taken steps to motivate the.damage",
    "i don't know.",
    "his failure to be safety aware",
    "he should have taken care of this prior",
    "i do not think the government's job is to hand out money. people should take care of each other.",
    "i'm not sure"
  ),
  manual_cat = c(
    "Unclear / no reason given",
    "Government role illegitimate",
    "Unclear / no reason given",
    "Government role illegitimate",
    "Personal responsibility / fault",
    "Taxpayer fairness / cost burden",
    "Lack of preparedness / mitigation",
    "Unclear / no reason given",
    "Personal responsibility / fault",
    "Government role illegitimate",
    "Taxpayer fairness / cost burden",
    "Taxpayer fairness / cost burden",
    "Personal responsibility / fault",
    "Unclear / no reason given",
    "Lack of preparedness / mitigation",
    "Unclear / no reason given",
    "Lack of preparedness / mitigation",
    "Lack of preparedness / mitigation",
    "Government role illegitimate",
    "Unclear / no reason given"
  )
) %>%
  mutate(reason_raw = normalize_reason(reason_raw_norm)) %>%
  select(reason_raw, manual_cat)

# ==============================================================================
# 4. HELPER FUNCTION TO CLASSIFY AND COMPUTE FREQUENCIES
# ==============================================================================

make_freq_table <- function(data, manual_map) {
  data %>%
    mutate(reason_cat = classify_reason_vec(reason_raw)) %>%
    mutate(reason_cat = fct_infreq(reason_cat)) %>%
    left_join(manual_map, by = "reason_raw") %>%
    mutate(
      reason_cat = if_else(
        as.character(reason_cat) == "Other" & !is.na(manual_cat),
        manual_cat,
        as.character(reason_cat)
      )
    ) %>%
    mutate(reason_cat = fct_infreq(factor(reason_cat))) %>%
    select(-manual_cat) %>%
    count(reason_cat, sort = TRUE) %>%
    mutate(percent = percent(n / sum(n), accuracy = 0.1))
}

# ==============================================================================
# 5. BASE CASE ANALYSIS
# ==============================================================================

base_zero <- hyp %>%
  filter(
    gov_amt == 0,
    second_home == 0,
    prior_info == 0,
    adaptive_measures == 0
  ) %>%
  select(ResponseID, hazard, NoGovCompensate_Reason) %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason)) %>%
  distinct(ResponseID, hazard, .keep_all = TRUE) %>%
  filter(reason_raw != "" & !is.na(reason_raw))

freq_base <- make_freq_table(base_zero, manual_map)
freq_base


#### ALL 

all_zero <- hyp %>%
  filter(gov_amt == 0) %>%   # or percent_aid == 0 if that’s what you use elsewhere
  select(ResponseID, hazard, NoGovCompensate_Reason) %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason)) %>%
  distinct(ResponseID, hazard, .keep_all = TRUE) %>%   # keep at most 1 per person×hazard
  filter(reason_raw != "" & !is.na(reason_raw))

freq_all_zero <- make_freq_table(all_zero, manual_map)
freq_all_zero
sum(freq_all_zero$n) #177
# ==============================================================================
# 6. ZERO-SLOPE RESPONDENTS (NO AID IN BOTH SCENARIOS)
# ==============================================================================

zero_both0_ids <- hyp %>%
  filter(ResponseID %in% zero_ids) %>%
  group_by(ResponseID) %>%
  summarise(both0 = all(gov_amt == 0), .groups = "drop") %>%
  filter(both0) %>%
  pull(ResponseID)

zero_both0_data <- hyp %>%
  filter(ResponseID %in% zero_both0_ids, gov_amt == 0) %>%
  select(ResponseID, hazard, NoGovCompensate_Reason) %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason)) %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  filter(reason_raw != "" & !is.na(reason_raw))

freq_zero_both0 <- make_freq_table(zero_both0_data, manual_map)

# ==============================================================================
# 7. PRIOR INFO AND ADAPTIVE MEASURES GROUPS
# ==============================================================================

# Prior info = 1, adaptive measures = 0
prior1_adapt0_data <- hyp %>%
  filter(
    prior_info == 1,
    adaptive_measures == 0,
    gov_amt == 0
  ) %>%
  select(ResponseID, NoGovCompensate_Reason) %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason)) %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  filter(reason_raw != "" & !is.na(reason_raw))

freq_prior1_adapt0 <- make_freq_table(prior1_adapt0_data, manual_map)

# Prior info = 1, adaptive measures = 1
prior1_adapt1_data <- hyp %>%
  filter(
    prior_info == 1,
    adaptive_measures == 1,
    gov_amt == 0
  ) %>%
  select(ResponseID, NoGovCompensate_Reason) %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason)) %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  filter(reason_raw != "" & !is.na(reason_raw))

freq_prior1_adapt1 <- make_freq_table(prior1_adapt1_data, manual_map)

# ==============================================================================
# 8. CREATE COMPARISON PLOT
# ==============================================================================

prep_panel <- function(freq_tbl, panel_name) {
  freq_tbl %>%
    mutate(
      panel = panel_name,
      p = n / sum(n)
    )
}

plot_df <- bind_rows(
  prep_panel(freq_base, "Base case (N=105)"),
  prep_panel(freq_zero_both0, "No aid, both scenarios (N=19)"),
  prep_panel(freq_prior1_adapt0, "Had prior info but did not adapt (N=25)"),
  prep_panel(freq_prior1_adapt1, "Had prior info and did adapt (N=27)")
) %>%
  mutate(reason_cat = fct_rev(fct_inorder(reason_cat)))

zeroplot <- ggplot(plot_df, aes(x = panel, y = p, fill = reason_cat)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "viridis") +
  labs(
    x = NULL,
    y = "Share of reasons",
    fill = "Reason for recommending no aid"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 12)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

print(zeroplot)
# ggsave("C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/zeroreasons.png",
#        plot = zeroplot , width = 15, height = 7, dpi = 300)





####################################################

#--------------------------------------------------------------------------------------------------
#             FIGURE: SHARE OF RESPONDENTS RECOMMENDING ZERO AID BY SCENARIO, PRI AND SEC HOME         
#--------------------------------------------------------------------------------------------------

# ==============================================================================
# ZERO AID RECOMMENDATIONS BY HOME TYPE AND SCENARIO
# ==============================================================================

# Define scenarios
hyp <- hyp %>%
  mutate(
    scenario = case_when(
      prior_info == 0 & adaptive_measures == 0 ~ "No prior info, no adaptation (base case)",
      prior_info == 1 & adaptive_measures == 0 ~ "Prior info, no adaptation",
      prior_info == 1 & adaptive_measures == 1 ~ "Prior info, adaptation",
      TRUE ~ "Other"
    )
  )

# Calculate percentages by home type and scenario
zero_aid_stats <- hyp %>%
  group_by(second_home, scenario) %>%
  summarize(
    n_total = n(),
    n_zero = sum(percent_aid == 0, na.rm = TRUE),
    pct_zero = n_zero / n_total * 100,
    .groups = "drop"
  ) %>%
  mutate(home_type = ifelse(second_home == 0, "Primary residence", "Second home"))

# Overall percentages (averaged across scenarios)
zero_aid_overall <- zero_aid_stats %>%
  group_by(home_type) %>%
  summarize(pct_zero = mean(pct_zero), .groups = "drop")

print(zero_aid_overall)

# By scenario (wide format for table)
zero_aid_table <- zero_aid_stats %>%
  select(scenario, home_type, pct_zero) %>%
  pivot_wider(names_from = home_type, values_from = pct_zero)

print(zero_aid_table)

# What was the mean resp of the responses saying no aid?
no_aid <- hyp %>%
  filter(percent_aid == 0)
summary(as.numeric(no_aid$resp))

table(no_aid$resp)
no_aid %>%
  summarise(
    n = n(),
    mean_resp = mean(as.numeric(resp), na.rm = TRUE),
    median_resp = median(as.numeric(resp), na.rm = TRUE),
    sd_resp = sd(as.numeric(resp), na.rm = TRUE),
    min_resp = min(as.numeric(resp), na.rm = TRUE),
    max_resp = max(as.numeric(resp), na.rm = TRUE)
  )


# ==============================================================================
# CREATE PLOT
# ==============================================================================

plot_df <- zero_aid_stats %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "Prior info, adaptation",
        "No prior info, no adaptation (base case)",
        "Prior info, no adaptation"
      )
    )
  )

zero_aid_plot <- ggplot(plot_df, aes(x = scenario, y = pct_zero, fill = home_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c(
      "Primary residence" = "#D55E00",
      "Second home" = "#0072B2"
    )
  ) +
  scale_y_continuous(
    name = "Respondents recommending zero aid (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  scale_x_discrete(name = NULL) +
  coord_flip() +
  theme_minimal(base_size = 18) +
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 12)),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  labs(fill = NULL)

print(zero_aid_plot)

# # Save plot
# ggsave(
#   "C:/Users/indumati/Box/Disaster aid survey/EDA Figures/Fairness paper figures/zeroaid_byscenario.png",
#   zero_aid_plot,
#   width = 15,
#   height = 7,
#   dpi = 300
# )


########################################################

#--------------------------------------------------------------------------------------------------
#            TABLE: PERCEIVED RESPONSIBILITY & PROBABILITY OF RECOMMENDING ZERO AID    
#--------------------------------------------------------------------------------------------------


df <- hyp %>%
  mutate(
    zero_aid = as.integer(percent_aid == 0),
    resp = as.numeric(resp)
  )

m1 <- feols(
  zero_aid ~ resp + DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = df,
  vcov = ~ResponseID
)

summary(m1)

vc <- vcovCL(m_logit, cluster = df$ResponseID, type = "HC1")
coeftest(m_logit, vcov. = vc)



############################## 
#--------------------------------------------------------------------------------------------------
#            TABLE: Effect of Responsibility on Recommended Post-Disaster Aid (w controls)    
#--------------------------------------------------------------------------------------------------



# 1/22 A regression of recommended aid on respondent’s demographic characteristics, controlling for respondent’s perceptions of responsibility
#shows that these individuals are more likely to be X, Y and Z*.

m <- feols(
  percent_aid ~ i(resp, ref = 5) +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped + Party + Race2 +
    RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
summary(m)




############################## 
#--------------------------------------------------------------------------------------------------
#          FIGURES S4 a and b : ZERO SLOPE ANALYSIS   
#--------------------------------------------------------------------------------------------------


dat=readRDS("C:\\Users\\indumati\\Downloads\\fran_data.rds")

#characterize individuals based on variation in government aid preferred with responsibility

aidcoef=function(dataset){
  #regress government compensation on responsibility score - note there will be no remaining dof since only 2 observations per respondent
  #if responsibility is the same then can't estimate an effect
  if(dataset$resp[1]==dataset$resp[2]) return(NA)
  mod=lm(gov_amt~resp,data=dataset)
  return(mod$coefficients[2])
}

dat$ResponseID=as.factor(dat$ResponseID)
dat$resp=as.numeric(dat$resp)

result <- dat %>%
  split(.$ResponseID) %>%
  map(aidcoef) %>%
  enframe(name = "ResponseID", value = "coef") %>%
  mutate(ResponseID = as.character(ResponseID)) %>%
  mutate(coef=unlist(coef))

#also look at individual's residuals from regression of aid on responsibility (treat as factor)

respmod=lm(gov_amt~resp,data = dat%>%mutate(resp=as.factor(resp)))

resids=data.frame(ID=dat$ResponseID,resid=respmod$residuals)
#take mean for each respondent
resids=resids%>%
  group_by(ID)%>%
  dplyr::summarise(resid=mean(resid))

#merge with slope data
inddat=merge(result%>%mutate(ResponseID=as.factor(ResponseID)),resids,by.x="ResponseID",by.y="ID")

#identify respondents that only saw hypotheticals with primary or secondary home, not a mix
rels=dat%>%
  select(ResponseID,second_home)%>%
  group_by(ResponseID)%>%
  dplyr::summarise(second_home_total=sum(second_home))%>%
  filter(second_home_total%in%c(0,2))

a=ggplot(inddat%>%filter(ResponseID%in%rels$ResponseID), aes(resid/250*100, coef/250*100)) +
  stat_density_2d(aes(fill = after_stat(density)),
                  geom = "raster",
                  contour = FALSE) +
  theme_minimal() +
  scale_fill_viridis_c(option = "magma",guide=FALSE) +
  coord_cartesian(ylim = c(-50, 50)) + 
  labs(x = "Residual Aid Allocation\n(% of $250,000 Loss)", y = "Slope of Aid Award with Individual Responsibility\n(% of $250,000 Loss per Unit Increase in Responsibility)")

countzeros=dim(inddat%>%filter(ResponseID%in%rels$ResponseID&coef!=0))[1]

b=ggplot(inddat%>%filter(ResponseID%in%rels$ResponseID&coef!=0),aes(coef/250*100))+geom_histogram(breaks=c(seq(-100,0,length.out=15),seq(0+100/14,100,length.out=14)))+theme_bw()+labs(x="Slope of Aid Award with Individual Responsibility\n(% of $250,000 Loss per Unit Increase in Responsibility)",y="Count")+
  geom_segment(x=0,y=0,yend=countzeros,col="goldenrod",lwd=1.5)+coord_cartesian(ylim=c(0,400))

# ggsave("C:\\Users\\fmoore\\Box\\Davis Stuff\\Insurance\\Survey Paper\\Figures\\individualslopes_restrictedsample.pdf",plot=b)
# 
# ggsave("C:\\Users\\fmoore\\Box\\Davis Stuff\\Insurance\\Survey Paper\\Figures\\individualdensities_restrictedsample.pdf",plot=a)




#--------------------------------------------------------------------------------------------------
#          TABLES: EFFECT OF SCENARIOS ON LEVEL OF RESP, PERCAID
#--------------------------------------------------------------------------------------------------



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


#--------------------------------------------------------------------------------------------------
#          HETEROGENEITY in the adaptation rewards, prior info scenarios only
#--------------------------------------------------------------------------------------------------


# Subset to only cases with prior information
hyp_prior <- hyp %>%
  filter(prior_info == 1)
hyp_prior$resp <- as.numeric(hyp_prior$resp)

# Model 1: Responsibility with adaptation × risk aversion interaction
m_resp_prior_risk <- feols(
  resp ~ second_home + adaptive_measures * RiskAversion_bin +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + GovTrustBin + gap_quartile,
  data = hyp_prior,
  vcov = ~ResponseID
)

# Model 2: Percent aid with adaptation × risk aversion interaction
m_percaid_prior_risk <- feols(
  percent_aid ~ second_home + adaptive_measures * RiskAversion_bin + 
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + GovTrustBin + gap_quartile,
  data = hyp_prior,
  vcov = ~ResponseID
)

summary(m_percaid_prior_risk)

# Test the difference in adaptation reward between risk-averse and risk-tolerant
hypotheses(m_resp_prior_risk, 
           "`adaptive_measures:RiskAversion_binRisk averse` - `adaptive_measures:RiskAversion_binRisk tolerant` = 0")
# Test the difference in adaptation reward between risk-averse and risk-tolerant
hypotheses(m_percaid_prior_risk, 
           "`adaptive_measures:RiskAversion_binRisk averse` - `adaptive_measures:RiskAversion_binRisk tolerant` = 0")


# Display results
etable(m_resp_prior_risk, m_percaid_prior_risk,
       title = "Effect of Adaptation on Responsibility and Aid, by Risk Aversion (Prior Info Cases Only)")

#~~~GOV TRUST INTERACTION

# Model 1: Responsibility with adaptation × gov trust interaction
m_resp_gt <- feols(
  resp ~ second_home + adaptive_measures * GovTrustBin +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + gap_quartile,
  data = hyp_prior,
  vcov = ~ResponseID
)

# Model 2: Percent aid with adaptation × risk gov trust interaction
m_percaid_gt <- feols(
  percent_aid ~ second_home + adaptive_measures * GovTrustBin + 
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + gap_quartile,
  data = hyp_prior,
  vcov = ~ResponseID
)


# Display results
etable(m_resp_gt, m_percaid_gt,
       title = "Effect of Adaptation on Responsibility and Aid, by Government Trust (Prior Info Cases Only)")

# ~~~ tax redist pref INTERACTION
# Model 1: Responsibility with adaptation × gap quartile interaction
m_resp_tx <- feols(
  resp ~ second_home + adaptive_measures * gap_quartile +
    DisasterExperience + Gender + AgeGroup + GovTrustBin +
    Party + Race2 + RiskAversion_bin + AnnualIncome_grouped,
  data = hyp_prior,
  vcov = ~ResponseID
)

# Model 2: Percent aid with adaptation × risk gap quartile interaction
m_percaid_tx <- feols(
  percent_aid ~ second_home + adaptive_measures * gap_quartile + 
    DisasterExperience + Gender + AgeGroup + GovTrustBin +
    Party + Race2 + RiskAversion_bin + AnnualIncome_grouped,
  data = hyp_prior,
  vcov = ~ResponseID
)


# Display results
etable(m_resp_tx, m_percaid_tx,
       title = "Effect of Adaptation on Responsibility and Aid, by Tax Progressivity (Prior Info Cases Only)")


hypotheses(m_resp_tx, 
           "`adaptive_measures:gap_quartileMost tax progressive` - `adaptive_measures:gap_quartileLeast tax progressive` = 0")

hypotheses(m_percaid_tx, 
           "`adaptive_measures:gap_quartileMost tax progressive` - `adaptive_measures:gap_quartileLeast tax progressive` = 0")

#######################

#--------------------------------------------------------------------------------------------------
#         TABLE: ORDINAL regression results
#--------------------------------------------------------------------------------------------------



# ==============================================================================
# FUNCTION: CREATE POLICY PREFERENCE TABLE
# ==============================================================================

create_policy_table <- function(ordinal_results, question_labels) {
  
  # Define model categories and order
  model_categories <- list(
    "Government Mandates" = c("GovRole", "GovRoleA", "GovInsurA"),
    "Market Mechanisms" = c("GovInsur", "GovInsurB"),
    "Public Subsidy" = c("GovRoleB", "GovInsurC", "GovInsurD")
  )
  
  # Predictor labels
  predictor_labels <- c(
    "DisasterExperience1" = "Disaster Experience",
    "PartyDemocrat" = "Democrat (vs Independent)",
    "PartyRepublican" = "Republican (vs Independent)",
    "RiskAversion_binRisk tolerant" = "Risk Tolerant (vs Risk Averse)",
    "RiskAversion_binRisk neutral" = "Risk Neutral (vs Risk Averse)",
    "GovTrustBinHigh government trust" = "High Gov Trust",
    "gap_quartileLeast tax progressive" = "Least tax progressive",
    "gap_quartileMost tax progressive" = "Most tax progressive"
  )
  
  # Predictor order
  predictor_order <- c(
    "DisasterExperience1",
    "PartyDemocrat",
    "PartyRepublican",
    "RiskAversion_binRisk tolerant",
    "RiskAversion_binRisk neutral",
    "GovTrustBinHigh government trust",
    "gap_quartileLeast tax progressive",
    "gap_quartileMost tax progressive"
  )
  
  # Extract and format results for each category
  for (cat_name in names(model_categories)) {
    model_vars <- model_categories[[cat_name]]
    n_models <- length(model_vars)
    
    # Start table
    cat("\\begin{tabular}[t]{>{\\raggedright\\arraybackslash}p{6.4cm}", 
        paste(rep("c", n_models), collapse = ""), "}\n", sep = "")
    cat("\\toprule\n")
    
    # Header row
    if (n_models > 2) {
      cat("\\multicolumn{1}{c}{ } & \\multicolumn{", n_models, "}{c}{OR (SE)} \\\\\n", sep = "")
      cat("\\cmidrule(l{3pt}r{3pt}){2-", n_models + 1, "}\n", sep = "")
    } else {
      cat("\\cmidrule(l{3pt}r{3pt}){2-", n_models + 1, "}\n", sep = "")
    }
    
    cat("\\multicolumn{", n_models + 1, "}{l}{\\textbf{", cat_name, "}} \\\\\n", sep = "")
    cat("\\cmidrule(l{3pt}r{3pt}){1-", n_models + 1, "}\n", sep = "")
    
    # Column headers
    col_headers <- sapply(model_vars, function(x) question_labels[[x]])
    cat("Predictor & ", paste(col_headers, collapse = " & "), " \\\\\n", sep = "")
    cat("\\midrule\n")
    
    # Extract coefficients for each predictor
    for (pred in predictor_order) {
      row_vals <- character(n_models)
      
      for (i in seq_along(model_vars)) {
        m <- model_vars[i]
        res <- ordinal_results[[m]]$results
        
        if (!is.null(res) && pred %in% res$Variable) {
          coef_row <- res[res$Variable == pred, ]
          or <- exp(coef_row$Coefficient)
          se <- coef_row$SE
          p <- coef_row$`Pr(>|z|)`
          
          # Significance stars
          stars <- ifelse(p < 0.001, "***",
                          ifelse(p < 0.01, "**",
                                 ifelse(p < 0.05, "*",
                                        ifelse(p < 0.1, ".", ""))))
          
          row_vals[i] <- sprintf("%.2f (%.3f)%s", or, se, stars)
        } else {
          row_vals[i] <- ""
        }
      }
      
      pred_label <- predictor_labels[pred]
      cat(pred_label, " & ", paste(row_vals, collapse = " & "), " \\\\\n", sep = "")
      
      # Add space after first 5 predictors
      if (pred == "RiskAversion_binRisk neutral") {
        cat("\\addlinespace\n")
      }
    }
    
    # Table footer
    if (cat_name == "Public Subsidy") {
      cat("\\bottomrule\n")
    } else {
      cat("\\midrule\n")
    }
    cat("\\end{tabular}\n")
    
    if (cat_name != "Public Subsidy") {
      cat("\\vspace{0.5em}\n")
      cat("\\hspace*{-3cm}\n")
    }
  }
  
  # Table caption and label
  cat("\\caption{Odds ratios (OR) with standard errors (SE) for support of government interventions")
  cat("\\label{sitab:policy_ordinal_table}\n")
}

# ==============================================================================
# GENERATE TABLE
# ==============================================================================

# Capture output to a file or console
sink("policy_table.tex")
cat("\\begin{table}[!h]\n")
cat("\\centering\n")
cat("\\footnotesize\n")
cat("\\setlength{\\tabcolsep}{3.5pt}\n")
cat("\\renewcommand{\\arraystretch}{1.1}\n")
cat("\\hspace*{-3cm}\n")

create_policy_table(ordinal_results, question_labels)

cat("\\end{table}\n")
sink()

# Also print to console
cat("\\begin{table}[!h]\n")
cat("\\centering\n")
cat("\\footnotesize\n")
cat("\\setlength{\\tabcolsep}{3.5pt}\n")
cat("\\renewcommand{\\arraystretch}{1.1}\n")
cat("\\hspace*{-3cm}\n")

create_policy_table(ordinal_results, question_labels)

cat("\\end{table}\n")



#--------------------------------------------------------------------------------------------------
#                  FIGURE:  Ordinal regression for policy support, govtrust and party              |        
#--------------------------------------------------------------------------------------------------
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
    Party = fct_relevel(Party, "Independent"),   # if you want Independent as baseline
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


#--------------------------------------------------------------------------------------------------
#                  FIGURE:  Disaster Experience by Type                                           |        
#--------------------------------------------------------------------------------------------------

#Script to generate summary statistics related to experience of disaster and aid 


disaster_sample=hyp%>%filter(DisasterExperience=="Yes")

#deal with respondents listing several disasters and reclassify "Other" answers
#Primary categories:
#Blizzard / Ice storm
#Extreme heat
#Flooding
#Hurricane
#Wildfire
#Tornado
#Earthquake
#Drought
#Landslide

#reclassify "Hail" into "Blizzard / Ice Storm"

disaster_sample$DisasterType[grep("hail",disaster_sample$DisasterType_Text,ignore.case = TRUE)]="Blizzard / Ice storm"

#reclassify various windstorms in "Other"

disaster_sample$DisasterType[c(grep("hurricane",disaster_sample$DisasterType_Text,ignore.case = TRUE),grep("cyclone",disaster_sample$DisasterType_Text))]="Hurricane"

disaster_sample$DisasterType[c(grep("wind",disaster_sample$DisasterType_Text,ignore.case = TRUE),grep("Derecho",disaster_sample$DisasterType_Text,ignore.case = TRUE),grep("thunderstorm",disaster_sample$DisasterType_Text),grep("Durasho",disaster_sample$DisasterType_Text))]="Tornado"

#now deal with people listing multiple disasters

#1. if more than 2 disasters listed, drop as too difficult to figure out which disaster was meant
disaster_sample=disaster_sample%>%
  filter(str_count(DisasterType,",")<=1)

#2. use lexicographic assingment for remaining based on likely source of property damage:
#NOTE - there are quite a number of people reporting Flooding and Hurricane - this procedure assigns these damages to Hurricanes

ordering=c("Hurricane","Flooding","Tornado","Wildfire","Earthquake","Blizzard / Ice storm","Landslide","Extreme heat","Drought")

for(i in 1:length(ordering)){
  doubles=which(str_count(disaster_sample$DisasterType,",")==1)
  #if none left then break
  if(length(doubles)==0) break
  #replace double with assigned single disaster based on lexicographic ordering
  toreplace=grep(ordering[i],disaster_sample$DisasterType[doubles])
  disaster_sample$DisasterType[doubles[toreplace]]=ordering[i]
}

#filter out remaining other
disaster_sample=disaster_sample%>%
  filter(DisasterType!="Other")

#rename some categories based on reclassification

disaster_sample$DisasterType=fct_recode(disaster_sample$DisasterType,"Blizzard / Hail"="Blizzard / Ice storm","Tornado / Wind Storm"="Tornado")

#plot stacked bar of home damage by disaster type

disaster_sample$HomeDamage_Dollar=ordered(disaster_sample$HomeDamage_Dollar,levels=c("Under $1,000","$1,000 – $9,999","$10,000 – $19,999","$20,000 – $49,999","$50,000 – $99,999","$100,000 or more"))

#order so that largest disaster type is on the bottom
disaster_sample$DisasterType=ordered(disaster_sample$DisasterType,levels=rev(c("Hurricane","Flooding","Tornado / Wind Storm","Blizzard / Hail","Wildfire","Earthquake","Landslide","Extreme heat","Drought")))

a=ggplot(disaster_sample,aes(x=HomeDamage_Dollar,group=DisasterType,fill=DisasterType))+geom_bar(stat="count",position="stack")+
  theme_bw()+labs(x="Damage to Home",y="Number of Respondents",fill="Disaster")
