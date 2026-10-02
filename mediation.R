library(fixest)

hyp$resp = as.numeric(hyp$resp)
covs <- ~ DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
  Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile

# 1. Mediator equation  -> a
m_a <- feols(
  resp ~ scenario6 +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp, vcov = ~ResponseID
)

# 2. Total effect  -> c
m_c <- feols(
  percent_aid ~ scenario6 +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp, vcov = ~ResponseID
)

# 3. Direct effect  -> c' and b  (same as model 2, plus resp)
m_cp <- feols(
  percent_aid ~ resp + scenario6+
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp, vcov = ~ResponseID
)

etable(m_a, m_c, m_cp)
# -------
b  <- coef(m_cp)["resp"]
A  <- coef(m_a); C <- coef(m_c); CP <- coef(m_cp)

# ------------- 8/5/2026
# =====================================================================
# Mediation, minimal version -- exactly the estimation text on the DAG:
#
#   M ~ X + W          and    Y ~ X + M + X:M + W
#   -> report ACME, ADE, total effect per attribute
#
#   X1 prior information  } one three-arm randomized factor `info_arm`
#   X2 adaptive measures  }  (X2 is nested in X1: 6 design cells, not 8)
#   X3 second home        -> `second_home`
#   M  perceived responsibility -> `resp`      Y  aid -> `percent_aid`
#   W  demographics only, per advisor
#
# X randomized => X -> M and X -> Y need no adjustment.
# W is there for the M -> Y edge only (sequential ignorability).
# =====================================================================

library(dplyr)
library(mediation)

hyp <- readRDS("data/data_updated.rds")


# ---- 1. Exposures ---------------------------------------------------

hyp <- hyp %>%
  mutate(
    info_arm = factor(
      case_when(prior_info == 0                          ~ "none",
                prior_info == 1 & adaptive_measures == 0 ~ "info",
                prior_info == 1 & adaptive_measures == 1 ~ "info_adapt"),
      levels = c("none", "info", "info_adapt")),
    second_home = factor(second_home, levels = c(0, 1),
                         labels = c("primary", "second"))
  )

table(hyp$info_arm, hyp$second_home)      # the 6 cells


# ---- 2. Estimation sample -------------------------------------------
# Subset ONCE so the mediator and outcome models are fit on identical rows.

W <- c("Gender", "AgeGroup", "AnnualIncome_grouped", "Race2", "DisasterExperience", "Party", "GovTrustBin", "gap_quartile", "RiskAversion_bin", "hazard")

est <- hyp %>%
  dplyr::select(all_of(c("percent_aid", "resp", "info_arm", "second_home",
                  "hazard", W, "ResponseID"))) %>%
  filter(complete.cases(.)) %>%
  as.data.frame()

nrow(est); n_distinct(est$ResponseID)
est$resp <- as.numeric(est$resp)





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
  mediate(m_M, m_Y, treat = treat, mediator = "resp",
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
med_X3_ni <- mediate(m_M, m_Y_ni, treat = "second_home", mediator = "resp",
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
summary(med_X3_fe)


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


fig1 <- ggplot(dec, aes(x = est, y = quantity, colour = quantity)) +
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

fig1
ggsave("output/fig_mediation_decomposition.png", fig1,
       width = 6.8, height = 6.4, dpi = 300, bg = "white")


# -------------
# ---- 5. Heterogeneity: split-sample mediation ------------------------
library(purrr); library(ggplot2)

split_vars <- c("Party", "gap_quartile", "AnnualIncome_grouped",
                "RiskAversion_bin", "DisasterExperience", "GovTrustBin")

contrasts <- list(
  list(lab = "X1: prior info vs. none",      t = "info_arm",    a = "none",    b = "info"),
  list(lab = "X2: adaptation | prior info",  t = "info_arm",    a = "info",    b = "info_adapt"),
  list(lab = "X3: second home vs. primary",  t = "second_home", a = "primary", b = "second"))

tidy2 <- function(x) data.frame(
  effect = c("ACME", "ADE", "Total"),
  est = c(x$d.avg, x$z.avg, x$tau.coef),
  lo  = c(x$d.avg.ci[1], x$z.avg.ci[1], x$tau.ci[1]),
  hi  = c(x$d.avg.ci[2], x$z.avg.ci[2], x$tau.ci[2]))

# refit both models on `dat` and run all three contrasts
med_on <- function(dat, drop = NULL, sims = 1000) {
  keep <- setdiff(W, drop)
  keep <- keep[vapply(dat[keep], function(z) length(unique(z)) > 1, TRUE)]  # drop constants
  r  <- paste(keep, collapse = " + ")
  mM <- do.call(lm, list(as.formula(paste("resp ~ info_arm + second_home +", r)), data = dat))
  mY <- do.call(lm, list(as.formula(paste("percent_aid ~ info_arm * resp + second_home * resp +", r)), data = dat))
  map_dfr(contrasts, function(cc) {
    set.seed(2026)
    m <- mediate(mM, mY, treat = cc$t, mediator = "resp",
                 control.value = cc$a, treat.value = cc$b,
                 sims = sims, cluster = dat$ResponseID)
    transform(tidy2(m), contrast = cc$lab)
  })
}

pooled <- transform(med_on(est), split_var = "All", level = "All respondents")

het <- map_dfr(set_names(split_vars), function(sv) {
  map_dfr(set_names(sort(unique(as.character(est[[sv]])))), function(lv) {
    d <- est[as.character(est[[sv]]) == lv, ]
    if (sv == "Party" && lv == "Other") return(NULL) #Exclude "Other" party which is nonresponse from the analysis
    med_on(d, drop = sv)
  }, .id = "level")
}, .id = "split_var")

# ---- 6. Figure -------------------------------------------------------
# counts per split_var × level, plus the pooled row
cnt <- map_dfr(set_names(split_vars), function(sv) {
  data.frame(level = names(table(as.character(est[[sv]]))),
             n_vig = as.integer(table(as.character(est[[sv]]))),
             n_resp = as.integer(tapply(est$ResponseID, as.character(est[[sv]]),
                                        function(z) length(unique(z)))))
}, .id = "split_var") %>%
  bind_rows(data.frame(split_var = "All", level = "All respondents",
                       n_vig = nrow(est), n_resp = n_distinct(est$ResponseID)))

pd <- bind_rows(pooled, het) %>%
  filter(effect != "Total",
         !(split_var == "Party" & level == "Other")) %>%
  left_join(cnt, by = c("split_var", "level")) %>%
  mutate(split_var = factor(split_var, c("All", split_vars)),
         lab = paste0(level, "  (n = ", n_resp, ")"),
         lab = factor(lab, levels = rev(unique(lab))))

ref <- filter(pd, split_var == "All") %>% dplyr::select(contrast, effect, est)

ggplot(pd, aes(est, lab, colour = effect)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_vline(data = ref, aes(xintercept = est, colour = effect),
             linetype = 2, linewidth = .3) +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = .55), size = .35) +
  facet_grid(split_var ~ contrast, scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c(ACME = "blue", ADE = "orange")) +
  labs(x = "Effect on recommended aid (percentage points)", y = NULL, colour = NULL) +
  theme_bw(base_size = 13) +
  theme(strip.text.y   = element_text(angle = 0, size = 12),
        strip.text.x   = element_text(size = 12),
        axis.text.y    = element_text(size = 11),
        axis.text.x    = element_text(size = 11),
        axis.title.x   = element_text(size = 13),
        legend.text    = element_text(size = 12),
        legend.position = "top")

ggsave("figures/Mediation/mediation_heterogeneity.png", width = 15, height = 11, dpi = 600)

# ---- 6b. One figure per split variable --------------------------------
pd <- bind_rows(pooled, het) %>%
  filter(effect != "Total") %>%
  mutate(effect = factor(effect, c("ACME", "ADE")))

ref <- filter(pd, split_var == "All") %>% dplyr::select(contrast, effect, est)
dodge <- position_dodge(width = .6)

plot_one <- function(sv) {
  cnt <- c(table(as.character(est[[sv]])), "All respondents" = nrow(est))
  d <- pd %>%
    filter(split_var %in% c("All", sv)) %>%
    mutate(lab = paste0(level, "  (n = ", cnt[as.character(level)], ")"))
  d$lab <- factor(d$lab, levels = rev(c(unique(d$lab[d$split_var == sv]),
                                        unique(d$lab[d$split_var == "All"]))))
  
  ggplot(d, aes(est, lab, colour = effect)) +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_hline(yintercept = 1.5, colour = "grey80", linewidth = .3) +
    geom_pointrange(aes(xmin = lo, xmax = hi, shape = split_var == "All"),
                    position = dodge, size = .45, linewidth = .6) +
    geom_text(aes(label = sprintf("%.1f", est)), position = dodge,
              vjust = -1.1, size = 3.6, show.legend = FALSE) +
    facet_wrap(~ contrast, nrow = 1) +
    scale_colour_manual(values = c(ACME = "green4", ADE = "blue"),
                        labels = c(ACME = "Mediated (ACME)", ADE = "Direct (ADE)"),
                        na.translate = FALSE) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
    scale_x_continuous(expand = expansion(mult = .15)) +
    labs(title = sv, x = "Effect on preferred aid (percentage points)",
         y = NULL, colour = NULL) +
    theme_bw(base_size = 13) +
    theme(legend.position = "top", plot.title = element_text(face = "bold", size = 15),
          strip.text = element_text(size = 12), axis.text = element_text(size = 11))
}
walk(split_vars, function(sv) {
  nlev <- n_distinct(c(unique(as.character(het$level[het$split_var == sv])), "All"))
  ggsave(file.path("figures/mediation", paste0("mediation_het_", sv, ".png")),
         plot_one(sv), width = 9, height = 1.8 + 0.7 * nlev, dpi = 300)
})

plot_one("gap_quartile")   # preview one


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### RESPONDENT FIXED EFFECTS FOR U
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ---- 1a. Build one row per respondent: the DIFFERENCE between their two scenarios
d <- est %>%
  group_by(ResponseID) %>%
  filter(n() == 2,
         sum(hazard == "flood") == 1,
         sum(hazard == "fire")  == 1) %>%
  summarise(
    d_resp      = resp[hazard == "flood"]        - resp[hazard == "fire"],
    d_aid       = percent_aid[hazard == "flood"] - percent_aid[hazard == "fire"],
    d_info      = (info_arm[hazard == "flood"] == "info")       -
      (info_arm[hazard == "fire"]  == "info"),
    d_infoadapt = (info_arm[hazard == "flood"] == "info_adapt") -
      (info_arm[hazard == "fire"]  == "info_adapt"),
    d_second    = (second_home[hazard == "flood"] == "second")  -
      (second_home[hazard == "fire"]  == "second"),
    .groups = "drop"
  )

nrow(d)                        # ≈ number of respondents
table(d$d_second)              # sanity: you need plenty of -1s and +1s, not all 0s
table(d$d_info, d$d_infoadapt)

# ---- 1b. The same two models, now on differences
m_M_fe <- lm(d_resp ~ d_info + d_infoadapt + d_second, data = d)
m_Y_fe <- lm(d_aid  ~ d_info + d_infoadapt + d_second + d_resp, data = d)

summary(m_M_fe)   # X -> M, within person
summary(m_Y_fe)   # d_resp coefficient is the b-path; the d_* coefficients are the ADEs

# ------------

# ---- 1c. ACME = a * b, with a bootstrap over RESPONDENTS for the interval
fe_effects <- function(dat) {
  mM <- lm(d_resp ~ d_info + d_infoadapt + d_second, dat)
  mY <- lm(d_aid  ~ d_info + d_infoadapt + d_second + d_resp, dat)
  b  <- coef(mY)[["d_resp"]]                       # one unit more responsibility -> b pp of aid
  a  <- coef(mM)[c("d_info","d_infoadapt","d_second")]   # X -> M
  cp <- coef(mY)[c("d_info","d_infoadapt","d_second")]   # X -> Y holding M fixed
  c(
    ACME_X1 = a[["d_info"]] * b,                         # info vs none
    ACME_X2 = (a[["d_infoadapt"]] - a[["d_info"]]) * b,  # adapt vs info-only
    ACME_X3 = a[["d_second"]] * b,                       # second home vs primary
    ADE_X1  = cp[["d_info"]],
    ADE_X2  = cp[["d_infoadapt"]] - cp[["d_info"]],
    ADE_X3  = cp[["d_second"]],
    b_path  = b
  )
}

point <- fe_effects(d)

set.seed(2026)
boot <- replicate(1000, {
  dd <- d[sample(nrow(d), replace = TRUE), ]   # resample WHOLE respondents, with replacement
  fe_effects(dd)
})

fe_results <- data.frame(
  estimate = point,
  lo = apply(boot, 1, quantile, 0.025),
  hi = apply(boot, 1, quantile, 0.975)
)
round(fe_results, 2)

##### ----------------
# check, regress ind and dep on fe, take residuals and put into model 
# email jo at ucdavis address


### FIGURE 
# =====================================================================
# Figure: mediation decomposition, respondent fixed effects
# Matches the format of fig1 (pooled mediate() decomposition).
# Inputs: `point` and `boot` from fe_effects()/the bootstrap, `d`.
# =====================================================================
library(dplyr); library(ggplot2); library(grid)

# ---- 1. Totals, with the same bootstrap draws --------------------------
# ACME + ADE within a single bootstrap column is that draw's total effect,
# so the interval respects the correlation between the two components.
tot_boot <- rbind(
  X1 = boot["ACME_X1", ] + boot["ADE_X1", ],
  X2 = boot["ACME_X2", ] + boot["ADE_X2", ],
  X3 = boot["ACME_X3", ] + boot["ADE_X3", ]
)

dec_fe <- bind_rows(
  # mediated + direct, pulled out of fe_results
  fe_results %>%
    tibble::rownames_to_column("term") %>%
    filter(term != "b_path") %>%
    tidyr::separate(term, c("quantity", "x"), sep = "_") %>%
    rename(est = estimate),
  # totals
  data.frame(
    quantity = "Total",
    x        = c("X1", "X2", "X3"),
    est      = c(point[["ACME_X1"]] + point[["ADE_X1"]],
                 point[["ACME_X2"]] + point[["ADE_X2"]],
                 point[["ACME_X3"]] + point[["ADE_X3"]]),
    lo       = apply(tot_boot, 1, quantile, 0.025),
    hi       = apply(tot_boot, 1, quantile, 0.975),
    row.names = NULL
  )
)

# ---- 2. Labels ---------------------------------------------------------
# Reuse the pooled figure's quantity levels so colours and y-axis wording
# are identical across the two specs. If `dec` is not in the session,
# set these three strings by hand to whatever fig1 uses.
q_levels <- levels(dec$quantity)   # e.g. Total / Direct (ADE) / Mediated (ACME)
dec_fe <- dec_fe %>%
  mutate(quantity = factor(
    q_levels[match(quantity, c("Total", "ADE", "ACME"))], levels = q_levels))

# Strip labels are computed FROM THIS FIGURE'S OWN numbers.
# Do not inherit levels(dec$strip) -- those carry the pooled shares.
share <- dec_fe %>%
  group_by(x) %>%
  summarise(pct = round(100 * abs(est[quantity == q_levels[match("ACME", c("Total","ADE","ACME"))]]) /
                          abs(est[quantity == q_levels[1]])), .groups = "drop")

strip_txt <- c(
  X1 = "Prior information vs. no information",
  X2 = "Prior information + adaptation vs. prior information only",
  X3 = "Second home vs. primary residence"
)

dec_fe <- dec_fe %>%
  left_join(share, by = "x") %>%
  mutate(strip = factor(sprintf("%s  \u2014  %d%% via responsibility",
                                strip_txt[x], pct),
                        levels = sprintf("%s  \u2014  %d%% via responsibility",
                                         strip_txt[c("X1","X2","X3")],
                                         share$pct[match(c("X1","X2","X3"), share$x)])))

# ---- 3. Figure ---------------------------------------------------------
fig1_fe <- ggplot(dec_fe, aes(x = est, y = quantity, colour = quantity)) +
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
       caption = paste0("Respondent fixed effects: each respondent's flood scenario minus their fire scenario, ",
                        "so all time-invariant\nrespondent traits difference out. Points are point estimates with 95% percentile intervals from 1,000\n",
                        "bootstrap resamples of respondents; n = ", nrow(d), " respondents (",
                        2 * nrow(d), " vignettes).\nMediated + direct sum to the total effect. ")) +
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
fig1_fe

# ggsave("figures/Mediation/mediation_fe.png", fig1_fe,
#        width = 7.2, height = 6.4, dpi = 300)


#### ----------------------
## ---- 5. medsens: sensitivity to unobserved M-Y confounding (U) --------
rhs <- paste(W, collapse = " + ")

## Two-arm subsample + binary treatment + additive outcome model.
run_sens <- function(dat, treat_var, from, to, other_var,
                     rho.by = 0.05, sims = 1000) {
  d <- dat[dat[[treat_var]] %in% c(from, to), ]
  d$Tbin <- as.numeric(d[[treat_var]] == to)          # 0 = from, 1 = to
  
  fM <- as.formula(paste("resp ~ Tbin +", other_var, "+", rhs))
  fY <- as.formula(paste("percent_aid ~ Tbin + resp +", other_var, "+", rhs))
  mM <- lm(fM, data = d)
  mY <- lm(fY, data = d)                              # NO Tbin:resp
  
  med  <- mediate(mM, mY, treat = "Tbin", mediator = "resp", sims = 500)
  sens <- medsens(med, rho.by = rho.by, effect.type = "both", sims = sims)
  list(d = d, m_M = mM, m_Y = mY, med = med, sens = sens)
}

s_X1 <- run_sens(est, "info_arm",    "none",    "info",       "second_home")
s_X2 <- run_sens(est, "info_arm",    "info",    "info_adapt", "second_home")
s_X3 <- run_sens(est, "second_home", "primary", "second",     "info_arm")

lapply(list(X1 = s_X1, X2 = s_X2, X3 = s_X3), function(s) summary(s$sens))

r_M <- resid(lm(as.formula(paste("resp ~ Tbin +", "second_home +", rhs)), data = s_X1$d))
r_Y <- resid(lm(as.formula(paste("percent_aid ~ Tbin +", "second_home +", rhs)), data = s_X1$d))
cor(r_M, r_Y)   # -0.3054291
# ------------------------------------------------------------------------------
# Interpretation: For all three treatment comparisons (prior information, adaptation, and second-home status), the ACME would be reduced to zero at approximately |ρ| = 0.30, suggesting that the mediation results are somewhat sensitive to moderate unmeasured confounding.

# would need a much stronger unmeasured confounder to eliminate the direct effect of second-home status than to eliminate its mediated effect through responsibility.
