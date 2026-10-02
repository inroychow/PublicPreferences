pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, splines, readr, ggridges, forcats, stringi, purr, viridis)

# source("src/analysis/useful_functions.R")
# source("src/indu_sandbox/generate_hyp.R")

hyp = readRDS("data/data_updated.rds")
hyp <- hyp %>%
  mutate(resp_numeric = as.numeric(resp))

# "This represents a loss in the 99th percentile of117
# disaster losses reported by those applying for FEMA assistance, meaning our survey scenario118
# examines preferences for assistance in the case of an exceptionally large loss that exceeds the119
# mean reported disaster damage ($7,849) by many orders (Federal Emergency Management120
# Agency, 2025b)."

ia_raw <- data.table::fread("Filepath/IndividualsAndHouseholdsProgramValidRegistrationsV2.csv") #26218889 rows

# keep inspected with any verified loss, and positive verified loss
ia <- ia_raw %>%
  filter(!is.na(rpfvl) | !is.na(ppfvl)) %>%
  mutate(verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0)) %>%
  filter(verified_loss > 0) |> 
  filter(ownRent == "O") #6285927 rows

# FILTER TO JUST UNINSURED LOSSES...
# ia_nofl <- ia %>%
#    filter(floodInsurance == 0) #5540780 rows
# 
# ia <- ia_nofl %>%
#   filter(homeOwnersInsurance == 0) #2406870 rows... this is 38% of 6285927, unfiltered row #


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

ia %>%
  summarise(
    n          = n(),
    mean_loss  = mean(verified_loss_2024, na.rm = TRUE),
    median_loss = median(verified_loss_2024, na.rm = TRUE),
    p99_loss   = quantile(verified_loss_2024, 0.99, na.rm = TRUE)
  ) # Mean loss is 8512 when we restrict to homeowners. 7819 when we do not.
# ------------------------------------------------------------------------------------------------------------------------------
#"When we restrict the IHP application data\footnote{We analyze the range of available data from 2002 to 2025. This is a total of 6,280,571 applicants.} to large-loss cases (reported losses above \$40{,}000), which better match our hypothetical vignette, grant coverage falls sharply. A large fraction of applicants (42\%) receive grants compensating less than 10\% of their disaster loss  \citep{FEMA2025_IHP_Registrations}."
# ------------------------------------------------------------------------------------------------------------------------------

library(data.table); library(dplyr); library(quantmod); library(lubridate)

ia_raw <- fread("C:\\Users\\indumati\\Box\\FEMA DATA\\Individual Assistance\\IndividualsAndHouseholdsProgramValidRegistrationsV2_2026.csv") #26218889

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

# #   0 = uninsured, 1 = insured, c(0, 1) = either
# build_owners <- function(ho = c(0,1), fl = c(0,1)) {
#   ia_raw |>
#     filter(!is.na(rpfvl) | !is.na(ppfvl), ownRent == "O",
#            homeOwnersInsurance %in% ho, floodInsurance %in% fl) |>
#     mutate(
#       verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0),
#       haAmount      = if (na_award_as_zero) coalesce(haAmount, 0) else haAmount,
#       year_decl     = year(as.Date(sub("T.*$", "", declarationDate)))
#     ) |>
#     filter(verified_loss > 0, is.na(haAmount) | haAmount <= ha_cap) |>
#     left_join(cpi_year, by = "year_decl") |>
#     mutate(verified_loss_2024 = verified_loss * cpi_2024 / cpi,
#            comp_rate          = haAmount / verified_loss) |>
#     filter(is.finite(comp_rate), comp_rate >= 0, !is.na(verified_loss_2024))
# }

## HA net of rental assistance 
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

all <- build_owners_noRA(ho = c(0), fl = c(0))   # everyone regardless of insurance
summarise_large(all)



# ------------------------------------------------------------------------------------------------------------------------------
#"On average, respondents recommend 64% aid coverage if assigning zero responsibility versus 22% at maximum responsibility. "
# ------------------------------------------------------------------------------------------------------------------------------

aid_by_responsibility <- hyp %>%
  group_by(resp) %>%
  summarise(
    mean_aid = mean(percent_aid, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(resp)

print(aid_by_responsibility)

#------------------------------------------------------------------------------------------------------------------------------
# MAIN TEXT: "Also, relatively few households make use of the program: only 2\% of FEMA grant recipients were approved for an SBA disaster loan. "
#------------------------------------------------------------------------------------------------------------------------------
sba_approved = raw |> 
   filter(sbaApproved==1)
raw_n = nrow(raw)
sba_n = nrow(sba_approved) 
sba_n/raw_n #0.01839239
View(sba_approved)

# ------------------------------------------------------------------------------------------------------------------------------
# RANDOM INTERCEPT MODEL 
# ------------------------------------------------------------------------------------------------------------------------------

library(lme4)

# (a) Between- vs within-respondent variance partition
icc_mod  <- lmer(resp ~ 1 + (1 | ResponseID), data = est)
vc        <- as.data.frame(VarCorr(icc_mod))
v_between <- vc$vcov[vc$grp == "ResponseID"]   # intrinsic ceiling
v_within  <- vc$vcov[vc$grp == "Residual"]     # extrinsic + noise

data.frame(
  component = c("Between respondents (intrinsic ceiling)",
                "Within respondent (scenario + noise)"),
  share     = c(v_between, v_within) / (v_between + v_within)
)

# (b) Variance explained by observed predictors
data.frame(
  predictors = c("Respondent characteristics (intrinsic)",
                 "Scenario features (extrinsic)",
                 "Both"),
  r_squared  = c(r2(W_resp), r2(scen), r2(c(W_resp, scen)))
)

library(fixest)
m_full <- feols(percent_aid ~ resp + hazard + info_arm + second_home +
                  Gender + AgeGroup + AnnualIncome_grouped + Race2 +
                  DisasterExperience + Party + GovTrustBin +
                  gap_quartile + RiskAversion_bin,
                data = est, vcov = ~ResponseID)
summary(m_full)


preds <- c("resp","hazard","info_arm","second_home",
           "Gender","AgeGroup","AnnualIncome_grouped","Race2",
           "DisasterExperience","Party","GovTrustBin",
           "gap_quartile","RiskAversion_bin")

full_lm <- lm(reformulate(preds, "percent_aid"), data = est)
full_r2 <- summary(full_lm)$r.squared

drop_r2 <- sapply(preds, function(v) {
  reduced <- lm(reformulate(setdiff(preds, v), "percent_aid"), data = est)
  full_r2 - summary(reduced)$r.squared          # how much R2 you lose without v
})

round(sort(drop_r2, decreasing = TRUE), 4)      # your ranked "strongest predictors"


r2_of <- function(rhs) summary(lm(reformulate(rhs, "percent_aid"), est))$r.squared
c(resp_only   = r2_of("resp"),
  design_only = r2_of(c("hazard","info_arm","second_home")),
  traits_only = r2_of(c("Gender","AgeGroup","AnnualIncome_grouped","Race2",
                        "DisasterExperience","Party","GovTrustBin",
                        "gap_quartile","RiskAversion_bin")),
  full        = full_r2)

# ------------------------------------------------------------------------------------------------------------------------------
# "A nonlinear specification shows a steeper decline in aid at low responsibility ratings (a 20 percentage-point drop from responsibility 0 to 3) that flattens at higher responsibility (a 7 percentage-point drop from responsibility 7 to 10) 
# ------------------------------------------------------------------------------------------------------------------------------

hyp_clean <- hyp %>%
  mutate(
    resp = parse_number(as.character(resp)),
    percent_aid = parse_number(as.character(percent_aid))
  ) %>%
  filter(!is.na(resp), !is.na(percent_aid))
spline_df <- 4

spl_fit <- lm(percent_aid ~ ns(resp, df = spline_df), data = hyp_clean)
pred_df <- data.frame(resp = 0:10) %>%
  mutate(
    pred_aid = predict(spl_fit, newdata = data.frame(resp = resp)),
    pred_aid = pmin(pmax(pred_aid, 0), 100)   # optional bounding to 0–100
  )

drop_0_3  <- with(pred_df, pred_aid[resp == 0] - pred_aid[resp == 3])
drop_7_10 <- with(pred_df, pred_aid[resp == 7] - pred_aid[resp == 10])

round(c(
  drop_0_3  = drop_0_3,
  drop_7_10 = drop_7_10
), 1)

sprintf(
  paste0(
    "A nonlinear specification shows a steeper decline in aid at low responsibility ",
    "ratings (a %.1f percentage-point drop from responsibility 0 to 3) that flattens ",
    "at higher responsibility (a %.1f percentage-point drop from responsibility 7 to 10)."
  ),
  round(drop_0_3, 1),
  round(drop_7_10, 1)
)

# ------------------------------------------------------------------------------------------------------------------------------
#"One third of respondents recommended no aid at all to the base case household; the remaining two thirds of respondents recommended coverage totalling 59.6% of the total loss."
# ------------------------------------------------------------------------------------------------------------------------------

# Overall: % recommending zero aid by home type
zero_aid_overall <- hyp %>%
  group_by(second_home) %>%
  summarize(
    n_total = n(),
    n_zero = sum(percent_aid == 0, na.rm = TRUE),
    pct_zero = n_zero / n_total * 100
  )

print(zero_aid_overall)

# By scenario: % recommending zero aid by home type and scenario
zero_aid_by_scenario <- hyp %>%
  mutate(
    Scenario = case_when(
      prior_info == 0 & adaptive_measures == 0 ~ "No prior info, no adaptation (base case)",
      prior_info == 1 & adaptive_measures == 0 ~ "Prior info, no adaptation",
      prior_info == 1 & adaptive_measures == 1 ~ "Prior info, adaptation",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(second_home, Scenario) %>%
  summarize(
    n_total = n(),
    n_zero = sum(percent_aid == 0, na.rm = TRUE),
    pct_zero = n_zero / n_total * 100,
    .groups = "drop"
  ) %>%
  arrange(Scenario, second_home)

print(zero_aid_by_scenario)

# Clean table format
zero_aid_by_scenario %>%
  mutate(
    Home_Type = ifelse(second_home == 0, "Primary Residence", "Second Home")
  ) %>%
  select(Scenario, Home_Type, pct_zero) %>%
  pivot_wider(names_from = Home_Type, values_from = pct_zero)



# ------------------------------------------------------------------------------------------------------------------------------
# For instance, about 19% of respondents recommend no assistance at the lowest responsibility score, while about 7% of respondents assign a maximum responsibility score of 10 but still recommend the victim be fully compensated by the government.
# ------------------------------------------------------------------------------------------------------------------------------


hyp_check <- hyp %>%
  mutate(
    resp = parse_number(as.character(resp)),
    percent_aid = parse_number(as.character(percent_aid))
  ) %>%
  filter(!is.na(resp), !is.na(percent_aid))

p_noaid_resp0 <- hyp_check %>%
  filter(resp == 0) %>%
  summarize(p = mean(percent_aid == 0), n = dplyr::n()) %>%
  mutate(pct = 100 * p)

p_full_resp10 <- hyp_check %>%
  filter(resp == 10) %>%
  summarize(p = mean(percent_aid == 100), n = dplyr::n()) %>%
  mutate(pct = 100 * p)

p_noaid_resp0
p_full_resp10

sprintf(
  "About %.0f%% of respondents recommend no assistance at the lowest responsibility score, while about %.0f%% of respondents assign a maximum responsibility score of 10 but still recommend the victim be fully compensated by the government.",
  p_noaid_resp0$pct, p_full_resp10$pct
)


# ------------------------------------------------------------------------------------------------------------------------------
# It should be noted that about 81% of these zero-slope respondents recommended no aid in both scenarios, suggesting they are opposing disaster assistance altogether rather than expressing an egalitarian aid ethic. The remaining 19% of that group recommends, on average, that 69% of the loss be covered in both scenarios
# ------------------------------------------------------------------------------------------------------------------------------
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


zero_both0 <- hyp %>%
  filter(ResponseID %in% zero_ids) %>%
  group_by(ResponseID) %>%
  summarise(both0 = all(gov_amt == 0), .groups = "drop")

# how many zero-slope people answered 0 in BOTH scenarios?
n_zero_both0 <- sum(zero_both0$both0, na.rm = TRUE)
n_zero_total <- nrow(zero_both0)

n_zero_both0/n_zero_total

remaining_zero_ids <- zero_both0 %>%
  filter(!both0) %>%
  pull(ResponseID)

# respondent-level mean aid (averaged across the 2 scenarios), then overall mean
remaining_summary <- hyp %>%
  filter(ResponseID %in% remaining_zero_ids) %>%
  group_by(ResponseID) %>%
  summarise(mean_gov = mean(gov_amt, na.rm = TRUE), .groups = "drop") %>%
  summarise(
    n_resp = n(),
    avg_recommended = mean(mean_gov, na.rm = TRUE),
    median_recommended = median(mean_gov, na.rm = TRUE)
  )

remaining_summary
remaining_avg = 172 / 250

# ------------------------------------------------------------------------------------------------------------------------------
#Approximately 47% of respondents recom- mend no aid at all for second homes, compared to 35% for primary residences in otherwise identical scenarios (Table S3).
# ------------------------------------------------------------------------------------------------------------------------------

zero_aid_overall <- hyp %>%
  group_by(second_home) %>%
  summarize(
    n_total = n(),
    n_zero = sum(percent_aid == 0, na.rm = TRUE),
    pct_zero = n_zero / n_total * 100
  )

print(zero_aid_overall)


#------------

# Filter for people who said no recommended aid (0% for percent_aid)
no_aid <- hyp %>%
  filter(percent_aid == 0)

# Look at the distribution of perceived responsibility (resp) for this group
summary(as.numeric(no_aid$resp))

# Create a simple table of responsibility values
table(no_aid$resp)

# Calculate mean and other statistics
no_aid %>%
  summarise(
    n = n(),
    mean_resp = mean(as.numeric(resp), na.rm = TRUE),
    median_resp = median(as.numeric(resp), na.rm = TRUE),
    sd_resp = sd(as.numeric(resp), na.rm = TRUE),
    min_resp = min(as.numeric(resp), na.rm = TRUE),
    max_resp = max(as.numeric(resp), na.rm = TRUE)
  )

#########################################
# We find no statistically significant interaction between the second home penalty and party identification, preferred tax progressivity, or respondent income
#########################################

m_het_party <- feols(
  percent_aid ~ second_home * Party + prior_info + adaptive_measures + DisasterExperience +
    Gender + AgeGroup + Race2 + gap_quartile + AnnualIncome_grouped +RiskAversion_bin + GovTrustBin,
  data = hyp, vcov = ~ResponseID
)
summary(m_het_party)
m_het_tax <- feols(
  percent_aid ~ second_home * gap_quartile + prior_info + adaptive_measures + DisasterExperience +
    Gender + AgeGroup + Race2 + gap_quartile + AnnualIncome_grouped +RiskAversion_bin + GovTrustBin,
  data = hyp, vcov = ~ResponseID
)
summary(m_het_tax)
m_het_income <- feols(
  percent_aid ~ second_home * AnnualIncome_grouped + prior_info + adaptive_measures + DisasterExperience +
    Gender + AgeGroup + Race2 + gap_quartile + AnnualIncome_grouped +RiskAversion_bin + GovTrustBin,
  data = hyp, vcov = ~ResponseID
)
summary(m_het_income)
m_het_risk <- feols(
  percent_aid ~ second_home * RiskAversion_bin + controls,
  data = hyp, vcov = ~ResponseID
)

m_het_govtrust <- feols(
  percent_aid ~ second_home * GovTrustBin + controls,
  data = hyp, vcov = ~ResponseID
)

# --- Hypothesis tests ---

# Party: Democrat vs Republican
hypotheses(m_het_party,
           "`second_homesecond:PartyDemocrat` - `second_homesecond:PartyRepublican` = 0")

# Tax progressivity: Most vs Least
hypotheses(m_het_tax,
           "`second_homesecond:gap_quartileMost tax progressive` - `second_homesecond:gap_quartileLeast tax progressive` = 0")

# Income: >$250k vs <$100k
hypotheses(m_het_income,
           "`second_homesecond:AnnualIncome_groupedAnnual Income > $250,000` - `second_homesecond:AnnualIncome_groupedAnnual Income < $100,000` = 0")

# Risk aversion: Averse vs Tolerant
hypotheses(m_het_risk,
           "`second_home:RiskAversion_binRisk Averse` - `second_home:RiskAversion_binRisk Tolerant` = 0")

# Government trust: High vs Low
hypotheses(m_het_govtrust,
           "`second_home:GovTrustBinHigh` - `second_home:GovTrustBinLow` = 0")

### --------------------------------

#  Lastly, the paper shows that responsibility attribution has limited predictive power for aid support, as reflected in the small R-squared value. It would be useful to clarify the extent to which individuals’ attribution of disaster responsibility varies across scenarios and whether it is primarily intrinsic (driven by respondent characteristics) or extrinsic (driven by scenario design). In addition, the paper should more clearly identify which factors are the strongest predictors of aid support overall. The manuscript also appears to overlook an important strand of literature on the distribution of federal disaster aid (e.g., Emrich et al., 2020; Drakes et al., 2021; Raker, 2023) along dimensions of social vulnerability. Beyond responsibility attribution, it may be valuable to examine support for directing post-disaster aid toward vulnerable populations.

# Shared setup: separate intrinsic (respondent) from extrinsic (scenario)
scen   <- c("info_arm", "second_home", "hazard")   # extrinsic: scenario design
W_resp <- setdiff(W, "hazard")                      # intrinsic: respondent chars
r2 <- function(vars) summary(lm(reformulate(vars, "resp"), est))$r.squared


hyp <- readRDS("data/data_updated.rds")

table(hyp$info_arm, hyp$second_home)      # the 6 cells


W <- c("Gender", "AgeGroup", "AnnualIncome_grouped", "Race2", "DisasterExperience", "Party", "GovTrustBin", "gap_quartile", "RiskAversion_bin", "hazard")

est <- hyp %>%
  dplyr::select(all_of(c("percent_aid", "resp", "info_arm", "second_home",
                         "hazard", W, "ResponseID"))) %>%
  filter(complete.cases(.)) %>%
  as.data.frame()

nrow(est); n_distinct(est$ResponseID)
est$resp <- as.numeric(est$resp)
# Mean responsibility by scenario cell and by hazard
aggregate(resp ~ info_arm + second_home, est, mean)
aggregate(resp ~ hazard, est, mean)

# Share of responsibility variance explained by scenario features alone
r2(scen)

### --------

library(lme4)

# (a) Between- vs within-respondent variance partition
icc_mod  <- lmer(resp ~ 1 + (1 | ResponseID), data = est)
vc        <- as.data.frame(VarCorr(icc_mod))
v_between <- vc$vcov[vc$grp == "ResponseID"]   # intrinsic ceiling
v_within  <- vc$vcov[vc$grp == "Residual"]     # extrinsic + noise

data.frame(
  component = c("Between respondents (intrinsic ceiling)",
                "Within respondent (scenario + noise)"),
  share     = c(v_between, v_within) / (v_between + v_within)
)

# (b) Variance explained by observed predictors
data.frame(
  predictors = c("Respondent characteristics (intrinsic)",
                 "Scenario features (extrinsic)",
                 "Both"),
  r_squared  = c(r2(W_resp), r2(scen), r2(c(W_resp, scen)))
)

library(fixest)
m_full <- feols(percent_aid ~ resp + hazard + info_arm + second_home +
                  Gender + AgeGroup + AnnualIncome_grouped + Race2 +
                  DisasterExperience + Party + GovTrustBin +
                  gap_quartile + RiskAversion_bin,
                data = est, vcov = ~ResponseID)
summary(m_full)


preds <- c("resp","hazard","info_arm","second_home",
           "Gender","AgeGroup","AnnualIncome_grouped","Race2",
           "DisasterExperience","Party","GovTrustBin",
           "gap_quartile","RiskAversion_bin")

full_lm <- lm(reformulate(preds, "percent_aid"), data = est)
full_r2 <- summary(full_lm)$r.squared

drop_r2 <- sapply(preds, function(v) {
  reduced <- lm(reformulate(setdiff(preds, v), "percent_aid"), data = est)
  full_r2 - summary(reduced)$r.squared          # how much R2 you lose without v
})

round(sort(drop_r2, decreasing = TRUE), 4)      # your ranked "strongest predictors"


r2_of <- function(rhs) summary(lm(reformulate(rhs, "percent_aid"), est))$r.squared
c(resp_only   = r2_of("resp"),
  design_only = r2_of(c("hazard","info_arm","second_home")),
  traits_only = r2_of(c("Gender","AgeGroup","AnnualIncome_grouped","Race2",
                        "DisasterExperience","Party","GovTrustBin",
                        "gap_quartile","RiskAversion_bin")),
  full        = full_r2)

#### Risk-averse respondents recommend 4 percentage points (pp) more than risk-neutral respondents ($p < 0.05$), consistent with a stronger valuation of public risk sharing against rare, severe losses. 

# 1. Check the current labels so the mapping below matches exactly
levels(factor(hyp$RiskAversion_bin))

# 2. Relabel to car-safe names (no spaces/hyphens) and set neutral as reference
#    Replace the left-hand strings with whatever levels() printed
hyp$RiskAversion_bin <- factor(
  hyp$RiskAversion_bin,
  levels = c("Risk neutral", "Risk averse", "Risk tolerant"),   # old labels
  labels = c("Riskneutral",  "Riskaverse",  "Risktolerant")     # new labels
)
hyp$RiskAversion_bin <- relevel(hyp$RiskAversion_bin, ref = "Riskneutral")

# 3. Re-estimate
full_model <- feols(
  percent_aid ~ info_arm*second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
summary(full_model)

# 4. Confirm the names car will see
grep("RiskAversion", names(coef(full_model)), value = TRUE)

# 5. Test averse vs. tolerant
linearHypothesis(
  full_model,
  "RiskAversion_binRiskaverse = RiskAversion_binRisktolerant"
)

full_model <- feols(
  percent_aid ~ info_arm*second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)


## For reviewer response only -----------------------------------------------


# RISK AVERSION MEASUREMENT SENSITIVITY TESTS #
library(dplyr); library(fixest)

make_bin <- function(x, low, high) {
  factor(case_when(x <= low  ~ "Risk averse",
                   x >= high ~ "Risk tolerant",
                   TRUE      ~ "Risk neutral"),
         levels = c("Risk neutral", "Risk tolerant", "Risk averse"))
}

schemes <- tribble(
  ~low, ~high,
  2,     8,
  4,     7,
  4,     6
)

sensitivity <- schemes %>%
  rowwise() %>%
  mutate(res = list({
    d <- hyp %>% mutate(RiskAversion_bin = make_bin(RiskAversion, low, high))
    feols(percent_aid ~ info_arm*second_home +
            DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
            Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
          data = d, vcov = ~ResponseID) %>%
      broom::tidy() %>%
      filter(grepl("RiskAversion_bin", term))
  })) %>%
  tidyr::unnest(res) %>%
  ungroup() %>%
  dplyr::select(low, high, term, estimate, std.error, p.value)

sensitivity


#Coninuous check
feols(percent_aid ~ info_arm*second_home +
        DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
        Party + Race2 + RiskAversion + GovTrustBin + gap_quartile,
      data = hyp, vcov = ~ResponseID) %>%
  broom::tidy() %>%
  filter(term == "RiskAversion")

# Original cutoff
feols(
  percent_aid ~ info_arm*second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
) |> 
  broom::tidy() %>%
  filter(grepl("RiskAversion_bin", term))


#### "Testing this empirically, we restrict our analysis to vignettes with prior experience of disasters and interact the household's adaptive actions with respondents' risk preferences. "

#--------------------------------------------------------------------------------------------------
#          sensitivity test: HETEROGENEITY in the adaptation rewards, prior info scenarios only
#--------------------------------------------------------------------------------------------------

library(dplyr); library(fixest); library(marginaleffects); library(tidyr); library(purrr)

hyp_prior <- hyp %>% filter(prior_info == 1)
hyp_prior$resp <- as.numeric(hyp_prior$resp)

make_bin <- function(x, low, high) {
  factor(case_when(x <= low  ~ "Risk averse",
                   x >= high ~ "Risk tolerant",
                   TRUE      ~ "Risk neutral"),
         levels = c("Risk neutral", "Risk tolerant", "Risk averse"))
}

schemes <- tribble(
  ~low, ~high,
  2,    8,     # widest neutral band
  3,    7,     # main specification
  4,    7,
  4,    6      # narrowest neutral band
)

diff_h <- paste("`adaptive_measures:RiskAversion_binRisk averse` -",
                "`adaptive_measures:RiskAversion_binRisk tolerant` = 0")

fit_scheme <- function(low, high, yvar) {
  d <- hyp_prior %>% mutate(RiskAversion_bin = make_bin(RiskAversion, low, high))
  f <- as.formula(paste0(
    yvar, " ~ second_home + adaptive_measures * RiskAversion_bin +",
    " DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +",
    " Party + Race2 + GovTrustBin + gap_quartile"))
  m <- feols(f, data = d, vcov = ~ResponseID)
  h <- hypotheses(m, diff_h)                        # averse − tolerant adaptation slope
  # smallest cell feeding the interaction — a fragility flag in this subsample
  min_cell <- d %>% count(RiskAversion_bin, adaptive_measures) %>% pull(n) %>% min()
  tibble(diff = h$estimate, se = h$std.error, p = h$p.value, min_cell = min_cell)
}

sensitivity_het <- expand_grid(schemes, outcome = c("percent_aid", "resp")) %>%
  rowwise() %>%
  mutate(out = list(fit_scheme(low, high, outcome))) %>%
  unnest(out) %>% ungroup()
sensitivity_het

# Continuous interaction — no binning at all
map_dfr(c("percent_aid", "resp"), function(y) {
  f <- as.formula(paste0(
    y, " ~ second_home + adaptive_measures * RiskAversion +",
    " DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +",
    " Party + Race2 + GovTrustBin + gap_quartile"))
  feols(f, data = hyp_prior, vcov = ~ResponseID) %>%
    broom::tidy() %>% filter(term == "adaptive_measures:RiskAversion") %>%
    mutate(outcome = y)
})

# POLICY PREFERENCE SENSITIVTY 
#--------------------------------------------------------------------------------------------------
#   SI SENSITIVITY: risk-aversion cutpoints, policy-preference ordinal models
#--------------------------------------------------------------------------------------------------
# --- Robustness: continuous risk tolerance (0-10, 10 = most risk tolerant) ---

model_variables_cont <- c("DisasterExperience", "Party", "RiskAversion",
                          "GovTrustBin", "gap_quartile")

regression_data_cont <- regression_data %>%
  dplyr::mutate(RiskAversion = as.numeric(RiskAversion)) %>%
  filter(!is.na(RiskAversion))

cat("N for continuous spec:", nrow(regression_data_cont), "\n")

ordinal_results_cont <- list()
for (var in existing_likert) {
  if (!var %in% names(regression_data_cont)) next
  if (length(table(regression_data_cont[[var]])) < 3) next
  res <- run_ordinal_regression(var, regression_data_cont,
                                predictors = model_variables_cont)
  if (!is.null(res)) ordinal_results_cont[[var]] <- res
}

create_risk_robustness_plot <- function(ordinal_results, question_labels) {
  
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
  model_order <- names(model_categories)
  
  coef_data <- purrr::map_dfr(names(ordinal_results), function(m) {
    res <- ordinal_results[[m]]$results
    if (is.null(res) || !nrow(res)) return(NULL)
    res <- res[res$Variable == "RiskAversion", , drop = FALSE]
    if (!nrow(res)) return(NULL)
    res$Model_Name <- m
    res$Question   <- question_labels[[m]]
    res$Category   <- unname(model_categories[m])
    res
  })
  
  coef_data <- coef_data %>%
    filter(!is.na(Category)) %>%
    mutate(
      Question_Wrapped = factor(
        stringr::str_wrap(Question, 20),
        levels = stringr::str_wrap(
          sapply(model_order, function(x) question_labels[[x]]), 20)
      ),
      Category = factor(Category, levels = c("Government\nMandates",
                                             "Market\nMechanisms",
                                             "Public\nSubsidy")),
      star_y    = ifelse(Coefficient >= 0,
                         Coefficient + 1.96 * SE,
                         Coefficient - 1.96 * SE),
      star_vjust = ifelse(Coefficient >= 0, -0.4, 1.2)
    )
  
  ggplot(coef_data, aes(x = Question_Wrapped, y = Coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .7) +
    geom_errorbar(aes(ymin = Coefficient - 1.96 * SE,
                      ymax = Coefficient + 1.96 * SE),
                  width = 0.15, colour = "grey25", linewidth = 0.6) +
    geom_point(shape = 21, size = 4, fill = "grey30", colour = "grey20") +
    geom_text(aes(y = star_y, label = sig, vjust = star_vjust),
              size = 6, colour = "black") +
    facet_grid(. ~ Category, scales = "free_x", space = "free_x") +
    scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
    labs(
      x = "",
      y = "Log-odds coefficient per 1-point increase\nin risk tolerance (0-10)",
      caption = paste(
        "*** p<0.001, ** p<0.01, * p<0.05, . p<0.1")
    ) +
    nature_theme() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 14),
      axis.title.y     = element_text(size = 14),
      axis.text.y      = element_text(size = 14),
      strip.text       = element_text(face = "bold", size = 14),
      strip.background = element_rect(fill = "grey90", colour = "white"),
      legend.position  = "none",
      plot.caption     = element_text(hjust = 0, size = 12, color = "gray30",
                                      margin = margin(t = 10))
    )
}

risk_robustness_plot <- create_risk_robustness_plot(ordinal_results_cont,
                                                    question_labels)
print(risk_robustness_plot)
ggsave("Figures/Supp/fig_ra_ord.png",
       plot = risk_robustness_plot, width = 12, height = 7, dpi = 300)
