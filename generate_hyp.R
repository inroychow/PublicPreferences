# Generate hypotheticals dataset, post-cleaning
# ----------------------

# Load libraries
library(tidyverse)
library(janitor)
library(fixest)
library(lmtest)
library(sandwich)
library(stargazer)
# Source scripts
source("cleaning.R")
source("useful_functions.R")

#-------- Indu 4.24 Hypothetical re-shaping 

responses_raw <- read_csv("C:\\Users\\indumati\\Box\\Disaster aid survey\\disaster_survey_github\\DisasterAssistance_FINAL.csv", show_col_types = FALSE)
key_raw <- read_csv("scenario_key.csv", show_col_types = FALSE) %>%
  mutate(
    scenario_id = row_number(),
    # keep the resp column name (Q1A/Q1B/etc) as a label
    question_label = question_col
  )
key <- key_raw %>%
  pivot_longer(
    cols = c(question_col, gov_comp_col, gov_amt_col),
    names_to = "question_type",
    values_to = "survey_col"
  ) %>%
  mutate(
    question_type = recode(question_type,
                           question_col = "resp",
                           gov_comp_col = "gov_comp",
                           gov_amt_col  = "gov_amt")
  )

#  Get all survey_cols that each respondent actually saw ───────────────────

# First, get just those that exist in responses
relevant_cols <- intersect(key$survey_col, names(responses_raw))

# Reshape just those columns
responses <- responses_raw %>%
  rename(ResponseID = ResponseId) %>%
  dplyr::select(ResponseID, all_of(relevant_cols)) %>%
  mutate(across(-ResponseID, as.character)) %>%
  pivot_longer(
    -ResponseID,
    names_to  = "survey_col",
    values_to = "value"
  )

# Join response values with key info (tagging each with scenario and question type)
merged <- responses %>%
  left_join(key, by = "survey_col") %>%
  filter(!is.na(scenario_id))   # remove unmatched columns

#  Keep only the (ResponseID × scenario_id) pairs that have at least one real answer
#    (i.e., only the two hypotheticals that respondent actually saw)
filtered <- merged %>%
  group_by(ResponseID, scenario_id) %>%
  filter(any(!is.na(value))) %>%
  ungroup()

# Pivot wider: one row per (ResponseID × scenario shown)
hyp <- filtered %>%
  pivot_wider(
    id_cols     = c(ResponseID, scenario_id, question_label,
                    hazard, second_home, prior_info, adaptive_measures),
    names_from  = question_type,
    values_from = value
  ) %>%
  mutate(across(c(second_home, prior_info, adaptive_measures), as.integer)) %>%
  relocate(ResponseID, scenario_id, question_label)

hyp <- hyp %>%
  mutate(
    base_noadapt = as.integer(
      hazard == "flood" &
        second_home == 0 &
        prior_info == 0 &
        adaptive_measures == 0 &
        question_label == "Hypothetical...Q1B"
    )
  )
rm(filtered)
rm(merged)
rm(relevant_cols)
rm(responses_raw)
rm(key_raw)


# ---- inputs -----------------------------------------------------------------
raw <- read_csv("C:\\Users\\indumati\\Box\\Disaster aid survey\\disaster_survey_github\\DisasterAssistance_FINAL.csv",
                show_col_types = FALSE)

key_raw <- read_csv("scenario_key.csv", show_col_types = FALSE) %>%
  mutate(scenario_id = row_number())

qcols <- c("Q471","Q472","Q473","Q474","Q475","Q476")
gov_comp_cols <- intersect(key_raw$gov_comp_col, names(raw))

# ---- long forms -------------------------------------------------------------
reason_long <- raw %>%
  dplyr::select(ResponseId, all_of(qcols)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-ResponseId, names_to = "reason_col", values_to = "reason") %>%
  mutate(reason = str_squish(reason)) %>%
  filter(!is.na(reason), reason != "")

no_long <- raw %>%
  dplyr::select(ResponseId, all_of(gov_comp_cols)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-ResponseId, names_to = "gov_comp_col", values_to = "gc") %>%
  filter(gc == "No")

# ---- crosswalk, from unambiguous respondents only ---------------------------
unambiguous <- no_long %>% count(ResponseId) %>% filter(n == 1) %>% pull(ResponseId)

crosswalk <- reason_long %>%
  filter(ResponseId %in% unambiguous) %>%
  inner_join(no_long, by = "ResponseId") %>%
  count(reason_col, gov_comp_col, sort = TRUE)

print(crosswalk, n = 40)

# ---- collapse to a one-to-one map, with a purity check ----------------------
cw <- crosswalk %>%
  group_by(reason_col) %>%
  mutate(share = n / sum(n)) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

cw %>% dplyr::select(reason_col, gov_comp_col, n, share)   # share should be ~1.00 for all six

cw <- cw %>%
  dplyr::select(reason_col, gov_comp_col) %>%
  left_join(key_raw %>% dplyr::select(scenario_id, gov_comp_col), by = "gov_comp_col")

stopifnot(!any(is.na(cw$scenario_id)), n_distinct(cw$reason_col) == nrow(cw))

# ---- respondent × scenario reason table -------------------------------------
reason_by_scenario <- reason_long %>%
  left_join(cw, by = "reason_col") %>%
  transmute(ResponseID = as.character(ResponseId),
            scenario_id,
            NoGovCompensate_Reason = reason)

nrow(reason_by_scenario)   # expect ~712

hyp <- hyp %>%
  dplyr::select(-any_of("NoGovCompensate_Reason")) %>%
  mutate(ResponseID = as.character(ResponseID)) %>%
  left_join(reason_by_scenario, by = c("ResponseID", "scenario_id")) %>%
  mutate(ResponseID = as.factor(ResponseID))








# ============================================================================
# 1. FORMAT HYP DATA
# ============================================================================

hyp <- hyp %>%
  mutate(
    gov_amt = if_else(gov_comp == "No", 0, as.numeric(gov_amt)),
    gov_binary = if_else(gov_comp == "Yes", 1, 0),
    ResponseID = as.factor(ResponseID)
  )

# ============================================================================
# 2. PREPARE SAMPLE DATA
# ============================================================================

sample <- sample %>%
  mutate(
    ResponseID = ResponseId,
    
    # Race grouping
    Race2 = case_when(
      Race == "White" ~ "White",
      Race == "Black or African American" ~ "Black or African American",
      TRUE ~ "Other"
    ),
    
    # Risk aversion categories
    RiskAversion_bin = case_when(
      RiskAversion <= 3 ~ "Risk averse",
      RiskAversion > 3 & RiskAversion <= 6 ~ "Risk neutral",
      RiskAversion >= 7 ~ "Risk tolerant"
    ),
    
    # Flood knowledge check
    CorrectFloodQuestion = factor(
      case_when(
        HomeownInsurCoversFloods == "No" ~ "Correct",
        HomeownInsurCoversFloods == "Yes" ~ "Incorrect",
        HomeownInsurCoversFloods == "I am not sure" ~ "Unsure",
        is.na(HomeownInsurCoversFloods) ~ "No answer",
        TRUE ~ NA_character_
      ),
      levels = c("Correct", "Incorrect", "Unsure", "No answer")
    ),
    
    # Government trust binary
    GovTrustBin = case_when(
      GovTrust %in% c("Never", "Only some of the time") ~ "Low government trust",
      GovTrust %in% c("Most of the time", "Always") ~ "High government trust",
      TRUE ~ NA_character_
    ),
    GovTrustBin = factor(GovTrustBin, levels = c("Low government trust", "High government trust")),
    
    # Income factor with proper ordering
    AnnualIncome = factor(AnnualIncome, levels = c(
      "Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999",
      "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
      "$200,000 to $249,999", "$250,000 or more"
    )),
    
    # FEMA approval likelihood
    LikelihoodFEMAApprove = case_when(
      LikelihoodFEMAApprove == "Certain (100%)" ~ "Certain",
      LikelihoodFEMAApprove == "Very likely (75% – 99%)" ~ "Very likely",
      LikelihoodFEMAApprove == "Likely (50% – 74%)" ~ "Likely",
      LikelihoodFEMAApprove == "Unlikely (25% – 49%)" ~ "Unlikely",
      LikelihoodFEMAApprove == "Very unlikely (0% – 24%)" ~ "Very unlikely",
      TRUE ~ LikelihoodFEMAApprove
    ),
    LikelihoodFEMAApprove = factor(LikelihoodFEMAApprove, levels = c(
      "I am not sure", "Very unlikely", "Unlikely", "Likely", "Very likely", "Certain"
    ))
  )

# ============================================================================
# 3. MERGE DEMOGRAPHICS AND TRUST VARIABLES
# ============================================================================

demo_vars <- c("AgeGroup", "AnnualIncome", "ChildrenUnder18", "HighestEduc", "Race", "Race2",
               "RiskAversion", "RiskAversion_bin", "CorrectFloodQuestion", "GovTrust",
               "HomeownInsurCoversFloods", "LikelihoodFEMAApprove", "DisasterExperience",
               "Party", "State", "Gender", "GovTrustBin")

trust_vars <- c("GovRole", "GovRoleA", "GovRoleB", 
                "PostDisasterGovAllocate", "PostDisasterGovUse", 
                "GovInsur", "GovInsurA", "GovInsurB", "GovInsurC", "GovInsurD")

hyp <- hyp %>%
  left_join(sample %>% dplyr::select(ResponseID, all_of(demo_vars), all_of(trust_vars)), 
            by = "ResponseID")

# Make GovTrust an ordered factor
hyp$GovTrust <- factor(hyp$GovTrust,
                       levels = c("Never", "Only some of the time", "Most of the time", "Always"),
                       ordered = TRUE)

# ============================================================================
# 4. FILTER AND CREATE SCENARIO VARIABLES
# ============================================================================

hyp <- hyp %>%
  mutate(
    percent_aid = 100 * (gov_amt / 250),
    scenario6 = case_when(
      second_home == 0 & prior_info == 0 & adaptive_measures == 0 ~ "Base Case",
      second_home == 0 & prior_info == 1 & adaptive_measures == 0 ~ "Primary Residence × Prior Info",
      second_home == 0 & prior_info == 1 & adaptive_measures == 1 ~ "Primary Residence × Prior Info × Adapt",
      second_home == 1 & prior_info == 0 & adaptive_measures == 0 ~ "Second Home",
      second_home == 1 & prior_info == 1 & adaptive_measures == 0 ~ "Second Home × Prior Info",
      second_home == 1 & prior_info == 1 & adaptive_measures == 1 ~ "Second Home × Prior Info × Adapt",
      TRUE ~ NA_character_
    ),
    scenario6 = factor(scenario6, levels = c(
      "Base Case",
      "Primary Residence × Prior Info",
      "Primary Residence × Prior Info × Adapt",
      "Second Home",
      "Second Home × Prior Info",
      "Second Home × Prior Info × Adapt"
    ))
  )

# ============================================================================
# 5. ADD TAX PROGRESSIVITY VARIABLES
# ============================================================================

hyp <- hyp %>%
  left_join(sample %>% dplyr::select(ResponseID, FairTaxA, FairTaxB),
            by = "ResponseID") %>% 
  dplyr::rename(top1_tax = FairTaxA,
                bottom50_tax = FairTaxB) %>% 
  dplyr::mutate(
    gap_tax_pct = top1_tax - bottom50_tax,
    gap_quartile = factor(ntile(gap_tax_pct, 4),
                          labels = c("Smallest", "Q2", "Q3", "Largest")),
    gap_quartile = case_when(
      gap_quartile %in% c("Q2", "Q3") ~ "Mid-range tax progressive",
      gap_quartile == "Smallest" ~ "Least tax progressive",
      gap_quartile == "Largest" ~ "Most tax progressive",
      TRUE ~ as.character(gap_quartile)
    )
  )



# ============================================================================
# 7. FINAL TRANSFORMATIONS AND FACTOR RELEVELING
# ============================================================================

hyp <- hyp %>% 
  mutate(
    # Clean AgeGroup labels
    AgeGroup = AgeGroup %>%
      str_replace_all("-", "–") %>%
      str_trim(),
    
    # Income grouping for analysis
    AnnualIncome_grouped = case_when(
      AnnualIncome %in% c("Less than $25,000", "$25,000 to $49,999", 
                          "$50,000 to $74,999", "$75,000 to $99,999") ~ "Annual Income < $100,000",
      AnnualIncome %in% c("$100,000 to $149,999", "$150,000 to $199,999", 
                          "$200,000 to $249,999") ~ "Annual Income $100,000 to $249,999",
      AnnualIncome == "$250,000 or more" ~ "Annual Income > $250,000",
      TRUE ~ NA_character_
    ),
    
    # Set all factor levels with reference categories
    gap_quartile = fct_relevel(factor(gap_quartile), "Mid-range tax progressive"),
    GovTrustBin = fct_relevel(GovTrustBin, "High government trust"),
    AnnualIncome_grouped = fct_relevel(factor(AnnualIncome_grouped), "Annual Income $100,000 to $249,999"),
    RiskAversion_bin = factor(RiskAversion_bin, levels = c("Risk neutral", "Risk tolerant", "Risk averse")),
    Race2 = factor(Race2, levels = c("Other", "Black or African American", "White")),
    AgeGroup = factor(AgeGroup, levels = c("25 – 44", "18 – 24", "45 – 64", "65 or older"))
  )

# ============================================================================
  # 7b. DEFINE ANALYSIS SAMPLE
  # ============================================================================
# 39 respondents left the demographics/attitudes block blank (Q439, GovTrust,
# tax sliders). 6 never reached the hypotheticals; the remaining 33 are dropped
# here so every specification runs on the same people. After this,
# Party == "Other" is only the 42 who explicitly selected "Other"

incomplete_ids <- sample %>%
  filter(Q439 == "" | is.na(Q439)) %>%
  pull(ResponseId)

hyp <- hyp %>%
  filter(!as.character(ResponseID) %in% incomplete_ids)

# checks
hyp %>% distinct(ResponseID) %>% nrow()                 # expect 1961
hyp %>% distinct(ResponseID, Party) %>% count(Party)    # Other = 42

# ============================================================================
# 8. DEFINE COVARIATE LIST FOR MODELS
# ============================================================================

demo_covars <- c("DisasterExperience", "Gender", "AgeGroup", "AnnualIncome",
                 "Party", "Race2", "RiskAversion_bin", "LikelihoodFEMAApprove",
                 "CorrectFloodQuestion", "GovTrustBin", "gap_quartile")
hyp = hyp |> 
  mutate(
    CorrectFloodQuestion = fct_na_value_to_level(
      droplevels(fct_drop(CorrectFloodQuestion, only = "No answer")),
      level = "No answer") %>%
      fct_relevel("Correct", "Incorrect", "Unsure", "No answer")
  )

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
# saveRDS(hyp,"data\\data_updated.rds")

# ------------------------
# R & R add in belief errors
# --------------------------------------------------------------------------
# (respondent-level, pulled from `sample`, joined onto the vignette panel)
# --------------------------------------------------------------------------

sample <- sample %>%
  mutate(
    # "not sure" should not masquerade as "exactly right"
    Beliefs_FEMA_error     = ifelse(Beliefs_FEMA_Prob_NotSure == 1 |
                                      Beliefs_FEMA_Amount_NotSure == 1,
                                    NA_real_, Beliefs_FEMA_error),
    Beliefs_FEMA_error_abs = abs(Beliefs_FEMA_error),
    Beliefs_SBA_error      = ifelse(Beliefs_SBA_Prob_NotSure == 1,
                                    NA_real_, Beliefs_SBA_error),
    Beliefs_SBA_error_abs  = abs(Beliefs_SBA_error),
    # these three were built off the level, not the error
    Beliefs_SBA_error_positive = as.integer(Beliefs_SBA_error > 0),
    Beliefs_SBA_error_negative = as.integer(Beliefs_SBA_error < 0),
    Beliefs_SBA_error_accurate = as.integer(Beliefs_SBA_error == 0)
  )

# ==============================================================
# 1. Respondent-level covariates for the "baseline knowledge" analysis
# ==============================================================
resp_vars <- sample %>%
  transmute(
    ResponseID = as.character(ResponseId),
    
    ## --- treatment arm (NOT currently in hyp) ---
    Treated,
    
    ## --- (a) FEMA beliefs: level, uncertainty, accuracy -------------
    FEMAAmount_Dollar,                                   # raw bins, for the distribution figure
    Beliefs_FEMA_Prob, Beliefs_FEMA_Amount, Beliefs_FEMA,
    Beliefs_FEMA_Prob_NotSure, Beliefs_FEMA_Amount_NotSure,
    Beliefs_FEMA_NotSure_Any = as.integer(Beliefs_FEMA_Prob_NotSure == 1 |
                                            Beliefs_FEMA_Amount_NotSure == 1),
    Beliefs_FEMA_error, Beliefs_FEMA_error_abs,
    Beliefs_FEMA_error_positive, Beliefs_FEMA_error_negative,
    Beliefs_FEMA_error_accurate,
    
    # share-of-loss metric comparable to percent_aid (conditional on approval,
    # i.e. NOT multiplied by the approval probability -- see note below)
    Beliefs_FEMA_share = Beliefs_FEMA_Amount / 75000 * 100,
    Beliefs_FEMA_EV_share = Beliefs_FEMA_Prob * Beliefs_FEMA_Amount / 75000 * 100,
    
    Beliefs_FEMA_bias = factor(case_when(
      Beliefs_FEMA_NotSure_Any == 1 ~ "Not sure",
      Beliefs_FEMA_error  > 0       ~ "Overestimates",
      Beliefs_FEMA_error  < 0       ~ "Underestimates",
      Beliefs_FEMA_error == 0       ~ "Accurate"
    ), levels = c("Accurate", "Underestimates", "Overestimates", "Not sure")),
    
    ## --- (b) SBA beliefs -------------------------------------------
    Beliefs_SBA_Prob, Beliefs_SBA_Prob_NotSure, SBA_Amount, Beliefs_SBA,
    SBA_Prob_Benchmark_group,
    Beliefs_SBA_error, Beliefs_SBA_error_abs, Beliefs_SBA_error_accurate,
    
    ## --- (c) timing beliefs ----------------------------------------
    Beliefs_FEMA_Timing, Beliefs_FEMA_Timing_Over6months,
    Beliefs_SBA_Timing,  Beliefs_SBA_Timing_Over6months,
    
    ## --- (d) prior-experience block (heterogeneity within ExpDisaster==1) ---
    DisasterYear, DisasterType, HomeDamage_Dollar, PercRepaired,
    AppliedFEMA, AppliedFEMA_Simple, ReasonNoFEMA,
    YesFEMA, FEMALowerHigher, WhenReceiveFEMA,
    AppliedSBA, AppliedSBA_Simple, ReasonNoSBA,
    YesSBA, SBALowerHigher, WhenReceivedSBA,
    
    ## --- (e) self-reported + objective knowledge -------------------
    InformedDisasterAssistance,
    HasHomeownInsur, HasSepFloodPolicy, HasFloodPolicyHome, SFHA,
    InsurConfidence, CouldGet2000_Ctrl, CreditScore,
    
    Know_HOI_NoFlood   = as.integer(HomeownInsurCoversFloods == "No"),
    Know_FEMA_Amt_Bin  = as.integer(FEMAAmount_Dollar == "$5,000 to $9,999"),
    Know_FEMA_Prob_Bin = as.integer(LikelihoodFEMAApprove == "Unlikely (25% – 49%)"),
    Know_FEMA_Timing   = as.integer(WhenThinkReceiveFEMA %in%
                                      c("Between 2 weeks and 2 months",
                                        "Between 2 months and 6 months")),
    
    ## --- (f) tax/fiscal items in raw form --------------------------
    FairTaxA, FairTaxB
  ) %>%
  mutate(
    # composite objective-knowledge score (0-1), NA-tolerant
    Knowledge_Index = rowMeans(dplyr::select(., Know_HOI_NoFlood, Know_FEMA_Amt_Bin,
                                             Know_FEMA_Prob_Bin, Know_FEMA_Timing),
                               na.rm = TRUE),
    Knowledge_Tercile = factor(ntile(Knowledge_Index, 3),
                               labels = c("Low", "Mid", "High")),
    # accuracy terciles computed at RESPONDENT level, not vignette level
    Beliefs_FEMA_acc_tercile = factor(ntile(-Beliefs_FEMA_error_abs, 3),
                                      labels = c("Least accurate", "Mid", "Most accurate")),
    
    # compact experience profile for the within-DisasterExperience==1 discussion
    ExperienceProfile = factor(case_when(
      is.na(AppliedFEMA)                                        ~ "No disaster experience",
      AppliedFEMA == "I was not aware of this program, so I did not apply" ~ "Unaware of program",
      AppliedFEMA == "I was aware of this program, but I did not apply"    ~ "Aware, chose not to apply",
      AppliedFEMA == "The disaster was not eligible for disaster assistance" ~ "Ineligible disaster",
      AppliedFEMA == "Yes, I applied and I was approved and I received funds" ~ "Applied, received funds",
      AppliedFEMA_Simple == "Yes"                               ~ "Applied, no funds received",
      TRUE ~ NA_character_
    ))
  )

# ==============================================================
# 2. Merge into hyp without creating .x/.y duplicates
# ==============================================================
# 2. merge
new_cols <- setdiff(names(resp_vars), setdiff(names(hyp), "ResponseID"))

hyp <- hyp %>%
  dplyr::select(-any_of("NoGovCompensate_Reason.x")) %>%
  dplyr::rename(any_of(c(NoGovCompensate_Reason = "NoGovCompensate_Reason.y"))) %>%
  mutate(ResponseID = as.character(ResponseID)) %>%
  left_join(resp_vars %>% dplyr::select(all_of(new_cols)), by = "ResponseID")

# 3. derived factors — ONCE, and safe to re-run
hyp <- hyp %>%
  mutate(
    info_arm = factor(
      case_when(prior_info == 0                          ~ "none",
                prior_info == 1 & adaptive_measures == 0 ~ "info",
                prior_info == 1 & adaptive_measures == 1 ~ "info_adapt"),
      levels = c("none", "info", "info_adapt")),
    second_home = if (is.factor(second_home)) second_home else
      factor(second_home, levels = c(0, 1),
             labels = c("primary", "second")),
    ResponseID = as.factor(ResponseID)
  )

# 4. sanity checks last
hyp %>% count(ResponseID) %>% count(n)
levels(hyp$CorrectFloodQuestion)
table(hyp$CorrectFloodQuestion, useNA = "ifany")

