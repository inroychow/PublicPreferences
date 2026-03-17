# Load libraries
library(tidyverse)
library(janitor)
library(fixest)
library(lmtest)
library(sandwich)
library(stargazer)

# Source scripts
source("src/Fairness_main_analyses/cleaning.R")
source("src/Fairness_main_analyses/useful_functions.R")

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
    CorrectFloodQuestion = case_when(
      HomeownInsurCoversFloods == "No" ~ 1,
      HomeownInsurCoversFloods == "Yes" ~ 0,
      TRUE ~ NA_real_
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
  filter(Party %in% c("Republican", "Democrat", "Independent")) %>%
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
# 6. ADD NO COMPENSATION REASON
# ============================================================================

hyp <- hyp %>%
  left_join(
    sample %>% dplyr::select(ResponseId, NoGovCompensate_Reason),
    by = c("ResponseID" = "ResponseId")
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
    Party = factor(Party, levels = c("Independent", "Democrat", "Republican")),
    RiskAversion_bin = factor(RiskAversion_bin, levels = c("Risk neutral", "Risk tolerant", "Risk averse")),
    Race2 = factor(Race2, levels = c("Other", "Black or African American", "White")),
    AgeGroup = factor(AgeGroup, levels = c("25 – 44", "18 – 24", "45 – 64", "65 or older"))
  )

# ============================================================================
# 8. DEFINE COVARIATE LIST FOR MODELS
# ============================================================================

demo_covars <- c("DisasterExperience", "Gender", "AgeGroup", "AnnualIncome",
                 "Party", "Race2", "RiskAversion_bin", "LikelihoodFEMAApprove",
                 "CorrectFloodQuestion", "GovTrustBin", "gap_quartile")

# saveRDS(hyp,"src\\analysis\\Fairness_main_analyses\\data\\data.rds")
