library(tidyverse)
library(ggtext)     # richer labels
library(ggridges)   # ridge plots
library(scales)     # pretty axes
library(patchwork)  # combine plots
library(sf)         # the map
library(tidyr)
library(stringr)
library(dplyr)
theme_set(theme_minimal())

DisasterAssistance_FINAL <- read.csv("DisasterAssistance_FINAL.csv", stringsAsFactors = F)


# [Note] Treated = 1   -> Individuals who received information treatment
# [Note] Please ignore "_NPS_GROUP" variables 



sample = DisasterAssistance_FINAL %>%
  mutate(

    region = ifelse(Q491=="CT" |
                    Q491=="ME" |
                    Q491=="MA" |
                    Q491=="NH" |
                    Q491=="NJ" |
                    Q491=="NY" |
                    Q491=="PA" |
                    Q491=="RI" |
                    Q491=="VT",         "Northeast",
                    
             ifelse(Q491=="TX" |
                    Q491=="OK" |
                    Q491=="NM" |
                    Q491=="AZ",         "Southwest",
                           
             ifelse(Q491=="CO" |
                    Q491=="ID" |
                    Q491=="NV" |
                    Q491=="UT" |
                    Q491=="WY" |
                    Q491=="MT" |
                    Q491=="CA" |
                    Q491=="OR" |
                    Q491=="WA" |
                    Q491=="AK" |
                    Q491=="HI",           "West",
                                  
             ifelse(Q491=="DC" |
                    Q491=="DE" |
                    Q491=="MD" |
                    Q491=="WV" |
                    Q491=="VA" |
                    Q491=="KY" |
                    Q491=="TN" |
                    Q491=="NC" |
                    Q491=="SC" |
                    Q491=="GA" |
                    Q491=="FL" |
                    Q491=="AL" |
                    Q491=="MS" |
                    Q491=="AR" |
                    Q491=="LA",        "Southeast",
                                         
             ifelse(Q491=="OH" |
                    Q491=="IN" |
                    Q491=="IL" |
                    Q491=="MI" |
                    Q491=="WI" |
                    Q491=="MN" |
                    Q491=="ND" |
                    Q491=="SD" |
                    Q491=="IA" |
                    Q491=="KS" |
                    Q491=="NE" |
                    Q491=="MO",         "Midwest", NA))))),
    
    DisasterExperience = ifelse(Q492=="Yes", 1, 0),                             # Dummy for individuals with disaster experience in the past 10 years
    
    MAX_WTP            = coalesce(Q113, Q114, Q115, Q116, Q117, 
                                  Q125, Q126, Q127, Q128, Q129),                # maximum amount ($) you would be willing to pay for flood insurance per month



    
    # Risk Aversion ----------------------------------------------------------
    RiskAversion       = 10 - Controls...Q1,                                    # [Controls...Q1] How would you rate your willingness to take financial risks? 0 = Not at all, 10 = Absolutely




    # --------------------------------------------------------------------------
    # Belief Variables: FEMA Grants
    # --------------------------------------------------------------------------
    
    Beliefs_FEMA_Prob    = ifelse(Knowledge...Q5A=="Very unlikely (0% – 24%)", 0.12,      #[Knowledge...Q5A] Suppose you apply for a grant from FEMA. How likely do you think you are to be approved?
                           ifelse(Knowledge...Q5A=="Unlikely (25% – 49%)",     0.37,
                           ifelse(Knowledge...Q5A=="Likely (50% – 74%)",       0.62,
                           ifelse(Knowledge...Q5A=="Very likely (75% – 99%)",  0.87,
                           ifelse(Knowledge...Q5A=="Certain (100%)",           1, 0))))),
    
    Beliefs_FEMA_Amount  = ifelse(Knowledge...Q5B=="Less than $100",      49.5,           #[Knowledge...Q5B] If approved, what would be the dollar amount of the grants that you would expect to receive from FEMA for home repair or replacement? Recall that you sustained $75,000 in uninsured damage.
                           ifelse(Knowledge...Q5B=="$100 to $999",        549.5,
                           ifelse(Knowledge...Q5B=="$1,000 to $4,999",    2999.5,
                           ifelse(Knowledge...Q5B=="$5,000 to $9,999",    7499.5,
                           ifelse(Knowledge...Q5B=="$10,000 to $29,999",  19999.5, 
                           ifelse(Knowledge...Q5B=="$30,000 to $49,999",  39999.5,
                           ifelse(Knowledge...Q5B=="$50,000 to $74,999",  62499.5,
                           ifelse(Knowledge...Q5B=="$75,000",             75000,
                           ifelse(Knowledge...Q5B=="More than $75,000",   75000, 0))))))))),  
    
    Beliefs_FEMA_Prob_NotSure   = ifelse(Knowledge...Q5A=="I am not sure", 1, 0),
    Beliefs_FEMA_Amount_NotSure = ifelse(Knowledge...Q5B=="I am not sure", 1, 0),
    Beliefs_FEMA                = Beliefs_FEMA_Prob*Beliefs_FEMA_Amount/75000,
    
    # [Benchmark] FEMA Approval Prob   = "Unlikely (25% – 49%)"          OR Beliefs_FEMA_Prob = 0.37
    # [Benchmark] FEMA Approved Amount = "$5,000 to $9,999" (~$6,666)    OR Beliefs_FEMA_Amount = 7499.5
    Beliefs_FEMA_error            = ifelse(Beliefs_FEMA_Prob_NotSure==1 | Beliefs_FEMA_Amount_NotSure==1, 0, 
                                         (Beliefs_FEMA_Prob*Beliefs_FEMA_Amount - 0.37*7499.5)/75000),
    Beliefs_FEMA_error_abs        = abs(Beliefs_FEMA_error),
    
    Beliefs_FEMA_error_positive   = ifelse(Beliefs_FEMA_error > 0, 1, 0),
    Beliefs_FEMA_error_negative   = ifelse(Beliefs_FEMA_error < 0, 1, 0),
    Beliefs_FEMA_error_accurate   = ifelse(Beliefs_FEMA_error == 0 & 
                                           Beliefs_FEMA_Prob_NotSure==0 & 
                                           Beliefs_FEMA_Amount_NotSure==0, 1, 0),
    
    
    # --------------------------------------------------------------------------
    # Belief Variables: SBA Loans 
    # --------------------------------------------------------------------------
    
    Beliefs_SBA_Prob       = ifelse(Knowledge...Q6B=="Very unlikely (0% – 25%)", 0.12,        #[Knowledge...Q6B] How likely do you think you are to be approved for an SBA loan?
                             ifelse(Knowledge...Q6B=="Unlikely (25% – 49%)",     0.37,
                             ifelse(Knowledge...Q6B=="Likely (50% – 74%)",       0.62,
                             ifelse(Knowledge...Q6B=="Very likely (75% – 99%)",  0.87,
                             ifelse(Knowledge...Q6B=="Certain (100%)",           1, 0))))),
    
    Beliefs_SBA_Prob_NotSure = ifelse(Knowledge...Q6B=="I am not sure", 1, 0),
    SBA_Amount               = Knowledge...Q6A_4*1000,                                        #[Knowledge...Q6A_4] How much (in thousands of dollars) would you apply to borrow from SBA?
    
    # [Benchmark] For Treated = 1, SBA Approval Prob = SBA; For Treated = 0, SBA Approval Prob by Income
    SBA_Prob_Benchmark_num = ifelse(Treated==1 & !is.na(SBA), SBA/100, 
                             ifelse(Demographics...Q1=="Less than $25,000",  0.03,
                             ifelse(Demographics...Q1=="$25,000 to $49,999", 0.2,
                             ifelse(Demographics...Q1=="$50,000 to $74,999", 0.32,
                             ifelse(Demographics...Q1=="$75,000 to $99,999", 0.42,
                             ifelse(Demographics...Q1=="$100,000 to $149,999", 0.53,
                             ifelse(Demographics...Q1=="$150,000 to $199,999", 0.63,
                             ifelse(Demographics...Q1=="$200,000 to $249,999", 0.68,
                             ifelse(Demographics...Q1=="$250,000 or more", 0.46, 0))))))))),
    
    SBA_Prob_Benchmark_group = ifelse(SBA_Prob_Benchmark_num < 0.25, 0.12,
                               ifelse(SBA_Prob_Benchmark_num < 0.50, 0.37,
                               ifelse(SBA_Prob_Benchmark_num < 0.75, 0.62,
                               ifelse(SBA_Prob_Benchmark_num < 1,    0.87, 
                               ifelse(SBA_Prob_Benchmark_num == 1,   1, 0))))),
    
    Beliefs_SBA           = Beliefs_SBA_Prob*SBA_Amount/75000,
    Beliefs_SBA_error     = ifelse(Beliefs_SBA_Prob_NotSure==1, 0, (Beliefs_SBA_Prob - SBA_Prob_Benchmark_group)*SBA_Amount/75000),
    Beliefs_SBA_error_abs = abs(Beliefs_SBA_error),

    Beliefs_SBA_error_positive   = ifelse(Beliefs_SBA > 0, 1, 0),
    Beliefs_SBA_error_negative   = ifelse(Beliefs_SBA < 0, 1, 0),
    Beliefs_SBA_error_accurate   = ifelse(Beliefs_SBA == 0 & Beliefs_SBA_Prob_NotSure==0, 1, 0),

    # --------------------------------------------------------------------------
    # Belief Variables: Timing
    # --------------------------------------------------------------------------
    Beliefs_FEMA_Timing = ifelse(Knowledge...Q5C=="Less than 2 weeks",             0.5,
                          ifelse(Knowledge...Q5C=="Between 2 weeks and 2 months",  1.25,
                          ifelse(Knowledge...Q5C=="Between 2 months and 6 months", 4,
                          ifelse(Knowledge...Q5C=="Between 6 months and 1 year",   9,
                          ifelse(Knowledge...Q5C=="Between 1 year and 2 years",    18,
                          ifelse(Knowledge...Q5C=="More than 2 years",             30, NA)))))),
    
    
    Beliefs_FEMA_Timing_Over6months = ifelse(Knowledge...Q5C=="Between 6 months and 1 year" |
                                               Knowledge...Q5C=="Between 1 year and 2 years" |
                                               Knowledge...Q5C=="More than 2 years", 1, 0), 
    
    Beliefs_SBA_Timing = ifelse(Knowledge...Q6C=="Less than 2 weeks",             0.5,
                         ifelse(Knowledge...Q6C=="Between 2 weeks and 2 months",  1.25,
                         ifelse(Knowledge...Q6C=="Between 2 months and 6 months", 4,
                         ifelse(Knowledge...Q6C=="Between 6 months and 1 year",   9,
                         ifelse(Knowledge...Q6C=="Between 1 year and 2 years",    18,
                         ifelse(Knowledge...Q6C=="More than 2 years",             30, NA)))))),
    
    Beliefs_SBA_Timing_Over6months = ifelse(Knowledge...Q6C=="Between 6 months and 1 year" |
                                              Knowledge...Q6C=="Between 1 year and 2 years" |
                                              Knowledge...Q6C=="More than 2 years", 1, 0),


    # --------------------------------------------------------------------------
    # Demographics Variables
    # --------------------------------------------------------------------------
    Income = ifelse(Demographics...Q1=="Less than $25,000" |
                      Demographics...Q1=="$25,000 to $49,999" |
                      Demographics...Q1=="$50,000 to $74,999", "Income < $75K", 
             ifelse(Demographics...Q1=="$75,000 to $99,999" |
                    Demographics...Q1=="$100,000 to $149,999", "Income $75 to <$100K", "Income $100K+")),
    
    Age = ifelse(Screening2=="18 - 24", "Age: 18-44", 
          ifelse(Screening2=="25 - 44", "Age: 18-44", 
          ifelse(Screening2=="45 - 64", "Age: 45-64", "Age: 65+"))),
    
    NonWhite = ifelse(Demographics...Q2=="White", 0, 1),
    
    Party = ifelse(Q439=="Democrat" | Q439=="Republican" | Q439=="Independent", Q439, 
            ifelse(Q439_4_TEXT=="I’m more independent. ", "Independent", "Other")),
    
    
    # scenario questions -------------------------------------------------------
    ResponsibleForLoss = coalesce(Hypothetical...Q1D,
                                  Hypothetical...Q1C,
                                  Hypothetical...Q1A,
                                  Hypothetical...Q1B,
                                  Q458,
                                  Q455,
                                  Q452,
                                  Hypothetical...Q2C,
                                  Hypothetical...Q2D,
                                  Hypothetical...Q2B,
                                  Q464,
                                  Q467.1,
                                  Q461),                                        # From a scale of 0 to 10: How responsible was Sam/Alex for the loss?
    
    
    ShouldGovCompensate = coalesce(Hypothetical...Q2BB,
                                         Hypothetical...Q2CC,
                                         Hypothetical...Q2DD,
                                         Q462,
                                         Q465,
                                         Q468.1,
                                         Q305,
                                         Hypothetical...Q1BB,
                                         Hypothetical...Q1CC,
                                         Hypothetical...Q1DD,
                                         Q453,
                                         Q456,
                                         Q459),                                 # Do you think the government should use taxpayer funds to fully or partially compensate Sam/Alex for the damage, given that the $250,000 loss was uninsured?
    
    GovCompensate_Dollar = coalesce(Q446_1,
                                  Q447_1,
                                  Q448_1,
                                  Q463_1,
                                  Q466_1,
                                  Q469_1,
                                  Q441_1,
                                  Hypothetical...Q1BBB_1,
                                  Q443_1,
                                  Q444_1,
                                  Q454_1,
                                  Q457_1,
                                  Q460_1),                                      # How much (in thousands of dollars) should the government provide to compensate for Sam's/Alex's loss? Recall that Alex sustained $250,000 in uninsured damage.
  
    NoGovCompensate_Reason = coalesce(Q471,
                                         Q472,
                                         Q473,
                                         Q474,
                                         Q475,
                                         Q476)                                  # You answered "No" to the question above. What is your primary reason?
    )



# disaster aid beliefs questions -------------------------------------------

#convert probability ranges into equivalent numerical values
probability_table=data.frame(qual_levels=c("Certain (100%)",
                                           "Very likely (75% – 99%)",
                                           "Likely (50% – 74%)",
                                           "Unlikely (25% – 49%)",
                                           "Very unlikely (0% – 25%)",
                                           "I am not sure"),
                             quant_levels=c(100,87,62,37,12.5,50)) #note - assigning 50% to Not Sure to avoid dropping

sample <- sample %>%
  left_join(
    probability_table %>% rename(Beliefs_Prob_FEMA = quant_levels),
    join_by(Knowledge...Q5A == qual_levels)
  )

sample <- sample %>%
  left_join(
    probability_table %>% rename(Beliefs_Prob_SBA = quant_levels),
    join_by(Knowledge...Q6B == qual_levels)
  )

amounts_table=data.frame(qual_amount=c("Less than $100",
                                       "$100 to $999",
                                       "$1,000 to $4,999",
                                       "$5,000 to $9,999",
                                       "$10,000 to $29,999",
                                       "$30,000 to $49,999",
                                       "$50,000 to $74,999",
                                       "$75,000",
                                       "More than $75,000",
                                       "I am not sure"),
                         Beliefs_Quant_FEMA=c(50,549.5,2999.5,7499.5,19999.5,39999.5,62499.5,75000,75000,NA))

#Note "More than $75,000 is top-coded here as 75,000. Not sure (325 answers) is converted to NA
 
sample <- sample %>%
  left_join(
    amounts_table,
    join_by(Knowledge...Q5B == qual_amount)
  )

sample = sample %>%
  rename(Beliefs_Quant_SBA=Knowledge...Q6A_4)


#------------- Indu renaming -----------

# note: Annual income shows up twice, once in demographics and once in Q323_1. I didn't change Q323_1.

sample = sample %>% 
  rename(AgeGroup = Screening2,
         PrimaryFinancial = Screening3,
         RentOwn = Screening1,
         State = Q491,
         AnnualIncome = Demographics...Q1,
         CouldGet2000_Ctrl = Controls...Q3,
         Race = Demographics...Q2,
         Race_Text = Demographics...Q2_6_TEXT,
         Hisp = Demographics...Q3,
         Gender = Demographics...Q4,
         HighestEduc = Demographics...Q5,
         ChildrenUnder18 = Demographics...Q6,
         ExpDisaster = Q492,
         InformedDisasterAssistance_Group = Q493_NPS_GROUP,
         InformedDisasterAssistance = Q493,
         DisasterYear = Disaster...Q0,
         DisasterType = Disaster...Q1,
         DisasterType_Text = Disaster...Q1_15_TEXT,
         HomeDamage_Dollar = Q440,
         PercRepaired = Disaster...Q3,
         AppliedFEMA = Disaster...Q6,
         ReasonNoFEMA = Q467,
         ReasonNoFEMA_Text = Q467_4_TEXT,
         YesFEMA = Disaster...Q7A,
         FEMALowerHigher = Disaster...Q7B,
         WhenReceiveFEMA = Disaster...Q7C,
         AppliedSBA = Disaster...Q8,
         ReasonNoSBA = Q468,
         ReasonNoSBA_Text = Q468_4_TEXT,
         YesSBA = Disaster...Q9A,
         SBALowerHigher= Disaster...Q9B,
         WhenReceivedSBA = Disaster...Q9C,
         Zipcode = Q493.1,
         YearHomePurchased = Home...Q1,
         HomePurchasePrice = Home...Q10,
         LikelihoodDisaster_Group_Ctrl = Controls...Q2B_NPS_GROUP,
         LikelihoodDisaster_Ctrl = Controls...Q2B,
         WorstDamage_Ctrl = Controls...Q2C,
         HasMortgage = Home...Q3,
         HasHomeownInsur = Home...Q4,
         HomeownInsurCoversFloods = Home...Q5,
         HasFloodPolicyHome = Home...Q6A,
         HasSepFloodPolicy = Home...Q6,
         AmountPaidFloodInsur = Home...Q7_4,
         SFHA = Home...Q9,
         LikelihoodFEMAApprove = Knowledge...Q5A,
         FEMAAmount_Dollar = Knowledge...Q5B,
         WhenThinkReceiveFEMA = Knowledge...Q5C,
         LikelihoodSBAApprove = Knowledge...Q6B,
         WhenThinkReceiveSBA = Knowledge...Q6C,
         CreditScore = Q324_1,
         FinancialRisk_Group_Ctrl = Controls...Q1_NPS_GROUP,
         FinancialRisk_Ctrl = Controls...Q1,
         FutureBenefit_Group_Ctrl = Controls...Q4_NPS_GROUP,
         FutureBenefit_Ctrl = Controls...Q4,
         InsurConfidence_Group = Q279_NPS_GROUP,
         InsurConfidence = Q279,
         PostDisasterGovAllocate = Preference...Q1,
         PostDisasterGovAllocate_Text = Preference...Q1_4_TEXT,
         PostDisasterGovUse = Preference...Q2,
         PostDisasterGovUse_Text = Preference...Q2_3_TEXT,
         GovRole = Preference...Q3A,
         GovRoleA = Preference...Q3B,
         GovRoleB = Preference...Q3C,
         GovInsur = Q316,
         GovInsurA = Q314,
         GovInsurB = Q315,
         GovInsurC = Q316.1,
         GovInsurD = Q317,
         FairTaxA = GenPolicy...Q1_1,
         FairTaxB = GenPolicy...Q1_4,
         GovTrust = GenPolicy...Q3,
         #Party = Q439, #already renamed earlier
         Party_Text = Q439_4_TEXT)

#recode disaster aid experience variable
sample$AppliedFEMA_Simple=fct_collapse(sample$AppliedFEMA,
                                       Yes=c("Yes, I applied, but I was not approved","Yes, I applied, but I did not hear back from FEMA","Yes, I applied and I was approved, but I did not receive funds","Yes, I applied and I was approved and I received funds"),
                                       No=c("The disaster was not eligible for disaster assistance","I was not aware of this program, so I did not apply","I was aware of this program, but I did not apply"),
                                       other_level = NA)

sample$AppliedSBA_Simple=fct_collapse(sample$AppliedSBA,
                                       Yes=c("Yes, I applied, but I was not approved","Yes, I applied, but I did not hear back from SBA","Yes, I applied and I was approved, but I did not receive funds","Yes, I applied and I was approved and I received funds"),
                                       No=c("The disaster was not eligible for disaster assistance","I was not aware of this program, so I did not apply","I was aware of this program, but I did not apply"),
                                      other_level=NA)

         
#-------- Indu 4.24 Hypothetical re-shaping 

responses_raw <- read_csv("DisasterAssistance_FINAL.csv", show_col_types = FALSE)
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
