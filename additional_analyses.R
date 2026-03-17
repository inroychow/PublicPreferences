setwd("C:/Users/indumati/Box/Disaster aid survey/disaster_survey_github")
pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, splines, readr, ggridges, forcats, stringi, purr, viridis)

source("src/analysis/useful_functions.R")
source("src/indu_sandbox/generate_hyp.R")

hyp <- hyp %>%
  mutate(resp_numeric = as.numeric(resp))

# "This represents a loss in the 99th percentile of117
# disaster losses reported by those applying for FEMA assistance, meaning our survey scenario118
# examines preferences for assistance in the case of an exceptionally large loss that exceeds the119
# mean reported disaster damage ($7,849) by many orders (Federal Emergency Management120
# Agency, 2025b)."

ia <- data.table::fread("Filepath/IndividualsAndHouseholdsProgramValidRegistrationsV2.csv")

# keep inspected with any verified loss, and positive verified loss
ia <- ia %>%
  filter(!is.na(rpfvl) | !is.na(ppfvl)) %>%
  mutate(verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0)) %>%
  filter(verified_loss > 0)

# # FILTER TO JUST UNINSURED LOSSES...
# ia <- ia %>%
#   filter(floodInsurance == 0)
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
  )

# ------------------------------------------------------------------------------------------------------------------------------
#"Sub-setting the universe of IHP grant applications\footnote{We analyze the range of available data from 2002 to 2025. This is a total of 8,747,100 applicants.} to those with large losses (>\$40,000) more comparable to our hypothetical, reveals the paucity of Federal grant assistance relative to public preferences: the median compensation level is just 7\% compared to the 43\% compensation recommended by survey respondents. A large fraction of applicants (59\%) receive grants compensating less than 10\% of their disaster loss. "
# ------------------------------------------------------------------------------------------------------------------------------

ia <- data.table::fread("L:/Wetland Flood Mitigation/Paper_NFIP/IndividualsAndHouseholdsProgramValidRegistrationsV2.csv")

ia <- ia %>% filter(!is.na(rpfvl) | !is.na(ppfvl)) #repair and personal property
ia <- ia %>% mutate(verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0)) %>% 
  filter(verified_loss > 0)
ia[, censusYear := year(as.Date(appliedDate))] #Get loss date

#CPI adjust
getSymbols("CPIAUCSL", src = "FRED", warnings = FALSE, quiet = TRUE)
cpi_year <- data.frame(date = index(CPIAUCSL), cpi = as.numeric(CPIAUCSL)) |>
  mutate(censusYear = year(date)) |>
  group_by(censusYear) |>
  summarise(cpi = mean(cpi, na.rm = TRUE), .groups = "drop")
cpi_2024 <- cpi_year$cpi[cpi_year$censusYear == 2024]
ia <- ia |>
  left_join(cpi_year, by = "censusYear") |>
  mutate(
    # Inflation adjustment factor (adjust to 2024 dollars)
    cpi_factor = cpi_2024 / cpi,
    # Adjust the total amount to 2024 dollars
    verified_loss_2024 = verified_loss * cpi_factor
  )

ia <- ia %>%
  filter(is.na(haAmount) | haAmount <= 43600) #Exclude rows that were greater than the limit, 0.05%

# average verified damage per household
overall_avg <- mean(ia$verified_loss_2024, na.rm = TRUE)
overall_avg
max = max(ia$verified_loss, na.rm = TRUE)
max


n_apps_rows   <- nrow(ia)
n_apps_unique <- length(unique(ia$id))

n_apps_rows
n_apps_unique

ia_40k <- ia %>%
  filter(verified_loss_2024 > 40000)

ia_40k %>%
  mutate(comp_rate = haAmount / verified_loss_2024) %>%
  filter(!is.na(comp_rate), is.finite(comp_rate), comp_rate >= 0) %>%
  summarise(
    mean_comp_pct   = mean(comp_rate) * 100,
    median_comp_pct = median(comp_rate) * 100,
    n_used          = n()
  )


ia_40k_rates <- ia %>%
  filter(verified_loss_2024 > 40000) %>%
  mutate(comp_rate = haAmount / verified_loss_2024) %>%
  filter(!is.na(comp_rate), is.finite(comp_rate), comp_rate >= 0)

yy <- ia_40k_rates %>%
  summarise(yy_pct = mean(comp_rate < 0.10) * 100) %>%
  pull(yy_pct)

yy





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

m_het_tax <- feols(
  second_home * gap_quartile + prior_info + adaptive_measures + DisasterExperience +
    Gender + AgeGroup + Race2 + gap_quartile + AnnualIncome_grouped +RiskAversion_bin + GovTrustBin,
  data = hyp, vcov = ~ResponseID
)

m_het_income <- feols(
  percent_aid ~ second_home * AnnualIncome_grouped + controls,
  data = hyp, vcov = ~ResponseID
)

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
           "`second_home:PartyDemocrat` - `second_home:PartyRepublican` = 0")

# Tax progressivity: Most vs Least
hypotheses(m_het_tax,
           "`second_home:gap_quartileMost tax progressive` - `second_home:gap_quartileLeast tax progressive` = 0")

# Income: >$250k vs <$100k
hypotheses(m_het_income,
           "`second_home:AnnualIncome_groupedAnnual Income > $250,000` - `second_home:AnnualIncome_groupedAnnual Income < $100,000` = 0")

# Risk aversion: Averse vs Tolerant
hypotheses(m_het_risk,
           "`second_home:RiskAversion_binRisk Averse` - `second_home:RiskAversion_binRisk Tolerant` = 0")

# Government trust: High vs Low
hypotheses(m_het_govtrust,
           "`second_home:GovTrustBinHigh` - `second_home:GovTrustBinLow` = 0")