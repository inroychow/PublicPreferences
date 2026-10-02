pacman::p_load(fixest, tidyverse,      janitor, lmtest, sandwich, stargazer, broom, quantmod, scales, ggridges, viridis, patchwork, RColorBrewer, marginaleffects, splines, readr, ggridges, forcats, stringi, purr, purrr, srvyr, tidyr, tidycensus, stringr)

source("useful_functions.R")
# source("generate_hyp.R")

hyp <- readRDS("data/data_updated.rds")

################################################################################
#-------------------------------------------------------------------------------
#            TABLES: BALANCE TABLE ACROSS POPULATION, AND SCENARIOS            |           
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------


source("cleaning.R")
head(sample)


sample_keep <- sample %>%
  transmute(
    ResponseID        = ResponseId,                     # note the case difference
    # field timing + effort
    StartDate         = as.POSIXct(StartDate,    tz = "UTC"),
    RecordedDate      = as.POSIXct(RecordedDate, tz = "UTC"),
    duration_sec      = Duration..in.seconds.,
    Finished, Progress,
    # universe / screener — needed to pick the ACS comparison population
    RentOwn, PrimaryFinancial, HasMortgage,
    # benchmark-critical
    Hisp,
    UserLanguage,
    Zipcode           = formatC(as.character(Zipcode), width = 5, flag = "0"),
    # housing context
    YearHomePurchased, HomePurchasePrice,
    HasHomeownInsur, HasFloodPolicyHome, HasSepFloodPolicy, SFHA,
    # data quality (reviewers on opt-in panels ask)
    recaptcha         = Q_RecaptchaScore,
    relid_dup         = Q_RelevantIDDuplicate,
    relid_fraud       = Q_RelevantIDFraudScore,
    DistributionChannel,
    Attention1, Attention_1, Attention_2
  ) %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))   # Qualtrics blanks -> NA

# pre-join checks
stopifnot(
  length(setdiff(hyp$ResponseID, sample_keep$ResponseID)) == 0,  # all hyp ids present
  !any(duplicated(sample_keep$ResponseID)),                      # one row per respondent
  setdiff(intersect(names(hyp), names(sample_keep)), "ResponseID") |> length() == 0
)

n0  <- nrow(hyp)
hyp <- left_join(hyp, sample_keep, by = "ResponseID", relationship = "many-to-one")
stopifnot(nrow(hyp) == n0)   # guard against row inflation


r <- distinct(hyp, ResponseID, .keep_all = TRUE)

range(r$RecordedDate)                    # field window for the methods section
count(r, RentOwn)                        # confirm the homeowner screen is universal
count(r, PrimaryFinancial)               # householder vs any-adult -> ACS universe
count(r, Hisp)                           # Hispanic share for the Race2 rebuild
count(r, UserLanguage)                   # English-only coverage caveat

# ------------- CENSUS

ACS_YEAR   <- 2024L
CACHE      <- "acs2024.rds"
USE_REPWTS <- TRUE   # FALSE = faster pull, no CIs on the benchmark column


## ---------------------------------------------------------------------
##    STATE -> CUSTOM 5-REGION SCHEME (used identically on both sides)
##    NOT census regions — Southwest is split out of the census South/West.
## ---------------------------------------------------------------------

region_defs <- list(
  Northeast = c("PA","NY","NJ","VT","NH","CT","RI","MA","ME"),
  Southwest = c("TX","OK","NM","AZ"),
  West      = c("CO","ID","NV","UT","WY","MT","CA","OR","WA","AK","HI"),
  Southeast = c("DC","DE","MD","WV","VA","KY","TN","NC","SC","GA",
                "FL","AL","MS","AR","LA"),
  Midwest   = c("OH","IN","IL","MI","WI","MN","ND","SD","IA","KS","NE","MO")
)

abb_region <- setNames(rep(names(region_defs), lengths(region_defs)),
                       unlist(region_defs, use.names = FALSE))

## covers all 50 states + DC exactly once, no state assigned twice
stopifnot(
  length(abb_region) == 51L,
  !any(duplicated(names(abb_region))),
  length(setdiff(c(state.abb, "DC"), names(abb_region))) == 0L
)

st_xwalk <- tidycensus::fips_codes |>
  distinct(state_code, state) |>
  filter(state %in% names(abb_region)) |>
  mutate(region = unname(abb_region[state]))

region_levels <- c("Northeast", "Southeast", "Midwest", "Southwest", "West")

hyp <- hyp |>
  mutate(region = factor(unname(abb_region[State]), levels = region_levels))

## catch territories, full state names, or stray whitespace in State
if (any(is.na(hyp$region))) {
  print(sort(unique(hyp$State[is.na(hyp$region)])))
  stop("unmapped State values — see above")
}



library(tidycensus); library(dplyr); library(tidyr); library(purrr)
# census_api_key("YOUR_KEY_HERE", install = TRUE)   # one time only

# ---- 1. the 5-region scheme, defined once and used on both sides ----
ne <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
mw <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
sw <- c("TX","OK","NM","AZ")
se <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA")
we <- c("CO","ID","MT","NV","UT","WY","AK","CA","HI","OR","WA")
stopifnot(length(c(ne,mw,sw,se,we)) == 51, !any(duplicated(c(ne,mw,sw,se,we))))

region5 <- function(abb) case_when(
  abb %in% ne ~ "Northeast", abb %in% mw ~ "Midwest", abb %in% sw ~ "Southwest",
  abb %in% se ~ "Southeast", abb %in% we ~ "West",    TRUE ~ NA_character_)

# CHECK existing region column matches this rule before going further:
distinct(hyp, ResponseID, .keep_all = TRUE) |>
  count(region, built = region5(State)) |> filter(region != built)   # want 0 rows

# ---- 2. pull PUMS ----
pums <- get_pums(
  variables = c("RELSHIPP","TEN","AGEP","SEX","SCHL","RAC1P","HISP","HINCP","ADJINC"),
  state = "all", survey = "acs1", year = 2024
)   

st_abb <- tidycensus::fips_codes |>
  distinct(state_code, state) |>
  filter(state %in% names(abb_region))     # drops PR and the territories

# Typed out explicitly, ascending. Do NOT use levels(hyp$AnnualIncome): that is
# positional and fails silently if the factor gains a level or is reordered.
inc_labs <- c("Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999",
              "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
              "$200,000 to $249,999", "$250,000 or more")

stopifnot(setequal(inc_labs, setdiff(unique(as.character(hyp$AnnualIncome)), NA)))

# Three-category collapse used in the main text. Defined once, applied to BOTH
# sides, so the coarse table nests exactly inside the eight-bin table.
inc3 <- function(x) dplyr::case_when(
  x %in% inc_labs[1:2] ~ "Less than $50,000",
  x == inc_labs[3]     ~ "$50,000 to $74,999",
  x %in% inc_labs[4:8] ~ "$75,000 or more",
  TRUE                 ~ NA_character_
)
bench <- pums |>
  filter(RELSHIPP == "20", TEN %in% c("1","2"), as.numeric(AGEP) >= 18) |>
  left_join(st_abb, by = c("STATE" = "state_code")) |>
  mutate(
    adjinc    = as.numeric(ADJINC),
    adjinc    = if_else(adjinc > 100, adjinc / 1e6, adjinc),
    hh_income = as.numeric(HINCP) * adjinc,
    wgt       = as.numeric(WGTP),
    schl      = as.integer(SCHL),
    age_grp   = cut(as.numeric(AGEP), c(17, 24, 44, 64, Inf),
                    labels = c("18 – 24","25 – 44","45 – 64","65 or older")),
    inc_grp   = cut(hh_income,
                    c(-Inf, 25000, 50000, 75000, 100000, 150000, 200000, 250000, Inf),
                    labels = inc_labs, right = FALSE),
    inc_grp3  = inc3(as.character(inc_grp)),
    educ = case_when(schl <= 17 ~ "High school or less",
                     schl <= 20 ~ "Some college",
                     TRUE       ~ "Bachelor's or more"),
    hisp = if_else(HISP != "01", "Yes", "No"),
    race4 = case_when(hisp == "Yes" ~ "Hispanic (any race)",
                      RAC1P == "1"  ~ "White, non-Hispanic",
                      RAC1P == "2"  ~ "Black, non-Hispanic",
                      TRUE          ~ "Other or multiple, non-Hispanic"),
    sex    = if_else(SEX == "1", "Male", "Female"),
    region = region5(state)
  )

stopifnot(!any(is.na(bench$region)))   # PR already dropped; catches anything else

bench_tbl <- map_dfr(
  c("age_grp","inc_grp","inc_grp3","educ","race4","sex","region"),
  \(v) bench |>
    filter(!is.na(.data[[v]])) |>
    group_by(level = as.character(.data[[v]])) |>
    summarise(w = sum(wgt), .groups = "drop") |>
    mutate(variable = v, p_bench = w / sum(w)) |>
    dplyr::select(variable, level, p_bench)
)
stopifnot(all(abs(tapply(bench_tbl$p_bench, bench_tbl$variable, sum) - 1) < 1e-9))

# ---- 3. sample side, harmonized to the same labels ----
r <- distinct(hyp, ResponseID, .keep_all = TRUE) |>
  mutate(
    age_grp  = as.character(AgeGroup),
    inc_grp  = as.character(AnnualIncome),
    inc_grp3 = inc3(inc_grp),
    educ     = HighestEduc,
    sex      = if_else(Gender %in% c("Male", "Female"), Gender, NA_character_),
    race4    = case_when(
      Hisp == "Yes"                       ~ "Hispanic (any race)",
      grepl(",", Race)                    ~ "Other or multiple, non-Hispanic",
      Race == "White"                     ~ "White, non-Hispanic",
      Race == "Black or African American" ~ "Black, non-Hispanic",
      TRUE                                ~ "Other or multiple, non-Hispanic")
  )

stopifnot(!any(is.na(r$inc_grp3) & !is.na(r$inc_grp)))   # no band fell through inc3()

n_sex_other <- sum(!is.na(r$Gender) & !r$Gender %in% c("Male", "Female"))  # footnote

samp_tbl <- r |>
  dplyr::select(age_grp, inc_grp, inc_grp3, educ, race4, sex, region) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "level") |>
  filter(!is.na(level)) |>
  count(variable, level) |>
  group_by(variable) |> mutate(p_samp = n / sum(n)) |> ungroup()

# ---- 4. the tables ----
std_diff <- function(p1, p2) (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)

make_balance <- function(inc_var) {
  vars <- c("age_grp", inc_var, "educ", "race4", "sex", "region")
  out <- samp_tbl |>
    filter(variable %in% vars) |>
    full_join(filter(bench_tbl, variable %in% vars), by = c("variable", "level")) |>
    mutate(pct_samp  = 100 * p_samp,
           pct_bench = 100 * p_bench,
           d         = std_diff(p_samp, p_bench),
           flag      = abs(d) > 0.10,
           quota     = variable %in% c("age_grp", inc_var, "region")) |>
    arrange(variable, desc(pct_samp)) |>
    dplyr::select(variable, level, n, pct_samp, pct_bench, d, flag, quota)
  stopifnot(!anyNA(out$d))   # any NA means a genuine label mismatch
  out
}

balance_main <- make_balance("inc_grp3")    # main text: 3 income categories
balance_si   <- make_balance("inc_grp")     # SI: all 8 bands

print(balance_main, n = Inf)
print(balance_si,   n = Inf)

# Write in LaTEX

library(kableExtra)

balance_kable <- function(tbl, inc_var, file,
                          caption = "Balance table: Sample composition relative to the ACS benchmark population",
                          label   = "sitab:balancetable") {
  
  blocks <- list(
    list(var = "region",  head = "Census region$^{\\dagger}$",
         levels = region_levels),
    list(var = inc_var,   head = "Household income$^{\\dagger}$",
         levels = if (inc_var == "inc_grp3")
           c("Less than $50,000", "$50,000 to $74,999", "$75,000 or more") else inc_labs),
    list(var = "age_grp", head = "Age",
         levels = c("18 – 24", "25 – 44", "45 – 64", "65 or older")),
    list(var = "educ",    head = "Education",
         levels = c("High school or less", "Some college", "Bachelor's or more")),
    list(var = "race4",   head = "Race and ethnicity",
         levels = c("White, non-Hispanic", "Black, non-Hispanic",
                    "Hispanic (any race)", "Other or multiple, non-Hispanic")),
    list(var = "sex",     head = "Sex",
         levels = c("Male", "Female"))
  )
  
  # stack blocks in display order, checking levels
  d <- purrr::map_dfr(blocks, function(b) {
    bt <- tbl[tbl$variable == b$var, ]
    stopifnot(setequal(bt$level, b$levels))
    bt[match(b$levels, bt$level), ]
  })
  
  tex_lab <- function(x) {
    x <- gsub(" – ", "--", x, fixed = TRUE)
    gsub("$", "\\$", x, fixed = TRUE)
  }
  
  body <- data.frame(
    level     = tex_lab(d$level),
    n         = d$n,
    pct_samp  = sprintf("%.1f", d$pct_samp),
    pct_bench = sprintf("%.1f", d$pct_bench),
    d         = ifelse(d$flag, sprintf("\\textbf{%.2f}", d$d), sprintf("%.2f", d$d))
  )
  
  # row index for pack_rows: named vector of block sizes
  idx <- setNames(lengths(lapply(blocks, `[[`, "levels")),
                  sapply(blocks, `[[`, "head"))
  
  tab <- body |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
        col.names = c("", "$\\textit{n}$", "Sample (\\%)", "ACS (\\%)", "Std.\\ diff."),
        align = "lrrrr") |>
    pack_rows(index = idx, italic = TRUE, bold = FALSE, escape = FALSE,
              latex_gap_space = "0.3em", indent = TRUE)
  
  fmt_n <- function(x) gsub(",", "{,}", format(x, big.mark = ","), fixed = TRUE)
  n_all <- nrow(r)
  n_sex <- sum(d$n[d$variable == "sex"])
  
  out <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{8pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    "\\begin{threeparttable}",
    "\\singlespacing",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    as.character(tab),
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    sprintf("\\item \\textit{Notes:} Sample is $\\textit{n} = %s$ homeowner respondents.", fmt_n(n_all)),
    "Benchmark shares are from the 2024 American Community Survey one-year public use",
    "microdata sample, restricted to owner-occupied household reference persons and",
    "weighted by the household weight. Std.\\ diff.\\ is the standardized difference in",
    "proportions, $(p_s - p_b)/\\sqrt{[p_s(1-p_s) + p_b(1-p_b)]/2}$; values with",
    "$|d| > 0.10$ are shown in bold. Respondents that report a sex other than male or",
    sprintf("female have no ACS counterpart and are excluded from the sex panel ($N = %s$);", fmt_n(n_sex)),
    "they are retained in all other panels.",
    "$^{\\dagger}$ Variable used as a sampling quota.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(out, file)
  invisible(out)
}

balance_kable(balance_main, "inc_grp3", "tab_balance_main.tex")


# ## =====================================================================
## Balance across the six vignette cells (scenario6)
## Unit: one row per shown vignette (ResponseID x scenario_id); each
## respondent appears in the two cells they drew, covariates constant
## across their two rows 
## =====================================================================
library(survey)

req <- c("scenario6","hazard","AgeGroup","AnnualIncome","HighestEduc","Gender",
         "Hisp","Race","region","DisasterExperience","Party",
         "RiskAversion_bin","GovTrustBin","gap_quartile")
stopifnot(all(req %in% names(hyp)))   

## covariates harmonized with the census table so the two speak the same
## language; race4/sex/inc3/region reuse the identical recipes
v <- hyp %>%
  filter(!is.na(scenario6)) %>%
  transmute(
    ResponseID,
    scenario6   = factor(scenario6),               # keep the 6-level order
    hazard,                                         # the row that actually moves
    AgeGroup    = as.character(AgeGroup),
    Income3     = inc3(as.character(AnnualIncome)),
    Education   = as.character(HighestEduc),
    Sex         = if_else(Gender %in% c("Male","Female"), Gender, NA_character_),
    Race4       = case_when(
      Hisp == "Yes"                       ~ "Hispanic (any race)",
      grepl(",", Race)                    ~ "Other or multiple, non-Hispanic",
      Race == "White"                     ~ "White, non-Hispanic",
      Race == "Black or African American" ~ "Black, non-Hispanic",
      TRUE                                ~ "Other or multiple, non-Hispanic"),
    Region      = as.character(region),
    DisasterExp = as.character(DisasterExperience),
    Party       = as.character(Party),
    RiskAvers   = as.character(RiskAversion_bin),
    GovTrust    = as.character(GovTrustBin),
    GapQuartile = as.character(gap_quartile)
  )

stopifnot(max(count(v, ResponseID)$n) <= 2L, !any(is.na(v$scenario6)))

cov_vars <- c("hazard","AgeGroup","Income3","Education","Sex","Race4","Region",
              "DisasterExp","Party","RiskAvers","GovTrust","GapQuartile")

## ---- display layer: % of each level within each cell, + overall ------
share_by <- function(cv, grp = NULL) {
  x <- v %>% filter(!is.na(.data[[cv]]))
  if (is.null(grp)) x %>% count(level = .data[[cv]], name = "n") %>%
    transmute(variable = cv, level, Overall = 100*n/sum(n))
  else x %>% count(scenario6, level = .data[[cv]], name = "n") %>%
    group_by(scenario6) %>% mutate(pct = 100*n/sum(n)) %>% ungroup() %>%
    transmute(variable = cv, level, scenario6, pct)
}
disp <- map_dfr(cov_vars, share_by, grp = TRUE) %>%
  pivot_wider(names_from = scenario6, values_from = pct)
overall <- map_dfr(cov_vars, share_by)

balance_scn <- disp %>%
  left_join(overall, by = c("variable","level")) %>%
  relocate(Overall, .after = level) %>%
  arrange(match(variable, cov_vars), desc(Overall))

cell_n <- count(v, scenario6, name = "n_vignettes")   # column header Ns

## ---- test layer: respondent-clustered independence per covariate -----
test_tbl <- map_dfr(cov_vars, function(cv) {
  dat <- v[!is.na(v[[cv]]), c("ResponseID","scenario6", cv)]
  des <- svydesign(ids = ~ResponseID, weights = ~1, data = dat)          # respondent = PSU
  rs  <- svychisq(reformulate(c(cv, "scenario6")), des, statistic = "F") # Rao-Scott
  naive <- suppressWarnings(chisq.test(table(dat[[cv]], dat$scenario6))$p.value)
  tibble(variable = cv, p_clustered = unname(rs$p.value), p_naive = naive)
})

print(cell_n)
print(balance_scn, n = Inf)
print(test_tbl)

# Make LaTex Table

library(kableExtra)

## ---- map scenario levels to the six columns (EDIT to your level names) ----
scn_order <- levels(v$scenario6)          # e.g. c("P_base","P_info","P_adapt","S_base","S_info","S_adapt")
stopifnot(length(scn_order) == 6, all(scn_order %in% names(balance_scn)))

scn_cols <- c("Base", "\\makecell{Prior\\\\info}", "\\makecell{Prior info\\\\+ adapt}")

## ---- which covariates to show, in which order, with what headers ----
blocks_scn <- c(Region    = "Region",
                Income3   = "Household income",
                AgeGroup  = "Age",
                Education = "Education",
                Race4     = "Race and ethnicity",
                Sex       = "Sex")

tex_lab <- function(x) {
  x <- gsub(" – ", "--", x, fixed = TRUE)                  # age ranges
  x <- gsub(" to ", "--", x, fixed = TRUE)                 # income ranges
  x <- gsub("Other or multiple", "Other/multiple", x, fixed = TRUE)
  x <- gsub(",", "{,}", x, fixed = TRUE)                   # thousands separators
  gsub("$", "\\$", x, fixed = TRUE)
}

balance_scn_kable <- function(file,
                              caption = "Covariate balance across the six vignette scenarios",
                              label   = "sitab:balancetable_scen") {
  
  d <- balance_scn |>
    filter(variable %in% names(blocks_scn)) |>
    mutate(variable = factor(variable, levels = names(blocks_scn))) |>
    arrange(variable, desc(Overall))
  
  body <- d |>
    transmute(Characteristic = tex_lab(level),
              across(all_of(scn_order), ~ sprintf("%.1f", .x)))
  
  # p-values from the respondent-clustered Rao-Scott test, for the notes
  p_txt <- test_tbl |>
    filter(variable %in% names(blocks_scn)) |>
    mutate(variable = factor(variable, levels = names(blocks_scn))) |>
    arrange(variable) |>
    transmute(txt = sprintf("%s, $p = %.2f$",
                            tolower(blocks_scn[as.character(variable)]), p_clustered)) |>
    pull(txt) |> paste(collapse = "; ")
  
  n_txt <- paste(cell_n$n_vignettes[match(scn_order, cell_n$scenario6)], collapse = ", ")
  
  idx <- table(droplevels(d$variable))
  names(idx) <- blocks_scn[names(idx)]
  
  tab <- body |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
        col.names = c("Characteristic", scn_cols, scn_cols),
        align = "lrrrrrr") |>
    add_header_above(c(" " = 1,
                       "Primary residence (\\\\%)" = 3,
                       "Second home (\\\\%)" = 3),
                     escape = FALSE) |>
    pack_rows(index = idx, italic = TRUE, bold = FALSE,
              latex_gap_space = "0.3em", indent = TRUE) |>
    as.character()
  
  # drop the gap kableExtra puts right under \midrule
  tab <- sub("\\\\midrule\n\\\\addlinespace\\[0.3em\\]", "\\\\midrule", tab)
  
  out <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{8pt}",
    "\\renewcommand{\\arraystretch}{1.1}",
    "\\begin{threeparttable}",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    tab,
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf("\\item \\textit{Notes:} Cells show the percentage of vignettes in each scenario with the given characteristic. Vignettes per cell, in column order: %s. Respondents saw up to two vignettes. $p$-values are from Rao--Scott $F$ tests of independence between each characteristic and scenario assignment, clustered by respondent: %s.",
            n_txt, p_txt),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  
  writeLines(out, file)
  invisible(out)
}

balance_scn_kable("tab_balance_scen.tex")
#-------------------------------------------------------------------------------
#             FIGURE: DIST PLOT WITH UNINSURED DAMAGES VALUES                  |           
#-------------------------------------------------------------------------------
hyp = readRDS("data/data_updated.rds")
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
ia <- build_owners_noRA(ho = 0, fl = 0) |>
  mutate(ha_pct_of_damage = pmin(comp_rate * 100, 100))   # same 0–100 cap as before

summarise_large(ia)


################# TRYING WITH SUBSETTING TO ONLY FLOOD LOSSES, AND PPL WITHOUT FLOOD INSURANCE ################################


# n_fmt <- function(d) format(nrow(d), big.mark = ",")
# 
# # Check coding / NAs before filtering
# ia_raw[, .N, by = floodDamage]
# 
# cat("All registrations:                    ", n_fmt(ia_raw), "\n")
# 
# ia_fd <- ia_raw |> filter(floodDamage == 1)
# cat("Step 1: floodDamage == 1:             ", n_fmt(ia_fd), "\n")
# 
# ia_fd_nfi <- ia_fd |> filter(floodInsurance == 0)
# cat("Step 2: ... & floodInsurance == 0:    ", n_fmt(ia_fd_nfi), "\n")
# 
# # NULL = no restriction on that insurance field
# build_owners_noRA <- function(data = ia_raw, ho = NULL, fl = NULL) {
#   data |>
#     filter(!is.na(rpfvl) | !is.na(ppfvl), ownRent == "O",
#            is.null(ho) | homeOwnersInsurance %in% ho,
#            is.null(fl) | floodInsurance %in% fl) |>
#     mutate(
#       verified_loss = coalesce(rpfvl, 0) + coalesce(ppfvl, 0),
#       haAmount      = if (na_award_as_zero) coalesce(haAmount, 0) else haAmount,
#       loss_award    = haAmount - coalesce(rentalAssistanceAmount, 0),
#       year_decl     = year(as.Date(sub("T.*$", "", declarationDate)))
#     ) |>
#     filter(verified_loss > 0, is.na(haAmount) | haAmount <= ha_cap) |>
#     left_join(cpi_year, by = "year_decl") |>
#     mutate(verified_loss_2024 = verified_loss * cpi_2024 / cpi,
#            comp_rate          = loss_award / verified_loss) |>
#     filter(is.finite(comp_rate), comp_rate >= 0, !is.na(verified_loss_2024))
# }
# 
# # New analysis sample: flood-damaged owners without flood insurance (any HO status)
# ia <- build_owners_noRA(ia_fd_nfi) |>
#   mutate(ha_pct_of_damage = pmin(comp_rate * 100, 100))
# cat("Step 3: ... & owner, valid loss/award:", n_fmt(ia), "\n")
# cat("        of which >= $40k (2024$):     ",
#     format(sum(ia$verified_loss_2024 >= large_loss), big.mark = ","), "\n")
# 
# # Side-by-side with the old sample (no HO, no flood, all damage types)
# bind_rows(
#   summarise_large(ia) |> mutate(sample = "Flood dmg, no flood ins."),
#   summarise_large(build_owners_noRA(ho = 0, fl = 0)) |> mutate(sample = "No HO, no flood (old)")
# )






################################################################################################################################
# ── 5) Heterogeneity side: keep estimates in percent (no $ conversion) ──────
baseline_terms <- c(
  "Risk neutral","Independent","High government trust",
  "Mid-range tax progressive","No disaster experience",
  "Annual Income $100,000 to $249,999"
)

plot_data_nobase_pct <- plot_data_full %>%
  filter(!(term_clean %in% baseline_terms)) %>%
  dplyr::mutate(
    sig_level = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

plot_data_numeric_pct <- plot_data_nobase_pct %>%
  dplyr::mutate(
    y_numeric = as.numeric(factor(
      paste(covariate_label, term_clean),
      levels = rev(unique(paste(covariate_label, term_clean)))
    ))
  )

# ── 6) BINNED IA densities ────────
# All records density
dens_all  <- density(ia$ha_pct_of_damage, adjust = 1.5, from = 0)
dens_ge40 <- density(ia$ha_pct_of_damage[ia$verified_loss_2024 >= large_loss],
                     adjust = 1.5, from = 0)
ia_ha_density_2 <- bind_rows(
  tibble(x = dens_all$x,   dens = dens_all$y,   damage_bin = "All damages"),
  tibble(x = dens_ge40$x,  dens = dens_ge40$y,  damage_bin = "≥ $40k damage")
)

ia_ha_mean_2 <- bind_rows(
  ia |> summarise(mean_ha_pct = mean(ha_pct_of_damage)) |> mutate(damage_bin = "All damages"),
  ia |> filter(verified_loss_2024 >= large_loss) |>
    summarise(mean_ha_pct = mean(ha_pct_of_damage)) |> mutate(damage_bin = "≥ $40k damage")
)

# ── 7) y scaling ─────────────────────────────────────────────────────────────
y_max    <- max(plot_data_numeric_pct$y_numeric, na.rm = TRUE)
dens_max <- max(ia_ha_density_2$dens, na.rm = TRUE)

scale_factor <- (y_max * 0.9) / dens_max

ia_ha_pct_density_2 <- ia_ha_density_2 %>%
  dplyr::mutate(dens_scaled = dens * scale_factor)

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
    name   = "Damage Assessed (flood-damaged owners without flood insurance)"
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
ggsave("Figures/Supp/dist_uninsured_9.26_flooddamage.png",
       plot = combined_plot_pct, width = 12, height = 7, dpi = 300)

################################################################################
# SUPP ADDITIONAL ANALYSES #####################################################
################################################################################



#-------------------------------------------------------------------------------
#             FIGURE: RESP / AID SPLINE                                        |           
#-------------------------------------------------------------------------------

plot_ridge_resp_aid_spline <- function(hyp, spline_df = 4) {
  
  hyp_plot <- hyp %>%
    dplyr::mutate(
      resp = parse_number(as.character(resp)),
      percent_aid = parse_number(as.character(percent_aid))
    ) %>%
    filter(!is.na(resp), !is.na(percent_aid)) %>%
    dplyr::mutate(
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
  
  spl_fit <- feols(
    percent_aid ~ ns(resp, df = spline_df),
    data = hyp_plot,
    vcov = ~ResponseID
  )
  
  # R-squared
  r2 <- fitstat(spl_fit, "r2")$r2
  r2_label <- paste0("R² = ", sprintf("%.2f", r2))  
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
    annotate(
      "text",
      x = 100,
      y = 12.75,
      label = r2_label,
      hjust = 1,
      vjust = 1,
      size = 6
    ) +
    scale_x_continuous(
      name = "Recommended government aid (% of loss)",
      limits = c(0, 100)
    ) +
    scale_y_discrete(
      name = "Perceived responsibility",
      expand = expansion(mult = c(0.02, 0.15))
    ) +
    coord_flip() +
    theme_minimal(base_size = 18) +
    theme(
      axis.title.x       = element_text(size = 20, margin = margin(t = 12)),
      axis.title.y       = element_text(size = 20),
      axis.text.x        = element_text(size = 16),
      axis.text.y        = element_text(size = 16),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position    = "none",
      plot.margin        = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

p_ridge_spline <- plot_ridge_resp_aid_spline(hyp, spline_df = 4)
p_ridge_spline
ggsave("Figures/Supp/fig_spline_7.30.png",
       plot = p_ridge_spline , width = 15, height = 7, dpi = 300)

####################################################

#-------------------------------------------------------------------------------
#             TABLES: KNOWLEDGE INDEX: DISTRIBUTION OF CORRECT ANSWERS         |
#       RECOMMENDED AID AND KNOWLEDGE OF EXISTING DISASTER AID PROGRAMS        |           
#-------------------------------------------------------------------------------
source("cleaning.R")
source("generate_hyp.R")

library(tidyverse)
library(fixest)

# --- ID-column guard: file uses both ResponseId and ResponseID on `sample` ---
if (!"ResponseId" %in% names(sample) && "ResponseID" %in% names(sample)) sample$ResponseId <- sample$ResponseID
if (!"ResponseID" %in% names(sample) && "ResponseId" %in% names(sample)) sample$ResponseID <- sample$ResponseId

# benchmark: midpoint of the $5,000-$9,999 bin for a $75k loss
BENCH_BIN        <- "$5,000 to $9,999"
IHP_SHARE_ACTUAL <- 15   # % of loss covered by typical IHP grant (comparator)

# analysis sample: one row per respondent, restricted to the vignette sample
sample_trunc <- sample %>% filter(ResponseID %in% hyp$ResponseID)


# =============================================================================
# PARAGRAPH 1 — what respondents think FEMA pays (respondent level)
# =============================================================================
bin_levels <- c("Less than $100", "$100 to $999", "$1,000 to $4,999",
                "$5,000 to $9,999", "$10,000 to $29,999", "$30,000 to $49,999",
                "$50,000 to $74,999", "$75,000", "More than $75,000", "I am not sure")

amt_dist <- sample_trunc %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  mutate(bin = factor(FEMAAmount_Dollar, levels = bin_levels)) %>%
  count(bin) %>%
  dplyr::mutate(
    share = n / sum(n),
    position = case_when(
      is.na(bin)             ~ "missing",
      bin == "I am not sure" ~ "not sure",
      bin == BENCH_BIN       ~ "at benchmark",
      match(as.character(bin), bin_levels) < match(BENCH_BIN, bin_levels) ~ "below benchmark",
      TRUE                   ~ "above benchmark"
    )
  )

knowledge_headline <- amt_dist %>%
  group_by(position) %>%
  summarise(n = sum(n), share = sum(share), .groups = "drop")

cat("\n--- PARA 1: FEMA amount beliefs (overestimate = 'above benchmark') ---\n")
print(knowledge_headline)   # 'above benchmark' -> 66.1% ; 'not sure' -> 16.1%

#### PLOT FROM SONGS PAPER ####


sample_trunc = sample |> 
  filter(ResponseID %in% hyp$ResponseID)
standardize_response <- function(x) {
  x %>%
    str_replace_all("FEMA|SBA", "the program") %>%
    str_trim()
}


# FEMA summary
fema_pct <- sample_trunc %>%
  filter(!is.na(AppliedFEMA), AppliedFEMA != "", AppliedFEMA != "NA") %>%
  count(Response = AppliedFEMA) %>%
  mutate(
    Response = standardize_response(Response),
    percent = n / sum(n) * 100,
    question = "FEMA IHP grant"
  )

# SBA summary
sba_pct <- sample_trunc %>%
  filter(!is.na(AppliedSBA), AppliedSBA != "", AppliedSBA != "NA") %>%
  count(Response = AppliedSBA) %>%
  mutate(
    Response = standardize_response(Response),
    percent = n / sum(n) * 100,
    question = "SBA loan"
  )

# Combine for plotting
combined_pct <- bind_rows(fema_pct, sba_pct)

response_levels <- c(
  "Yes, I applied and I was approved and I received funds",
  "Yes, I applied and I was approved, but I did not receive funds",
  "Yes, I applied, but I was not approved",
  "Yes, I applied, but I did not hear back from the program",
  "I was aware of this program, but I did not apply",
  "I was not aware of this program, so I did not apply",
  "The disaster was not eligible for disaster assistance"
)

combined_pct$Response <- factor(combined_pct$Response, levels = rev(response_levels))


# Change colors so grouped by yes or no

plot_data <- combined_pct %>%
  mutate(ResponseType = case_when(
    grepl("^Yes", Response) ~ "Yes",
    grepl("not apply|not eligible", Response, ignore.case = TRUE) ~ "No",
    TRUE ~ "Other"
  ))

yes_colors <- c(
  "Yes, I applied and I was approved and I received funds" = "#990000",
  "Yes, I applied and I was approved, but I did not receive funds" = "#cc6666",
  "Yes, I applied, but I was not approved" = "#e69999",
  "Yes, I applied, but I did not hear back from the program" = "#f2cccc"
)

no_colors <- c(
  "I was aware of this program, but I did not apply" = "#b3cde3",
  "I was not aware of this program, so I did not apply" = "#6497b1",
  "The disaster was not eligible for disaster assistance" = "#005b96"
)

response_colors <- c(yes_colors, no_colors)

ggplot(plot_data, aes(x = question, y = percent, fill = Response)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    size = 4, color = "white"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 100)) +
  scale_fill_manual(values = response_colors) +
  labs(
    x = NULL, y = "Percent of respondents",
    title = "Did you apply for disaster assistance?",
    fill = "Response"
  ) +
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5))

# =============================================================================
# PART2 — two-item knowledge index and its effect on preferred aid
# =============================================================================

# ---- respondent-level belief slice of `sample`, joined onto vignette panel ---
sample_bel <- sample %>%
  transmute(
    ResponseID = ResponseId,
    Beliefs_FEMA_Amount,
    Beliefs_FEMA_Amount_NotSure
  ) %>%
  mutate(across(where(is.character), ~ na_if(str_squish(.x), "")))

stopifnot(!anyDuplicated(sample_bel$ResponseID))

hyp2 <- hyp %>% left_join(sample_bel, by = "ResponseID")
stopifnot(nrow(hyp2) == nrow(hyp))

# ---- class fixes ------------------------------------------------------------
est <- hyp2 %>%
  mutate(
    percent_aid = as.numeric(percent_aid)
  )

# ---- inspect the flood-question level name BEFORE scoring (edit if needed) ---
cat("\n--- CorrectFloodQuestion levels ---\n")
print(if (is.factor(est$CorrectFloodQuestion))
  levels(est$CorrectFloodQuestion) else unique(est$CorrectFloodQuestion))
CORRECT_LAB <- "Correct"   # <-- must match the level above

# ---- build the two-item index at respondent level ---------------------------
BM_SHARE <- 7499.5 / 75000

bel <- est %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  transmute(
    ResponseID,
    not_sure = Beliefs_FEMA_Amount_NotSure,
    
    # item 1: flood insurance question correct
    k_flood = as.integer(!is.na(CorrectFloodQuestion) &
                           CorrectFloodQuestion == CORRECT_LAB),
    
    # item 2: FEMA amount estimate lands in the $5k-$10k bin ("unsure" scored 0)
    k_amount = as.integer(not_sure != 1 &
                            Beliefs_FEMA_Amount >= 5000 &
                            Beliefs_FEMA_Amount <  10000),
    
    know_index = k_flood + k_amount
  )

est <- est %>%
  dplyr::select(-any_of(c("k_flood", "k_amount", "know_index",
                          "knowledge_group", "not_sure"))) %>%
  left_join(bel, by = "ResponseID") %>%
  mutate(
    knowledge_group = factor(
      if_else(know_index >= 1, "One or more", "Neither"),
      levels = c("Neither", "One or more"))
  )
stopifnot(nrow(est) == nrow(hyp2))

# ---- counts cited in the text ----------------------------------------------
cat("\n--- PARA 2: index counts ---\n")
cat("Answered BOTH items correctly (respondents): ",
    est %>% distinct(ResponseID, know_index) %>% filter(know_index == 2) %>% nrow(), "\n")
cat("Neither vs One-or-more (respondents):\n")
print(est %>% distinct(ResponseID, knowledge_group) %>% count(knowledge_group))
# -> 'One or more' = 841 ; 'Neither' = 1,120

# ---- effect of knowledge on preferred aid (additive) ------------------------
knowledge_model <- feols(
  percent_aid ~
    second_home * prior_info * adaptive_measures + knowledge_group +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = est, vcov = ~ResponseID
)
cat("\n--- PARA 2: additive model (knowledge_group coef = -4.75pp, p<0.001) ---\n")
print(summary(knowledge_model))

# base-case predicted level for the informed group ("still 37.95%"):
#   intercept (Neither, base cell, reference covariates) + knowledge_group coef
cf <- coef(knowledge_model)
cat("\nBase-case level, informed group (intercept + knowledge_group):",
    round(cf[["(Intercept)"]] + cf[["knowledge_groupOne or more"]], 2), "%\n")

# ---- does knowledge move the adaptation effect? (fully interacted) ----------
knowledge_model_int <- feols(
  percent_aid ~
    second_home * prior_info * adaptive_measures * knowledge_group +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = est, vcov = ~ResponseID
)
cat("\n--- PARA 2: interacted model (adaptation x knowledge terms ~ null) ---\n")
print(summary(knowledge_model_int))

# =============================================================================
# TABLE + FINAL PARAGRAPH — prior-experience descriptives
# =============================================================================
exp <- sample_trunc %>% filter(DisasterExperience == 1)
cat("\n--- EXPERIENCE: respondents with a prior home-damaging disaster ---\n")
cat("n =", nrow(exp), "\n")   # 653

# home damage ($) distribution
cat("\nHome damage distribution:\n")
exp %>%
  filter(!is.na(HomeDamage_Dollar), HomeDamage_Dollar != "") %>%
  count(HomeDamage_Dollar) %>%
  mutate(percent = round(n / sum(n) * 100)) %>%
  as_tibble() %>% print(n = 30)

# share of damage repaired
cat("\nShare repaired distribution:\n")
exp %>%
  filter(!is.na(PercRepaired), PercRepaired != "") %>%
  count(PercRepaired) %>%
  mutate(percent = round(n / sum(n) * 100)) %>% 
  as_tibble() %>% print(n = 30)

# applied / approved-and-received (of the 653 experienced)
applied_yes <- c(
  "Yes, I applied and I was approved and I received funds",
  "Yes, I applied and I was approved, but I did not receive funds",
  "Yes, I applied, but I was not approved",
  "Yes, I applied, but I did not hear back from the program")

cat("\nFEMA application outcomes among experienced:\n")
exp %>%
  filter(!is.na(AppliedFEMA), AppliedFEMA != "", AppliedFEMA != "NA") %>%
  count(AppliedFEMA) %>% mutate(percent = round(n / sum(n) * 100)) %>%
  print(n = 30)

cat("\nApplied (any 'Yes, I applied'): ",
    round(100 * mean(exp$AppliedFEMA %in% applied_yes), 1), "% \n")
cat("Approved AND received funds: ",
    round(100 * mean(exp$AppliedFEMA ==
                       "Yes, I applied and I was approved and I received funds", na.rm = TRUE), 1),
    "%  (n =",
    sum(exp$AppliedFEMA == "Yes, I applied and I was approved and I received funds",
        na.rm = TRUE), ")\n")   # ~38% applied ; ~20% received (n = 131)

# ---- Table: FEMA assistance vs expectation, among the 131 recipients --------
recipients <- sample_trunc %>%
  filter(AppliedFEMA == "Yes, I applied and I was approved and I received funds") %>%
  filter(!is.na(FEMALowerHigher), FEMALowerHigher != "", FEMALowerHigher != "NA")

cat("\n--- TABLE: recipients (n =", nrow(recipients), ") FEMALowerHigher ---\n")

# raw 5-point
recipients %>% count(FEMALowerHigher) %>%
  mutate(percent = round(n / sum(n) * 100)) %>% print(n = 30)

# collapsed to 3 categories (About / Lower / Higher) -> 40 / 33 / 27
recipients %>%
  mutate(vs_exp = case_when(
    str_detect(FEMALowerHigher, regex("lower|less",       ignore_case = TRUE)) ~ "Lower than expected",
    str_detect(FEMALowerHigher, regex("higher|more",      ignore_case = TRUE)) ~ "Higher than expected",
    str_detect(FEMALowerHigher, regex("expected|about",   ignore_case = TRUE)) ~ "About as expected",
    TRUE ~ NA_character_)) %>%
  count(vs_exp) %>% mutate(percent = round(n / sum(n) * 100)) %>% print()

cat("\n--- done ---\n")


### In interpreting these findings, one point to note is that most respondents are not well informed about current levels of disaster assistance. For instance, in answer to a separate question, respondents report believing the existing FEMA grant program would cover \$XX of a \$75,000 loss, well above the median 7\% (\$5,250) that an IHP grant provides for this size of loss \$YYYY. Even respondents that have experienced a disaster and applied for FEMA aid tend to systematically over-estimate current generosity of disaster aid. This echoes previous findings that disaster insurance and assistance programs are generally not well understood by the public. 

est %>%
  distinct(ResponseID, .keep_all = TRUE) %>%
  filter(is.na(not_sure) | not_sure != 1) %>%
  summarise(
    n_answered    = sum(!is.na(Beliefs_FEMA_Amount)),
    median_belief = median(Beliefs_FEMA_Amount, na.rm = TRUE),
    mean_belief   = mean(Beliefs_FEMA_Amount,   na.rm = TRUE)
  )

# Even among these respondents with disaster experience who applied and received funds (a group that should have a greater knowledge of disaster aid programs)...
# =============================================================================
# SUBGROUP — experienced applicants who RECEIVED FEMA funds
#   the group that should know best: do they still over-estimate generosity?
# =============================================================================
# =============================================================================
# Belief over-estimation among the most-informed subgroup:
#   disaster experience (DisasterExperience == 1) AND applied + received FEMA funds.
# Stays entirely on sample_trunc (respondent level) — NO joins, so no .x/.y can
# arise. Belief columns are native to `sample`; do not touch the est/hyp panel.
# =============================================================================

stopifnot(!anyDuplicated(sample_trunc$ResponseID))   # base is 1 row/respondent

RECEIVED_LAB <- "Yes, I applied and I was approved and I received funds"
bench_idx    <- match(BENCH_BIN, bin_levels)

informed_grp <- sample_trunc %>%
  filter(DisasterExperience == 1, AppliedFEMA == RECEIVED_LAB)

cat("\n--- experienced + received FEMA funds ---\n")
cat("n =", nrow(informed_grp), "\n")   # expect ~131

# do the two definitions diverge? (any received-funds coded Experience != 1?)
sample_trunc %>%
  filter(AppliedFEMA == RECEIVED_LAB) %>%
  count(DisasterExperience) %>% print()

# --- (a) share who over-estimate vs the $5k-$9,999 benchmark -----------------
informed_grp %>%
  mutate(
    bin = factor(FEMAAmount_Dollar, levels = bin_levels),
    idx = match(as.character(bin), bin_levels),
    position = case_when(
      is.na(bin)             ~ "missing",
      bin == "I am not sure" ~ "not sure",
      idx >  bench_idx       ~ "above benchmark",
      idx == bench_idx       ~ "at benchmark",
      idx <  bench_idx       ~ "below benchmark"
    )
  ) %>%
  count(position) %>%
  mutate(share = round(100 * n / sum(n), 1)) %>%
  print()

# --- (b) median / mean numeric belief, dropping "not sure" ------------------
informed_grp %>%
  filter(is.na(Beliefs_FEMA_Amount_NotSure) | Beliefs_FEMA_Amount_NotSure != 1) %>%
  summarise(
    n_answered    = sum(!is.na(Beliefs_FEMA_Amount)),
    median_belief = median(Beliefs_FEMA_Amount, na.rm = TRUE),
    mean_belief   = mean(Beliefs_FEMA_Amount,   na.rm = TRUE)
  ) %>%
  print()

############################################
#--------------------------------------------------------------------------------------------------
#            TABLE: Effect of Responsibility on Recommended Post-Disaster Aid (w controls)    
#--------------------------------------------------------------------------------------------------

# 1/22 A regression of recommended aid on respondent’s demographic characteristics, controlling for respondent’s perceptions of responsibility
#shows that these individuals are more likely to be X, Y and Z*.
hyp = readRDS("data/data_updated.rds")
m <- feols(
  percent_aid ~ i(resp, ref = 5) +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped + Party + Race2 +
    RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
summary(m)

# Make Latex table with kable

library(fixest)
library(knitr)

# ---- 1. Coefficients -> "est*** (se)" cells ----
ct    <- coeftable(m)
nm    <- rownames(ct)
stars <- as.character(cut(ct[, 4], c(-Inf, .001, .01, .05, Inf),
                          labels = c("***", "**", "*", "")))
cells <- sprintf("%.3f%s (%.3f)", ct[, 1], stars, ct[, 2])

# ---- 2. Row labels ----
# Left-hand names must match names(coef(m)) exactly -- check and edit as needed
demo_map <- c(
  "DisasterExperienceYes"               = "Has disaster experience",
  "GenderMale"                          = "Gender: Male",
  "GenderOther"                         = "Gender: Other",
  "AgeGroup18-24"                       = "Age: 18--24",
  "AgeGroup45-64"                       = "Age: 45--64",
  "AgeGroup65 or older"                 = "Age: 65 or older",
  "AnnualIncome_grouped<$100,000"       = "Income $<$ \\$100{,}000",
  "AnnualIncome_grouped>$250,000"       = "Income $>$ \\$250{,}000",
  "PartyDemocrat"                       = "Party: Democrat",
  "PartyRepublican"                     = "Party: Republican",
  "PartyOther"                          = "Party: Other",
  "Race2Black or African American"      = "Race: Black or African American",
  "Race2White"                          = "Race: White",
  "RiskAversion_binRisk tolerant"       = "Risk tolerant",
  "RiskAversion_binRisk averse"         = "Risk averse",
  "GovTrustBinLow"                      = "Low government trust",
  "gap_quartileQ1"                      = "Least tax progressive",
  "gap_quartileQ4"                      = "Most tax progressive"
)

is_int  <- nm == "(Intercept)"
is_resp <- grepl("^resp::", nm)
is_demo <- !is_int & !is_resp

lab <- nm
lab[is_int]  <- "Constant"
lab[is_resp] <- sub("^resp::", "Responsibility = ", nm[is_resp])
hit <- nm %in% names(demo_map)
lab[hit] <- demo_map[nm[hit]]

unmapped <- is_demo & !hit
if (any(unmapped)) {
  warning("No label for: ", paste(nm[unmapped], collapse = " | "))
  lab[unmapped] <- gsub("([_$%&#])", "\\\\\\1", nm[unmapped])  # LaTeX-safe fallback
}

coef_rows <- data.frame(term = lab, est = cells)

# ---- 3. Assemble body + fit statistics ----
body <- rbind(
  coef_rows[is_int, ],
  data.frame(term = "\\textit{Perceived responsibility (ref = 5)}", est = ""),
  coef_rows[is_resp, ],
  data.frame(term = "\\textit{Demographic characteristics}", est = ""),
  coef_rows[is_demo, ]
)

stats <- data.frame(
  term = c("S.E. clustered by", "Observations", "Adj. $R^2$", "RMSE"),
  est  = c("ResponseID",
           format(nobs(m), big.mark = ","),
           sprintf("%.3f", r2(m, "ar2")),
           sprintf("%.1f", sqrt(mean(resid(m)^2))))
)

df <- rbind(body, stats)

# ---- 4. kable -> tabular ----
tab <- kable(df, format = "latex", booktabs = FALSE, escape = FALSE,
             align = "lc", vline = "", linesep = "", row.names = FALSE,
             col.names = c("", "\\multicolumn{1}{c}{percent aid}"))

lines <- strsplit(as.character(tab), "\n")[[1]]
lines <- lines[nzchar(lines)]

# double rules top and bottom
hl <- which(lines == "\\hline")
lines[hl[1]]          <- "\\hline\\hline"
lines[hl[length(hl)]] <- "\\hline\\hline"

# rule before fit statistics
se_row <- grep("^S\\.E\\. clustered by", lines)
lines  <- append(lines, "\\hline", after = se_row - 1)

# notes before \end{tabular}
notes <- c(
  "\\multicolumn{2}{l}{\\footnotesize Notes: The reference respondent is categorically neutral as indicated in the main text.}\\\\",
  "\\multicolumn{2}{l}{\\footnotesize Significance: *** $p<0.001$, ** $p<0.01$, * $p<0.05$}\\\\"
)
end_tab <- grep("^\\\\end\\{tabular\\}", lines)
lines   <- append(lines, notes, after = end_tab - 1)

# ---- 5. Wrap in table environment ----
out <- c(
  "\\begin{table}[H]\\centering",
  "\\caption{Effect of Demographic Characteristics on Recommended Post-Disaster Aid}",
  "\\label{sitab:demographics_aid}",
  lines,
  "\\end{table}"
)

cat(out, sep = "\n")
writeLines(out, "tables/si_demographics_aid.tex")

#####################################################


#--------------------------------------------------------------------------------------------------
#            TABLE: PERCEIVED RESPONSIBILITY & PROBABILITY OF RECOMMENDING ZERO AID    
#--------------------------------------------------------------------------------------------------

# Logit
df <- hyp %>%
  dplyr::mutate(
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

vc <- vcovCL(m1, cluster = df$ResponseID, type = "HC1")
coeftest(m1, vcov. = vc)

########################################################
#-------------------------------------------------------------------------------
#             TABLE and FIGURE : REASONS FOR RECOMMENDING NO AID ACROSS SCENARIOS          
#-------------------------------------------------------------------------------

library(stringi)

# ---- 0. helpers -------------------------------------------------------------

normalize_reason <- function(x) {
  x %>%
    str_squish() %>%
    str_to_lower() %>%
    stri_trans_general("Any-Latin; Latin-ASCII") %>%
    str_replace_all("['`\u2019]", "'")
}

alt <- function(...) paste0(c(...), collapse = "|")


# ---- multi-label rules ---------------------------------------------------

reason_rules <- list(
  # ---- arguments about the claimant -----------------------------------------
  mitigation = alt(
    "\\b(prepar|prevent|precaut|mitigat|preemptive|proactive|safeguard)",
    "\\bfire[\\s-]*proof", "\\bprotect", "\\bvegetation", "\\bbrush", "\\bshrub",
    "\\broof\\b", "\\bmaintenance", "\\bmaintain", "\\bdefensible\\s+space",
    "\\bclear(ed|ing)?\\s+(the\\s+)?(brush|vegetation)",
    "\\bclean(ed|ing)?\\s+(up\\s+)?around", "\\bsprinkler", "\\bdue\\s+diligence",
    "\\btook\\s+no\\s+(action|steps|precaution|measures)",
    "\\bdid\\s+(nothing|not|n.?t)\\s+.{0,25}(prevent|protect|mitigat|prepar|help|fix|improve)",
    "\\b(could|should)\\s+have\\s+done\\s+(more|those\\s+things|something)"
  ),
  foreknowledge = alt(
    "\\bkn[eo]w[s]?\\s+(the\\s+|about\\s+|it\\s+was\\s+|that\\s+)?(risk|possib|wildfire|fire|threat|danger)",
    "\\b(was|were)\\s+aware\\s+(of|even|that)", "\\bforesee?able",
    "\\bcho(o)?se\\s+to\\s+(live|purchase|buy)", "\\bhave\\s+chosen\\s+to",
    "\\btook\\s+a\\s+(chance|risk)", "\\bgambled", "\\bhappened\\s+before",
    "\\bprevious(ly)?\\s+fire", "\\balready\\s+had\\s+a\\s+fire", "\\brecurring\\s+event",
    "\\bexperienced?\\s+(a\\s+)?(previous|past|prior)",
    "\\bhigh[\\s-]*risk\\s+area", "\\bfire\\s+(prone|zone)", "\\bat[\\s-]*risk.{0,10}area",
    "\\bif\\s+you\\s+(live|know\\s+you\\s+live|choose)", "\\bassume\\s+the\\s+risk",
    "\\binformed\\s+decision"
  ),
  insurance = alt("\\binsur\\w*", "\\buninsured", "\\bcoverage", "\\bpolicy\\b"),
  second_home = alt(
    "\\bvacation\\s+(home|property|house)", "\\bsecond(ary)?\\s+(home|house|property)",
    "\\b2nd\\s+home", "\\bnot\\s+(his|her|a|the)?\\s*primary\\s+(home|resid|house|propert)",
    "\\bprimary\\s+resid.{0,30}only", "\\bhas\\s+a\\s+primary\\s+(home|residence)",
    "\\bluxury\\s+not\\s+a\\s+need", "\\bnot\\s+a\\s+necessity",
    "\\b(has|owns)\\s+another\\s+(home|house|place|residence|property)"
  ),
  financial_means = alt(
    "\\b(owns|has)\\s+(two|2|multiple|numerous|more\\s+than\\s+one)\\s+(home|house|propert)",
    "\\bhas\\s+two\\s+houses", "\\bowns\\s+2\\s+homes",
    "\\b(can|could)\\s+afford", "\\bafford\\s+(two|2|insurance|it|the\\s+damage)",
    "\\bfinancial(ly)?\\s+(means|resources|rich)", "\\bseems?\\s+(relatively\\s+)?well\\s+off",
    "\\bhas\\s+(other\\s+assets|resources|money)", "\\brich\\s+person",
    "\\bexpensive\\s+propert", "\\bpay\\s+for\\s+(the\\s+)?damages?\\s+(him|her)self",
    "\\bable\\s+to\\s+(pay|cover)"
  ),
  responsibility = alt(
    "\\bresponsib", "\\bfault\\b", "\\bnegligen", "\\baccountable", "\\bliable\\b",
    "\\bhis\\s+loss", "\\bhis\\s+to\\s+lose", "\\bown\\s+(fault|risk|bills|belongings|problem)",
    "\\bowner\\s+risk", "\\bhomeowner.?s?\\s+(error|expense)",
    "\\bpoor\\s+planning", "\\bcareless", "\\blazy\\b", "\\bstupidity",
    "\\bdoesn.?t\\s+deserve", "\\bpay\\s+your\\s+own"
  ),
  
  # ---- arguments about the state --------------------------------------------
  public_burden = alt(
    "\\btax\\s*payer", "\\btaxes\\b", "\\btax\\s+(dollars|money|funds)",
    "\\bmy\\s+tax", "\\bour\\s+(tax|money)", "\\bhard\\s+earned\\s+money",
    "\\bwhy\\s+should\\s+(i|we)\\s+have\\s+to\\s+pay",
    "\\b(other\\s+people|we)\\s+should(n.?t| not)?\\s+(have\\s+to\\s+)?pay",
    "\\bcan.?t\\s+afford\\s+(this|all)", "\\bgreater\\s+needs",
    "\\bnot\\s+(the\\s+)?gov(.?t|ernment)", "\\bgov(.?t|ernment).{0,40}(not|shouldn.?t|isn.?t)",
    "\\bnot\\s+their\\s+job", "\\bjob\\s+is(n.?t)?\\s+to", "\\bhand\\s+out\\s+money",
    "\\bnot\\s+(a\\s+)?charity", "\\bnot\\s+for\\s+them\\s+to\\s+do", "\\bno\\s+handouts",
    "\\bsugar\\s+daddy", "\\bnursemaid", "\\bsocialist",
    "\\blegitimate\\s+function\\s+of\\s+government", "\\banother\\s+fund"
  ),
  moral_hazard = alt(
    "no\\s+one\\s+would\\s+buy\\s+insurance", "more\\s+people\\s+would\\s+be\\s+uninsured",
    "(bail(ed)?\\s+(everyone|them)\\s+out).{0,60}(insur|no\\s+one)",
    "then\\s+more\\s+people\\s+would"
  ),
  
  # ---- response quality (not arguments) -------------------------------------
  conditional_loan = alt(
    "\\blow[\\s-]*interest\\s+loan", "\\bloans?\\s+(maybe|but\\s+not|not\\s+grants?)",
    "\\btax\\s+free\\s+loans?", "\\b(0|zero)%?\\s+interest",
    "\\bsome\\s+money\\s+but\\s+not\\s+all", "\\bhelp\\s+(assist\\s+)?with\\s+a\\s+little",
    "\\bpartially\\b", "\\bnot\\s+fully\\s+replace"
  ),
  misread = alt(
    "\\b(he|she|alex|sam)\\s+ha[sd]\\s+insurance",
    "\\b(his|her)\\s+insurance\\s+(should|will|would)\\s+cover",
    "insurance\\s+(will|should)\\s+cover\\s+(this|his|her)",
    "had\\s+necessary\\s+precautions", "did\\s+prep\\s+his\\s+house",
    "was\\s+it\\s+insured\\s+or\\s+not", "not\\s+equal\\s+to\\s+flood",
    "he\\s+did\\s+what\\s+he\\s+could",
    "should\\s+be\\s+funded\\s+by\\s+the\\s+government",
    "government\\s+should\\s+provide"
  ),
  unclear = alt(
    "^\\s*(n/?a|none|no|idk|unsure|neutral|because|\\.|-)\\s*$",
    "\\bi\\s*(don.?t|do\\s+not)\\s+know", "\\bnot\\s+sure", "\\bi.?m\\s+unsure",
    "\\bno\\s+particular\\s+reason", "\\bdon.?t\\s+care", "\\bonly\\s+two\\s+answers",
    "\\bhad\\s+to\\s+pick\\s+one", "\\bseemed\\s+like\\s+the\\s+right",
    "\\bbecause\\s+i\\s+can", "\\bpersonal\\s+opinion", "\\bnot\\s+relevant\\s+to\\s+me",
    "\\bthat.?s\\s+life", "\\bbad\\s+luck", "\\bact\\s+of\\s+(nature|god)",
    "\\bnatural\\s+cause", "\\bit.?s\\s+true", "\\bmakes\\s+no\\s+sence"
  )
)

arg_claimant <- paste0("arg_", c("mitigation","foreknowledge","insurance",
                                 "second_home","financial_means","responsibility"))
arg_state    <- paste0("arg_", c("public_burden","moral_hazard"))
arg_quality  <- paste0("arg_", c("conditional_loan","misread","unclear"))
flag_names   <- c(arg_claimant, arg_state, arg_quality)

flag_names <- paste0("arg_", names(reason_rules))

classify_multi <- function(x) {
  r <- normalize_reason(x)
  m <- sapply(reason_rules, function(p) str_detect(r, regex(p, ignore_case = TRUE)))
  m <- matrix(as.logical(m), nrow = length(r), dimnames = list(NULL, flag_names))
  m
}


# ---- 7. code and diagnose ---------------------------------------------------

coded <- reason_by_scenario %>%
  mutate(reason_raw = normalize_reason(NoGovCompensate_Reason))

flags <- classify_multi(coded$reason_raw)

coded2 <- bind_cols(coded, as_tibble(flags)) %>%
  mutate(n_flags = rowSums(flags),
         uncoded = n_flags == 0)

mean(coded2$uncoded)                      # target well under 10%
table(coded2$n_flags)

coded2 %>% filter(uncoded) %>% pull(reason_raw) %>% head(60) %>% writeLines()

# ---- 8. the figure table: share invoking each argument, by cell -------------

cells <- hyp %>%
  mutate(ResponseID = as.character(ResponseID)) %>%
  distinct(ResponseID, scenario_id, second_home, prior_info, adaptive_measures)

by_cell <- coded2 %>%
  left_join(cells, by = c("ResponseID", "scenario_id")) %>%
  group_by(second_home, prior_info, adaptive_measures) %>%
  summarise(n = n(),
            across(all_of(flag_names), mean),
            uncoded = mean(uncoded),
            .groups = "drop")

print(by_cell, width = Inf)

# long form for plotting
by_cell_long <- by_cell %>%
  filter(!is.na(second_home)) %>%
  pivot_longer(all_of(flag_names), names_to = "argument", values_to = "share") %>%
  mutate(argument = str_remove(argument, "^arg_"),
         cell = case_when(
           second_home == 0 & prior_info == 0 & adaptive_measures == 0 ~ "Base case",
           second_home == 0 & prior_info == 1 & adaptive_measures == 0 ~ "Primary × info",
           second_home == 0 & prior_info == 1 & adaptive_measures == 1 ~ "Primary × info × adapt",
           second_home == 1 & prior_info == 0 & adaptive_measures == 0 ~ "Second home",
           second_home == 1 & prior_info == 1 & adaptive_measures == 0 ~ "Second home × info",
           second_home == 1 & prior_info == 1 & adaptive_measures == 1 ~ "Second home × info × adapt"
         ))


# ---- A. every response with its text and the flags that fired ---------------
check <- coded2 %>%
  left_join(cells, by = c("ResponseID", "scenario_id")) %>%
  mutate(
    cell = case_when(
      second_home == 0 & prior_info == 0 & adaptive_measures == 0 ~ "Base case",
      second_home == 0 & prior_info == 1 & adaptive_measures == 0 ~ "Primary × info",
      second_home == 0 & prior_info == 1 & adaptive_measures == 1 ~ "Primary × info × adapt",
      second_home == 1 & prior_info == 0 & adaptive_measures == 0 ~ "Second home",
      second_home == 1 & prior_info == 1 & adaptive_measures == 0 ~ "Second home × info",
      second_home == 1 & prior_info == 1 & adaptive_measures == 1 ~ "Second home × info × adapt",
      TRUE ~ NA_character_
    ),
    args = pmap_chr(across(all_of(flag_names)),
                    function(...) {
                      v <- c(...)
                      if (!any(v)) "—" else paste(str_remove(flag_names[v], "^arg_"), collapse = ", ")
                    })
  ) %>%
  dplyr::select(ResponseID, cell, n_flags, args, NoGovCompensate_Reason)

# write it out and read it in Excel — easiest way to actually check 712 rows
# write_csv(check %>% arrange(cell, desc(n_flags)), "zero_text_unfilled.csv")

# I've now hand-coded each response into the categories:
text = read.csv("data/zero_text_filled.csv")

head(text)

library(tidyverse)

arg_labels <- c(
  mitigation     = "Failed to mitigate",
  foreknowledge  = "Had foreknowledge of risk",
  insurance      = "Should have bought insurance",
  responsibility = "Personal responsibility",
  public_burden  = "Taxpayer cost / public burden",
  need           = "Need",
  wealth         = "Can afford it / wealthy",
  unclear        = "Unclear / No reason given"
)

cell_labels <- c(
  "Base case"                  = "Base Case",
  "Primary × info"             = "Primary residence, had prior info but did not adapt",
  "Primary × info × adapt"     = "Primary residence, had prior info and adapted",
  "Second home"                = "Second home",
  "Second home × info"         = "Second home, had prior info but did not adapt",
  "Second home × info × adapt" = "Second home, had prior info and adapted"
)

# who gave "no aid" in both of their scenarios
both_ids <- text %>%
  filter(!is.na(cell)) %>%
  distinct(ResponseID, cell) %>%
  count(ResponseID) %>%
  filter(n >= 2) %>%
  pull(ResponseID)

# relabel, then bolt on the "both" pseudo-cell
base <- text %>%
  filter(!is.na(cell)) %>%
  mutate(cell = unname(cell_labels[cell]))

dat <- bind_rows(
  base,
  base %>% filter(ResponseID %in% both_ids) %>% mutate(cell = "No aid in BOTH scenarios")
)

cell_order <- c("No aid in BOTH scenarios", unname(cell_labels))

long <- dat %>%
  filter(args != "—") %>%
  separate_rows(args, sep = ",\\s*") %>%
  mutate(args = str_squish(args),
         args = if_else(args %in% c("second_home", "financial_means"),
                        "asset_means", args)) %>%
  distinct(ResponseID, cell, args)

panel_n <- dat %>% distinct(ResponseID, cell) %>% count(cell, name = "N")

plot_df <- long %>%
  count(cell, args, name = "mentions") %>%
  group_by(cell) %>%
  mutate(p = mentions / sum(mentions)) %>%
  ungroup() %>%
  left_join(panel_n, by = "cell") %>%
  mutate(
    panel = factor(paste0(cell, "\n(N = ", N, ")"),
                   levels = rev(paste0(cell_order, "\n(N = ",
                                       panel_n$N[match(cell_order, panel_n$cell)], ")"))),
    args  = factor(arg_labels[args],
                   levels = rev(arg_labels[names(arg_labels) %in% unique(args)]))
  )

zeroplot <- ggplot(plot_df, aes(x = panel, y = p, fill = args)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = hcl.colors(8, "Dark 3")) +
  labs(x = NULL, y = "Share of reasons",
       fill = "Reason for recommending no aid") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 12)),
        legend.position = "right")

print(zeroplot)
ggsave("Figures/Supp/zeroreasons_7.30.png", zeroplot, width = 15, height = 8, dpi = 300)


# Among responses allocating zero aid who gave open-ended text explanations (\textit{n=701}),  who allocated zero aid ... "


####################################################

#--------------------------------------------------------------------------------------------------
#             FIGURE: SHARE OF RESPONDENTS RECOMMENDING ZERO AID BY SCENARIO, PRI AND SEC HOME         
#--------------------------------------------------------------------------------------------------

# ==============================================================================
# ZERO AID RECOMMENDATIONS BY HOME TYPE AND SCENARIO
# ==============================================================================

# Define scenarios
hyp <- hyp %>%
  dplyr::mutate(
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
  dplyr::mutate(home_type = ifelse(second_home == "primary", "Primary residence", "Second home"))

# Overall percentages (averaged across scenarios)
zero_aid_overall <- zero_aid_stats %>%
  group_by(home_type) %>%
  summarize(pct_zero = mean(pct_zero), .groups = "drop")

print(zero_aid_overall)

# By scenario (wide format for table)
zero_aid_table <- zero_aid_stats %>%
  dplyr::select(scenario, home_type, pct_zero) %>%
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
  dplyr::mutate(
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

# Save plot
ggsave(
  "Figures/Supp/zeroaid_byscenario_7.30.png",
  zero_aid_plot,
  width = 15,
  height = 7,
  dpi = 300
)



############################## 
# #--------------------------------------------------------------------------------------------------
# #          FIGURES S4 a and b : ZERO SLOPE ANALYSIS   
# #--------------------------------------------------------------------------------------------------
# 
# 
# dat=readRDS("C:\\Users\\indumati\\Downloads\\fran_data.rds")
# 
# #characterize individuals based on variation in government aid preferred with responsibility
# 
# aidcoef=function(dataset){
#   #regress government compensation on responsibility score - note there will be no remaining dof since only 2 observations per respondent
#   #if responsibility is the same then can't estimate an effect
#   if(dataset$resp[1]==dataset$resp[2]) return(NA)
#   mod=lm(gov_amt~resp,data=dataset)
#   return(mod$coefficients[2])
# }
# 
# dat$ResponseID=as.factor(dat$ResponseID)
# dat$resp=as.numeric(dat$resp)
# 
# result <- dat %>%
#   split(.$ResponseID) %>%
#   map(aidcoef) %>%
#   enframe(name = "ResponseID", value = "coef") %>%
#   dplyr::mutate(ResponseID = as.character(ResponseID)) %>%
#   dplyr::mutate(coef=unlist(coef))
# 
# #also look at individual's residuals from regression of aid on responsibility (treat as factor)
# 
# respmod=lm(gov_amt~resp,data = dat%>%dplyr::mutate(resp=as.factor(resp)))
# 
# resids=data.frame(ID=dat$ResponseID,resid=respmod$residuals)
# #take mean for each respondent
# resids=resids%>%
#   group_by(ID)%>%
#   dplyr::summarise(resid=mean(resid))
# 
# #merge with slope data
# inddat=merge(result%>%dplyr::mutate(ResponseID=as.factor(ResponseID)),resids,by.x="ResponseID",by.y="ID")
# 
# #identify respondents that only saw hypotheticals with primary or secondary home, not a mix
# rels=dat%>%
#   dplyr::select(ResponseID,second_home)%>%
#   group_by(ResponseID)%>%
#   dplyr::summarise(second_home_total=sum(second_home))%>%
#   filter(second_home_total%in%c(0,2))
# 
# a=ggplot(inddat%>%filter(ResponseID%in%rels$ResponseID), aes(resid/250*100, coef/250*100)) +
#   stat_density_2d(aes(fill = after_stat(density)),
#                   geom = "raster",
#                   contour = FALSE) +
#   theme_minimal() +
#   scale_fill_viridis_c(option = "magma",guide=FALSE) +
#   coord_cartesian(ylim = c(-50, 50)) + 
#   labs(x = "Residual Aid Allocation\n(% of $250,000 Loss)", y = "Slope of Aid Award with Individual Responsibility\n(% of $250,000 Loss per Unit Increase in Responsibility)")
# 
# countzeros=dim(inddat%>%filter(ResponseID%in%rels$ResponseID&coef!=0))[1]
# 
# b=ggplot(inddat%>%filter(ResponseID%in%rels$ResponseID&coef!=0),aes(coef/250*100))+geom_histogram(breaks=c(seq(-100,0,length.out=15),seq(0+100/14,100,length.out=14)))+theme_bw()+labs(x="Slope of Aid Award with Individual Responsibility\n(% of $250,000 Loss per Unit Increase in Responsibility)",y="Count")+
#   geom_segment(x=0,y=0,yend=countzeros,col="goldenrod",lwd=1.5)+coord_cartesian(ylim=c(0,400))

# ggsave("C:\\Users\\fmoore\\Box\\Davis Stuff\\Insurance\\Survey Paper\\Figures\\individualslopes_restrictedsample.pdf",plot=b)
# 
# ggsave("C:\\Users\\fmoore\\Box\\Davis Stuff\\Insurance\\Survey Paper\\Figures\\individualdensities_restrictedsample.pdf",plot=a)




#--------------------------------------------------------------------------------------------------
#          TABLES: EFFECT OF SCENARIOS ON LEVEL OF RESP, PERCAID
#--------------------------------------------------------------------------------------------------



hyp$resp = as.numeric(hyp$resp)

# RESP ---
m_resp <- feols(
  resp ~ info_arm * second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
) # resp controls: yes hazard: no

m_resp_hazard <- feols(
  resp ~ info_arm * second_home +
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile+hazard,
  data = hyp,
  vcov = ~ResponseID
) #resp controls: yes hazard: yes

m_resp_simple <- feols(
  resp ~ info_arm * second_home,
  data = hyp,
  vcov = ~ResponseID
) #resp controls: no hazard: no

m_resp_simple_hazard <- feols(
  resp ~ info_arm * second_home +hazard,
  data = hyp,
  vcov = ~ResponseID
) #resp controls:no hazard: yes

etable(m_resp, m_resp_hazard, m_resp_simple, m_resp_simple_hazard)

# PERCAID---
m_percaid <- feols(
  percent_aid ~  info_arm * second_home + 
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile,
  data = hyp,
  vcov = ~ResponseID
)
m_percaid_hazard <- feols(
  percent_aid ~  info_arm * second_home + 
    DisasterExperience + Gender + AgeGroup + AnnualIncome_grouped +
    Party + Race2 + RiskAversion_bin + GovTrustBin + gap_quartile + hazard,
  data = hyp,
  vcov = ~ResponseID
)

m_percaid_simple <- feols(
  percent_aid ~ info_arm * second_home,
  data = hyp,
  vcov = ~ResponseID
)

m_percaid_simple_hazard <- feols(
  percent_aid ~ info_arm * second_home + hazard,
  data = hyp,
  vcov = ~ResponseID
)

etable(m_percaid, m_percaid_hazard, m_percaid_simple, m_percaid_simple_hazard)
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
summary(m_resp_prior_risk)

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

hypotheses(
  m_resp_gt,
  "`adaptive_measures:GovTrustBinLowgovernmenttrust` = 0"
)

hypotheses(
  m_percaid_gt,
  "`adaptive_measures:GovTrustBinLowgovernmenttrust` = 0"
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
#     MEDIATION
#######################

# --------------------------------------------#
# ---    FIGURE: MEDIATION Fixed Effects    --#
# --------------------------------------------#
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

library(dplyr)
library(ggplot2)
library(sandwich)

LAB <- c(X1 = "X1 Prior info only\nvs. baseline",
         X2 = "X2  Prior info + adaptation\nvs. prior info only",
         X3 = "X3  Second home\nvs. primary residence")

# ---- FE robustness: respondent fixed effects -------------------------

# Gate check: do info_arm / second_home vary within respondent?
# If either shows ~0 here, person-FE won't identify that treatment.
est %>%
  group_by(ResponseID) %>%
  summarise(n_info = n_distinct(info_arm), n_home = n_distinct(second_home)) %>%
  summarise(share_info_varies = mean(n_info > 1),
            share_home_varies = mean(n_home > 1))

est$ResponseID_f <- factor(est$ResponseID)

m_M_fe <- lm(resp ~ info_arm + second_home + hazard + ResponseID_f, est)
m_Y_fe <- lm(percent_aid ~ info_arm * resp + second_home * resp + hazard + ResponseID_f, est)

run_med_fe <- function(treat, from, to, sims = 1000) {
  set.seed(2026)
  mediation::mediate(m_M_fe, m_Y_fe, treat = treat, mediator = "resp",
                     control.value = from, treat.value = to,
                     sims = sims, cluster = est$ResponseID)
}

med_X1_fe <- run_med_fe("info_arm",    "none",    "info")
med_X2_fe <- run_med_fe("info_arm",    "info",    "info_adapt")
med_X3_fe <- run_med_fe("second_home", "primary", "second")

results_fe <- bind_rows(
  tidy_med(med_X1_fe, "X1: prior info vs. none (FE)"),
  tidy_med(med_X2_fe, "X2: adaptation vs. prior info only (FE)"),
  tidy_med(med_X3_fe, "X3: second home vs. primary (FE)"))

print(results_fe, digits = 3)

# side-by-side
bind_rows(results %>% mutate(spec = "covariate-adjusted"),
          results_fe %>% mutate(spec = "person FE"))

# FE Figure
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

dec <- bind_rows(pull_long(med_X1_fe, LAB["X1"]),
                 pull_long(med_X2_fe, LAB["X2"]),
                 pull_long(med_X3_fe, LAB["X3"]))


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


fig1_fe<- ggplot(dec, aes(x = est, y = quantity, colour = quantity)) +
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

fig1_fe
ggsave("mediation_fe.png", fig1_fe,
       width = 6.8, height = 6.4, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# ##### DIFFERENT MODEL INCORPORATING SECOND HOME * INFO ARM - comment out when done
# # ---- 5. Is second-home responsibility amplified by the adaptation arm? -------
# # First-stage moderated mediation: add second_home × info_arm to the mediator
# # model. That interaction IS the "more able to adapt" story.  m_Y unchanged.
# m_M2 <- lm(as.formula(paste("resp ~ info_arm * second_home +", rhs)), est)
# summary(m_M2)   # info_adapt:second coef = extra responsibility pinned on 2nd-home owners
# 
# # Carry it through to aid: second-home mediation evaluated within each info arm.
# run_sh <- function(arm, sims = 1000) {
#   set.seed(2026)
#   mediation::mediate(m_M2, m_Y, treat = "second_home", mediator = "resp",
#                      control.value = "primary", treat.value = "second",
#                      covariates = list(info_arm = arm),
#                      sims = sims, cluster = est$ResponseID)
# }
# med_sh_info  <- run_sh("info")
# med_sh_adapt <- run_sh("info_adapt")
# summary(med_sh_info); summary(med_sh_adapt)   # ACME of second-home in each arm
# 
# # Does the indirect effect actually differ between plain-info and adaptation?
# set.seed(2026)
# mediation::test.modmed(med_sh_info,
#                        covariates.1 = list(info_arm = "info"),
#                        covariates.2 = list(info_arm = "info_adapt"),
#                        sims = 1000)
# 


#-------------------------------------------------------------------------------
#                  FIGURE: MEDIATION HETEROGENEITY                            |        
#-------------------------------------------------------------------------------
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
  mutate(
    split_var = factor(
      split_var,
      levels = c("All", split_vars),
      labels = c(
        "All",
        "Political Party",
        "Tax Progressivity",
        "Annual Income",
        "Risk Aversion",
        "Disaster Experience",
        "Government Trust"
      )
    ),
    lab = paste0(level, "  (n = ", n_resp, ")"),
    lab = sub("^Annual Income ", "", lab))
inc_lower <- function(x) {
  ifelse(grepl("<|less than|under", x, ignore.case = TRUE), 0,
         as.numeric(gsub("[^0-9]", "", sub(" to .*$| or more.*$", "", x))))
}
# y-axis order: keep each facet's current order, but sort income numerically
lab_lv <- pd %>%
  distinct(split_var, level, lab) %>%
  group_by(split_var) %>%
  mutate(within = if (first(split_var) == "Annual Income")
    inc_lower(level) else row_number()) %>%
  ungroup() %>%
  arrange(split_var, within) %>%
  pull(lab) %>%
  unique()

pd <- pd %>% mutate(lab = factor(lab, levels = rev(lab_lv)))
ref <- filter(pd, split_var == "All") %>% dplyr::select(contrast, effect, est)

het_full_plot = ggplot(pd, aes(est, lab, colour = effect)) +
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

# ggsave(het_full_plot, "figures/Mediation/mediation_heterogeneity.png", width = 15, height = 11, dpi = 600)


#-------------------------------------------------------------------------------
#                  FIGURE:  MEDIATION split by demographics                    |        
#-------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(scales)

all_eff <- bind_rows(pooled, het) %>%
  filter(!(split_var == "Party" & level == "Other")) %>%
  left_join(cnt, by = c("split_var", "level"))

# wide form: one row per split_var x level x contrast
dec <- all_eff %>%
  dplyr::select(split_var, level, contrast, effect, est) %>%
  pivot_wider(names_from = effect, values_from = est) %>%
  mutate(
    total_chk   = ACME + ADE,                      # sanity: should equal Total
    same_sign   = sign(ACME) == sign(ADE),
    prop_med    = ACME / Total,
    prop_lab    = ifelse(same_sign & abs(Total) > 1e-8,
                         percent(prop_med, accuracy = 1), "n.d."),
    tot_lab     = sprintf("%.1f pp", Total)
  )

stopifnot(max(abs(dec$total_chk - dec$Total), na.rm = TRUE) < 1e-6)

# long form for the stack, carrying the labels along
bars <- dec %>%
  dplyr::select(split_var, level, contrast, ACME, ADE, Total,
                prop_lab, tot_lab, same_sign) %>%
  pivot_longer(c(ACME, ADE), names_to = "effect", values_to = "est") %>%
  left_join(cnt, by = c("split_var", "level")) %>%
  mutate(
    split_var = factor(split_var,
                       levels = c("All", split_vars),
                       labels = c("All", "Political Party", "Tax Progressivity", "Annual Income",
                                  "Risk Aversion", "Disaster Experience", "Government Trust")),
    lab = sub("^Annual Income ", "", paste0(level, "  (n = ", n_resp, ")")),
    lab = factor(lab, levels = rev(lab_lv)),
    effect = factor(effect, levels = c("ADE", "ACME"))   # ACME on top of stack
  )

# in-bar segment labels: value + share, placed at each segment's midpoint
seg <- bars %>%
  group_by(split_var, lab, contrast) %>%
  arrange(desc(effect), .by_group = TRUE) %>%
  mutate(
    pos_run = cumsum(ifelse(est > 0, est, 0)),
    neg_run = cumsum(ifelse(est < 0, est, 0)),
    mid     = ifelse(est > 0, pos_run - est / 2, neg_run - est / 2),
    seg_lab = sprintf("%.1f", est)
  ) %>%
  ungroup()

# end-of-bar annotation: total effect and % mediated
ends <- dec %>%
  left_join(cnt, by = c("split_var", "level")) %>%
  mutate(
    split_var = factor(split_var,
                       levels = c("All", split_vars),
                       labels = c("All", "Political Party", "Tax Progressivity", "Annual Income",
                                  "Risk Aversion", "Disaster Experience", "Government Trust")),
    lab = sub("^Annual Income ", "", paste0(level, "  (n = ", n_resp, ")")),
    lab = factor(lab, levels = rev(lab_lv)),
    end_lab = paste0("Total ", tot_lab, " | ACME ", prop_lab),
    hj      = ifelse(Total >= 0, -0.08, 1.08)
  )

rng <- range(c(dec$Total, dec$ACME, dec$ADE), na.rm = TRUE)
pad <- diff(rng) * 0.45

ggplot(seg, aes(est, lab, fill = effect)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_col(width = .68) +
  geom_text(aes(x = mid, label = seg_lab), size = 2.7, colour = "white") +
  geom_text(data = ends,
            aes(x = Total, y = lab, label = end_lab, hjust = hj),
            inherit.aes = FALSE, size = 2.9, colour = "grey25") +
  facet_grid(split_var ~ contrast, scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(ACME = "blue", ADE = "orange"),
                    breaks = c("ACME", "ADE")) +
  scale_x_continuous(expand = expansion(mult = c(.25, .25))) +
  coord_cartesian(xlim = c(rng[1] - pad, rng[2] + pad)) +
  labs(x = "Decomposition of total effect on recommended aid (percentage points)",
       y = NULL, fill = NULL) +
  theme_bw(base_size = 13) +
  theme(strip.text.y    = element_text(angle = 0, size = 12),
        strip.text.x    = element_text(size = 12),
        axis.text.y     = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        legend.position = "top")

# ggsave("figures/Mediation/mediation_decomposition.png", width = 16, height = 11, dpi = 600)
##-------------
setDT(pd)
pd[grepl("Risk Aversion|Government Trust", split_var),
   .(split_var, level, contrast, effect,
     est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2))]

dcast(pd[split_var %in% c("Risk Aversion", "Government Trust")],
      split_var + level + contrast ~ effect,
      value.var = c("est", "lo", "hi"))
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

#-------------------------------------------------------------------------------
#                  FIGURE:  MEDIATION split by flood and fire                  |        
#-------------------------------------------------------------------------------

W_sub <- setdiff(W, "hazard")   # hazard is constant within each split

fit_hazard <- function(hz, sims = 1000) {
  
  est_h <- est %>% filter(hazard == hz) %>% as.data.frame()
  
  cells <- table(est_h$info_arm, est_h$second_home)
  if (any(cells == 0)) stop("empty design cell in ", hz)
  
  rhs_h <- paste(W_sub, collapse = " + ")
  
  fM <- as.formula(paste("resp ~ info_arm + second_home +", rhs_h))
  fY <- as.formula(paste("percent_aid ~ info_arm * resp + second_home * resp +", rhs_h))
  
  # do.call splices the formula and data in as values, so the stored call
  # has no free symbols for update() to resolve later
  mM <- do.call(lm, list(formula = fM, data = est_h))
  mY <- do.call(lm, list(formula = fY, data = est_h))
  
  cl <- est_h$ResponseID
  
  run <- function(treat, from, to) {
    set.seed(2026)
    mediation::mediate(mM, mY, treat = treat, mediator = "resp",
                       control.value = from, treat.value = to,
                       sims = sims, cluster = cl)
  }
  
  list(hazard = hz, n = nrow(est_h), n_id = dplyr::n_distinct(est_h$ResponseID),
       m_M = mM, m_Y = mY,
       med = list(X1 = run("info_arm",    "none",    "info"),
                  X2 = run("info_arm",    "info",    "info_adapt"),
                  X3 = run("second_home", "primary", "second")))
}
haz_levels <- levels(droplevels(factor(est$hazard)))
fits <- lapply(haz_levels, fit_hazard)
names(fits) <- haz_levels

results_hazard <- bind_rows(lapply(names(fits), function(hz) {
  f <- fits[[hz]]
  bind_rows(
    tidy_med(f$med$X1, "X1: prior info vs. none"),
    tidy_med(f$med$X2, "X2: adaptation vs. prior info only"),
    tidy_med(f$med$X3, "X3: second home vs. primary")) %>%
    mutate(hazard = hz, n = f$n, n_id = f$n_id, .before = 1)
}))

print(results_hazard, digits = 3)


# FLood v fire

W_sub  <- setdiff(W, "hazard")
rhs_sub <- paste(W_sub, collapse = " + ")

run_by_hazard <- function(haz) {
  d <- est %>% filter(hazard == haz)
  
  m_M <- lm(as.formula(paste("resp ~ info_arm + second_home +", rhs_sub)), d)
  m_Y <- lm(as.formula(paste("percent_aid ~ info_arm * resp + second_home * resp +", rhs_sub)), d)
  
  set.seed(2026)
  list(
    X1 = mediation::mediate(m_M, m_Y, treat = "info_arm", mediator = "resp",
                            control.value = "none", treat.value = "info", sims = 1000, cluster = d$ResponseID),
    X2 = mediation::mediate(m_M, m_Y, treat = "info_arm", mediator = "resp",
                            control.value = "info", treat.value = "info_adapt", sims = 1000, cluster = d$ResponseID),
    X3 = mediation::mediate(m_M, m_Y, treat = "second_home", mediator = "resp",
                            control.value = "primary", treat.value = "second", sims = 1000, cluster = d$ResponseID)
  )
}

res_flood <- run_by_hazard("flood")   # adjust these labels to match your coding of `hazard`
res_fire  <- run_by_hazard("fire")

dec <- bind_rows(
  pull_long(res_flood$X1, LAB["X1"]) %>% mutate(hazard = "Flood"),
  pull_long(res_flood$X2, LAB["X2"]) %>% mutate(hazard = "Flood"),
  pull_long(res_flood$X3, LAB["X3"]) %>% mutate(hazard = "Flood"),
  pull_long(res_fire$X1,  LAB["X1"]) %>% mutate(hazard = "Fire"),
  pull_long(res_fire$X2,  LAB["X2"]) %>% mutate(hazard = "Fire"),
  pull_long(res_fire$X3,  LAB["X3"]) %>% mutate(hazard = "Fire")
)
library(patchwork)

# strip text no longer needs the hazard tag — hazard is now shown once per block
strip_lab <- dec %>%
  filter(!is.na(pm)) %>%
  transmute(contrast, hazard,
            strip = sprintf("%s   \u00b7   %.0f%% via responsibility",
                            gsub("\n", " ", contrast), 100 * pm))
dec <- dec %>%
  dplyr::select(-any_of("strip")) %>%
  left_join(strip_lab, by = c("contrast", "hazard")) %>%
  mutate(contrast = factor(contrast, levels = c(LAB["X1"], LAB["X2"], LAB["X3"])),
         quantity = factor(quantity,
                           levels = c("Total effect", "Direct (ADE)", "Mediated (ACME)")))

n_flood <- nrow(est %>% filter(hazard == "flood"))
n_fire  <- nrow(est %>% filter(hazard == "fire"))
HAZ_TXT <- c(Flood = "#1F4E79", Fire = "#9C4A1A")

# exact fig1 styling, parameterized by hazard subset + title
make_fig1 <- function(data, title_text, title_colour, show_caption = FALSE) {
  data <- data %>%
    arrange(contrast) %>%
    mutate(strip = factor(strip, levels = unique(strip)))
  
  p <- ggplot(data, aes(x = est, y = quantity, colour = quantity)) +
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
    labs(x = "Effect on recommended aid", y = NULL, title = title_text) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(colour = title_colour, face = "bold",
                                        size = 12, hjust = .5, margin = margin(b = 8)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
      panel.spacing      = unit(1.1, "lines"),
      strip.text         = element_text(hjust = 0, face = "bold", size = 10.5,
                                        margin = margin(b = 6)),
      axis.text.y        = element_text(colour = "grey20", size = 9.8),
      axis.title.x       = element_text(margin = margin(t = 10), size = 9.8),
      plot.margin        = margin(12, 16, 4, 12)
    )
  
  if (show_caption) {
    p <- p + labs(caption = paste0(
      "Points are posterior means with 95% quasi-Bayesian intervals; 1,000 draws. ",
      sprintf("n = %d flood vignettes, n = %d fire vignettes.", n_flood, n_fire),
      "\nMediated + direct sum to the total effect.")) +
      theme(plot.caption = element_text(hjust = 0, colour = "grey45", size = 8,
                                        margin = margin(t = 12)))
  }
  p
}

fig_flood <- make_fig1(dec %>% filter(hazard == "Flood"), "Flood", HAZ_TXT["Flood"]) +
  theme(axis.title.x = element_blank())

fig_fire <- make_fig1(dec %>% filter(hazard == "Fire"), "Fire", HAZ_TXT["Fire"],
                      show_caption = TRUE)

fig_haz = fig_flood |fig_fire
fig_haz
ggsave("Figures/Mediation/med_haz.png", fig_haz,
       width = 13.6, height = 6.4, dpi = 300, bg = "white")

#######################


#-------------------------------------------------------------------------------
#                  FIGURE:  % distributions of policy questions                |        
#-------------------------------------------------------------------------------
#library(MASS)


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
# Separate Likert-scale from categorical choice variables
likert_vars <- c("GovRole", "GovRoleA", "GovRoleB", "GovInsur", "GovInsurA", 
                 "GovInsurB", "GovInsurC", "GovInsurD")
choice_vars <- c("PostDisasterGovAllocate", "PostDisasterGovUse")


person_vars <- c("ResponseID", likert_vars, choice_vars,
                 "DisasterExperience", "Party", "RiskAversion_bin",
                 "GovTrustBin", "gap_quartile", "RiskAversion")

person <- hyp %>%
  dplyr::select(all_of(person_vars)) %>%
  dplyr::distinct(ResponseID)

# must equal n_distinct(hyp$ResponseID); if larger, something above is not person-level
nrow(person); dplyr::n_distinct(hyp$ResponseID)
stopifnot(nrow(person) == dplyr::n_distinct(hyp$ResponseID))

bad_vars <- hyp %>%
  dplyr::select(all_of(person_vars)) %>%
  dplyr::group_by(ResponseID) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ dplyr::n_distinct(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = -ResponseID,
    names_to = "variable",
    values_to = "n_unique"
  ) %>%
  dplyr::filter(n_unique > 1)

bad_ids <- unique(bad_vars$ResponseID)

hyp %>%
  filter(ResponseID %in% bad_ids) %>%
  dplyr::select(ResponseID, all_of(person_vars)) %>%
  arrange(ResponseID) %>%
  as.data.frame()

person <- hyp %>%
  dplyr::select(all_of(person_vars)) %>%
  group_by(ResponseID) %>%
  summarise(across(everything(), ~ dplyr::first(na.omit(.x))), .groups = "drop")
#----


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
    "Voluntary home buyouts",
    "Taxes make insurance affordable for all",
    "Taxes make insurance affordable for low-income"
  )
  
  question_labels <- c(
    "GovInsur"  = "Higher insurance cost in riskier areas",
    "GovInsurA" = "Mandatory disaster insurance",
    "GovInsurB" = "Public disaster insurance option",
    "GovInsurC" = "Taxes make insurance affordable for all",
    "GovInsurD" = "Taxes make insurance affordable for low-income",
    "GovRole"   = "Restrict development in high-risk areas",
    "GovRoleA"  = "Require disaster-resistant construction",
    "GovRoleB"  = "Voluntary home buyouts",
    "PostDisasterGovAllocate" = "Preferred way government should allocate post-disaster aid",
    "PostDisasterGovUse"      = "Preferred way households should use post-disaster aid"
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

create_overall_heatmap(person, likert_vars)

# ggsave("Figures/question_dist_7.30.png", width = 12, height = 7, dpi = 300)

#--------------------------------------------------------------------------------------------------
#         TABLE: WEIGHTED policy agree and strongly disagree
#--------------------------------------------------------------------------------------------------


s  <- mean(hyp$DisasterExperience)
wt <- function(p) ifelse(hyp$DisasterExperience == 1, p / s, (1 - p) / (1 - s))

question_order <- c(
  "Restrict development in high-risk areas",
  "Require disaster-resistant construction",
  "Mandatory disaster insurance",
  "Higher insurance cost in riskier areas",
  "Public disaster insurance option",
  "Voluntary home buyouts",
  "Taxes make insurance affordable for all",
  "Taxes make insurance affordable for low-income"
)

likert_tab <- hyp %>%
  mutate(w_unw = 1, w_07 = wt(0.07), w_43 = wt(0.43)) %>%
  select(all_of(likert_vars), w_unw, w_07, w_43) %>%
  pivot_longer(all_of(likert_vars), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  mutate(agree = Response %in% c("Agree", "Strongly agree")) %>%
  group_by(Question) %>%
  summarise(across(starts_with("w_"), ~ 100 * sum(.x * agree) / sum(.x))) %>%
  mutate(
    Category = str_replace(question_categories[Question], "\n", " "),
    Question = question_labels[Question]
  ) %>%
  arrange(factor(Question, levels = question_order)) %>%
  select(Category, Question,
         `Unweighted (33%)` = w_unw, `7%` = w_07, `43%` = w_43)

knitr::kable(
  likert_tab, format = "latex", booktabs = TRUE, digits = 1,
  caption = "Percent agreeing or strongly agreeing with each policy under alternative population shares of disaster experience",
  label = "weight_likert"
)


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
  "GovRoleB"  = "Voluntary home buyouts",
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

regression_data <- person %>%
  dplyr::mutate(
    Party              = fct_relevel(factor(Party), "Independent"),
    GovTrustBin        = fct_relevel(factor(GovTrustBin), "Low government trust"),
    RiskAversion_bin   = fct_relevel(factor(RiskAversion_bin), "Risk neutral"),
    DisasterExperience = fct_relevel(factor(DisasterExperience), "0"),
    gap_quartile = fct_relevel(
      factor(gap_quartile,
             levels = c("Mid-range tax progressive",
                        "Least tax progressive",
                        "Most tax progressive")),
      "Mid-range tax progressive")
  ) %>%
  filter(!is.na(Party), !is.na(gap_quartile), !is.na(DisasterExperience),
         !is.na(RiskAversion_bin), !is.na(GovTrustBin))

nrow(regression_data)

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
  dplyr::mutate(
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
                               groups_to_plot = c("RiskAversion_bin", "gap_quartile", "DisasterExperience", "GovTrustBin", "Party")) {
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
    dplyr::mutate(
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
    filter(Group %in% unname(group_map[groups_to_plot])) %>%
    filter(!grepl("^PartyOther", Variable))  
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
  
  coef_data <- coef_data %>%
    mutate(Predictor_Clean = factor(Predictor_Clean, levels = names(pal)))
  
  # Dodge bars (and their error bars) side-by-side within each policy.
  # preserve = "single" keeps bar widths constant even where a facet row
  # has a different number of predictors, so nothing balloons or shrinks.
  dodge <- position_dodge(width = 0.8, preserve = "single")
  
  ggplot(coef_data,
         aes(x = Question_Wrapped, y = Coefficient,
             fill = Predictor_Clean, group = Predictor_Clean)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = .7) +
    geom_col(position = dodge, width = 0.75, alpha = .9, colour = NA) +
    geom_errorbar(aes(ymin = Coefficient - 1.96*SE,
                      ymax = Coefficient + 1.96*SE),
                  position = dodge, width = 0.2,
                  colour = "grey25", linewidth = 0.5, alpha = .85) +
    facet_grid(Group ~ Category, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = pal, drop = TRUE) +
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
  question_labels = question_labels)

print(policy_plot)

# ggsave("Figures/fig_policies_bar.png",  plot = policy_plot, width = 12, height = 12, dpi = 300)

#--------------------------------------------------------------------------------------------------
#         TABLE: ORDINAL regression results
#--------------------------------------------------------------------------------------------------

# ==============================================================================
# FUNCTION: CREATE POLICY PREFERENCE TABLE
# ==============================================================================

create_policy_table <- function(ordinal_results, question_labels, stat = c("ci", "se")) {
  
  stat <- match.arg(stat)
  
  # Define model categories and order
  model_categories <- list(
    "Government Mandates" = c("GovRole", "GovRoleA", "GovInsurA"),
    "Market Mechanisms"   = c("GovInsur", "GovInsurB"),
    "Public Subsidy"      = c("GovRoleB", "GovInsurC", "GovInsurD")
  )
  
  # Predictor labels.
  # Reference categories: Party = Independent, RiskAversion_bin = Risk neutral,
  # gap_quartile = Mid-range tax progressive, GovTrustBin = Low government trust.
  predictor_labels <- c(
    "DisasterExperience1"                 = "Disaster Experience",
    "PartyDemocrat"                       = "Democrat (vs Independent)",
    "PartyRepublican"                     = "Republican (vs Independent)",
    "RiskAversion_binRisk tolerant"       = "Risk Tolerant (vs Risk Neutral)",
    "RiskAversion_binRisk averse"         = "Risk Averse (vs Risk Neutral)",
    "GovTrustBinHigh government trust"    = "High Gov Trust",
    "gap_quartileLeast tax progressive"   = "Least tax progressive (vs Mid-range)",
    "gap_quartileMost tax progressive"    = "Most tax progressive (vs Mid-range)"
  )
  
  predictor_order <- names(predictor_labels)
  
  header_stat <- if (stat == "ci") "OR [95\\% CI]" else "OR (SE)"
  
  for (cat_name in names(model_categories)) {
    model_vars <- model_categories[[cat_name]]
    n_models   <- length(model_vars)
    
    # Start table
    cat("\\begin{tabular}[t]{>{\\raggedright\\arraybackslash}p{6.4cm}",
        paste(rep("c", n_models), collapse = ""), "}\n", sep = "")
    cat("\\toprule\n")
    
    # Header row
    cat("\\multicolumn{1}{c}{ } & \\multicolumn{", n_models, "}{c}{", header_stat, "} \\\\\n", sep = "")
    cat("\\cmidrule(l{3pt}r{3pt}){2-", n_models + 1, "}\n", sep = "")
    
    cat("\\multicolumn{", n_models + 1, "}{l}{\\textbf{", cat_name, "}} \\\\\n", sep = "")
    cat("\\cmidrule(l{3pt}r{3pt}){1-", n_models + 1, "}\n", sep = "")
    
    # Column headers
    col_headers <- vapply(model_vars, function(x) question_labels[[x]], character(1))
    cat("Predictor & ", paste(col_headers, collapse = " & "), " \\\\\n", sep = "")
    cat("\\midrule\n")
    
    # Extract coefficients for each predictor
    for (pred in predictor_order) {
      row_vals <- character(n_models)
      
      for (i in seq_along(model_vars)) {
        res <- ordinal_results[[model_vars[i]]]$results
        row_vals[i] <- ""
        if (is.null(res) || !(pred %in% res$Variable)) next
        
        cr <- res[res$Variable == pred, , drop = FALSE][1, ]
        sg <- if (is.na(cr$sig)) "" else as.character(cr$sig)
        
        row_vals[i] <- if (stat == "ci") {
          sprintf("%.2f [%.2f, %.2f]%s", cr$OR, cr$CI_lower, cr$CI_upper, sg)
        } else {
          sprintf("%.2f (%.3f)%s", cr$OR, cr$SE, sg)
        }
      }
      
      cat(predictor_labels[[pred]], " & ", paste(row_vals, collapse = " & "), " \\\\\n", sep = "")
      
      if (pred == "RiskAversion_binRisk averse") cat("\\addlinespace\n")
    }
    
    # Table footer
    cat(if (cat_name == "Public Subsidy") "\\bottomrule\n" else "\\midrule\n")
    cat("\\end{tabular}\n")
    
    if (cat_name != "Public Subsidy") {
      cat("\\vspace{0.5em}\n")
      cat("\\hspace*{-3cm}\n")
    }
  }
  
  cat("\\caption{Odds ratios (OR) with ",
      if (stat == "ci") "95\\% confidence intervals" else "standard errors (SE)",
      " for support of government interventions. Reference categories: Independent, ",
      "risk neutral, mid-range tax progressivity, low government trust. ",
      "The `Other' party category is estimated but omitted from the table.}\n", sep = "")
  cat("\\label{sitab:policy_ordinal_table}\n")
}

create_policy_table(ordinal_results, question_labels, stat = "se")
# ==============================================================================
# GENERATE TABLE
# ==============================================================================

create_policy_table(ordinal_results, question_labels, stat = "se")




#--------------------------------------------------------------------------------------------------
#                  FIGURE:  Disaster Experience by Type                                           |        
#--------------------------------------------------------------------------------------------------

#Script to generate summary statistics related to experience of disaster and aid 
source("cleaning.R")

sample=sample |> 
  rename(ResponseID=ResponseId)
disaster_sample = sample |> 
  semi_join(hyp, by = "ResponseID")
disaster_sample=disaster_sample%>%filter(DisasterExperience==1)

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

ggsave("Figures/Supp/disaster_type.png",
       plot = a, width = 12, height = 7, dpi = 300)

###


# Disaster type distribution
disaster_type_pct <- disaster_sample %>%
  count(DisasterType) %>%
  mutate(
    pct = 100 * n / sum(n)
  ) %>%
  arrange(desc(pct))

disaster_type_pct

high_damage_pct <- disaster_sample %>%
  filter(!is.na(HomeDamage_Dollar)) %>%
  summarise(
    pct_over_50k = 100 * sum(
      HomeDamage_Dollar %in% c("$50,000 – $99,999", "$100,000 or more")
    ) / n()
  )

high_damage_pct
