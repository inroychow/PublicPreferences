
We surveyed 2,000 households (via Cloud Research online survey administration) from across the continental U.S., screening for adults that were 1) owners of their residence and 2) responsible for household decisions. This analysis involves administering several randomized vignettes on preferences for disaster assistance.
---
## 1. System Requirements

### Software Dependencies

We use **RStudio version 4.4.3** to run all code. Required R packages are loaded within the scripts. This code can be run on a standard desktop or laptop computer with no specialized hardware required.

## 2. Installation Guide

### Instructions

1. Install **R**: https://cran.r-project.org/  
2. Install **RStudio**: https://posit.co/download/rstudio-desktop/ (typical install time is 5-10 minutes).
3. Clone or download this repository  
4. Open the project in RStudio
5. Install required packages, indicated in the individual script files
6. Set working directory to the project folder.

## Files and variables

### 3. Data

- **DisasterAssistance_Final.csv**: Raw survey data of all questions and answers.
-  **data.rds**: Survey dataset of hypothetical vignettes and demographic questions.
  
## 4. Code and software

These files should be run in order. 

- **useful_functions.R**: File of functions used in data cleaning and analysis.
- **cleaning.R**: Cleans raw data into usable format.
- **generate_hyp.R**: Generates main dataset for analysis.
- **fair_main_figures.R**: Generates main figures in paper. 
- **fair_supp_figures.R**: Generates supplementary figures in paper.
- **additional_analyses.R**: Generates additional analyses in paper. 
  
### Expected Output
- Analysis-ready dataset of hypothetical vignettes
- Main and supplementary figures generated and saved, as well as additional analyses referenced in the paper. 
- Expected run time is less than 20 minutes on a 16GB RAM computer.
