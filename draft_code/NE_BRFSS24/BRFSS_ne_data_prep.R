# BRFSS data loading and cleaning

## Package loading

pacman::p_load(foreign, tidyverse, forcats, survey, srvyr, lme4, lmerTest, knitr, 
               kableExtra, gt, gtsummary, scales, patchwork, ggeffects, palettes)

## Data loading

brfss_complete <- read.xport('BRFSS_2024.XPT')

sprintf("BRFSS data have %d records and %d number of variables.",
        nrow(brfss_complete),
        ncol(brfss_complete))

brfss_complete[1:5, 1:10] # just to get familiar with the dataset

# to be refined
sel_variables <- c(
  "X_PSU", "X_STSTR", "X_LLCPWT", # Weights
  "X_STATE", "X_AGE_G", "X_SEX", "X_RACE", "MARITAL", "VETERAN3", # Demographics
  "CERVSCRN", "CRVCLPAP", "CRVCLHPV", "HADMAM", "HADSIGM4", # Cancer screening (absolute)
  "SMALSTOL", "LCSCTSC1", "PSATEST1", "HADHYST2",
  "X_RFPAP37", "X_PAPHPV1", "X_HPV5YR1", "X_RFMAM23", "X_MAM402Y", # Cancer screening (recency / combined)
  "X_INCOMG1", "X_EDUCAG", "EMPLOY1", "RENTHOM1", # Socioeconomic
  "X_HLTHPL2", "X_HCVU654", "PERSDOC3", "MEDCOST1", # Insurance
  "X_RFSMOK3", "EXERANY2", "X_BMI5", "X_RFDRHV9" # Behavioral
)

brfss24 <- brfss_complete %>%
  select(any_of(sel_variables))


### Data cleaning

# state_labels <- c(
#   "1" = "Alabama", "2" = "Alaska", "4" = "Arizona", "5" = "Arkansas", "6" = "California", 
#   "8" = "Colorado", "9" = "Connecticut", "10" = "Delaware", "11" = "District of Columbia", 
#   "12" = "Florida", "13" = "Georgia", "15" = "Hawaii", "16" = "Idaho", "17" = "Illinois", 
#   "18" = "Indiana", "19" = "Iowa", "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana", 
#   "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota", 
#   "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", "32" = "Nevada", 
#   "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico", "36" = "New York", 
#   "37" = "North Carolina", "38" = "North Dakota", "39" = "Ohio", "40" = "Oklahoma", 
#   "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island", "45" = "South Carolina", 
#   "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont", 
#   "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", "55" = "Wisconsin", 
#   "56" = "Wyoming", "66" = "Guam", "72" = "Puerto Rico", "78" = "Virgin Islands"
# )

sub_labels <- c("6" = "California", "12" = "Florida", "23" = "Maine", "38" = "North Dakota", "17" = "Illinois", "48" = "Texas", "28" = "Mississippi")

sub_codes <- names(sub_labels)

# Filter the dataset
brfss24_sub <- brfss24 %>%
  filter(X_STATE %in% sub_codes)

sprintf("After subsetting, the dataset presents %d records and %d number of variables.",
        nrow(brfss24_sub),
        ncol(brfss24_sub))


# Cleaning and re-levelling the data:

brfss24_sub <- brfss24_sub %>%
  # transforming 7, 9, 77 and 99 values into NAs
  mutate(across(
    # except for variables where those values mean something
    .cols = c(where(is.numeric), where(is.character), 
              -X_RACE, -EMPLOY1, -X_STATE, -X_INCOMG1),
    .fns = ~ .x %>% 
      na_if(7) %>% 
      na_if(9) %>% 
      na_if(77) %>% 
      na_if(99)
  )) %>%
  mutate(
    X_STATE = factor(
      as.character(X_STATE),
      levels = sub_codes, # hidden chunk in knitted file
      labels = sub_labels
    ),
    X_SEX = factor(
      X_SEX,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    MARITAL = factor(
      MARITAL,
      levels = 1:6,
      labels = c(
        "Married",
        "Divorced",
        "Widowed",
        "Separated",
        "Never married",
        "Unmarried couple"
      )
    ),
    VETERAN3 = factor(
      VETERAN3,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_AGE_G = factor(
      X_AGE_G,
      levels = 1:6,
      labels = c(
        "Age 18 to 24",
        "Age 25 to 34",
        "Age 35 to 44",
        "Age 45 to 54",
        "Age 55 to 64",
        "Age 65 or older"
      ),
      ordered = TRUE
    ),
    X_INCOMG1 = factor(
      X_INCOMG1,
      levels = 1:7,
      labels = c(
        "<15k",
        "15–25k",
        "25–35k",
        "35–50k",
        "50–100k",
        "100–200k",
        "200k+"
      )
    ),
    X_EDUCAG = factor(
      X_EDUCAG,
      levels = 1:4,
      labels = c(
        "Did not graduate High School",
        "Graduated High School",
        "Attended College or Technical School",
        "Graduated from College or Technical"
      ),
      ordered = TRUE
    ), 
    EMPLOY1  = factor(
      EMPLOY1,
      levels = c(1:9),
      labels = c(
        "Employed for wages",
        "Self-employed",
        "Out of work for 1 year or more",
        "Out of work for less than 1 year",
        "A homemaker",
        "A student",
        "Retired",
        "Unable to work",
        "Refused"
      )
    ), 
    RENTHOM1 = factor(RENTHOM1),
    X_HLTHPL2 = factor(
      X_HLTHPL2,
      levels = c(1, 2),
      labels = c("Insured", "Not insured")
    ),
    PERSDOC3 = factor(
      PERSDOC3,
      levels = c(1, 2, 3),
      labels = c(
        "Yes, one",
        "Yes, more than one",
        "No"
      )
    ),
    MEDCOST1 = factor(
      MEDCOST1,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_HCVU654 = factor(
      X_HCVU654,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_RFSMOK3 = factor(X_RFSMOK3),
    EXERANY2 = factor(
      EXERANY2,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_RFDRHV9 = factor(X_RFDRHV9),
    X_BMI5 = as.numeric(X_BMI5) / 100,
    X_RACE = factor(
      X_RACE,
      levels = 1:8,
      labels = c(
        "White only, non-Hispanic",
        "Black only, non-Hispanic",
        "American Indian or Alaskan Native only, non-Hispanic",
        "Asian only, non-Hispanic",
        "Native Hawaiian or other Pacific Islander only, non-Hispanic",
        "Other race only, non-Hispanic",
        "Multiracial, non-Hispanic",
        "Hispanic"
      )
    ),
    CERVSCRN = factor(
      CERVSCRN,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    CRVCLPAP = factor(
      CRVCLPAP,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    CRVCLHPV = factor(
      CRVCLHPV,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_RFPAP37 = factor(
      X_RFPAP37,
      levels = c(1, 2),
      labels = c(
        "Had PAP test in last 3 years",
        "Had PAP test more than 3 years ago or have not had one"
      )
    ),
    X_PAPHPV1 = factor(
      X_PAPHPV1,
      levels = c(1, 2),
      labels = c(
        "Had PAP test in last 3 years or HPV test in last 5 years",
        "Have not had PAP test in last 3 years and HPV test in last 5 years"
      )
    ),
    X_HPV5YR1 = factor(
      X_HPV5YR1,
      levels = c(1, 2),
      labels = c(
        "Had HPV test in last 5 years",
        "Had HPV test more than 5 years ago or have not had one"
      )
    ),
    HADMAM = factor(
      HADMAM,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_RFMAM23 = factor(
      X_RFMAM23,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    X_MAM402Y = factor(
      X_MAM402Y,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    HADSIGM4 = factor(
      HADSIGM4,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    SMALSTOL = factor(
      SMALSTOL,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    LCSCTSC1 = factor(
      LCSCTSC1,
      levels = c(1, 2),
      labels = c(1, 0)
    ),
    PSATEST1 = factor(
      PSATEST1,
      levels = c(1, 2),
      labels = c(1, 0)
    )
    
  )


# Creating a composite any-screening indicator (1 = participated in at least one relevant screening, 
# 0 = none) for modelling. We also keep sex-stratified outcomes for deeper models.

brfss24_sub <- brfss24_sub %>%
  mutate(
    # composite: any screening
    any_screening = factor(case_when(if_any(
      c(CERVSCRN, HADMAM, HADSIGM4, SMALSTOL, PSATEST1, LCSCTSC1),
      ~ !is.na(.) & . == 1L # whether the value is not missing or it is equal to 1
    ) ~ 1L, TRUE ~ 0L)),
    # mammography among women only (excluding those with hysterectomy, see below)
    mam_women = factor(if_else(
      X_SEX == "Female",
      if_else(HADMAM == "Yes", 1L, 0L, missing = NA_integer_),
      NA_integer_ # males get an NA and not a 0, since they must not be coded as "didn't get screened"
    ), levels = c(0, 1)),
    # cervical screening: restrict to women without hysterectomy
    cerv_screen = factor(case_when(
      X_SEX == "Female" & (is.na(HADHYST2) | HADHYST2 == "No") &
        CERVSCRN == "Yes"  ~ 1L,
      X_SEX == "Female" & (is.na(HADHYST2) | HADHYST2 == "No") &
        CERVSCRN == "No"   ~ 0L,
      TRUE ~ NA_integer_ # men and women with hysterectomy
    ))
  )

# Exporting the dataset to CSV
doc_path <- "/Users/elis/Library/Mobile Documents/com~apple~CloudDocs/Documents/magistrale_cloud/courses/HEALTH_DATA_SCIENCE_EM1413/HDS_gp_local/brfss24_sub.csv"

already_exported <- "yes" # da cambiare in "no" per esportarlo

if (already_exported != "yes") {
  write.csv(brfss24_sub, doc_path, row.names = FALSE)
}


# Converting the dataset into a survey design object:

brfss.design <- brfss24_sub %>%
  as_survey_design(
    ids = X_PSU,
    weights = X_LLCPWT,
    strata = X_STSTR,
    nest = TRUE
  )

# in the case of variables (State?) having just 1 PSU, to avoid calculation errors:
options(survey.lonely.psu = "adjust")
