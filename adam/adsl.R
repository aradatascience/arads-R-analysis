# Step-by-step building of ADSL

# set. Environment setup and data loading
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(admiral)

# load SDTM domains from the CDISC Pilot Study
data("dm")
data("ex")
data("ds")

# convert empty character strings to NA
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)

# 1. Derive treatment dates from EX domain
# convert ISO 8601 strings to R Date objects

# derive first and last dates (TRTSDT, TRTEDT)
adsl_step1 <- dm |> 
  # derive TRTSDT
  derive_vars_merged(
    dataset_add = ex,
    filter_add = !is.na(EXSTDTC),
    new_vars = exprs(TRTSDT = convert_dtc_to_dt(EXSTDTC)),
    order = exprs(EXSTDTC),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) |> 
  # derive TRTEDT
  derive_vars_merged(
    dataset_add = ex,
    filter_add = !is.na(EXENDTC),
    new_vars = exprs(TRTEDT = convert_dtc_to_dt(EXENDTC)),
    order = exprs(EXENDTC),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

glimpse(adsl_step1)

# 2. Calculate treatment duration (TRTDURD)
adsl_step2 <- adsl_step1 |> 
  derive_var_trtdurd()

glimpse(adsl_step2)

# 3. Categorical variables (age grouping) and population flags
adsl_step3 <- adsl_step2 |> 
  # create categorical age variable (AGEGR1)
  mutate(
    AGEGR1 = case_when(
      AGE < 65 ~ "65",
      AGE >= 65 & AGE <= 75 ~ "65-75",
      AGE > 75 ~ ">75",
      TRUE ~ NA_character_
    ),
    # derive safety population flag
    SAFFL = if_else(!is.na(TRTSDT), "Y", "N")
  )

glimpse(adsl_step3)

# 4. Merge discontinuation reasons and date (from DS)
adsl_step4 <- adsl_step3 |> 
  derive_vars_merged(
    dataset_add = ds,
    filter_add = DSCAT == "DISPOSITON EVENT" & DSDECOD != "COMPLETED",
    new_vars = exprs(DCSREAS = DSDECOD, DCDEVDT = convert_dtc_to_dt(DSSTDTC)),
    by_vars = exprs(STUDYID, USUBJID)
  )

glimpse(adsl_step4)

# Final inspection
adsl_step4 |> 
  select(USUBJID, AGE, AGEGR1, TRTSDT, TRTEDT, TRTDURD, SAFFL, DCSREAS) |> 
  head(10)

renv::snapshot()
renv::status()
  









