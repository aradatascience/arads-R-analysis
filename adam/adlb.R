# Step-by-step building of ASLB (BDS)
# I have ADSL already

# set. Environment setup and data loading
library(pharmaversesdtm)
library(dplyr)
library(admiral)

# load SDTM domains from the CDISC Pilot Study
data("lb")

# convert empty character strings to NA
lb <- convert_blanks_to_na(lb)

glimpse(lb)

# 1. Merge ADSL variables into SDTM LB
# filtering out missing/non done records and join ADSL
adlb_step1 <- lb |> 
  filter(!is.na(LBSTRESC) | !is.na(LBSTRESN)) |> 
  derive_vars_merged(
    dataset_add = adsl_step4,
    new_vars = exprs(TRTSDT, TRTEDT, ARM, ACTARM, SAFFL),
    by_vars = exprs(STUDYID, USUBJID)
  )

glimpse(adlb_step1)

# 2. Derive analysis DTC, date/time, and study days
adlb_step2 <- adlb_step1 |> 
  # convert LBDTC to numeric date (ADT) 
  mutate(
    ADT = convert_dtc_to_dt(LBDTC)
  ) |> 
  # derive analysis study day ADY relative to TRTSDT
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

glimpse(adlb_step2)

# 3. Derive parameter variables
adlb_step3 <- adlb_step2 |> 
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    PARAMN = as.numeric(as.factor(PARAMCD)),
    AVAL = LBSTRESN,
    AVALC = LBSTRESC
  )

# 4. Derive on-treatment flag (ONTRTFL)
adlb_step4 <- adlb_step3 |> 
  derive_var_ontrtfl(
    start_date = ADT,
    ref_start_date = TRTSDT,
    ref_end_date = TRTEDT,
    ref_end_window = 30
  )

glimpse(adlb_step4)

# 5. Derive baseline flag (ABLFL) and baseline values
# flag baseline record
adlb_step5 <- adlb_step4 |> 
  filter(!is.na(AVAL) & ADT <= TRTSDT) |> 
  derive_var_extreme_flag(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    order = exprs(ADT, LBSEQ),
    mode = "last",
    new_var = ABLFL
  ) |> 
  # merge ABLFL flag back into the full dataset
  right_join(adlb_step4, by = colnames(adlb_step4)) |> 
  # derive BASE and BASEC across all records
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  ) |> 
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVALC,
    new_var = BASEC
  )

# 6. Derive CHG, PCHG - change form baseline
# calculate absolute change CHG = AVAL - BASE 
# and percentage change (PCHG = (CHG/BASE) * 100)
adlb_step6 <- adlb_step5 |> 
  derive_var_chg() |> 
  derive_var_pchg()

# 7. Derive analysis visit (AVISIT) and analysis flag (ANL01FL)
adlb_step7 <- adlb_step6 %>%
  mutate(
    AVISIT = case_when(
      ABLFL == "Y" ~ "Baseline",
      ADY >= 1 & ADY <= 14 ~ "Week 2",
      ADY > 14 & ADY <= 30 ~ "Week 4",
      TRUE ~ "Unscheduled"
    )
  ) %>%
  # Apply extreme flag derivation ONLY to filtered records
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, PARAMCD, AVISIT),
      order = exprs(ADT, LBSEQ),
      mode = "last",
      new_var = ANL01FL
    ),
    filter = !is.na(AVAL) & AVISIT != "Unscheduled"
  )

# Verification
adlb_step7 %>%
  filter(ANL01FL == "Y") %>%
  select(USUBJID, PARAMCD, AVISIT, ADT, AVAL, ANL01FL) %>%
  head()