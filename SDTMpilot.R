# instalation
install.packages(c("pharmaversesdtm", "dplyr"))
library(pharmaversesdtm)
library(dplyr)

# SDTM domains review
data("dm")
glimpse(dm)
data("ae")
data("vs")

# analysis of unique CDISC variables
dm |> 
  select(USUBJID, AGE, SEX, RACE, ARM, RFSTDTC) |> 
  head(10)

# domain joining and relations study
ae_with_arm <- ae |> 
  inner_join(dm |> select(USUBJID, ARM), by = "USUBJID")

ae_with_arm |> 
  count(ARM, AETERM, sort = TRUE) |> 
  head(10)