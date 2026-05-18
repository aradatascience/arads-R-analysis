# Renv
install.packages("renv")
renv::init()

# Create the project structure
folders <- c("data", "adam", "tfls", "functions")
lapply(folders, dir.create)

# Install packages
install.packages(c(
  "tidyverse",
  "admiral",
  "Tplyr",
  "gtsummary",
  "ggsurvfit",
  "survival",
  "pharmaversesdtm"
))

# Renv
renv::status()
renv::snapshot()

# Data folder
file.create("data/.gitkeep")

# Renv to restore
# renv::restore()