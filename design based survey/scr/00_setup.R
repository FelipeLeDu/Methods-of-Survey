# 00_setup.R
# Project setup — Packages and environment
#
# This script installs and loads all packages required for the
# Phillips Curve ARDL project.

cat("Loading required packages...\n")

# List of required packages
packages <- c(
  "fredr",
  "tidyverse",
  "dynlm",
  "lmtest",
  "sandwich",
  "forecast",
  "tseries",
  "FinTS",
  "gridExtra",
  "knitr",
  "rbcb",
  "zoo",
  "urca",
  "strucchange",
  "lubridate",
  "modelsummary"
)

# Function to install missing packages and load them
install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  
  library(pkg, character.only = TRUE)
}

# Install and load all packages
invisible(sapply(packages, install_if_needed))

cat("All packages loaded successfully.\n\n")
