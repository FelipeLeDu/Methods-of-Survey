# 00_setup.R
# Project setup — Packages and environment

cat("Loading required packages...\n")

# List of required packages
packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "forcats",
  "lme4",
  "sf"
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
