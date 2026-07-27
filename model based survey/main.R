# main.R 
# MrP and model based survey 
#
# This script runs the full project pipeline.

cat("Starting project...\n\n")

# 1.Data treatment and analysis

cat("Step 1: Loading packages and preparing data...\n")

source("scr/00_setup.R")
source("scr/01_treatment.R")
source("scr/02_analysis.R")

# Specification and estimation

cat("Step 2: specifying and estimating models...\n")

source("scr/03_specification.R")
source("scr/04_estimation.R")

# Stratification

cat("Step 2: stratification...\n")

source("scr/05_stratification.R")
