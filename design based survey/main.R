# main.R 
# AAS, AASE and clusters - design based survey
#
# This script runs the full project pipeline.

cat("Starting project...\n\n")

# 1.Data treatment and analysis

cat("Step 1: Loading packages and preparing data...\n")

source("scr/00_setup.R")
source("scr/01_treatment.R")
source("scr/02_analysis.R")

# Methods of sampling 

cat("Step 2: running methods of sampling...\n")

source("scr/03_aas.R")
source("scr/04_aase.R")
source("scr/05_cluster.R")

# Results

cat("Step 3: results...\n")

source("scr/06_results.R")
