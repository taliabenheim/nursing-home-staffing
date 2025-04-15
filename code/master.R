# Executes all other files sequentially
source("code/01_retrieve_process_pbj.R") ## Warning: This file takes several hours to run
source("code/02_read_merge_nhc.R")
source("code/03_plot_trends.R")
source("code/04_tables.R")