source("Code/data-loader.R")
install.packages("devtools")
library(devtools)
install_github("trinker/pacman")

p_load("modelr")

research_data <- load_datasets()

crossv_kfold(research_data, k = 3)