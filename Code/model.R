# import data-loader file
source("Code/data-loader.R")

# install devtools
install.packages("devtools")

# install dev pacman library
install_github("trinker/pacman")

# load libraries
library(devtools)
library(pacman)

# load in packages
pacman::p_load("modelr")

# laod dataset
research_data <- load_datasets()

crossv_kfold(research_data, k = 3)