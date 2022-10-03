##### GUIDE #####
# Before running, go to the file "data loader and run the whole scipt"
# Then run this one
# NOTE: make sure you set your current directory to the folder STDS_AT2_2022



# import libraries
library(pacman)
library(caret)

set.seed(42)
# use pacman to load variables being used
pacman::p_load(
  "modelr",
  "AER",
  "MASS",
  "tidyverse",
  "readxl",
  "kableExtra",
  "vcd",
  "VGAM",
  "actuar",
  "fitdistrplus",
  "AER",
  "mctest", 
  "boot", 
  "broom",
  "TidyDensity"
)


# import data
research_data <- load_datasets()




########### CPI


#####################################
# POISSON MODEL: FATALITY_NUMBER ~ CPI)
####################################
train_control <- trainControl(
  method='cv',
  number = 10,
  #verboseIter = TRUE
)
#  Poisson Regression 
cpi.pos.mod <-  train(fatality_number ~ ., data=research_data, family="poisson",
                      trainControl = train_control)



# Display Summary Statistics 
print(cpi.pos.mod)
summary(cpi.pos.mod)
aictab(cand.set = models, modnames = mod.names)


# Display Coeeficients
summary(cpi.pos.mod)$coefficients

# Confidence Intervals for fatality ~ CPI
confint(cpi.pos.mod)

# Dispersion Test for Fatality ~ CPI
dispersiontest(cpi.pos.mod)
# 1.47 - therefore, we check negative binomial


#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ CPI)
####################################

# model negative bimomial model


cpi.nb.mod <- train(fatality_number ~ ., family = negative.binomial,
                    data = research_data,
                    trainControl= train_control)

print(cpi.nb.mod)
