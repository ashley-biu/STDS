
source("Code/data-loader.R")
library(devtools)
library(modelr)


#Checking the variables relationship before starting the testing. More precisely, checking how the fatality number is related to all the other variables

p_load("modelr","AER") #package management, loads packages needed
research_data <- load_datasets() #loading the data set previously created
crossv_kfold(research_data, k = 3)
view(research_data)


#Creating a linear model for fatality number related to CPI

lm.CPI <- lm(fatality_number ~  CPI, data = research_data)
summary(lm.CPI)
par(mfrow = c(2,2))
plot(lm.CPI) 

#Create a Poisson model for CPI

CPI.Poisson <- glm(fatality_number ~  CPI, data = research_data,
                   family = poisson())
plot(CPI.Poisson)


#checking  the dispersion

dispersiontest(CPI.Poisson)

#CONCLUSION: The dispersion value is 1.473716

#Creating a linear model for fatality number related to employment rate

lm.er <- lm(fatality_number ~  employment_rate, data = research_data)
summary(lm.er)
par(mfrow = c(2,2))
plot(lm.er)


#Creating a Poisson employment rate model

er.Poisson <- glm(fatality_number ~  employment_rate, data = research_data,
                   family = poisson())
plot(er.Poisson)


#Checking the dispersion

dispersiontest(er.Poisson)

#CONCLUSION: The dispersion is 3.18917

#Creating a linear model for fatality number related to youth proportion

lm.yp <- lm(fatality_number ~  youth_proportion, data = research_data)
summary(lm.yp)
par(mfrow = c(2,2))
plot(lm.yp) 


#Creating a Poisson model for youth proportion

yp.Poisson <- glm(fatality_number ~  youth_proportion, data = research_data,
                   family = poisson())
plot(yp.Poisson)


#Checking the dispersion

dispersiontest(yp.Poisson)

#CONCLUSION: The dispersion is 2.035607

#Creating a linear model for fatality number related to vehicles registered in millions

lm.vehicles <- lm(fatality_number ~  vehicles_registered_in_millions, data = research_data)
summary(lm.vehicles)
par(mfrow = c(2,2))
plot(lm.vehicles) 


#Create a Poisson model for vehicles registered in millions

vehicles.Poisson <- glm(fatality_number ~  vehicles_registered_in_millions, data = research_data,
                   family = poisson())
plot(vehicles.Poisson)


#Checking the dispersion

dispersiontest(vehicles.Poisson)


#CONCLUSION: The dispersion is 1.655533

#Creating a linear model for fatality number related to TWI data

lm.TWI <- lm(fatality_number ~  TWI_data, data = research_data)
summary(lm.TWI)
par(mfrow = c(2,2))
plot(lm.TWI) 


#Creating a Poisson model for TWI data

TWI.Poisson <- glm(fatality_number ~  TWI_data, data = research_data,
                        family = poisson())
plot(TWI.Poisson)

#Check for dispersion

dispersiontest(TWI.Poisson)

#CONCLUSION: The dispersion is 3.782818
#The higer dispersion value is for the TWI data, while the lowest is for CPI data