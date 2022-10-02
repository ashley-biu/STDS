##### GUIDE #####
# Before running, go to the file "data loader and run the whole scipt"
# Then run this one
# NOTE: make sure you set your current directory to the folder STDS_AT2_2022



# import libraries
library(pacman)

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

#  Poisson Regression 
cpi.pos.mod <-  glm(fatality_number ~ CPI, data=research_data, family="poisson")

# Display Summary Statistics 
summary(cpi.pos.mod)


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
cpi.nb.mod <- glm.nb(fatality_number ~ CPI,
                        data = research_data)


# Summary of negative binomial (Fatality ~ CPI)
summary(cpi.nb.mod)


# Confidence Intervals
confint(cpi.nb.mod)

# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(cpi.nb.mod),
     residual.degrees.of.freedom = df.residual(cpi.nb.mod),
     chisq.p.value               = pchisq(deviance(cpi.nb.mod), df.residual(cpi.nb.mod), lower = F))


# chi value of 0.34, therefore it's a good fit. (still waiting for Kyle to confirm to do this - sent him an email)


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(cpi.nb.mod)
glm.diag.plots(cpi.nb.mod, leuk.diag)





########### EMPLOYMENT RATE



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ Employment Rate
####################################

#  Poisson Regression 
employment.pos.mod <-  glm(fatality_number ~ employment_rate, data=research_data, family="poisson")

# Display Summary Statistics 
summary(employment.pos.mod)
# AIC = 629.37

# Display Coeeficients
summary(employment.pos.mod)$coefficients

# Confidence Intervals for fatality ~  Employment Rate
confint(employment.pos.mod)

# Dispersion Test for Fatality ~  Employment Rate
dispersiontest(employment.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ EMPLOYMENT RATE)
####################################

# model negative bimomial model
employment.nb.mod <- glm.nb(fatality_number ~ employment_rate,
                            data = research_data)


# Summary of negative binomial
summary(employment.nb.mod)


# Confidence Intervals 
confint(employment.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(employment.nb.mod),
     residual.degrees.of.freedom = df.residual(employment.nb.mod),
     chisq.p.value               = pchisq(deviance(employment.nb.mod), df.residual(employment.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(employment.nb.mod)
glm.diag.plots(employment.nb.mod, leuk.diag)




########### UNEMPLOYMENT


#####################################
# POISSON MODEL: FATALITY_NUMBER ~ UNEMPLOYMENT)
####################################

#  Poisson Regression (Fatality ~ CPI) Model
unemployment.pos.mod <-  glm(fatality_number ~ unemployment_rate, data=research_data, family="poisson")

# Display Summary Statistics 
summary(unemployment.pos.mod)

# Display Coeeficients
summary(unemployment.pos.mod)$coefficients

# Confidence Intervals for fatality ~ unemployment
confint(unemployment.pos.mod)

# Dispersion Test
dispersiontest(unemployment.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ UNEMPLOYMENT RATE)
####################################

# model negative bimomial model
unemployment.nb.mod <- glm.nb(fatality_number ~ unemployment_rate,
                              data = research_data)


# Summary of negative binomial
summary(unemployment.nb.mod)


# Confidence Intervals 
confint(unemployment.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(unemployment.nb.mod),
     residual.degrees.of.freedom = df.residual(unemployment.nb.mod),
     chisq.p.value               = pchisq(deviance(unemployment.nb.mod), df.residual(unemployment.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(unemployment.nb.mod)
glm.diag.plots(unemployment.nb.mod, leuk.diag)





########### TOTAL POPULATION

#####################################
# POISSON MODEL: FATALITY_NUMBER ~ TOTAL POPULATION NUMBER)
####################################

#  Poisson Regression 
population.pos.mod <-  glm(fatality_number ~ tot_pop_num, data=research_data, family="poisson")


# Display Summary Statistics 
summary(population.pos.mod)


# Display Coeeficients
summary(population.pos.mod)$coefficients


# Confidence Intervals 
confint(population.pos.mod)


# Dispersion Test 
dispersiontest(population.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ TOTAL POPULATION NUMBER)
####################################

# negative bimomial model
population.nb.mod <- glm.nb(fatality_number ~ tot_pop_num,
                            data = research_data)


# Summary of negative binomial
summary(population.nb.mod)


# Confidence Intervals 
confint(population.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(population.nb.mod),
     residual.degrees.of.freedom = df.residual(population.nb.mod),
     chisq.p.value               = pchisq(deviance(population.nb.mod), df.residual(population.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(population.nb.mod)
glm.diag.plots(population.nb.mod, leuk.diag)



########### YOUTH PROPORTION



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ YOUTH PROPORTION)
####################################

#  Poisson Regression 
youth.pos.mod <-  glm(fatality_number ~ youth_proportion, data=research_data, family="poisson")


# Display Summary Statistics 
summary(youth.pos.mod)


# Display Coeeficients
summary(youth.pos.mod)$coefficients


# Confidence Intervals 
confint(youth.pos.mod)


# Dispersion Test 
dispersiontest(youth.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ YOUTH PROPORTION)
####################################


# negative bimomial model
youth.nb.mod <- glm.nb(fatality_number ~ youth_proportion,
                       data = research_data)


# Summary of negative binomial
summary(youth.nb.mod)


# Confidence Intervals 
confint(youth.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(youth.nb.mod),
     residual.degrees.of.freedom = df.residual(youth.nb.mod),
     chisq.p.value               = pchisq(deviance(youth.nb.mod), df.residual(youth.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(youth.nb.mod)
glm.diag.plots(youth.nb.mod, leuk.diag)



########### GDP



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ GDP PER CAP)
####################################

#  Poisson Regression 
gdp.pos.mod <-  glm(fatality_number ~ gdp_per_capita, data=research_data, family="poisson")


# Display Summary Statistics 
summary(gdp.pos.mod)


# Display Coeeficients
summary(gdp.pos.mod)$coefficients


# Confidence Intervals 
confint(gdp.pos.mod)


# Dispersion Test 
dispersiontest(gdp.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ GDP)
####################################


# negative bimomial model
gdp.nb.mod <- glm.nb(fatality_number ~ gdp_per_capita,
                     data = research_data)


# Summary of negative binomial
summary(gdp.nb.mod)


# Confidence Intervals 
confint(gdp.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(gdp.nb.mod),
     residual.degrees.of.freedom = df.residual(gdp.nb.mod),
     chisq.p.value               = pchisq(deviance(gdp.nb.mod), df.residual(gdp.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(gdp.nb.mod)
glm.diag.plots(gdp.nb.mod, leuk.diag)



########### LIGHT VEHICLES




#####################################
# POISSON MODEL: FATALITY_NUMBER ~ REGISTERED LIGHT VECHICLES)
####################################

#  Poisson Regression 
light.vehicles.pos.mod <-  glm(fatality_number ~ registered_light_vehicles, data=research_data, family="poisson")


# Display Summary Statistics 
summary(light.vehicles.pos.mod)


# Display Coeeficients
summary(light.vehicles.pos.mod)$coefficients


# Confidence Intervals 
confint(light.vehicles.pos.mod)


# Dispersion Test 
dispersiontest(light.vehicles.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ REGISTERED LIGHT VECHICLES)
####################################


# negative bimomial model
light.vehicles.nb.mod <- glm.nb(fatality_number ~ registered_light_vehicles,
                                data = research_data)


# Summary of negative binomial
summary(light.vehicles.nb.mod)


# Confidence Intervals 
confint(light.vehicles.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(light.vehicles.nb.mod),
     residual.degrees.of.freedom = df.residual(light.vehicles.nb.mod),
     chisq.p.value               = pchisq(deviance(light.vehicles.nb.mod), df.residual(light.vehicles.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(light.vehicles.nb.mod)
glm.diag.plots(light.vehicles.nb.mod, leuk.diag)





########### HEAVY VECHICLES



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ HEAVY  VECHICLES)
####################################

#  Poisson Regression 
heavy.vehicles.pos.mod <-  glm(fatality_number ~ registered_heavy_vehicles, data=research_data, family="poisson")


# Display Summary Statistics 
summary(heavy.vehicles.pos.mod)


# Display Coeeficients
summary(heavy.vehicles.pos.mod)$coefficients


# Confidence Intervals 
confint(heavy.vehicles.pos.mod)


# Dispersion Test 
dispersiontest(heavy.vehicles.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ REGISTERED HEAVY VECHICLES)
####################################


# negative bimomial model
heavy.vehicles.nb.mod <- glm.nb(fatality_number ~ registered_heavy_vehicles,
                                data = research_data)


# Summary of negative binomial
summary(heavy.vehicles.nb.mod)


# Confidence Intervals 
confint(heavy.vehicles.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(heavy.vehicles.nb.mod),
     residual.degrees.of.freedom = df.residual(heavy.vehicles.nb.mod),
     chisq.p.value               = pchisq(deviance(heavy.vehicles.nb.mod), df.residual(heavy.vehicles.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(heavy.vehicles.nb.mod)
glm.diag.plots(heavy.vehicles.nb.mod, leuk.diag)




########### MOTORCYCLES



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ MOTORCYCLES)
####################################

#  Poisson Regression 
motorcycle.pos.mod <-  glm(fatality_number ~ registered_motorcycles, data=research_data, family="poisson")


# Display Summary Statistics 
summary(motorcycle.pos.mod)


# Display Coeeficients
summary(motorcycle.pos.mod)$coefficients


# Confidence Intervals 
confint(motorcycle.pos.mod)


# Dispersion Test 
dispersiontest(motorcycle.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ MOTORCYCLES)
####################################


# negative bimomial model
motorcycles.nb.mod <- glm.nb(fatality_number ~ registered_motorcycles,
                                data = research_data)


# Summary of negative binomial
summary(motorcycles.nb.mod)


# Confidence Intervals 
confint(motorcycles.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(motorcycles.nb.mod),
     residual.degrees.of.freedom = df.residual(motorcycles.nb.mod),
     chisq.p.value               = pchisq(deviance(motorcycles.nb.mod), df.residual(motorcycles.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(motorcycles.nb.mod)
glm.diag.plots(motorcycles.nb.mod, leuk.diag)





########### VEHICLES IN MILLIONS



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ VEHICLES IN MILS)
####################################

#  Poisson Regression 
vehicles.mil.pos.mod <-  glm(fatality_number ~ vehicles_registered_in_millions, data=research_data, family="poisson")


# Display Summary Statistics 
summary(vehicles.mil.pos.mod)


# Display Coeeficients
summary(vehicles.mil.pos.mod)$coefficients


# Confidence Intervals 
confint(vehicles.mil.pos.mod)


# Dispersion Test 
dispersiontest(vehicles.mil.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ VEHICLES IN MIL)
####################################


# negative bimomial model
vehicles.mil.nb.mod <- glm.nb(fatality_number ~ vehicles_registered_in_millions,
                             data = research_data)


# Summary of negative binomial
summary(vehicles.mil.nb.mod)


# Confidence Intervals 
confint(vehicles.mil.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(vehicles.mil.nb.mod),
     residual.degrees.of.freedom = df.residual(vehicles.mil.nb.mod),
     chisq.p.value               = pchisq(deviance(vehicles.mil.nb.mod), df.residual(vehicles.mil.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(vehicles.mil.nb.mod)
glm.diag.plots(vehicles.mil.nb.mod, leuk.diag)






########### TWI



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ TWI)
####################################

#  Poisson Regression 
twi.pos.mod <-  glm(fatality_number ~ TWI_data, data=research_data, family="poisson")


# Display Summary Statistics 
summary(twi.pos.mod)


# Display Coeeficients
summary(twi.pos.mod)$coefficients


# Confidence Intervals 
confint(twi.pos.mod)


# Dispersion Test 
dispersiontest(twi.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ TWI)
####################################


# negative bimomial model
twi.nb.mod <- glm.nb(fatality_number ~ TWI_data,
                              data = research_data)


# Summary of negative binomial
summary(twi.nb.mod)


# Confidence Intervals 
confint(twi.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(twi.nb.mod),
     residual.degrees.of.freedom = df.residual(twi.nb.mod),
     chisq.p.value               = pchisq(deviance(twi.nb.mod), df.residual(twi.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(twi.nb.mod)
glm.diag.plots(twi.nb.mod, leuk.diag)




########### PETROL


#####################################
# POISSON MODEL: FATALITY_NUMBER ~ PETROL PRICE)
####################################

#  Poisson Regression 
petrol.pos.mod <-  glm(fatality_number ~ petrol_price, data=research_data, family="poisson")


# Display Summary Statistics 
summary(petrol.pos.mod)


# Display Coeeficients
summary(petrol.pos.mod)$coefficients


# Confidence Intervals 
confint(petrol.pos.mod)


# Dispersion Test 
dispersiontest(petrol.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ PETROL)
####################################


# negative bimomial model
petrol.nb.mod <- glm.nb(fatality_number ~ petrol_price,
                     data = research_data)


# Summary of negative binomial
summary(petrol.nb.mod)


# Confidence Intervals 
confint(petrol.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(petrol.nb.mod),
     residual.degrees.of.freedom = df.residual(petrol.nb.mod),
     chisq.p.value               = pchisq(deviance(petrol.nb.mod), df.residual(petrol.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(petrol.nb.mod)
glm.diag.plots(petrol.nb.mod, leuk.diag)





########### DIESEL



#####################################
# POISSON MODEL: FATALITY_NUMBER ~ DIESEL PRICE)
####################################

#  Poisson Regression 
diesel.pos.mod <-  glm(fatality_number ~ diesel_price, data=research_data, family="poisson")


# Display Summary Statistics 
summary(diesel.pos.mod)


# Display Coeeficients
summary(diesel.pos.mod)$coefficients


# Confidence Intervals 
confint(diesel.pos.mod)


# Dispersion Test 
dispersiontest(diesel.pos.mod)



#####################################
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ DIESEL)
####################################


# negative bimomial model
diesel.nb.mod <- glm.nb(fatality_number ~ diesel_price,
                        data = research_data)


# Summary of negative binomial
summary(diesel.nb.mod)


# Confidence Intervals 
confint(diesel.nb.mod)


# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(diesel.nb.mod),
     residual.degrees.of.freedom = df.residual(diesel.nb.mod),
     chisq.p.value               = pchisq(deviance(diesel.nb.mod), df.residual(diesel.nb.mod), lower = F))


# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(diesel.nb.mod)
glm.diag.plots(diesel.nb.mod, leuk.diag)





######### Summary Results All At Once ###########


# print out poisson regression results for all variables
map_df(
  set_names(names(research_data[5:17])),
  ~ glm(formula(paste(
    "fatality_number ~ ", .x
  )), data = research_data, family = "poisson") %>%
    tidy(conf.int = TRUE, p.value = TRUE)
) %>%
  filter(term != "(Intercept)")


# print out negative binomial regression results for all variables
map_df(
  set_names(names(research_data[5:17])),
  ~ glm.nb(formula(paste(
    "fatality_number ~ ", .x
  )), data = research_data) %>%
    tidy(conf.int = TRUE, p.value = TRUE)
) %>%
  filter(term != "(Intercept)")




