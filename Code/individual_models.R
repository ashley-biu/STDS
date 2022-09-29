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
  "boot"
)

# import data
research_data <- load_datasets()


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


######################################################


#####################################
# POISSON MODEL: FATALITY_NUMBER ~ Employment Rate)
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


################################################################################





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
# NEGATIVE BINOMIAL MODEL: FATALITY_NUMBER ~ unemployment RATE)
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



####################################

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






