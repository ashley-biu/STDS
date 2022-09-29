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

#  Poisson Regression (Fatality ~ CPI) Model
cpi.pos.mod <-  glm(fatality_number ~ CPI, data=research_data, family="poisson")

# Display Summary Statistics 
summary(cpi.pos.mod)
# AIC = 629.37

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
# AIC = 625.49 - Negative Binomial has a lower AIC


# Confidence Intervals for fatality ~ CPI
confint(cpi.nb.mod)

# Goodness of Fit Test (deviance) for negative binomial model
list(residual.deviance           = deviance(cpi.nb.mod),
     residual.degrees.of.freedom = df.residual(cpi.nb.mod),
     chisq.p.value               = pchisq(deviance(cpi.nb.mod), df.residual(cpi.nb.mod), lower = F))


# chi value of 0.34, therefore it's a good fit. (still waiting for Kyle to confirm to do this - sent him an email)


#####################################
# DIAGNOSTICS: NEGATIVE BINOMIAL FATALITY_NUMBER ~ CPI)
####################################

# plot diagnostics
par(mfrow=c(2,2))
leuk.diag <- glm.diag(cpi.nb.mod)
glm.diag.plots(cpi.nb.mod, leuk.diag)








