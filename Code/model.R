# import data-loader file
source("Code/data-loader.R")

# install devtools
install.packages("devtools")

library(devtools)
# install dev pacman library
install_github("trinker/pacman")

# load libraries
library(pacman)

# load in packages
pacman::p_load("modelr", "AER", "MASS")

# laod dataset
research_data <- load_datasets()

splits <- crossv_kfold(research_data, k = 3)

research_data

glm.poisson <- glm(fatality_number ~ CPI + employment_rate +
                youth_proportion + gdp_per_capita +
                vehicles_registered_in_millions + TWI_data,
                family = poisson(),
                data = research_data)

summary(glm.poisson)

hist(research_data$fatality_number)

length(research_data[,1])
(lambda <-  mean(research_data$fatality_number))
tbl <- NULL
for (k in 0:130) {
    tbl <- rbind(tbl, c(obs=sum(research_data$fatality_number == k),
        exp = length(research_data[,1]) *  exp(lambda) * lambda ^k / factorial(k)))
}
tbl
barplot(tbl[,2])

dimnames(tbl)[[1]] <- paste(0:130)
tbl[1:5,]

?predict
E.fatality_number <- predict(glm.poisson, type = "response")

par(mfrow=c(2,2))
plot(E.fatality_number, research_data$fatality_number - E.fatality_number)
plot(E.fatality_number, (research_data$fatality_number - E.fatality_number)/sqrt(E.fatality_number))
plot(research_data$fatality_number - E.fatality_number)
plot((research_data$fatality_number - E.fatality_number)/sqrt(E.fatality_number))


plot(E.fatality_number, research_data$fatality_number)
plot(log(E.fatality_number + 1), log(research_data$fatality_number + 1))

glm.negative_binomial <- glm.nb(fatality_number ~ CPI + employment_rate +
                youth_proportion + gdp_per_capita +
                vehicles_registered_in_millions + TWI_data,
                data = research_data)

###########################################################
#garbage below


hist(research_data$fatality_number)

poisson_CPI <- glm(fatality_number ~ CPI,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_CPI)

#checking vor over/underdispersion >1 or < -1 is bad 
deviance(poisson_CPI) / df.residual(poisson_CPI)

poisson_employment <- glm(fatality_number ~ employment_rate,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_employment)

deviance(poisson_employment) / df.residual(poisson_employment)

poisson_youth <- glm(fatality_number ~ youth_proportion,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_youth)

deviance(poisson_youth) / df.residual(poisson_youth)

poisson_gdp <- glm(fatality_number ~ gdp_per_capita,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_gdp)

deviance(poisson_gdp) / df.residual(poisson_gdp)

poisson_registered_vehicles <- glm(fatality_number ~ vehicles_registered_in_millions,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_registered_vehicles)

deviance(poisson_registered_vehicles) / df.residual(poisson_registered_vehicles)

poisson_TWI <- glm(fatality_number ~ TWI_data,
            family = poisson(link = "log"),
            data = research_data)

summary(poisson_TWI)

deviance(poisson_TWI) / df.residual(poisson_TWI)

research_data

poisson_complete <- glm(fatality_number ~ CPI + employment_rate + youth_proportion + gdp_per_capita + vehicles_registered_in_millions + TWI_data,
            family = poisson(link = "log"),
            data = research_data)

deviance(poisson_complete) / df.residual(poisson_complete)

