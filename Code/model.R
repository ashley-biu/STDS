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

# load dataset
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


###########################################################

#looping over columns and adding dispersions to array "dispersions"
dispersions <- c()
counter <- 1
for (i in colnames(research_data)) {
    if (i != "time_frame" && i != "fatality_number") {
        tmp <- as.formula(paste0("fatality_number~",i))
        poisson <- glm(tmp,
                family = poisson(),
                data = research_data)

                dispersion <- deviance(poisson) / df.residual(poisson)
                dispersions[counter] <- dispersion
        counter <- counter + 1
    }
}
colnames(research_data)
dispersions

#creating poisson model with all explanatory variables to check for dispersion
poisson_complete <- glm(fatality_number ~ CPI + employment_rate + youth_proportion + gdp_per_capita + vehicles_registered_in_millions + TWI_data,
            family = poisson(),
            data = research_data)

deviance(poisson_complete) / df.residual(poisson_complete)

#create poisson model with explanatory variables with low dispersion
poisson_good_dispersion <- glm(fatality_number ~ CPI + youth_proportion + gdp_per_capita + vehicles_registered_in_millions,
            family = poisson(),
            data = research_data)
deviance(poisson_good_dispersion) / df.residual(poisson_complete)
#result = almost no difference between dispersions


glm.negative_binomial <- glm.nb(fatality_number ~ CPI + employment_rate +
                youth_proportion + gdp_per_capita +
                vehicles_registered_in_millions + TWI_data,
                data = research_data)

summary(poisson_complete)
summary(glm.negative_binomial)

anova(poisson_complete, glm.negative_binomial, test = "Chisq")



#maybe compare residuals?????
E.fatality_number_nb <- predict(glm.negative_binomial, type = "response")

raw_residual <- research_data$fatality_number - E.fatality_number
standardized_residual <- (research_data$fatality_number - E.fatality_number) / sqrt(E.fatality_number)
raw_residual
standardized_residual
plot(raw_residual)
plot(standardized_residual)