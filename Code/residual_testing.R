source("Code/data-loader.R")

research_data <- load_datasets()

head(research_data)
?glm
par(mfrow = c(2, 2))

#using a linear model

#linear model using all features for reference
lmout <- lm(fatality_number ~ year + gdp_per_capita
            + vehicles_registered_in_millions
            + youth_proportion + employment_rate + avg_petrol_price
            + CPI + TWI_data,
            data = research_data)

plot(lmout)

#"ideal" lm
ideallm <- lm(fatality_number ~ avg_petrol_price
            + vehicles_registered_in_millions
            + employment_rate + CPI,
            data = research_data)

plot(ideallm)

#glm using gaussian distribution

#glm using all features for reference
glmout <- glm(fatality_number ~ year + gdp_per_capita
            + vehicles_registered_in_millions
            + youth_proportion + employment_rate + avg_petrol_price
            + CPI + TWI_data,
            family = gaussian(),
            data = research_data)

plot(glmout)

poissontest <- glm(fatality_number ~ youth_proportion,
            family = poisson(link = "log"),
            data = research_data)

plot(poissontest)

poissonglm <- glm(fatality_number ~ year + gdp_per_capita
            + vehicles_registered_in_millions
            + youth_proportion + employment_rate + avg_petrol_price
            + CPI + TWI_data,
            family = poisson(link = "log"),
            data = research_data)

plot(poissonglm)