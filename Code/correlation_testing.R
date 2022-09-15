source("Code/data-loader.R")
library(AER)

research_data <- load_datasets()
quaterly <- load_datasets_quaterly()

head(research_data)

#explains the least
cor(research_data$fatality_number, research_data$employment_rate)
#explains the most
cor(research_data$fatality_number, research_data$youth_proportion)

fatality_youth <- ggplot(data=research_data, aes(x=fatality_number,y=youth_proportion)) + 
  geom_point(size = 2) +
  theme_bw()

fatality_youth

lmout <- lm(fatality_number ~ 
            youth_proportion,
            data = research_data)

ggplot(lmout)
plot(lmout)

cor(quaterly$fatality_number, quaterly$youth_proportion)

plot <- ggplot(data=quaterly, aes(x=youth_proportion,y=fatality_number)) + 
  geom_point(size = 2) +
  geom_smooth(method='lm', color = "#faa92b") +
  xlab("Proportion of population under the age of 25") +
  ylab("Number of fatal accidents") +
  labs(title = "Trying to explain number of fatal accidents with youth proportion") +
  theme(plot.title = element_text(face = "bold", hjust = 0.3, size = 24),
        text = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plot

cor(research_data$year, research_data$avg_petrol_price)
#year and CPI are strongly positive correlated
cor(research_data$year, research_data$CPI)

year_CPI <- ggplot(data=research_data, aes(x=year,y=CPI)) + 
  geom_point(size = 2) +
  theme_bw()

year_CPI

cor(research_data$year, research_data$employment_rate)
cor(research_data$year, research_data$youth_proportion)
cor(research_data$year, research_data$gdp_per_capita)
cor(research_data$year, research_data$vehicles_registered_in_millions)

cor(research_data$avg_petrol_price, research_data$CPI)
cor(research_data$avg_petrol_price, research_data$employment_rate)
cor(research_data$avg_petrol_price, research_data$youth_proportion)
cor(research_data$avg_petrol_price, research_data$gdp_per_capita)
cor(research_data$avg_petrol_price, research_data$vehicles_registered_in_millions)
cor(research_data$avg_petrol_price, research_data$TWI_data)

cor(research_data$CPI, research_data$employment_rate)
cor(research_data$CPI, research_data$youth_proportion)

CPI_youth <- ggplot(data=research_data, aes(x=CPI,y=youth_proportion)) + 
  geom_point(size = 2) +
  theme_bw()

CPI_youth

cor(research_data$CPI, research_data$gdp_per_capita)

CPI_GDP <- ggplot(data=research_data, aes(x=CPI,y=gdp_per_capita)) + 
  geom_point(size = 2) +
  theme_bw()

CPI_GDP

cor(research_data$CPI, research_data$vehicles_registered_in_millions)

CPI_vehicles <- ggplot(data=research_data, aes(x=CPI,y=vehicles_registered_in_millions)) + 
  geom_point(size = 2) +
  theme_bw()

CPI_vehicles

cor(research_data$CPI, research_data$TWI_data)

cor(research_data$employment_rate, research_data$youth_proportion)
cor(research_data$employment_rate, research_data$gdp_per_capita)
cor(research_data$employment_rate, research_data$vehicles_registered_in_millions)
cor(research_data$employment_rate, research_data$TWI_data)

cor(research_data$youth_proportion, research_data$gdp_per_capita)

youth_GDP <- ggplot(data=research_data, aes(x=youth_proportion,y=gdp_per_capita)) + 
  geom_point(size = 2) +
  theme_bw()

youth_GDP

cor(research_data$youth_proportion, research_data$vehicles_registered_in_millions)

youth_vehicles <- ggplot(data=research_data, aes(x=youth_proportion,y=vehicles_registered_in_millions)) + 
  geom_point(size = 2) +
  theme_bw()

youth_vehicles

cor(research_data$youth_proportion, research_data$TWI_data)

cor(research_data$gdp_per_capita, research_data$vehicles_registered_in_millions)

GDP_vehicles <- ggplot(data=research_data, aes(x=gdp_per_capita,y=vehicles_registered_in_millions)) + 
  geom_point(size = 2) +
  theme_bw()

youth_vehicles

cor(research_data$gdp_per_capita, research_data$TWI_data)

cor(research_data$vehicles_registered_in_millions, research_data$TWI_data)


poissonglm <- glm(fatality_number ~ year + gdp_per_capita
            + vehicles_registered_in_millions
            + youth_proportion + employment_rate + avg_petrol_price
            + CPI + TWI_data,
            family = poisson(link = "log"),
            data = research_data)

summary(poissonglm)

dispersiontest(poissonglm)

plot(poissonglm)

plot_correlation(iris)
plot_correlation(iris, type = "c")
plot_correlation(airquality, cor_args = list("use" = "pairwise.complete.obs"))

?plot_correlation

library(GGally)
ggpairs(research_data) + theme_bw()
