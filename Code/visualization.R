source("Code/data-loader.R")
library(DataExplorer)
library(gridExtra)

rm(list=ls())

research_data <- load_datasets()

research_data_vis <-research_data[,-2:-3]
research_data_vis2 <-research_data[,-1:-3]

#histograms for outliers
plot_histogram(research_data)

# plot SCATTERPLOT for each pair
pairs(~ ., data = research_data_vis2,
      main = "Figure 3: Scatterplot Matrix")


# Correlation plots for each pair with some formatting
plot_correlation(research_data_vis, title = "Figure 4: Correlation Matrix of Macro Variables and Road Fatalities") +
  
  scale_x_discrete(breaks=c(
    "year", 
    "diesel_price", 
    "petrol_price", 
    "fatality_number", 
    "CPI", 
    "employment_rate", 
    "unemployment_rate", 
    "youth_proportion_in_millions", 
    "tot_pop_num_in_millions", 
    "gdp_per_capita", 
    "vehicles_registered_in_millions", 
    "TWI_data", 
    "registered_motorcycles", 
    "registered_heavy_vehicles", 
    "registered_light_vehicles"),
    labels=c(
      "Year", 
      "Diesel price", 
      "Petrol price", 
      "Fatalities", 
      "CPI", 
      "Employment rate", 
      "Unemployment rate", 
      "Youth proportion", 
      "Total population", 
      "GDP per capita", 
      "Vehicles registered", 
      "TWI", 
      "Registered motorcycles", 
      "Registered heavy vehicles", 
      "registered light vehicles" )) +
  
  scale_y_discrete(breaks=c(
    "year", 
    "diesel_price", 
    "petrol_price", 
    "fatality_number", 
    "CPI", 
    "employment_rate", 
    "unemployment_rate", 
    "youth_proportion_in_millions", 
    "tot_pop_num_in_millions", 
    "gdp_per_capita", 
    "vehicles_registered_in_millions", 
    "TWI_data", 
    "registered_motorcycles", 
    "registered_heavy_vehicles", 
    "registered_light_vehicles"),
    labels=c(
      "Year", 
      "Diesel price", 
      "Petrol price", 
      "Fatalities", 
      "CPI", 
      "Employment rate", 
      "Unemployment rate", 
      "Youth proportion", 
      "Total population", 
      "GDP per capita", 
      "Vehicles registered", 
      "TWI", 
      "Registered motorcycles", 
      "Registered heavy vehicles", 
      "registered light vehicles" )) +
  
  
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.title = element_blank()
  )



# DATA VISUALISATIONS ---------------------------

# Plot frequency of fatalities per year
ggplot(research_data, aes(x = year, y = fatality_number)) +
  geom_line(stat = "identity") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Fatality Numbers",
    title = paste("NSW Fatality Numbers by Year")
  )


# plot petrol NSW data
ggplot(research_data, aes(x = year, y = avg_petrol_price, group = 1)) +
  geom_point(stat = "identity") +
  geom_line() +
  theme_classic() +
  labs(
    x = "Year",
    y = "Average Price",
    title = paste("Average Petrol Retail Price in NSW from 2002 to 2021")
  )

# plot research_data variables
ggplot(research_data)  +
  geom_bar(
    aes(x = year, y = avg_petrol_price),
    stat = "identity",
    fill = "cyan",
    colour = "#006000"
  ) +
  geom_line(aes(x = year, y = fatality_number),
            stat = "identity",
            color = "red") +
  labs(title = "Road death versus average petrol price in NSW, 2002-2021",
       x = "Year", y = "Road deaths per year") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1.0, name = "Cents per litre"))


# scatterplots for each variable -----------------------

# diesel price
glm1 <- glm(fatality_number ~ diesel_price, 
            data = research_data_vis, family = poisson)

mat1 <- ggplot(research_data_vis, aes(x=diesel_price, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Diesel Price") + 
  xlab("Diesel Price") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm1)[1],
              slope = coef(glm1)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat1

# petrol price
glm2 <- glm(fatality_number ~ petrol_price, 
            data = research_data_vis, family = poisson)

mat2 <- ggplot(research_data_vis, aes(x=petrol_price, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Petrol Price") + 
  xlab("Petrol Price") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm2)[1],
              slope = coef(glm2)[2],
              colour = 'blue', size = 1, alpha = 0.7)+
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat2


# CPI
glm3 <- glm(fatality_number ~ CPI, 
            data = research_data_vis, family = poisson)

mat3 <- ggplot(research_data_vis, aes(x=CPI, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & CPI") + 
  xlab("CPI") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm3)[1],
              slope = coef(glm3)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat3


# TWI
glm4 <- glm(fatality_number ~ TWI_data, 
            data = research_data_vis, family = poisson)

mat4 <- ggplot(research_data_vis, aes(x=TWI_data, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & TWI") + 
  xlab("TWI") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm4)[1],
              slope = coef(glm4)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat4

# Employment rate
glm5 <- glm(fatality_number ~ employment_rate, 
            data = research_data_vis, family = poisson)

mat5 <- ggplot(research_data_vis, aes(x=employment_rate, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Employment Rate") + 
  xlab("Employment Rate") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm5)[1],
              slope = coef(glm5)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat5

# Unemployment rate
glm6 <- glm(fatality_number ~ unemployment_rate, 
            data = research_data_vis, family = poisson)

mat6 <- ggplot(research_data_vis, aes(x=unemployment_rate, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Unemployment Rate") + 
  xlab("Unemployment Rate") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm6)[1],
              slope = coef(glm6)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat6

# Population
glm7 <- glm(fatality_number ~ tot_pop_num_in_millions, 
            data = research_data_vis, family = poisson)

mat7 <- ggplot(research_data_vis, aes(x=tot_pop_num_in_millions, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Total Population") + 
  xlab("Total Population") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm7)[1],
              slope = coef(glm7)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat7

# youth population
glm8 <- glm(fatality_number ~ youth_proportion_in_millions, 
            data = research_data_vis, family = poisson)

mat8 <- ggplot(research_data_vis, aes(x=youth_proportion_in_millions, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Youth Population") + 
  xlab("Youth Population") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm8)[1],
              slope = coef(glm8)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat8

# GDP

glm9 <- glm(fatality_number ~ gdp_per_capita, 
            data = research_data_vis, family = poisson)

mat9 <- ggplot(research_data_vis, aes(x=gdp_per_capita, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & GDP per capita") + 
  xlab("GDP per capita") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm9)[1],
              slope = coef(glm9)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat9

# Light vehicles

glm10 <- glm(fatality_number ~ registered_light_vehicles, 
             data = research_data_vis, family = poisson)

mat10 <- ggplot(research_data_vis, aes(x=registered_light_vehicles, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Registered light vehicles") + 
  xlab("Registered light vehicles") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm10)[1],
              slope = coef(glm10)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat10

# Heavy vehicles

glm11 <- glm(fatality_number ~ registered_heavy_vehicles, 
             data = research_data_vis, family = poisson)

mat11 <- ggplot(research_data_vis, aes(x=registered_heavy_vehicles, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Registered heavy vehicles") + 
  xlab("Registered heavy vehicles") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm11)[1],
              slope = coef(glm11)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat11

# Motorcycles

glm12 <- glm(fatality_number ~ registered_motorcycles, 
             data = research_data_vis, family = poisson)

mat12 <- ggplot(research_data_vis, aes(x=registered_motorcycles, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & Registered motorcycles") + 
  xlab("Registered motorcycles") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm12)[1],
              slope = coef(glm12)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat12

# All vehicles

glm13 <- glm(fatality_number ~ vehicles_registered_in_millions, 
             data = research_data_vis, family = poisson)

mat13 <- ggplot(research_data_vis, aes(x=vehicles_registered_in_millions, y=fatality_number)) +
  geom_point() + 
  theme_bw() + 
  labs(title = "Scatterplot - Fatalities & All registered vehicles") + 
  xlab("All registered vehicles") +
  ylab("Fatality Number") + 
  
  geom_abline(intercept = coef(glm13)[1],
              slope = coef(glm13)[2],
              colour = 'blue', size = 1, alpha = 0.7) +
  
  geom_smooth(method = lm) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color ="light grey", fill = NA, size = 0.5),
    legend.title = element_text()
  )

mat13

# arrange scatterplots into grid
gridExtra::grid.arrange(mat1, 
                        mat2, 
                        mat3, 
                        mat4,
                        mat5,
                        mat6,
                        mat7,
                        mat8,
                        mat9,
                        mat10,
                        mat11,
                        mat12,
                        mat13,
                        ncol=2)

fit <- goodfit(research_data_vis$fatality_number) 
summary(fit) 
rootogram(fit)
Ord_plot(research_data_vis$fatality_number)

distplot(research_data_vis$fatality_number, type="poisson")
distplot(research_data_vis$fatality_number, type="nbinomial")


