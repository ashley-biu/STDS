source("Code/data-loader.R")
library(DataExplorer)

rm(list=ls())

research_data <- load_datasets()

head(research_data)

# plot SCATTERPLOT for each pair
pairs(~ ., data = research_data,
       main = "Figure 1: Scatterplot Matrix")


# Correlation plots for each pair
plot_correlation(research_data,
                 type = "all",
                 title = "Figure 2: Correlation Matrix of Macro Variables and Road Fatalities")


?plot_correlation

# Correlation plots for each pair with some formatting
plot_correlation(research_data,
                 type = "all",
                 title = "Figure 2: Correlation Matrix of Macro Variables and Road Fatalities") +
  
  scale_x_discrete(breaks=c("year", "avg_petrol_price", "fatality_number",  "CPI", "employment_rate", "youth_proportion", "gdp_per_capita", "vehicles_registered_in_millions", "TWI_data"),
                   labels=c("Year", "Average petrol price", "Fatalities", "CPI", "Employment rate", "Youth proportion", "GDP per capita", "Vehicles registered", "TWI")) +
  
  scale_y_discrete(breaks=c("TWI_data", "vehicles_registered_in_millions", "gdp_per_capita", "youth_proportion", "employment_rate", "CPI", "fatality_number", "avg_petrol_price", "year"),
                   labels=c("TWI", "Vehicles registered", "GDP per capita", "Youth proportion", "Employment rate", "CPI", "Fatalities", "Average petrol price", "Year")) +
  

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