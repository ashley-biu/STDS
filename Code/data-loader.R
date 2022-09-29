# Load in datasets ---------------------------
library(tidyverse)
library(lubridate)
library(readxl)

configure_settings <- function() {
  Sys.setlocale("LC_TIME", "C")
}

load_crash_data <- function(research_data){
  
  # load in fatalities data
  fatal_crashes <-
    read_csv("Data/ardd_fatal_crashes_jul2022.csv")
  
  # convert fatal crashes df to tibble
  fatal_crashes <- as_tibble(fatal_crashes)
  
  # rename column names for data hygiene
  names(fatal_crashes) <-
    c(
      'crash_id',
      'state',
      'month',
      'year',
      'day_week',
      'time',
      'crash_type',
      'fatality_num',
      'bus_involvement',
      'heavy_truck_involved',
      'articulated_truck_involved',
      'speed_limit',
      'remoteness',
      'sa4',
      "lga",
      "road_type",
      "christmas_period",
      "easter_period",
      "day_of_week",
      "time_of_day"
    )
  
  # filter fatal crashes by NSW
  nsw_fatal_crashes <- fatal_crashes %>%
    filter(state == "NSW")
  
  crashes_with_quarter <- nsw_fatal_crashes %>% mutate(quarter = case_when(
    between(month, 1, 3) ~ "Q1",
    between(month, 4, 6) ~ "Q2",
    between(month, 7, 9) ~ "Q3",
    between(month, 10, 12) ~ "Q4",
    TRUE ~ NA_character_
  ))
  
  fatalities_by_quarter_and_year <- crashes_with_quarter %>%
    count(quarter, year)
  
  # rename count column to fatality number
  names(fatalities_by_quarter_and_year)[which(names(fatalities_by_quarter_and_year) == "n")] <-
    "fatality_number"
  
  fatalities_by_quarter_and_year$time_frame <- paste(fatalities_by_quarter_and_year$year,fatalities_by_quarter_and_year$quarter, sep = "-")
 
  # create new DF and add yearly fatalities
  research_data <- fatalities_by_quarter_and_year
  
  return(research_data)
}

# PETROL DATA ---------------------------

load_petrol_data <- function(research_data) {
  
  # Read in dataset
  petrol <-
    read_excel("Data/AIP_Annual_Retail_Price_Data.xlsx",
               sheet = "Average Petrol Retail")
  
  # Convert petrol dataset to tibble
  petrol <- as_tibble(petrol)
  
  # Remove subheading rows
  petrol <-  petrol[3:22, ]
  
  # Rename Petrol year column
  names(petrol)[which(names(petrol) == "AVERAGE PETROL RETAIL PRICE")] <-
    "year"
  
  # Select Petrol prices for NSW
  nsw_petrol <- petrol %>%
    dplyr::select("year", "NSW")
  
  # Rename NSW column to avg_petrol_price
  names(nsw_petrol)[2] <- c("avg_petrol_price")
  
  # Join petrol dataset to research DF with similar years
  # (It will discard years that are not similar for exploratory purposes)
  research_data <- merge(research_data, nsw_petrol)
  
  return(research_data)
}

# CPI DATA  ---------------------------

load_CPI_data <- function(research_data) {
  
  # load in dataset
  cpi_data <- read_excel(
    "Data/CPI_Weighted average.xlsx",
    sheet = "Data1",
    range = cell_cols("A:B"),
    col_types = c("date", "numeric")
  )
  
  # remove rows 10 to 305
  cpi_data <-  cpi_data[10:305, ]
  
  # rename CPI column names for data hygiene
  names(cpi_data) <- c('date', 'cpi_per_quarter')
  
  cpi_data$date <- as.Date(cpi_data$date, format = "%Y-%m-%d")
  
  # create new CPI column for month from date
  cpi_data$month <- month(cpi_data$date)
  
  # create new CPI column for year from date
  cpi_data$year <- strftime(cpi_data$date, "%Y")
  
  cpi_data <- cpi_data %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # convert date to date format.
  
  cpi_quarterly <- cpi_data %>%
    group_by(time_frame) %>%
    dplyr::summarize(CPI = mean(cpi_per_quarter, na.rm = TRUE)) %>%
    as.data.frame
  
  research_data <- merge(research_data, cpi_quarterly)
  return(research_data)
}

# NSW EMPLOYMENT RATE DATA -----------------------

load_employment_data <- function(research_data) {
  
  # import employment data
  employment <- read_csv("Data/ABS_NSW_emp.csv")
  
  # set employment to a tibble
  employment <- as_tibble(employment)
  
  # use filter to get employment data
  nsw_employment <- employment %>%
    filter(`REGION: Region` == "1: New South Wales") %>%
    filter(`MEASURE: Measure` == "M16: Employment to population ratio") %>%
    filter(`TSEST: Adjustment Type` == "20: Seasonally Adjusted") %>%
    filter(`SEX: Sex` == "3: Persons")
  
  #unemployment rates
  nsw_unemployment <- employment %>%
    filter(`REGION: Region` == "1: New South Wales") %>%
    filter(`MEASURE: Measure` == "M13: Unemployment rate") %>%
    filter(`TSEST: Adjustment Type` == "20: Seasonally Adjusted") %>%
    filter(`SEX: Sex` == "3: Persons")
  
  # convert time period to date format
  nsw_employment$date <- paste('01 ', nsw_employment$`TIME_PERIOD: Time Period`) %>%
    as.Date(nsw_employment$date, format = "%d %Y-%m")
  
  nsw_unemployment$date <- paste('01 ', nsw_unemployment$`TIME_PERIOD: Time Period`) %>%
    as.Date(nsw_unemployment$date, format = "%d %Y-%m")  
  
  # create new column for month from date
  nsw_employment$month <- strftime(nsw_employment$date, "%m")
  nsw_unemployment$month <- strftime(nsw_unemployment$date, "%m")
  
  # create new column for year from date
  nsw_employment$year <- strftime(nsw_employment$date, "%Y")
  nsw_unemployment$year <- strftime(nsw_unemployment$date, "%Y")
  
  # update date grouping to quarters
  nsw_employment <- nsw_employment %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  nsw_unemployment <- nsw_unemployment %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # find mean employment rate by quarter
  nsw_emp_quarterly <- nsw_employment %>%
    group_by(time_frame) %>%
    dplyr::summarize(employment_rate = mean(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  nsw_unemp_quarterly <- nsw_unemployment %>%
    group_by(time_frame) %>%
    dplyr::summarize(unemployment_rate = mean(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  # new combined df
  nsw_emp_rates_quarterly <- data.frame(
    nsw_emp_quarterly$time_frame, 
    nsw_emp_quarterly$employment_rate,
    nsw_unemp_quarterly$unemployment_rate
  )
  
  names(nsw_emp_rates_quarterly) <- c('time_frame', 'employment_rate', 'unemployment_rate') 
  
  # add employment rates for NSW to final analysis data
  research_data <- merge(research_data, nsw_emp_rates_quarterly, by = "time_frame")
  
  return(research_data)
}

# NSW POPULATION DATA ---------------------------

load_NSW_population_data <- function(research_data){
  
  nsw_population <- read_csv("Data/ABS_NSW_pop_sum.csv")
  
  # convert nsw population df to tibble
  nsw_population <- as_tibble(nsw_population)
  nsw_population <- nsw_population %>% rename(time_frame = `TIME_PERIOD: Time Period`,
                                              age = `AGE: Age`)
  
  nsw_totalpop_quarterly <- nsw_population %>%
    group_by(time_frame, age) %>%
    dplyr::summarize(tot_pop_num = sum(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  # define youth group ages
  youth_group <-
    c("A04: 0-4", "A59: 5-9", "A10: 10-14", "A15: 15-19", "A20: 20-24")
  
  #summarise by quarterly total
  nsw_youth_prop_quarterly <- nsw_totalpop_quarterly %>% 
    filter(age %in% youth_group) %>% 
    group_by(time_frame) %>%
    dplyr::summarize(youth_proportion = sum(tot_pop_num, na.rm = TRUE)) %>%
    as.data.frame()
  
  nsw_totalpop_quarterly <- nsw_population %>%
    group_by(time_frame) %>%
    dplyr::summarize(tot_pop_num = sum(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  #new df
  nsw_pop <- data.frame(
    nsw_totalpop_quarterly$time_frame, 
    nsw_totalpop_quarterly$tot_pop_num,
    nsw_youth_prop_quarterly$youth_proportion
  )
  
  names(nsw_pop) <- c('time_frame', 'tot_pop_num', 'youth_proportion')   
  
  # add annual young age group proportion for NSW to final analysis data
  research_data <- merge(research_data, nsw_pop, by="time_frame")
  
  return(research_data)
}

# AUS GDP PER CAP DATA ---------------------------

load_GDP_data <- function(research_data) {
  
  # import gdp aus data
  gdp_qrtly <-
    read_csv("Data/AUSTRALIA_QUARTERLY_GDP_PER_CAPITA.csv")
  
  # convert gdp dataset to tibble
  gdp_qrtly <- as_tibble(gdp_qrtly)
  
  gdp_qrtly <- rename(gdp_qrtly, time_frame = `TIME_PERIOD: Time Period`)
  
  gdp_qrtly <- gdp_qrtly %>%
    group_by(time_frame) %>%
    dplyr::summarize(gdp_per_capita = mean(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  # add annual GDP yearly to final analysis data
  research_data <- merge(research_data, gdp_qrtly, by="time_frame")
  
  return(research_data)
}

# NSW REGISTRATION DATA ---------------------------

load_registration_data <- function(research_data) {
  
  # import NSW registration data
  nsw_vehicles_registered <-
    read_excel("Data/NSW_vehicles_registered.xlsx")
  
  # convert nsw registration data to tibble
  nsw_vehicles_registered <- as_tibble(nsw_vehicles_registered)
  
  # Convert date column to date format
  nsw_vehicles_registered$date <- as.Date(nsw_vehicles_registered$QUARTER, format = "%Y-%m-%d")
  
  # create new column for month from date
  nsw_vehicles_registered$month <- strftime(nsw_vehicles_registered$date, "%m")
  
  # create new column for year from date
  nsw_vehicles_registered$year <- strftime(nsw_vehicles_registered$date, "%Y")
  
  # split motorcycles & scooter data into new df
  nsw_vehicles_motorcycles_scooters <- data.frame(
    nsw_vehicles_registered$`Light Vehicles Motor cycles` + nsw_vehicles_registered$`Light Vehicles Scooters`,
    nsw_vehicles_registered$date,
    nsw_vehicles_registered$month,
    nsw_vehicles_registered$year
  )
  
  names(nsw_vehicles_motorcycles_scooters) <- c('total', 'date', 'month', 'year')
  
  # split light vehicles data into new df
  nsw_vehicles_light <- data.frame(
    nsw_vehicles_registered$`Light Vehicles All`- (nsw_vehicles_registered$`Light Vehicles Motor cycles` + nsw_vehicles_registered$`Light Vehicles Scooters`),
    nsw_vehicles_registered$date,
    nsw_vehicles_registered$month,
    nsw_vehicles_registered$year
  )
  
  names(nsw_vehicles_light) <- c('total', 'date', 'month', 'year')
  
  # split heavy vehicles data into new df
  nsw_vehicles_heavy <- data.frame(
    nsw_vehicles_registered$`Heavy Vehicles All`,
    nsw_vehicles_registered$date,
    nsw_vehicles_registered$month,
    nsw_vehicles_registered$year
  )
  
  names(nsw_vehicles_heavy) <- c('total', 'date', 'month', 'year') 
  
  # update date grouping to quarters
  nsw_vehicles_motorcycles_scooters <- nsw_vehicles_motorcycles_scooters %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  
  nsw_vehicles_light <- nsw_vehicles_light %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  nsw_vehicles_heavy <- nsw_vehicles_heavy %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # find registered vehicles in millions by year for all the dfs
  registered_vehicles_quarterly_motorcycles <- nsw_vehicles_motorcycles_scooters %>%
    group_by(time_frame) %>%
    dplyr::summarize(motorcycles_registered_in_millions = sum(`total`, na.rm =
                                                                TRUE) / 1000000) %>%
    as.data.frame()
  
  registered_vehicles_quarterly_light <- nsw_vehicles_light %>%
    group_by(time_frame) %>%
    dplyr::summarize(lightvehicles_registered_in_millions = sum(`total`, na.rm =
                                                                  TRUE) / 1000000) %>%
    as.data.frame()
  
  registered_vehicles_quarterly_heavy <- nsw_vehicles_heavy %>%
    group_by(time_frame) %>%
    dplyr::summarize(heavyvehicles_registered_in_millions = sum(`total`, na.rm =
                                                                  TRUE) / 1000000) %>%
    as.data.frame()
  
  # new combined df
  registered_vehicles_quarterly <- data.frame(
    registered_vehicles_quarterly_light$time_frame, 
    registered_vehicles_quarterly_light$lightvehicles_registered_in_millions,
    registered_vehicles_quarterly_heavy$heavyvehicles_registered_in_millions,
    registered_vehicles_quarterly_motorcycles$motorcycles_registered_in_millions,
    registered_vehicles_quarterly_light$lightvehicles_registered_in_millions + registered_vehicles_quarterly_heavy$heavyvehicles_registered_in_millions + registered_vehicles_quarterly_motorcycles$motorcycles_registered_in_millions
  )
  
  names(registered_vehicles_quarterly) <- c('time_frame', 'registered_light_vehicles', 'registered_heavy_vehicles', 'registered_motorcycles', 'vehicles_registered_in_millions') 
  
  # add registered vehicles in millions to final analysis data
  research_data <- merge(research_data, registered_vehicles_quarterly,  by = "time_frame")
  
  
  return(research_data)
}

# TWI AUS DATA ---------------------------

load_TWI_data <- function(research_data) {
  
  # load TWI first dataset containing years 1969 to 2009
  twi_exchange_data_1 <- read_csv("Data/twi_data_1969_2009.csv")
  
  # load TWI second dataset containing years 2010 to 2022
  twi_exchange_data_2 <- read_csv("Data/twi_data_2010_2022.csv")
  
  # select date and twi colummns from TWI dataset 1
  twi_data_1 <- twi_exchange_data_1 %>%
    dplyr::select(`Series ID`, FXRTWI)
  
  # select date and twi colummns from TWI dataset 2
  twi_data_2 <- twi_exchange_data_2 %>%
    dplyr::select(`Series ID`, FXRTWI)
  
  # remove NA values from twi_data_1 (empty twi values from first 10 rows)
  twi_data_1 <- twi_data_1 %>%
    drop_na()
  
  # remove NA values from twi_data_2 (empty rows from end of dataset)
  twi_data_2 <- twi_data_2 %>%
    drop_na()
  
  # add dates to 
  twi_data_1$`Series ID` <- as.Date(twi_data_1$`Series ID`, format = "%d-%b-%Y")
  twi_data_2$`Series ID` <- as.Date(twi_data_2$`Series ID`, format = "%d-%b-%y")
  
  # Combine TWI datasets
  twi_data <- rbind(twi_data_1, twi_data_2)
  
  # rename twi columns for data hygiene
  twi_data <- rename(twi_data, c(date = `Series ID`, twi = FXRTWI))  
  
  # create new column for month from date
  twi_data$month <- month(twi_data$date)
  
  # create new column for year from date
  twi_data$year <- year(twi_data$date)
  
  twi_data <- twi_data %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # find twi by year
  twi_quarterly <- twi_data %>%
    group_by(time_frame) %>%
    dplyr::summarize(TWI_data = mean(twi, na.rm = TRUE)) %>%
    as.data.frame()
  
  # add annual twi to final analysis data
  research_data <- merge(research_data, twi_quarterly, by = "time_frame")
  
  return(research_data)
}

# TGP ---------------------------

load_tgp_data <- function(research_data) {
  
  # import TGP
  sydney_tgp <-
    read_excel("Data/AIP_TGP_Data_16-Sep-2022.xlsx", sheet = 2)
  
  # convert data to tibble
  sydney_tgp <- as_tibble(sydney_tgp)
  
  # update names
  names(sydney_tgp) <- c('date', 'sydney', 'melbourne', 'brisbane', 'adelaide', 'perth', 'darwin', 'hobart', 'national_average')
  
  # convert date column to date format.
  sydney_tgp$date <- as.Date(sydney_tgp$date, format = "%Y-%m-%d")
  
  # create new column for month from date
  sydney_tgp$month <- strftime(sydney_tgp$date, "%m")
  
  # create new column for year from date
  sydney_tgp$year <- strftime(sydney_tgp$date, "%Y")
  
  # select TGP for Sydney
  sydney_tgp <- sydney_tgp %>%
    dplyr::select("sydney", "date", "month", "year" )
  
  # update date grouping to quarters
  sydney_tgp <- sydney_tgp %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # summarise tgp by quarterly mean
  sydney_tgp_quarterly <- sydney_tgp %>%
    group_by(time_frame) %>%
    dplyr::summarize(TGP = mean(sydney, na.rm = TRUE)) %>%
    as.data.frame
  
  # add tgp to final analysis data
  research_data <- merge(research_data, sydney_tgp_quarterly)
  
  return(research_data)
  
}

# Brent_Oil ---------------------------

load_brent_oil_data <- function(research_data) {
  
  # import brent oil
  brent_oil <-
    read_csv("Data/Brent_Oil_Futures_Historical_Data.csv")
  
  # convert data to tibble
  brent_oil <- as_tibble(brent_oil)
  
  # update names
  names(brent_oil) <- c('date', 'price', 'open', 'high', 'low', 'vol', 'change')
  
  # convert date column to date format.
  brent_oil$date <- as.Date(brent_oil$date, format = "%d/%m/%Y")
  
  # create new column for month from date
  brent_oil$month <- strftime(brent_oil$date, "%m")
  
  # create new column for year from date
  brent_oil$year <- strftime(brent_oil$date, "%Y")
  
  # select columns needed
  brent_oil <- brent_oil %>%
    dplyr::select("price", "date", "month", "year" )
  
  # update date grouping to quarters
  brent_oil <- brent_oil %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # summarise by quarterly mean
  brent_oil_quarterly <- brent_oil %>%
    group_by(time_frame) %>%
    dplyr::summarize(brent_oil_price = mean(price, na.rm = TRUE)) %>%
    as.data.frame
  
  # add brent oil price to final analysis data
  research_data <- merge(research_data, brent_oil_quarterly)
  
  return(research_data)
  
}


# Petrol and Diesel Prices ---------------------------

load_petrol_price_data <- function(research_data) {
  
  # import price data
  petrol_prices <-
    read_csv("Data/Monthly-ULP-prices-Metro-200101-202209.csv")
  
  diesel_prices <-
    read_csv("Data/Monthly-Diesel-prices-Metro-200101-202209.csv")
  
  # convert data to tibble
  petrol_prices <- as_tibble(petrol_prices)
  diesel_prices <- as_tibble(diesel_prices)
  
  # update names
  names(petrol_prices) <- c('date', 'region', 'product', 'price')
  names(diesel_prices) <- c('date', 'region', 'product', 'price')
  
  # convert date column to date format
  petrol_prices$date <- paste('01 ', petrol_prices$date) %>%
    as.Date(petrol_prices$date, format = "%d %B %Y")
  
  diesel_prices$date <- paste('01 ', diesel_prices$date) %>%
    as.Date(diesel_prices$date, format = "%d %B %Y")
  
  # create new column for month from date
  petrol_prices$month <- strftime(petrol_prices$date, "%m")
  diesel_prices$month <- strftime(diesel_prices$date, "%m")
  
  # create new column for year from date
  petrol_prices$year <- strftime(petrol_prices$date, "%Y")
  diesel_prices$year <- strftime(diesel_prices$date, "%Y")
  
  # select columns
  petrol_prices <- petrol_prices %>%
    dplyr::select("price", "date", "month", "year" )
  diesel_prices <- diesel_prices %>%
    dplyr::select("price", "date", "month", "year" )
  
  # update date grouping to quarters
  petrol_prices <- petrol_prices %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  diesel_prices <- diesel_prices %>% mutate(time_frame = case_when(
    between(month, 1, 3) ~ paste(year,"-Q1", sep = ""),
    between(month, 4, 6) ~ paste(year,"-Q2", sep = ""),
    between(month, 7, 9) ~ paste(year,"-Q3", sep = ""),
    between(month, 10, 12) ~ paste(year,"-Q4", sep = ""),
  ))
  
  # summarise by quarterly mean
  petrol_prices_quarterly <- petrol_prices%>%
    group_by(time_frame) %>%
    dplyr::summarize(petrol_price = mean(price, na.rm = TRUE)) %>%
    as.data.frame
  
  diesel_prices_quarterly <- diesel_prices%>%
    group_by(time_frame) %>%
    dplyr::summarize(diesel_price = mean(price, na.rm = TRUE)) %>%
    as.data.frame
  
  # new combined df
  petrol_diesel_prices_quarterly <- data.frame(
    petrol_prices_quarterly$time_frame, 
    petrol_prices_quarterly$petrol_price,
    diesel_prices_quarterly$diesel_price
  )
  
  names(petrol_diesel_prices_quarterly) <- c('time_frame', 'petrol_price', 'diesel_price') 
  
  # add prices to final analysis data
  research_data <- merge(research_data, petrol_diesel_prices_quarterly)
  
  return(research_data)

}


load_datasets <- function() {
  
  configure_settings()
  research_data <- load_crash_data(NULL)
  research_data <- load_CPI_data(research_data)
  research_data <- load_employment_data(research_data)
  research_data <- load_NSW_population_data(research_data)
  research_data <- load_GDP_data(research_data)
  research_data <- load_registration_data(research_data)
  research_data <- load_TWI_data(research_data)
  #research_data <- load_tgp_data(research_data)
  #research_data <- load_brent_oil_data(research_data)
  research_data <- load_petrol_price_data(research_data)
  
  return(research_data)
}


