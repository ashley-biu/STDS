# Load in datasets ---------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

load_crash_data <- function(research_data) {

    # Load in fatalities data
    fatal_crashes <-
    read_csv("Data/ardd_fatal_crashes_jul2022.csv")

    # Convert fatal crashes df to tibble
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

    #find numbers of death by year
    yearly_fatalities <- nsw_fatal_crashes %>%
    count(year)

    # rename count column to fatality number
    names(yearly_fatalities)[which(names(yearly_fatalities) == "n")] <-
    "fatality_number"

    # Create new DF and add yearly fatalities
    research_data <- yearly_fatalities

    return(research_data)
}

load_crash_data_by_quarter <- function(research_data){

        # Load in fatalities data
    fatal_crashes <-
    read_csv("Data/ardd_fatal_crashes_jul2022.csv")

    # Convert fatal crashes df to tibble
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
    fatalities_by_quarter_and_year <- select(fatalities_by_quarter_and_year[, c("time_frame", "fatality_number", "year", "quarter")],-3:-4)

      # Create new DF and add yearly fatalities
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
    select("year", "NSW")

    # Rename NSW column to avg_petrol_price
    names(nsw_petrol)[2] <- c("avg_petrol_price")

    # Join petrol dataset to research DF with similar years
    # (It will discard years that are not similar for exploratory purposes)
    research_data <- merge(research_data, nsw_petrol)

    return(research_data)
}

# CPI DATA  ---------------------------

load_CPI_data <- function(research_data) {

    # Load in dataset
    cpi_data <- read_excel(
    "Data/CPI_Weighted average.xlsx",
    sheet = "Data1",
    range = cell_cols("A:B"),
    col_types = c("date", "numeric")
    )

    # Remove rows 10 to 305
    cpi_data <-  cpi_data[10:305, ]

    # rename CPI column names for data hygiene
    names(cpi_data) <- c('date', 'cpi_per_quarter')

    # Convert date to date format.
    cpi_data$date <- as.Date(cpi_data$date, format = "%Y-%m-%d")

    # create new CPI column for month from date
    cpi_data$month <- strftime(cpi_data$date, "%m")

    # create new CPI column for year from date
    cpi_data$year <- strftime(cpi_data$date, "%Y")

    #find mean CPI rate by year
    cpi_yearly <- cpi_data %>%
    group_by(year) %>%
    dplyr::summarize(CPI = mean(cpi_per_quarter, na.rm = TRUE)) %>%
    as.data.frame()

    # merge annual CPI rate for NSW to final analysis data
    research_data <- merge(research_data, cpi_yearly)
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

    # Convert time period to date format.
    nsw_employment$date <-
    as.Date(as.yearmon(nsw_employment$`TIME_PERIOD: Time Period`))

    # create new employment column for employment month from date
    nsw_employment$month <- strftime(nsw_employment$date, "%m")

    # create new employment year column for year from date
    nsw_employment$year <- strftime(nsw_employment$date, "%Y")

    #find mean employment rate by year
    nsw_emp_yearly <- nsw_employment %>%
    group_by(year) %>%
    dplyr::summarize(employment_rate = mean(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()

    # add annual employment rate for NSW to final analysis data
    research_data <- merge(research_data, nsw_emp_yearly)

    return(research_data)
}

# NSW POPULATION DATA ---------------------------

load_NSW_population_data <- function(research_data) {

    # Import ABS NSW population data
    nsw_population <- read_csv("Data/ABS_NSW_pop_sum.csv")

    # Convert nsw population df to tibble
    nsw_population <- as_tibble(nsw_population)


    # split char TIME_PERIOD '%Y-Q' into Year and Quarter
    nsw_population  <-
    cbind(nsw_population,
            str_split_fixed(nsw_population$`TIME_PERIOD: Time Period`, "-", 2))

    # rename newly created year and quarter columns
    names(nsw_population)[12:13] <- c("year", "quarter")

    # calculate total population by year
    year_values <- nsw_population %>%
    group_by(year) %>%
    dplyr::summarize(tot_pop_num = sum(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()

    # rename age column
    nsw_population <- nsw_population %>%
    rename(age = `AGE: Age`)

    # Calculate total population by year and age group
    total_population_year <- nsw_population %>%
    group_by(year, age) %>%
    dplyr::summarize(pop_num = sum(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()

    # Merge new population and total population by year DF's
    nsw_young_prop <- merge(total_population_year, year_values)

    # Define youth group ages
    youth_group <-
    c("A04: 0-4", "A59: 5-9", "A10: 10-14", "A15: 15-19", "A20: 20-24")

    # Get proportion for youth group by year (< 25 years old)
    nsw_youth_prop_yearly <- nsw_young_prop %>%
    mutate(proportion_of_youth = pop_num / tot_pop_num * 100)  %>%
    filter(age %in% youth_group) %>%
    group_by(year) %>%
    dplyr::summarize(youth_proportion = sum(proportion_of_youth, na.rm = TRUE)) %>%
    as.data.frame()

    # add annual young age group proportion for NSW to final analysis data
    research_data <- merge(research_data, nsw_youth_prop_yearly)

    return(research_data)
}

load_NSW_population_data_by_quarter <- function(research_data){
  nsw_population <- read_csv("STDS/STDS_AT2_2022/Data/ABS_NSW_pop_sum.csv")
  # Convert nsw population df to tibble
  nsw_population <- as_tibble(nsw_population)
  nsw_population <- nsw_population %>% rename(time_frame = `TIME_PERIOD: Time Period`, age = `AGE: Age`)
  
  nsw_youth_prop_quaterly <- nsw_population %>%
    group_by(time_frame, age) %>%
    dplyr::summarize(tot_pop_num = sum(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()
  
  # Define youth group ages
  youth_group <-
    c("A04: 0-4", "A59: 5-9", "A10: 10-14", "A15: 15-19", "A20: 20-24")
  
  nsw_youth_prop_quaterly <- nsw_youth_prop_quaterly %>% filter(age %in% youth_group) %>% group_by(time_frame) %>%#
    dplyr::summarize(youth_proportion = sum(tot_pop_num, na.rm = TRUE)) %>%
    as.data.frame()
  
  # add annual young age group proportion for NSW to final analysis data
  research_data <- merge(research_data, nsw_youth_prop_quaterly)
  
  return(research_data)
}

# AUS GDP PER CAP DATA ---------------------------

load_GDP_data <- function(research_data) {

    # Import gdp aus data
    gdp_qrtly <-
    read_csv("Data/AUSTRALIA_QUARTERLY_GDP_PER_CAPITA.csv")

    # convert gdp dataset to tibble
    gdp_qrtly <- as_tibble(gdp_qrtly)

    # split char TIME_PERIOD into Year and Quarter
    gdp_qrtly  <-
    cbind(gdp_qrtly,
            str_split_fixed(gdp_qrtly$`TIME_PERIOD: Time Period`, "-", 2))

    # rename year and quarter columns
    names(gdp_qrtly)[13:14] <- c("year", "quarter")

    #find GDP by year
    gdp_yearly <- gdp_qrtly %>%
    filter(`MEASURE: Measure` == "M3: Current prices") %>%
    group_by(year) %>%
    dplyr::summarize(gdp_per_capita = mean(OBS_VALUE, na.rm = TRUE)) %>%
    as.data.frame()

    # add annual GDP yearly to final analysis data
    research_data <- merge(research_data, gdp_yearly)

    return(research_data)
}

# NSW REGISTRATION DATA ---------------------------

load_registration_data <- function(research_data) {

    # Import NSW registration data
    nsw_vehicles_registered <-
    read_excel("Data/NSW_vehicles_registered.xlsx")

    # convert nsw registration data to tibble
    nsw_vehicles_registered <- as_tibble(nsw_vehicles_registered)

    # Convert date column to date format.
    nsw_vehicles_registered$date <-
    as.Date(nsw_vehicles_registered$QUARTER, format = "%Y-%m-%d")

    # create new CPI column for month from date
    nsw_vehicles_registered$month <-
    strftime(nsw_vehicles_registered$date, "%m")

    # create new CPI column for year from date
    nsw_vehicles_registered$year <-
    strftime(nsw_vehicles_registered$date, "%Y")

    #find registered vehicles in millions by year
    registerd_vehicles_yearly <- nsw_vehicles_registered %>%
    group_by(year) %>%
    dplyr::summarize(vehicles_registered_in_millions = sum(`GRAND TOTAL`, na.rm =
                                                            TRUE) / 1000000) %>%
    as.data.frame()

    # add annual registered vehicles in millions to final analysis data
    research_data <- merge(research_data, registerd_vehicles_yearly)

    return(research_data)
}

# TWI AUS DATA ---------------------------

load_TWI_data <- function(research_data) {

    twi_exchange_data <- read_csv("Data/twi_exchange_data.csv")

    # select date rows and twi
    twi_data <- twi_exchange_data %>%
    select(`Series ID`, FXRTWI)

    ## rename twi columns for data hygiene
    twi_data = rename(twi_data, c(date = `Series ID`, twi = FXRTWI))

    # convert date column to date datatype
    twi_data$date <- as.Date(twi_data$date, format = "%d-%b-%y")

    # create new column for month from date
    twi_data$month <- strftime(twi_data$date, "%m")

    # create new column for year from date
    twi_data$year <- strftime(twi_data$date, "%Y")

    # remove date column from twi_data
    twi_data <- twi_data %>%
    select(-date)

    #find twi by year
    twi_yearly <- twi_data %>%
    group_by(year) %>%
    dplyr::summarize(TWI_data = mean(twi, na.rm = TRUE)) %>%
    as.data.frame()

    # add annual twi to final analysis data
    research_data <- merge(research_data, twi_yearly)

    return(research_data)
}



load_datasets_quaterly <- function(){
  
  research_data <- load_crash_data_by_quarter(NULL)
  research_data <- load_NSW_population_data_by_quarter(research_data)
  
  return(research_data)
}

load_datasets <- function() {

    research_data <- load_crash_data(NULL)
    research_data <- load_petrol_data(research_data)
    research_data <- load_CPI_data(research_data)
    research_data <- load_employment_data(research_data)
    research_data <- load_NSW_population_data(research_data)
    research_data <- load_GDP_data(research_data)
    research_data <- load_registration_data(research_data)
    research_data <- load_TWI_data(research_data)

    return(research_data)
}