
# NSW REGISTRATION DATA ---------------------------

# Import NSW registration data
nsw_vehicles_registered <-
  read_excel("Data/NSW_vehicles_registered.xlsx")

# convert nsw registration data to tibble
nsw_vehicles_registered <- as_tibble(nsw_vehicles_registered)

# Convert date column to date format.
nsw_vehicles_registered$date <-
  as.Date(nsw_vehicles_registered$QUARTER, format = "%Y-%m-%d")

# create new column for month from date
nsw_vehicles_registered$month <-
  strftime(nsw_vehicles_registered$date, "%m")

# create new column for year from date
nsw_vehicles_registered$year <-
  strftime(nsw_vehicles_registered$date, "%Y")

# split into light vehicles, cycles and heavy vehicles

#light
nsw_vehicles_registered_light <- data.frame(
  nsw_vehicles_registered$`Light Vehicles All` - nsw_vehicles_registered$`Light Vehicles Scooters` - nsw_vehicles_registered$`Light Vehicles Motor cycles`, 
  nsw_vehicles_registered$date,
  nsw_vehicles_registered$month,
  nsw_vehicles_registered$year
  )

names(nsw_vehicles_registered_light) <- c('nsw_registered_light_vehicles', 'date', 'month', 'year')

#motorcycles & scooters
nsw_vehicles_registered_motorcycles_scooters <- data.frame(
  nsw_vehicles_registered$`Light Vehicles Motor cycles` + nsw_vehicles_registered$`Light Vehicles Scooters`, 
  nsw_vehicles_registered$date,
  nsw_vehicles_registered$month,
  nsw_vehicles_registered$year
  )

names(nsw_vehicles_registered_motorcycles_scooters) <- c('nsw_registered_motorcycles_scooters', 'date', 'month', 'year')

#heavy
nsw_vehicles_registered_heavy <-data.frame(
  nsw_vehicles_registered$`Heavy Vehicles All`,
  nsw_vehicles_registered$date,
  nsw_vehicles_registered$month,
  nsw_vehicles_registered$year
)

names(nsw_vehicles_registered_heavy) <- c('nsw_registered_heavy_vehicles', 'date', 'month', 'year')



