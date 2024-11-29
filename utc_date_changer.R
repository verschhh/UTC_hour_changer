#import the library
library(tidyverse)

# Import the different files

## The .tsv file are important through read_delim wit the delimitation being the "\t" character representing the tabulation,
## deleting possible double double quote and deleting the trailing space
clicks <- read_delim("clicks.tsv", delim = "\t", escape_double = TRUE, trim_ws = TRUE)
impressions <- read_delim("impression.tsv", delim = "\t", escape_double = TRUE, trim_ws = TRUE)

## The .csv file are imported through read_csv which is a built-in function
advertiser <- read_csv("advertiser.csv")
campaigns <- read_csv("campaigns.csv")

# Function to check if the time given is a UTC based time or not
check_time <- function(datetime_str) {

  # list of the registered timezone
  timezone_difference <- list(
    # Anywhere on Earth (UTC-12)
    "Anywhere on Earth" = -12,
    # Samoa Standard Time (UTC-11)
    "Samoa time" = -11,
    # Hawaii Standard Time (UTC-10)
    "Hawaii time" = -10,
    # Alaska Standard Time (UTC-9)
    "Alaska time" = -9,
    # Pacific Standard Time (UTC-8)
    "Pacific time" = -8,
    # Mountain Standard Time (UTC-7)
    "Mountain time" = -7,
    # Central Standard Time (UTC-6)
    "Central time" = -6,
    # Eastern Standard Time (UTC-5)
    "Eastern time" = -5,
    # Atlantic Standard Time (UTC-4)
    "Atlantic time" = -4,
    # Brasília Time (UTC-3)
    "Brasilia time" = -3,
    # South Georgia Time (UTC-2)
    "South Georgia time" = -2,
    # Azores Time (UTC-1)
    "Azores time" = -1,
    # Coordinated Universal Time (UTC+0)
    "UTC" = 0,
    # Central European Time (UTC+1)
    "Central European time" = 1,
    # Eastern European Time (UTC+2)
    "Eastern European time" = 2,
    # Moscow Standard Time (UTC+3)
    "Moscow time" = 3,
    # Gulf Standard Time (UTC+4)
    "Gulf time" = 4,
    # Pakistan Standard Time (UTC+5)
    "Pakistan time" = 5,
    # Bangladesh Standard Time (UTC+6)
    "Bangladesh time" = 6,
    # Indochina Time (UTC+7)
    "Indochina time" = 7,
    # China Standard Time (UTC+8)
    "China time" = 8,
    # Japan Standard Time (UTC+9)
    "Japan time" = 9,
    # Australian Eastern Standard Time (UTC+10)
    "Australian Eastern time" = 10,
    # Solomon Islands Time (UTC+11)
    "Solomon Island time" = 11,
    # New Zealand Standard Time (UTC+12)
    "New Zealand time" = 12
  )

  # Split the input into 3 parts :
  ## - date_part
  ## - time_part
  ## - timezone_part
  # time_part and timezone_part will be handled together as they are essential to calculate the time offset
  # date-part will be handled if it happens that the timezone_part isn't already UTC and the offset pushes us to a new day / month / year
  parts <- strsplit(datetime_str, "/")[[1]]
  date_part <- parts[1]
  time_part <- parts[2]
  timezone_part <- parts[3]

  # Extract the time in 3 variable :
  ## - second
  ## - minute
  ## - hour
  # hour will be manipulated while the two other are kept for the formatting at the end
  time_parts <- as.integer(strsplit(time_part, ":")[[1]])
  hour <- time_parts[1]
  minute <- time_parts[2]
  second <- time_parts[3]

  # If the time is already in UTC, return the time as is it since we don't need to make modification
  if (timezone_difference[[timezone_part]] == 0) {
    result <- datetime_str
    return(result)
  }

  # Find timezone offset in hours
  offset <- timezone_difference[[timezone_part]]
  if (is.null(offset)) {
    stop("Unsupported timezone: ", timezone_part)
  }

  # Calculate new hour in UTC
  utc_hour <- hour - offset

  # Adjust for crossing day boundaries
  if (utc_hour >= 24) {
    utc_hour <- utc_hour %% 24
    date_part <- adjust_date(date_part, 1)
  } else if (utc_hour < 0) {
    utc_hour <- (utc_hour + 24) %% 24
    date_part <- adjust_date(date_part, 0)
  }

  # Reformat to UTC string
  utc_time <- sprintf("%02d:%02d:%02d", utc_hour, minute, second)
  result <- paste(date_part, utc_time, "UTC")

  return(result)
}

# adjust the date / month / year depending on the situation
adjust_date <- function(date_str, trigger) {
  # Days per month
  nb_of_day_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  date_parts <- as.integer(strsplit(date_str, "-")[[1]])
  year <- date_parts[1]
  month <- date_parts[2]
  day <- date_parts[3]
  if (trigger == 1) {
    day <- day + 1
    if (day > nb_of_day_in_month[month]) {
      day <- 1
      month <- month + 1
    }

    if (month > 12) {
      month <- 1
      year <- year + 1
    }
  } else {
    day <- day - 1
    if (day == 0) {
      month <- month - 1
      if (month == 0) {
        month <- 12
        year <- year - 1
      }
      day <- nb_of_day_in_month[month]
    }
  }

  return(sprintf("%04d-%02d-%02d", year, month, day))
}