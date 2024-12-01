#import the library
library(tidyverse)

# Import the different files

## The .tsv file are important through read_delim wit the delimitation being the "\t" character representing the tabulation,
## deleting possible double double quote and deleting the trailing space
clicks <- read_delim("clicks.tsv", delim = "\t", escape_double = TRUE, trim_ws = TRUE)
impressions <- read_delim("impressions.tsv", delim = "\t", escape_double = TRUE, trim_ws = TRUE)

## The .csv file are imported through read_csv which is a built-in function
advertiser <- read_csv("advertiser.csv")
campaigns <- read_csv("campaigns.csv")

# Function to check if the time given is a UTC based time or not
check_time <- function(date_part, time_part, timezone_part) {

  # List of the registered timezones with offsets
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

  # Extract the time in 3 variables: hour, minute, second
  time_parts <- as.integer(strsplit(time_part, ":")[[1]])
  hour <- time_parts[1]
  minute <- time_parts[2]
  second <- time_parts[3]

  # Get timezone offset
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
  return(paste(date_part, utc_time, "UTC"))
}

# adjust the date / month / year depending on the situation
adjust_date <- function(date_str, trigger) {
  # Days per month
  nb_of_day_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  date_parts <- as.integer(strsplit(date_str, "/")[[1]])

  if (length(date_parts) != 3 || any(is.na(date_parts))) {
    stop("Invalid date format or NA encountered in date: ", date_str)
  }
  # Day comes first in DD-MM-YYYY
  day <- date_parts[1]
  # Month is second
  month <- date_parts[2]
  # Year is last
  year <- date_parts[3]
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
  return(sprintf("%02d/%02d/%04d", day, month, year))
}

# impression processing
impressions$new_datetime <- mapply(
  check_time,
  as.character(impressions$date),
  as.character(impressions$time),
  as.character(impressions$timezone)
)

# Update impressions dataset
impressions <- impressions %>%
  mutate(
    date = sub(" .*", "", new_datetime),
    time = sub(".* ", "", sub(" UTC$", "", new_datetime)),
    timezone = "UTC"
  ) %>%
  select(-new_datetime)

# Join the campaign and advertiser together to make joining them easier later on
campaigns_advertiser <- campaigns %>%
  left_join(advertiser, by = c("advertiser_id" = "ID"))
print("Campaigns and advertiser data joined successfully")

# Join the campaigns_advertiser to the impressions dataset
impressions <- impressions %>%
  left_join(campaigns_advertiser, by = c("campaign_id" = "id"))
print("Campaigns_advertiser data joined successfully to the impressions dataset.")

# Save processed impressions
write.csv(impressions, "impressions_processed.csv", row.names = FALSE)
print("Impressions saved to impressions_processed.csv")

# Clicks processing
clicks$new_datetime <- mapply(
  check_time,
  as.character(clicks$date),
  as.character(clicks$time),
  as.character(clicks$timezone)
)

# Update impressions dataset
clicks <- clicks %>%
  mutate(
    date = sub(" .*", "", new_datetime),
    time = sub(".* ", "", sub(" UTC$", "", new_datetime)),
    timezone = "UTC"
  ) %>%
  select(-new_datetime)

# Join the campaign_advertiser to the clicks dataset
clicks <- clicks %>%
  left_join(campaigns_advertiser, by = c("campaign_id" = "id"))
print("Campaigns_advertiser data joined successfully to the clicks dataset.")

# Save processed clicks
write.csv(clicks, "clicks_processed.csv", row.names = FALSE)
print("Clicks saved to clicks_processed.csv")
