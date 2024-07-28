# Load libraries
library(lubridate)
library(openxlsx)
library(dplyr)
library(magrittr)
library(stringr)

# Read bulletin data
oil_bulletin <- read.xlsx("https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx", sheet = "Prices with taxes, per CTR")

# Name columns
colnames(oil_bulletin) <- letters[1:8]


# Take countries from the oil bulletin data
countries_vector <- oil_bulletin$a[grep("[A-Z]", oil_bulletin$a)]

# Delete observation that are not countries names
countries_vector <- countries_vector[str_length(countries_vector) == 2]


# Apply change to how to process the bulletin data
# Take countries names
oil_bulletin <- oil_bulletin %>% 
  filter(!is.na(a))

# Countries rownames
out <- rownames(oil_bulletin[which(oil_bulletin$a %in% countries_vector), ])

# Add 1 to determine the correct row positions for date start
start <- out %>% 
  as.numeric() + 1

# Subtract 1 to determine the correct row positions for date end
end <- out %>% 
  as.numeric() -1 

# Add a final rownumber
end <- c(end[2:length(end)], nrow(oil_bulletin))

# Make vector of start and ends for different countries
vector <- c(0, 2, start[2:length(start)], end) %>% 
  sort() %>% 
  as.data.frame()

# Take ranges
ranges <- tibble(vector, lag = lag(vector), minus = vector - lag) %>% pull(minus)

# Delete na's
ranges <- c(ranges[!is.na(ranges)])

# Make countries code and sort, the ones with 1 in will be later used to filter not important data. The ones without one will be present only twice per country in the gap in between them
countries <- c(paste0(countries_vector, "1"), countries_vector) %>% sort()

# Reproduce countries the number of different ranges
country_col <- rep(countries, ranges)

# Add country column
fuel_price_EU <- oil_bulletin %>% 
  mutate(country = country_col) %>% 
  filter(grepl("1", country)) %>% 
  mutate(
    a = as_date(as.numeric(a), origin = "1899-12-30"),
    country = str_remove(country, "1")
    ) %>% 
  set_colnames(c("date", "exchange_rate_to_eur",	"pb95", "diesel", "heating_gasoil", "heavy_fuel_oil", "lpg", "lpg_some_countries", "country_code")) %>% 
  mutate(lpg_some_countries = ifelse(is.na(lpg_some_countries), lpg, lpg_some_countries)) %>% 
  select(country_code, date, colnames(.), -lpg, -exchange_rate_to_eur, lpg = lpg_some_countries) %>% 
  # Make a final tibble. Change petroleum products prices to reflect prices per 1 liter (heavy_fuel_oil is the only that's depicted in tones)
  mutate(
    dplyr::across(c(pb95:lpg), ~as.double(str_replace(., ",", ""))),
    dplyr::across(c(pb95, diesel, heating_gasoil, lpg), ~round(as.numeric(.)) / 1000),
  )

# Change a code for Greece to 'EL'. This will be important in plotting when other eurostat databases use 'EL' code for Greece
fuel_price_EU$country_code[fuel_price_EU$country_code == "GR"] <- "EL"

# Read csv for countries full names
countries_all <- read.csv("https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Processed_data/slim-2_pl.csv", sep = ";", na.strings = "blank") %>% 
  select(-"country.code") %>% 
  rename(country_name = name, country_name_pl = name_pl, country_code = "alpha.2")

# Change Greece country code to be compatible with eurostat package
countries_all$country_code[countries_all$country_code == "GR"] <- "EL"

# Right join to code
countries_eu <- countries_all %>% 
  right_join(
    data.frame(
      country_code = c(
        "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LT", "LU", "LV", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR"
        )
      ), 
    by = "country_code"
    )

# Join fuel prices with country names in polish and english
fuel_price_EU %<>% 
  left_join(countries_eu, by = "country_code") %>% 
  arrange(country_code, desc(date))

# Write csv with fuel price
write.csv(fuel_price_EU, "Processed_data/fuel_price_EU.csv")
w