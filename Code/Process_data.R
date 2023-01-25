library(lubridate)
library(openxlsx)
library(dplyr)
library(magrittr)
library(stringr)

# Read bulletin data
oil_bulletin <- read.xlsx("https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx", sheet = "Prices with taxes, per CTR")

# Name columns
colnames(oil_bulletin) <- letters[1:8]

# Clean bulletin prices column for Pb95 
oil_bulletin_prices <- oil_bulletin$c[grep("[0-9]+$", oil_bulletin$c)] %>% gsub(",", "", .)

# Clean bulletin prices column for Diesel
diesel_bulletin_prices <- oil_bulletin$d[grep("[0-9]+$", oil_bulletin$d)] %>% gsub(",", "", .)


# Data for lpg is bugged- for some countries there are 4 products differentiated while for others 5 or 6. This can be fixed
# using either calculation of how many observations there are, applying sequenced and repeated number to make groups and pivoting 
# the data to filter for certain words appearing in the column. The alternative, probably faster option, would be to duplicate what
# is in the last column with data to the point that all columns are filled- 5th and 6th in case of some countries
# # Clean bulletin prices column for LPG
# lpg_bulletin_prices <- oil_bulletin %>% 
#   mutate(
#     # Add a column for a situation when there are no data for a given date
#   i = ifelse(grepl("[0-9]",a) & !grepl("[0-9]", h), 0, NA),
#   h = ifelse(is.na(h), i, h)
#   ) %>% 
#   select(h) %>% 
#   filter(!is.na(h)) %>% 
#   pull()

# Clean bulletin date column
date <- oil_bulletin$a[grep("[0-9]+$", oil_bulletin$a)]

# Correct date from integer to proper format
date <- as_date(as.numeric(date), origin = "1899-12-30")

# Delete NA's
date <- date[!is.na(date)]

# Drop NA's from oil bulletin
# oil_bulletin <- oil_bulletin$a %>% drop_na()

# Take countries from the oil bulletin data
countries_vector <- oil_bulletin$a[grep("[A-Z]", oil_bulletin$a)]

# Delete observation that are not countries names
countries_vector <- countries_vector[str_length(countries_vector) == 2]

# Check number of dates- there should be equal number to later add countries code to proper date points
count <- data.frame(date) %>%
  table()

# Make a data frame 
count <- data.frame(
  char = names(count), 
  count = as.numeric(count), 
  stringsAsFactors=FALSE) 

# Filter for date that appears 27 times. Check what date it is
starting_date <- count %>%
  filter(count == 27) %>%
  head(n = 1) %>% 
  select(char) %>% 
  pull()

# Make a tibble with date and oil prices column. Leave observations below starting date
fuel_price <- tibble(date, Euro_super_95 = oil_bulletin_prices, Diesel = diesel_bulletin_prices) %>% filter(date >= as.Date(starting_date))

# Make a vector with countries names
countries <- rep(countries_vector, length.out = nrow(fuel_price)) %>% sort()

# Make a final tibble. Change a oil price to reflect prices per 1 liter
fuel_price_EU <- tibble(code = countries, fuel_price) %>% 
  mutate(
    Euro_super_95 = round(as.numeric(Euro_super_95)) / 1000,
    Diesel = round(as.numeric(Diesel)) / 1000,
  )

# Change a code for Greece to 'EL'. This will be important in plotting when other eurostat databases use 'EL' code for Greece
fuel_price_EU$code[fuel_price_EU$code == "GR"] <- "EL"

# Read csv for countries full names
countries_all <- read.csv("https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Processed_data/slim-2_pl.csv", sep = ";", na.strings = "blank") %>% 
  select(-"country.code") %>% 
  rename(country_name = name, country_name_pl = name_pl, code = "alpha.2")

# Change Greece country code to be compatible with eurostat package
countries_all$code[countries_all$code == "GR"] <- "EL"

# Right join to code
countries_eu <- countries_all %>% 
  right_join(data.frame(code = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LT", "LU", "LV", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR")), by = "code")

fuel_price_EU %<>% 
  left_join(countries_eu, by = "code")

# Write csv with fuel price
write.csv(fuel_price_EU, "Processed_data/fuel_price_EU.csv")


# # Make additional tibble for other regions
# additional_countries_code <-  tibble(code = c("EA19", "EA", "EU"), country_name = c("Euro area (19 countries)", "Euro area", "European Union"), country_name_pl = c("Kraje strefy euro (19 krajów)", "Kraje strefy euro", "Unia Europejska"))
# 
# # Joing two data frames by binding rows
# countries_eu <- countries_eu %>% 
#   bind_rows(additional_countries_code) %>% 
#   arrange(code)
# 
# # Write csv
# write.csv(countries_eu, "Processed_data/countries_eu_inflation.csv") 

# # Web scrape special aggregates for inflation categories
# special_aggregates <- read_html("https://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=LST_NOM_DTL&StrNom=HICP_2000&StrLanguageCode=EN&IntPcKey=37598921&StrLayoutCode=HIERARCHIC") %>% 
#   html_nodes(".text") %>% html_text2()
# 
# # Select only upper elements in the list- the actual aggregates
# special_aggregates_u <- special_aggregates[
#   grepl("[[:upper:]]+$", special_aggregates)]
# 
# # Adjust it
# special_aggregates_u <- special_aggregates_u[-c(1:3)]
# 
# # Select only lower elements in the list- description of the aggregates
# special_aggregates_l <- special_aggregates[
#   grepl("[[:lower:]]", special_aggregates)]
# 
# # Adjust it and remove some left overs
# special_aggregates_l <- special_aggregates_l[6:42] %>% stringr::str_remove("\n")
# 
# # Make a tibble
# special_aggregates <- tibble(special_aggregates_u, special_aggregates_l)
# 
# # Write csv
# write_csv(special_aggregates, "Processed_data/special_aggregates.csv")

