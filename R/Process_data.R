# initial setup
libraries <- c("lubridate", "openxlsx", "data.table", "stringi")
lapply(libraries, require, character.only = TRUE)
path <- paste0(
  "https://energy.ec.europa.eu/document/download/906e60ca-8b6a-44e7-8589-6",
  "52854d2fd3f_en?filename=Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx"
)

oil_bulletin_ori <- as.data.table(read.xlsx(path, sheet = "Prices with taxes"))

# drop country columns (anyway we have country names in colnames)
oil_bulletin <- oil_bulletin_ori[,
  grep("CTR", names(oil_bulletin_ori), invert = TRUE, value = TRUE),
  with = FALSE
]

# remove non data rows
names(oil_bulletin)[1] <- "date"
suppressWarnings(
  oil_bulletin[, date := as.numeric(date)]
)
oil_bulletin <- oil_bulletin[!is.na(oil_bulletin[[1]])]

oil_bulletin <- oil_bulletin[,
  lapply(.SD, as.numeric), .SDcols = names(oil_bulletin)
]

oil_bulletin_long <- data.table::melt(oil_bulletin, id.vars = "date")
oil_bulletin_long[, date := as_date(date, origin = "1899-12-30")]

# create a country column and adjust variable naming
oil_bulletin_long[,
  country := stringi::stri_replace_all_regex(variable, "_.*", "")
]
oil_bulletin_long[,
  variable := stringi::stri_replace_all_fixed(variable, country, "")
]
oil_bulletin_long[,
  variable := stringi::stri_replace_first_fixed(variable, "_", "")
]
oil_bulletin_long[,
  variable := stringi::stri_replace_first_fixed(variable, "price_with_tax_", "")
]

# adjust the incorrect naming and remove some obsolete values
oil_bulletin_long[variable == "heing_oil", variable := "heating_oil"]
oil_bulletin_long[,
  variable := stringi::stri_replace_all_fixed(variable, "_", " ")
]
oil_bulletin_long <- oil_bulletin_long[
  !variable %in% c("fuel oil 2", "fuel oil 1", "exchange rate")
]

# change to price per litre
oil_bulletin_long <- oil_bulletin_long[
  variable %in% c("euro95", "diesel", "heating oil", "LPG"),
  value := round(value / 1000, 2)
]

# adjust naming to shiny app
oil_bulletin_long[, variable := stringi::stri_trans_totitle(variable)]
oil_bulletin_long[variable == "Euro95", variable := "Euro 95"]
oil_bulletin_long[variable == "Lpg", variable := "LPG"]

# map country codes
dt_country_codes <- data.table(
  country = c(
    "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "GR", "ES", "FI",
    "FR", "HR", "HU", "IE", "IS", "IT", "LT", "LU", "LV", "MK", "MT", "NL",
    "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR", "EUR", "EU", "UK"
  ),
  country_name = c(
    "Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czechia",
    "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France",
    "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania",
    "Luxembourg", "Latvia", "North Macedonia", "Malta", "Netherlands",
    "Norway", "Poland", "Portugal", "Romania", "Serbia", "Sweden", "Slovenia",
    "Slovakia", "Turkey", "Euro Zone", "European Union", "United Kingdom"
  )
)

# join and save
oil_bulletin_long <- dt_country_codes[oil_bulletin_long, on = "country"]
data.table::fwrite(oil_bulletin_long, "Data/oil_bulletin_long.csv")
