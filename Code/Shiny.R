library(shiny)
library(shinythemes)
library(plotly)
library(wesanderson)
library(ggtext)
library(tidyverse)
library(magrittr)
library(lubridate)
library(patchwork)
library(glue)
library(ggh4x)
library(shinybrowser)

ui <- fluidPage(
  tags$head(tags$style(".well {background-color: #FFFFFF; border-color: #FFFFFF}")),
  theme = shinytheme("cosmo"),
  navbarPage(title = "Average fuel prices"),
  shinybrowser::detect(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_country", 
        "Select a country or countries to plot", 
        choices = c('Austria', 'Belgium', 'Bulgaria', 'Cyprus', 'Czechia', 'Germany', 'Denmark', 'Estonia', 'Spain', 'Finland', 'France', 'Greece', 'Croatia', 'Hungary', 'Ireland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Sweden', 'Slovenia', 'Slovakia'),
        selected = "Austria",
        multiple = TRUE
      ),
      selectInput(
        "selected_fuel", 
        "Select a fuel to plot", 
        choices = list("Gasoline" = "Euro_super_95", "Diesel" = "Diesel"),
        selected = ""
      ),
      dateRangeInput(
        "date", "Choose date range",
        start = Sys.Date() - years(3),
        end = Sys.Date(),
        startview = "year",
        weekstart = 1
      ),
      tags$style(HTML(".datepicker {z-index:99999 !important;}"))
    ),
    mainPanel(
      plotOutput(
        "fuel_prices",
        )
      )
  )
)

server <- function(input, output) {
  
  # Read fuel price data
  fuel_price_EU <- read_csv("https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Processed_data/fuel_price_EU.csv")
  
  palette <- reactive({wes_palette("Darjeeling1", n = length(input$selected_country), type = "continuous")})

  date_start <- reactive({input$date[1] %>% 
    as.Date(origin = "1970-01-01")})
  date_end <- reactive({input$date[2] %>% 
    as.Date(origin = "1970-01-01")})
  
  # Render plot
  output$fuel_prices <- renderPlot({
    tryCatch(
    {
      # Fuel name on a plot
      fuel <- reactive({ifelse(input$selected_fuel == "Diesel", "Diesel", "Gasoline")})
      
      # Rename the selected fuel to plot it and remove observations after selected date_end
      fuel_price_EU_0 <- reactive({fuel_price_EU %>% 
        rename(fuel = input$selected_fuel) %>% 
        filter(date < date_end())
      })
        
      # Filter data from the date_start
      react_df <- reactive({fuel_price_EU_0() %>% 
        filter(date > date_start())
      })
    
    
      # Make a tibble to use in correct order as colors on the plot 
      countries_letters <- tibble(
        country_name = input$selected_country, 
        color = letters[1:length(input$selected_country)]
        )
    
      # Join two data frames and keep only selected countries
      countries <- tibble(react_df()) %>% 
        right_join(countries_letters, by = "country_name") 
      
      # Pull gasoil prices to use as 
      y_line <- countries %>% 
        group_by(country_name) %>% 
        filter(row_number() == 1) %>% 
        pull(fuel)
      
      # Add y axis lines that start and end when there are observations in those dates
      y_axis_lines <- tibble(
        x = rep(floor_date(as.Date(min(react_df()$date)), "year"), 5),
        y = seq(0.5, 2.5, 0.5),
        xend = rep(round_date(as.Date(max(react_df()$date)) + days(155), "year"), 5),
        yend = seq(0.5, 2.5, 0.5)
        )
      
      # Determine if the width of the plot is enough for the whole title
      if (shinybrowser::get_width() > 1500) {
        title <- paste("Price of", tolower(fuel()), "among EU Member States on", format(max(react_df() %>% pull(date)), "%d %B %Y")) 
      } else {
        title <- paste("Price of", tolower(fuel()), "among\nEU Member States on", format(max(react_df() %>% pull(date)), "%d %B %Y")) 
      }
      
      ggplot(data = fuel_price_EU_0(), aes(x = date, y = fuel, group = country_name)) +
      # Make lines for y axis that does not go beyond scale_x_continuous limits
      geom_segment(
        data = y_axis_lines, 
        mapping = aes(
          x = x, 
          y = y, 
          xend = xend, 
          yend = yend, 
          group = y), 
        linewidth = 0.1, 
        color = "lightgrey"
        ) +
      # Make a line that goes from the end point, most recent date, to a country name
      geom_segment(
        data = data.frame(
          x = as.Date(max(countries$date)),
          y = y_line, 
          xend = as.Date(max(countries$date) + days(60)),
          yend = y_line),
        mapping = aes(
          x = x, 
          y = y, 
          xend = xend, 
          yend = yend, 
          color = unique(countries$color)
          ),
        # curvature = 0, 
        linewidth = 0.2, 
        inherit.aes = FALSE
        ) +
      # Make a country name label
      geom_text(
        data = data.frame(
          x = as.Date(max(countries$date) + days(63)),
          y = y_line
          ),
        mapping = aes(
          x = x, 
          y = y, 
          color = unique(countries$color), 
          label = unique(countries$country_name)
          ),
        size = 3, 
        inherit.aes = FALSE, 
        hjust = 0, 
        fontface = "bold"
        ) +
      # Add lines for other countries to be in the background
      geom_line(
        colour = "#F0F0F0",
        linewidth = 0.3
        ) +
      # Add lines for selected in the shiny app countries 
      geom_line(
        data = countries, 
        aes(color = color),
        linewidth = 0.75
        ) +
      # Add color palette to selected countries
      scale_color_manual(
        values = c(palette()),
        name = NULL
        ) +
      # Make a hline for y axis at the 0 level
      geom_segment(
        aes(
          x = floor_date(as.Date(min(react_df()$date)), unit = "year"), 
          y = 0, 
          xend = round_date(as.Date(max(react_df()$date)) + days(155), "year"), 
          yend = 0), 
        linewidth = 0.5
        ) +
      # Add points for each of the selected counties on the right side
      geom_point(
        data = countries %>% group_by(color) %>% filter(row_number() == 1),
        aes(color = color),
        size = 1, 
        shape = 21, 
        fill = "white", 
        stroke = 1
        ) +
      # Change date to show full year data no matter the start to the end of the year; if the date_end is before 15th January plot until a year before
      scale_x_date(
        limits = c(floor_date(as.Date(min(react_df()$date)), "year"), round_date(as.Date(max(react_df()$date)) + days(155), "year") + days(round(0.18*(as.numeric(round_date(as.Date(max(react_df()$date))) + days(155), "year") - as.numeric(floor_date(as.Date(min(react_df()$date)), "year")))))),
        expand = c(0,0),
        breaks = seq.Date(floor_date(as.Date(min(react_df()$date)), "year"), round_date(as.Date(max(react_df()$date)) + days(155), "year"), "1 year"),
        date_labels = "%Y",
        minor_breaks = seq.Date(floor_date(as.Date(min(react_df()$date)), "year"), round_date(as.Date(max(react_df()$date)) + days(155), "year"), by = "quarter"),
        guide = "axis_minor"
        ) +
      # Scale y from 0 to 2.5 with EUR labels 
      scale_y_continuous(
        breaks = seq(0, 2.5, 0.5), 
        labels = paste0(seq(0, 2.5, 0.5), "€"),
        limits = c(0, 2.75),
        expand = c(0,0)
        ) +
      labs(
        x = NULL, 
        y = NULL, 
        title = title,
        caption = paste("Data source: Oil Bulletin. European Commission, ", format(Sys.Date(), "%Y"), "\nAuthor: Szymon Lisowski")
        ) + 
      theme_classic() +
      theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(hjust = 0, size = 4),
        panel.grid.major = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        legend.position = "none"
        )
    }, 
    # If there is no country selected display empty plot
    error = function(e) {""})
  })
}

shinyApp(ui, server)
