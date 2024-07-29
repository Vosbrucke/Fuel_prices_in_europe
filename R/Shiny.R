libraries <- c(
  "shiny", "shinythemes", "shinybrowser", "wesanderson",
  "ggtext", "data.table", "lubridate", "patchwork", "ggh4x"
)
lapply(libraries, require, character.only = TRUE)

ui <- fluidPage(
  tags$head(tags$style(".well {background-color: #FFFFFF; border-color: #FFFFFF}")),
  theme = shinytheme("cosmo"),
  navbarPage(title = "Consumer prices of petroleum products"),
  shinybrowser::detect(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_country",
        "Select a country or countries to plot",
        choices = c('Austria', 'Belgium', 'Bulgaria', 'Cyprus', 'Czechia', 'Germany', 'Great Britain', 'Denmark', 'European Union', 'Euro Zone', 'Estonia', 'Spain', 'Finland', 'France', 'Greece', 'Croatia', 'Hungary', 'Ireland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Sweden', 'Slovenia', 'Slovakia'),
        selected = "Austria",
        multiple = TRUE
      ),
      selectInput(
        "selected_fuel",
        "Select a fuel to plot",
        choices = c("Euro 95", "Diesel", "LPG", "Heating Oil"),
        selected = "Euro 95"
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
  # TODO: #1 get back to wider format for performance gain
  path <- "https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Data/oil_bulletin_long.csv"
  oil_bulletin_long <- fread(path)
  oil_bulletin_long[variable == "Heating Oil"]
  palette <- reactive({wes_palette("Darjeeling1", n = length(input$selected_country), type = "continuous")})

  date_start <- reactive({as.Date(input$date[1], origin = "1970-01-01")})
  date_end <- reactive({as.Date(input$date[2], origin = "1970-01-01")})

  output$fuel_prices <- renderPlot({
    tryCatch({

      # remove observations after selected date_end
      oil_bulletin_long_0 <- reactive({oil_bulletin_long[
        variable == input$selected_fuel & date < date_end()
      ]})

      # filter data from the date_start
      react_df <- reactive({oil_bulletin_long_0()[date > date_start()]})

      # make a data table to use in correct order as colors on the plot
      countries_letters <- data.table(
        country_name = input$selected_country,
        color = letters[seq_along(input$selected_country)]
      )

      # keep only selected countries with colors assigned
      dt_plot <- data.table(react_df())[countries_letters, on = "country_name"]

      # the start of y axis
      y_axis <- dt_plot[dt_plot[, .I[1], by = country]$V1, value]

      # add y axis lines that start and end when there are observations in those dates
      y_axis_lines <- data.table(
        x = rep(floor_date(as.Date(min(react_df()$date)), "year"), 5),
        y = seq(0.5, 2.5, 0.5),
        xend = rep(round_date(as.Date(max(react_df()$date)) + days(155), "year"), 5),
        yend = seq(0.5, 2.5, 0.5)
      )

      # determine if the width of the plot is enough for the whole title
      title <- paste(
        "Price of", input$selected_fuel, "among",
        "EU Member States on", format(max(react_df()[["date"]]), "%d %B %Y")
      )
      if (shinybrowser::get_width() > 1500) {
        title <- gsub("among EU", "among\nEU", title)
      }

      ggplot(
        data = oil_bulletin_long_0(),
        aes(x = date, y = value, group = country_name)
      ) +
        # make lines for y axis that does not go
        # beyond scale_x_continuous limits
        geom_segment(
          data = y_axis_lines,
          mapping = aes(
            x = x, y = y,
            xend = xend, yend = yend,
            group = y
          ),
          linewidth = 0.1,
          color = "lightgrey"
        ) +
        # make a line that goes from the end point,
        # most recent date, to a country name
        geom_segment(
          data = data.frame(
            x = as.Date(max(dt_plot$date)), y = y_axis,
            xend = as.Date(max(dt_plot$date)) + days(60),
            yend = y_axis
          ),
          mapping = aes(
            x = x, y = y,
            xend = xend, yend = yend,
            color = unique(dt_plot$color)
          ),
          # curvature = 0,
          linewidth = 0.2,
          inherit.aes = FALSE
        ) +
        # make a country name label
        geom_text(
          data = data.frame(
            x = as.Date(max(dt_plot$date)) + days(63),
            y = y_axis
          ),
          mapping = aes(
            x = x, y = y,
            color = unique(dt_plot$color),
            label = unique(dt_plot$country_name)
          ),
          size = 3,
          inherit.aes = FALSE,
          hjust = 0,
          fontface = "bold"
        ) +
        # add lines for other countries to be in the background
        geom_line(
          colour = "#F0F0F0",
          linewidth = 0.3
        ) +
        # add lines for selected in the shiny app countries
        geom_line(
          data = dt_plot,
          aes(color = dt_plot$color),
          linewidth = 0.75
        ) +
        # add color palette to selected countries
        scale_color_manual(
          values = c(palette()),
          name = NULL
        ) +
        # make a hline for y axis at the 0 level
        geom_segment(
          aes(
            x = floor_date(as.Date(min(react_df()$date)), unit = "year"),
            y = 0,
            xend = round_date(as.Date(max(react_df()$date)) + days(155), "year"),
            yend = 0
          ),
          linewidth = 0.5
        ) +
        # add points for each of the selected counties on the right side
        geom_point(
          data = dt_plot[1, 1:ncol(dt_plot), by = "color"],
          aes(color = color),
          size = 1,
          shape = 21,
          fill = "white",
          stroke = 1
        ) +
        # change date to show full year data no matter the start to
        # the end of the year;
        # if the date_end is before 15th January plot until a year before
        scale_x_date(
          # TODO: #2 what a monstrosity...
          # simplify and write as a separate object
          limits = c(
            floor_date(
              as.Date(
                min(
                  react_df()$date
                )
              ), "year"
            ),
            round_date(
              as.Date(
                max(
                  react_df()$date
                )
              ) + days(155), "year"
            ) + days(
              round(
                0.18 * (
                  as.numeric(
                    round_date(
                      as.Date(
                        max(
                          react_df()$date
                        )
                      ) + days(155), "year"
                    )
                  ) - as.numeric(
                    floor_date(
                      as.Date(
                        min(
                          react_df()$date
                        )
                      ), "year"
                    )
                  )
                )
              )
            )
          ),
          expand = c(0, 0),
          breaks = seq.Date(
            floor_date(as.Date(min(react_df()$date)), "year"),
            round_date(as.Date(max(react_df()$date)) + days(155), "year"),
            "1 year"
          ),
          date_labels = "%Y",
          minor_breaks = seq.Date(
            floor_date(as.Date(min(react_df()$date)), "year"),
            round_date(as.Date(max(react_df()$date)) + days(155), "year"),
            by = "quarter"
          ),
          guide = "axis_minor"
        ) +
        # scale y from 0 to 2.5 with EUR labels
        scale_y_continuous(
          breaks = seq(0, 2.5, 0.5),
          labels = paste0(seq(0, 2.5, 0.5), "€"),
          limits = c(0, 2.75),
          expand = c(0, 0)
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = title,
          caption = paste(
            "Data source: Oil Bulletin. European Commission, ",
            format(Sys.Date(), "%Y"),
            "\nAuthor: Szymon Lisowski"
          )
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
    # if no country selected display empty plot
    error = function(e) ""
    )
  })
}

shinyApp(ui, server)
