library("shiny")
library("shinythemes")
library("shinybrowser")
library("wesanderson")
library("ggtext")
library("data.table")
library("ggtext")
library("lubridate")
library("patchwork")
library("ggh4x")
library("echarts4r")
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
        choices = c('Austria', 'Belgium', 'Bulgaria', 'Cyprus', 'Czechia', 'Germany', 'United Kingdom', 'Denmark', 'European Union', 'Euro Zone', 'Estonia', 'Spain', 'Finland', 'France', 'Greece', 'Croatia', 'Hungary', 'Ireland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Sweden', 'Slovenia', 'Slovakia'),
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
        start = Sys.Date() - lubridate::years(3),
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
  oil_bulletin_long <- as.data.table(fread(path))

  palette <- reactive({wes_palette("Darjeeling1", n = length(input$selected_country), type = "continuous")})

  date_start <- reactive({as.Date(input$date[1], origin = "1970-01-01")})
  date_end <- reactive({as.Date(input$date[2], origin = "1970-01-01")})

  output$fuel_prices <- renderEcharts4r({
    tryCatch({
      input <- list()
      input$selected_country <- c("Austria", "Germany")
      palette <- wes_palette("Darjeeling1", n = length(input$selected_country), type = "continuous")
      # input$selected_country <- c("Other", input$selected_country)
      # palette <- c("#d3d3d3", palette)
      input$selected_fuel <- "Diesel"
      date_start <- as.Date("2021-01-01")
      date_end <- as.Date("2024-01-01")

      # remove observations after selected date_end
      oil_bulletin_long_0 <- oil_bulletin_long[
        variable == input$selected_fuel & date < date_end
      ]

      # filter data from the date_start
      react_df <- oil_bulletin_long_0[date > date_start][,
        country_name := fifelse(country_name %in% input$selected_country, country_name, "Other")
      ]

      # make a data table to use in correct order as colors on the plot
      countries_letters <- data.table(
        country_name = input$selected_country,
        color = letters[seq_along(input$selected_country)]
      )

      # keep only selected countries with colors assigned
      dt_plot <- react_df[countries_letters, on = "country_name"][!is.na(value)]
      dt_plot[, country_name := factor(country_name, levels = input$selected_country)]

      # the start of y axis
      y_axis <- dt_plot[dt_plot[, .I[1], by = country_name]$V1, value]
      if (any(na_ind <- is.na(y_axis))) {
        y_axis <- y_axis[!na_ind]
        dt_plot <- dt_plot[!country_name %in% countries_letters[na_ind][[1]]]
      }

      # add y axis lines that start and end when there are observations in those dates
      y_axis_lines <- data.table(
        x = rep(floor_date(as.Date(min(react_df$date)), "year"), 5),
        y = seq(0.5, 2.5, 0.5),
        xend = rep(round_date(as.Date(max(react_df$date)) + lubridate::days(155), "year"), 5),
        yend = seq(0.5, 2.5, 0.5)
      )

      points <- dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name) |>
          dplyr::filter(date == max(date))

      # determine if the width of the plot is enough for the whole title
      title <- paste(
        "Price of", input$selected_fuel, "among",
        "EU Member States on", format(max(react_df[["date"]]), "%d %B %Y")
      )
      if (shinybrowser::get_width() > 1500) {
        title <- gsub("among EU", "among\nEU", title)
      }

      formatter <- htmlwidgets::JS(
        "
        function(params) {
            var tooltipContent = '<b>Date:</b> ' + params[0].axisValue + '<br/>';
        
            params.forEach(function(item) {
              var groupName = item.seriesName;
              var fmt = new Intl.NumberFormat('%s', %s);
              var groupValue = item.value[item.encode.y[0]];
              var groupColor = item.color;
              var groupValueFormatted = fmt.format(item)
              
              tooltipContent += '<div style=\"display: inline-block; width: 10px; height: 10px; margin-right: 5px; background-color: ' + groupColor + '\"></div>' +
                                '<b>' + groupName + ':</b> ' + groupValueFormatted + '<br/>';
            });
            
            return tooltipContent;
          console.log(params);
          }
        "
      )
      formatter <- htmlwidgets::JS(
              "function(params){
                var tp = [];
                params.forEach(function(x){
                  tp.push([x.seriesName, x.value].join('-'))
                });
                return(tp.join('<br/>'))
              }")

  formatter <- function (style = c("decimal", "percent", "currency"), digits = 0, 
    locale = NULL, currency = "USD") {
    if (rstudioapi::isAvailable()) {
        warning("`e_axis_formatter` breaks the plot in RStudio, open it in your browser.", 
            call. = FALSE)
    }
    if (is.null(locale)) {
        locale <- echarts4r:::.get_locale()
    }
    style <- match.arg(style)
    opts <- list(style = style, minimumFractionDigits = digits, 
        maximumFractionDigits = digits, currency = currency)
    htmlwidgets::JS(sprintf("function(value, 2) {\n        var fmt = new Intl.NumberFormat('%s', %s);\n        return fmt.format(value);\n    }", 
        locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
  }

    htmlwidgets::JS(
      sprintf("function(value, 1) {\n        var fmt = new Intl.NumberFormat('%s', %s);\n        return fmt.format(value);\n    }",
      locale,
      jsonlite::toJSON(opts, auto_unbox = TRUE)
      )
    )
      
      # dcast(dt_plot, date ~ country, value.var = "value") |>
      react_df |>
      group_by(country) |>
      e_charts(
        x = date
      ) |>
      e_line(
        serie = value,
        legend = list(show = FALSE),
        lineStyle = list(
          width = 0.5
        ),
        color = "#d3d3d3",
        symbol = "none"
      ) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name)
      ) |>
      e_line(
        serie = value,
        # symbol = 'circle',
        symbolSize = 2
      ) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name) |>
          dplyr::filter(date == max(date))
      ) |>
      e_line(
        serie = value,
        symbolSize = 5
      ) |>
      e_color(
        color = palette
      ) |>
      e_tooltip(formatter = htmlwidgets::JS("
    function(params) {
      var value = params.value[1];
      return params.seriesName + ': $' + value.toFixed(2);
    }
  ")) |>
  e_y_axis(formatter = htmlwidgets::JS("
    function(value) {
      return '$' + value;
    }
  "))
      e_tooltip(formatter = formatter(style = "currency", currency = "USD")) |>
      # e_y_axis(
      #   formatter = e_axis_formatter("currency", currency = "EUR", digits = 1)
      # ) |>
      # e_tooltip(
      #   trigger = "axis",
      #   # formatter = formatter
      #   # formatter = formatter("currency", currency = "EUR", digits = 1)
      #   formatter = e_tooltip_item_formatter("currency", currency = "EUR", digits = 0)
      # ) 
      e_x_axis(axisLabel = list(formatter = htmlwidgets::JS("function(value) { return value; }"))) |> # no formatter for x-axis
      e_y_axis(axisLabel = list(formatter = formatter(style = "currency", currency = "USD"))) # formatter for y-axis


      plot <- ggplot(
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
            xend = as.Date(max(dt_plot$date)) + lubridate::days(60),
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
            x = as.Date(max(dt_plot$date)) + lubridate::days(63),
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
            xend = round_date(as.Date(max(react_df()$date)) + lubridate::days(155), "year"),
            yend = 0
          ),
          linewidth = 0.5
        ) +
        # add points for each of the selected counties on the right side
        geom_point(
          data = dt_plot[dt_plot[, .I[1], by = color]$V1, 1:ncol(dt_plot)],
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
              ) + lubridate::days(155), "year"
            ) + lubridate::days(
              round(
                0.18 * (
                  as.numeric(
                    round_date(
                      as.Date(
                        max(
                          react_df()$date
                        )
                      ) + lubridate::days(155), "year"
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
            round_date(as.Date(max(react_df()$date)) + lubridate::days(155), "year"),
            "1 year"
          ),
          date_labels = "%Y",
          minor_breaks = seq.Date(
            floor_date(as.Date(min(react_df()$date)), "year"),
            round_date(as.Date(max(react_df()$date)) + lubridate::days(155), "year"),
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
            "Data source: Oil Bulletin. European Commission,",
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
      plot(plot)

      
    },
    # if no country selected display empty plot
    error = function(e) ""
    )
  })
}

shinyApp(ui, server)
