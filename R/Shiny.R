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
library("htmlwidgets")
library("webshot2")
ui <- fluidPage(
  tags$head(tags$style(".well {background-color: #FFFFFF; border-color: #FFFFFF}")),
  theme = shinytheme("cosmo"),
  navbarPage(title = "Consumer prices of petroleum products"),
  shinybrowser::detect(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_country",
        "Select country/zone or more to plot",
        choices = c(
          "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia",
          "Germany", "United Kingdom", "Denmark", "European Union",
          "Euro Zone", "Estonia", "Spain", "Finland", "France",
          "Greece", "Croatia", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands",
          "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia"
        ),
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
      tags$style(HTML(".datepicker {z-index:99999 !important;}")),
      downloadButton("download_plot", "Download Graph")
    ),
    mainPanel(
      echarts4rOutput("chart")
    )
  )
)

server <- function(input, output) {
  # TODO: #1 get back to wider format for performance gain
  path <- "https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Data/oil_bulletin_long.csv"
  oil_bulletin_long <- as.data.table(fread(path))

  palette <- reactive({
    wes_palette(
      "Darjeeling1",
      n = length(input$selected_country), type = "continuous"
    )
  })

  date_start <- reactive({as.Date(input$date[1], origin = "1970-01-01")})
  date_end <- reactive({as.Date(input$date[2], origin = "1970-01-01")})

  selected_fuel <- reactive({input$selected_fuel})

  # remove observations after selected date_end
  oil_bulletin_long_0 <- reactive({oil_bulletin_long[
    variable == selected_fuel() & date < date_end()
  ]})

  selected_country <- reactive({
    if (length(input$selected_country) == 0) "Othrs" else input$selected_country
  })
  
  # filter data from the date_start and rename non selected countries to 'Other'
  react_df <- reactive({oil_bulletin_long_0()[date > date_start()][,
    country_name := fifelse(
      country_name %in% selected_country(), country_name, "Other"
    )
  ]})

  output$chart <- renderEcharts4r({

    # ensure `selected_country()` is a vector
    selected_countries_vector <- unlist(selected_country())

    # data table for plotting with color assignments
    countries_letters <- data.table(
      country_name = selected_countries_vector,
      color = letters[seq_along(selected_countries_vector)]
    )

    # join the filtered data with the countries_letters table
    dt_plot <- react_df()[countries_letters, on = "country_name", nomatch = 0L]
    dt_plot[,
      country_name := factor(country_name, levels = selected_countries_vector)
    ]

    # the start of y axis dot point
    y_axis <- dt_plot[dt_plot[, .I[1], by = country_name]$V1, value]
    if (any(na_ind <- is.na(y_axis))) {
      y_axis <- y_axis[!na_ind]
      dt_plot <- dt_plot[!country_name %in% countries_letters[na_ind][[1]]]
    }

    main_title <- paste(
      "Price of", input$selected_fuel, "in Europe until",
      format(max(react_df()[["date"]]), "%d %B %Y")
    )

    caption_title <- paste0(
      "Data source: Oil Bulletin, European Commission, ",
      max(year(oil_bulletin_long$date)),
      "\nAuthor: Szymon Lisowski"
    )

    formatter <- htmlwidgets::JS(
      "function(params) {
      var tooltipContent = '<b>Date:</b> ' + params[0].axisValueLabel + '<br/>';
      var locale = 'en-US';  // Set your desired locale
      var options = { style: 'currency', currency: 'EUR', minimumFractionDigits: 1 };

      // Use a Set to store unique series names
      var uniqueItems = new Set();

      params.forEach(function(item) {
        var fmt = new Intl.NumberFormat(locale, options);
        var groupValue = item.data.value[item.encode.y[0]];
        var groupColor = item.color;
        var groupValueFormatted = fmt.format(groupValue);
        
        // Check if the series name is already in the Set
        if (!uniqueItems.has(item.seriesName) && item.color !== '#d3d3d3') {
          uniqueItems.add(item.seriesName);  // Add to the Set if not present
          tooltipContent += '<div style=\"display: inline-block; width: 10px; height: 10px; margin-right: 5px; background-color: ' + groupColor + '\"></div>' +
                            '<b>' + item.seriesName + ':</b> ' + groupValueFormatted + '<br/>';
        }
      });
      return tooltipContent;
    }"
    )

  react_df() |>
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
      symbolSize = 6
    ) |>
    e_tooltip(
      trigger = "axis",
      formatter = formatter
    ) |>
    e_toolbox_feature(feature = "saveAsImage") |>
    e_color(
      color = palette()
    ) |>
    # return y axis value with EUR symbol and 1 decimal point
    e_y_axis(
      formatter = htmlwidgets::JS(
        "function(value) {
          return '€' + value.toFixed(1);
        }"
      )
    ) |>
    e_title(
      text = main_title,
      left = "center",
      textStyle = list(
        fontSize = 12,
        fontWeight = "bold"
      )
    ) |>
    e_legend(
      top = "4%",
      left = "center",
      orient = "horizontal",
      textStyle = list(
        fontSize = 10
      ),
      itemWidth = 15,
      itemHeight = 7.5,
      padding = c(5, 10)
    ) |>
    e_text_g(
      left = 55,
      bottom = 30,
      style = list(
        text = caption_title,
        fontSize = 5,
        color = "#888888",
        textAlign = "left"
      )
    )
  })

  # # download handler for the graph
  # output$download_plot <- downloadHandler(
  #   filename = paste0("Fuel_prices_in_", Sys.Date(), ".png"),
  #   content = function(file) {
  #     # save the chart to an HTML file
  #     temp_html <- tempfile(fileext = ".html")
  #     saveWidget(as_widget(output$chart), file = temp_html, selfcontained = TRUE)

  #     # convert HTML to PNG using webshot2
  #     webshot2::webshot(temp_html, file = file, selector = "#chart")
  #   }
  # )
}

shinyApp(ui, server)
