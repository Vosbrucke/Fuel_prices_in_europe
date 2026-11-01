if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny", repos = "https://cloud.r-project.org")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes", repos = "https://cloud.r-project.org")
if (!requireNamespace("shinybrowser", quietly = TRUE)) install.packages("shinybrowser", repos = "https://cloud.r-project.org")
if (!requireNamespace("wesanderson", quietly = TRUE)) install.packages("wesanderson", repos = "https://cloud.r-project.org")
if (!requireNamespace("ggtext", quietly = TRUE)) install.packages("ggtext", repos = "https://cloud.r-project.org")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table", repos = "https://cloud.r-project.org")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate", repos = "https://cloud.r-project.org")
if (!requireNamespace("echarts4r", quietly = TRUE)) install.packages("echarts4r", repos = "https://cloud.r-project.org")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets", repos = "https://cloud.r-project.org")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly", repos = "https://cloud.r-project.org")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT", repos = "https://cloud.r-project.org")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis", repos = "https://cloud.r-project.org")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf", repos = "https://cloud.r-project.org")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth", repos = "https://cloud.r-project.org")

library("shiny")
library("shinythemes")
library("shinybrowser")
library("wesanderson")
library("ggtext")
library("data.table")
library("lubridate")
library("echarts4r")
library("htmlwidgets")
library("DT")
library("ggplot2")
library("viridis")
library("plotly")
library("sf")
library("rnaturalearth")

country_group_lookup <- c(
  "Austria" = "Western",
  "Belgium" = "Western",
  "Bulgaria" = "Eastern",
  "Croatia" = "Southern",
  "Cyprus" = "Southern",
  "Czechia" = "Eastern",
  "Denmark" = "Northen",
  "Estonia" = "Northen",
  "Finland" = "Northen",
  "France" = "Western",
  "Germany" = "Western",
  "Greece" = "Southern",
  "Hungary" = "Eastern",
  "Iceland" = "Northen",
  "Ireland" = "Western",
  "Italy" = "Southern",
  "Kosovo" = "Southern",
  "Latvia" = "Northen",
  "Lithuania" = "Northen",
  "Luxembourg" = "Western",
  "Malta" = "Southern",
  "Moldova" = "Eastern",
  "Montenegro" = "Southern",
  "Netherlands" = "Western",
  "North Macedonia" = "Southern",
  "Norway" = "Northen",
  "Poland" = "Eastern",
  "Portugal" = "Southern",
  "Romania" = "Eastern",
  "Serbia" = "Southern",
  "Slovakia" = "Eastern",
  "Slovenia" = "Southern",
  "Spain" = "Southern",
  "Sweden" = "Northen",
  "Switzerland" = "Western",
  "Turkey" = "Southern",
  "Ukraine" = "Eastern",
  "United Kingdom" = "Western"
)

country_group_colors <- c(
  "Northen" = "#3B9AB2",
  "Southern" = "#F98400",
  "Eastern" = "#00A08A",
  "Western" = "#FF0000",
  "EU" = "#5BBCD6"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {background-color: #FFFFFF; border-color: #FFFFFF; margin-bottom: 8px;}
      .col-sm-4, .col-sm-8 {
        padding-top: 0 !important;
        padding-left: 8px;
        padding-right: 8px;
      }
      .container-fluid {
        padding-left: 16px;
        padding-right: 16px;
      }
      .navbar .container {
        width: 100%;
        max-width: none;
        padding-left: 16px;
        padding-right: 16px;
      }
      .navbar-brand {
        padding-top: 10px;
        padding-bottom: 10px;
        margin-right: 12px;
      }
      .global-fuel-wrap {
        display: flex; align-items: center; justify-content: flex-end;
        margin-left: auto;
        padding-right: 8px;
      }
      #header-fuel-control {
        position: fixed;
        top: 10px;
        right: 20px;
        z-index: 1100;
      }
      .global-fuel-select {
        width: 180px;
        font-size: 12px;
      }
      .global-fuel-select .selectize-input {
        min-height: 34px;
        height: 34px;
        padding: 5px 30px 5px 12px;
        border: 1px solid #dfe6ee;
        border-radius: 999px;
        background: linear-gradient(180deg, #ffffff 0%, #f5f7fa 100%);
        box-shadow: 0 2px 8px rgba(15, 23, 42, 0.08);
        font-weight: 600;
        color: #1f2937;
      }
      .global-fuel-select .selectize-input.focus {
        border-color: #7c9ed9;
        box-shadow: 0 0 0 3px rgba(124, 158, 217, 0.15);
      }
      .global-fuel-select .selectize-dropdown {
        font-size: 12px;
        border-radius: 12px;
        overflow: hidden;
        border: 1px solid #e5e7eb;
        box-shadow: 0 10px 24px rgba(15, 23, 42, 0.12);
      }
      .global-fuel-select .item {
        background: #eff6ff !important;
        border-color: #bfdbfe !important;
        color: #1d4ed8 !important;
        border-radius: 999px !important;
      }
      .dataTables_wrapper {
        width: 100%;
      }
      #country_snapshot_table, #change_tracking_table {
        width: 100%;
      }
      #annotations_table {
        font-size: 11px;
      }
      #annotations_table th,
      #annotations_table td {
        padding: 4px 6px;
      }
      .tab-content {
        padding-top: 8px;
      }
      @media (max-width: 767px) {
        #header-fuel-control {
          position: static !important;
          display: flex;
          justify-content: flex-end;
          width: 100%;
          margin: 8px 0 10px;
        }
        .navbar {
          margin-bottom: 6px;
        }
        .navbar-brand {
          font-size: 18px !important;
          white-space: normal !important;
          line-height: 1.2;
          max-width: calc(100% - 190px);
        }
        .global-fuel-select {
          width: 150px;
          font-size: 11px;
        }
        .global-fuel-select .selectize-input {
          min-height: 30px !important;
          height: 30px !important;
          font-size: 11px;
        }
        .row {
          display: flex;
          flex-direction: column;
        }
        .col-sm-8 {
          order: 1;
          width: 100%;
          float: none;
        }
        .col-sm-4 {
          order: 2;
          width: 100%;
          float: none;
        }
        .html-widget {
          height: 420px !important;
        }
        svg text,
        .plotly svg text,
        .echarts4r text,
        .echarts4r .axis-label,
        .echarts4r .xAxis .axis-label,
        .echarts4r .yAxis .axis-label,
        .echarts4r .legend-label,
        .echarts4r .chart-title,
        #chart .axis-label,
        #relative_change_line_chart .axis-label,
        #chart .legend-label,
        #relative_change_line_chart .legend-label {
          font-size: 5px !important;
        }
      }
    "))
  ),
  theme = shinytheme("cosmo"),
  tags$div(
    id = "header-fuel-control",
    div(
      class = "global-fuel-select",
      selectInput(
        "global_fuel",
        NULL,
        choices = c("Euro 95", "Diesel", "LPG", "Heating Oil"),
        selected = "Euro 95",
        width = "170px"
      )
    )
  ),
  navbarPage(
    title = span("Consumer prices of petroleum products", style = "white-space: nowrap;"),
    tabPanel(
      "Price trend chart",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "trend_mode",
            "Chart type",
            choices = c(
              "Price trend" = "price",
              "Relative price index" = "index"
            ),
            selected = "price",
            inline = FALSE
          ),
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
          checkboxInput(
            "show_annotations",
            "Show market notes",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.trend_mode == 'price'",
            dateRangeInput(
              "date", "Choose date range",
              start = Sys.Date() - lubridate::years(3),
              end = Sys.Date(),
              startview = "year",
              weekstart = 1
            )
          ),
          conditionalPanel(
            condition = "input.trend_mode == 'index'",
            dateRangeInput(
              "relative_change_date_range",
              "Choose date range",
              start = Sys.Date() - lubridate::years(2),
              end = Sys.Date(),
              startview = "year",
              weekstart = 1
            )
          ),
          tags$style(HTML(".datepicker {z-index:99999 !important;}"))
        ),
        mainPanel(
          uiOutput("trend_chart_ui"),
          conditionalPanel(
            condition = "input.show_annotations == true",
            tags$div(
              style = "margin-top: 12px;",
              tableOutput("annotations_table")
            )
          )
        )
      )
    ),
    tabPanel(
      "Price on a map",
      sidebarLayout(
        sidebarPanel(
          uiOutput("map_date_ui"),
          uiOutput("map_reference_date_ui"),
          radioButtons(
            "map_metric",
            "Map metric",
            choices = c(
              "Price level" = "price",
              "Percent change vs reference date" = "pct_change"
            ),
            selected = "price"
          ),
          selectInput(
            "map_selected_country",
            "Highlight countries",
            choices = c(
              "Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
              "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
              "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
              "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta",
              "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Norway",
              "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia",
              "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom"
            ),
            selected = character(0),
            multiple = TRUE
          ),
          checkboxInput(
            "map_labels",
            "Show country labels",
            value = FALSE
          )
        ),
        mainPanel(
          plotOutput("map_chart", height = "520px")
        )
      )
    ),
    tabPanel(
      "Price bar chart",
      fluidPage(
        fluidRow(
          column(12, uiOutput("bar_date_ui"))
        ),
        fluidRow(
          column(12, plotOutput("bar_chart", height = "760px", width = "100%"))
        )
      )
    ),
    tabPanel(
      "Price change tracking",
      fluidPage(
        fluidRow(
          column(12, plotOutput("heatmap_chart", height = "760px", width = "100%"))
        )
      )
    ),
  ),
  shinybrowser::detect()
)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "#ECECE9", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#F5F5F2", color = NA),
      panel.background = element_rect(fill = "#F5F5F2", color = NA),
      legend.background = element_rect(fill = "#F5F5F2", color = NA),
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.title = element_text(size = 11, color = "#494E4F"),
      legend.text = element_text(size = 9, hjust = 0, color = "#494E4F"),
      plot.title = element_text(size = 15, hjust = 0.5, color = "#494E4F", face = "bold"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#494E4F", margin = margin(b = -0.1, t = -0.1, l = 2, unit = "cm")),
      plot.caption = element_text(size = 7, hjust = .5, margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184"),
      ...
    )
}

server <- function(input, output, session) {
  path <- "https://raw.githubusercontent.com/Vosbrucke/Fuel_prices_in_europe/main/Data/oil_bulletin_long.csv"
  oil_bulletin_long <- as.data.table(fread(path))
  oil_bulletin_long[, date := as.Date(date)]

  annotations <- reactive({
    ann_path <- "Data/annotations.csv"
    if (file.exists(ann_path)) {
      ann <- as.data.table(fread(ann_path))
      if (nrow(ann) > 0) {
        ann[, date := as.Date(date)]
      }
      return(ann)
    }
    data.table(
      date = as.Date(character()),
      country = character(),
      fuel = character(),
      short_label = character(),
      note = character(),
      category = character(),
      impact = character()
    )
  })

  annotations_filtered <- reactive({
    req(input$global_fuel)

    ann <- annotations()
    if (nrow(ann) == 0L) {
      return(ann)
    }

    if (isTRUE(input$trend_mode == "index")) {
      req(input$relative_change_date_range)
      date_start <- as.Date(input$relative_change_date_range[1])
      date_end <- as.Date(input$relative_change_date_range[2])
    } else {
      req(input$date)
      date_start <- as.Date(input$date[1])
      date_end <- as.Date(input$date[2])
    }

    out <- ann[fuel == input$global_fuel & date >= date_start & date <= date_end]

    if (!is.null(input$selected_country) && length(input$selected_country) > 0L) {
      out <- out[country %in% input$selected_country]
    }

    out
  })

  map_dates <- reactive({
    sort(unique(oil_bulletin_long$date))
  })

  normalize_to_week_monday <- function(x) {
    as.Date(floor_date(as.Date(x), unit = "week", week_start = 1))
  }

  valid_week_dates <- reactive({
    dates <- oil_bulletin_long$date
    sort(unique(dates[weekdays(dates) == "Monday"]))
  })

  output$bar_date_ui <- renderUI({
    valid_dates <- valid_week_dates()
    dateInput(
      "bar_date",
      "Reference date",
      value = normalize_to_week_monday(max(valid_dates)),
      min = normalize_to_week_monday(min(valid_dates)),
      max = normalize_to_week_monday(max(valid_dates)),
      format = "yyyy-mm-dd",
      startview = "years",
      weekstart = 1,
      width = "100%"
    )
  })

  observe({
    req(input$bar_date)
    valid_dates <- valid_week_dates()
    normalized <- normalize_to_week_monday(input$bar_date)
    if (!isTRUE(normalized %in% valid_dates)) {
      updateDateInput(session, "bar_date", value = max(valid_dates))
    } else if (!identical(as.Date(input$bar_date), normalized)) {
      updateDateInput(session, "bar_date", value = normalized)
    }
  })

  output$map_date_ui <- renderUI({
    dateInput(
      "map_date",
      "Selected date",
      value = max(map_dates()),
      min = min(map_dates()),
      max = max(map_dates()),
      format = "yyyy-mm-dd",
      startview = "years",
      weekstart = 1,
      width = "100%"
    )
  })

  output$map_reference_date_ui <- renderUI({
    dateInput(
      "map_reference_date",
      "Compare against date",
      value = max(map_dates()) - 7,
      min = min(map_dates()),
      max = max(map_dates()),
      format = "yyyy-mm-dd",
      startview = "years",
      weekstart = 1,
      width = "100%"
    )
  })

  palette <- reactive({
    wes_palette(
      "Darjeeling1",
      n = length(input$selected_country), type = "continuous"
    )
  })

  date_start <- reactive({as.Date(input$date[1], origin = "1970-01-01")})
  date_end <- reactive({as.Date(input$date[2], origin = "1970-01-01")})

  selected_fuel <- reactive({input$global_fuel})

  oil_bulletin_long_0 <- reactive({oil_bulletin_long[
    variable == selected_fuel() & date < date_end()
  ]})

  selected_country <- reactive({
    if (length(input$selected_country) == 0) "Othrs" else input$selected_country
  })

  react_df <- reactive({oil_bulletin_long_0()[date > date_start()][,
    country_name := fifelse(
      country_name %in% selected_country(), country_name, "Other"
    )
  ]})

  country_snapshot_data <- reactive({
    req(input$global_fuel)

    fuel_data <- oil_bulletin_long[variable == input$global_fuel & !is.na(value)]
    if (nrow(fuel_data) == 0L) {
      return(data.table(
        country_name = character(),
        country_group = character(),
        current_price = numeric(),
        pct_prev_week = numeric(),
        pct_prev_month = numeric(),
        pct_prev_six_months = numeric(),
        pct_ytd = numeric(),
        pct_prev_year = numeric()
      ))
    }

    valid_dates <- sort(unique(fuel_data$date))
    ref_date_raw <- if (!is.null(input$change_tracking_date)) as.Date(input$change_tracking_date) else if (!is.null(input$snapshot_date)) as.Date(input$snapshot_date) else max(valid_dates)
    ref_date <- normalize_to_week_monday(ref_date_raw)
    if (!ref_date %in% valid_dates) ref_date <- max(valid_dates)

    get_nearest_value <- function(target_date) {
      available <- fuel_data[date <= target_date]
      if (nrow(available) == 0L) {
        return(data.table(country_name = unique(fuel_data$country_name), value = NA_real_))
      }
      available[order(date), .(value = last(value)), by = country_name]
    }

    current_values <- fuel_data[date == ref_date, .(country_name, current_price = value)]
    previous_week <- get_nearest_value(ref_date - 7)
    setnames(previous_week, "value", "prev_week")
    previous_month <- get_nearest_value(ref_date %m-% months(1))
    setnames(previous_month, "value", "prev_month")
    previous_six_months <- get_nearest_value(ref_date %m-% months(6))
    setnames(previous_six_months, "value", "prev_six_months")
    ytd_start <- as.Date(paste0(year(ref_date), "-01-01"))
    ytd_values <- get_nearest_value(ytd_start)
    setnames(ytd_values, "value", "ytd_value")
    previous_year_values <- get_nearest_value(ref_date - years(1))
    setnames(previous_year_values, "value", "prev_year")

    snapshot <- current_values[previous_week, on = "country_name"]
    snapshot <- snapshot[previous_month, on = "country_name"]
    snapshot <- snapshot[previous_six_months, on = "country_name"]
    snapshot <- snapshot[ytd_values, on = "country_name"]
    snapshot <- snapshot[previous_year_values, on = "country_name"]

    snapshot[, country_group := vapply(country_name, function(x) {
      if (x %in% names(country_group_lookup)) country_group_lookup[[x]] else "EU"
    }, character(1L))]

    snapshot[, `:=`(
      pct_prev_week = ifelse(!is.na(prev_week) & prev_week != 0, (current_price - prev_week) / prev_week, NA_real_),
      pct_prev_month = ifelse(!is.na(prev_month) & prev_month != 0, (current_price - prev_month) / prev_month, NA_real_),
      pct_prev_six_months = ifelse(!is.na(prev_six_months) & prev_six_months != 0, (current_price - prev_six_months) / prev_six_months, NA_real_),
      pct_ytd = ifelse(!is.na(ytd_value) & ytd_value != 0, (current_price - ytd_value) / ytd_value, NA_real_),
      pct_prev_year = ifelse(!is.na(prev_year) & prev_year != 0, (current_price - prev_year) / prev_year, NA_real_)
    )]

    snapshot[, .(
      country_name,
      country_group,
      current_price,
      pct_prev_week,
      pct_prev_month,
      pct_prev_six_months,
      pct_ytd,
      pct_prev_year
    )][order(-current_price)]
  })

  output$trend_chart_ui <- renderUI({
    if (isTRUE(input$trend_mode == "index")) {
      echarts4rOutput("relative_change_line_chart", height = "650px")
    } else {
      echarts4rOutput("chart", height = "650px")
    }
  })

  output$relative_change_line_chart <- renderEcharts4r({
    req(input$global_fuel)
    req(input$relative_change_date_range)

    fuel_data <- oil_bulletin_long[variable == input$global_fuel & !is.na(value)]
    if (nrow(fuel_data) == 0L) {
      return(e_charts())
    }

    start_date <- normalize_to_week_monday(as.Date(input$relative_change_date_range[1]))
    end_date <- normalize_to_week_monday(as.Date(input$relative_change_date_range[2]))
    if (!start_date %in% fuel_data$date) start_date <- min(fuel_data$date)
    if (!end_date %in% fuel_data$date) end_date <- max(fuel_data$date)

    filtered <- fuel_data[date >= start_date & date <= end_date]
    if (nrow(filtered) == 0L) {
      return(e_charts())
    }

    selected_countries_vector <- if (is.null(input$selected_country) || length(input$selected_country) == 0L) {
      unique(filtered$country_name)
    } else {
      input$selected_country
    }

    filtered[, country_name := fifelse(country_name %in% selected_countries_vector, country_name, "Other")]
    selected_countries_vector <- unique(c(selected_countries_vector, "Other"))

    relative_palette <- c(
      wes_palette("Darjeeling1", n = length(selected_countries_vector) - 1L, type = "continuous"),
      "#d3d3d3"
    )

    base_values <- filtered[order(date), .(base_value = first(value)), by = country]
    dt_plot <- filtered[base_values, on = "country"]
    dt_plot[, value := ifelse(!is.na(base_value) & base_value != 0,
      100 * value / base_value, NA_real_)]
    dt_plot <- dt_plot[!is.na(value)]

    countries_letters <- data.table(
      country_name = selected_countries_vector,
      color = letters[seq_along(selected_countries_vector)]
    )

    dt_plot <- dt_plot[countries_letters, on = "country_name", nomatch = 0L]
    dt_plot[, country_name := factor(country_name, levels = selected_countries_vector)]

    y_axis <- dt_plot[dt_plot[, .I[1], by = country_name]$V1, value]
    if (any(na_ind <- is.na(y_axis))) {
      y_axis <- y_axis[!na_ind]
      dt_plot <- dt_plot[!country_name %in% countries_letters[na_ind][[1]]]
    }

    y_axis_values <- dt_plot$value[!is.na(dt_plot$value)]
    y_axis_max <- max(c(y_axis_values, 100), na.rm = TRUE)
    y_axis_min <- 0
    y_axis_interval <- if (y_axis_max <= 60) 10 else if (y_axis_max <= 120) 20 else if (y_axis_max <= 200) 25 else 50

    main_title <- paste(
      "Relative price of", input$global_fuel, "in Europe",
      "from", format(start_date, "%d %B %Y"), "to", format(end_date, "%d %B %Y")
    )

    caption_title <- paste0(
      "Data source: Oil Bulletin, European Commission, ",
      max(year(oil_bulletin_long$date)),
      "\nAuthor: Szymon Lisowski"
    )

    formatter <- htmlwidgets::JS(
      "function(params) {
        if (!params || !params.length) return '';

        var firstSeries = null;
        for (var j = 0; j < params.length; j++) {
          var item = params[j];
          if (item && item.data && item.data.value && item.seriesName) {
            firstSeries = item;
            break;
          }
        }

        if (!firstSeries) {
          return '';
        }

        var tooltipContent = '';
        var locale = 'en-US';
        var options = { minimumFractionDigits: 1, maximumFractionDigits: 1 };

        var items = params.map(function(item) {
          if (!item || !item.data || !item.data.value || !item.seriesName) return null;
          return {
            seriesName: item.seriesName,
            value: item.data.value[item.encode && item.encode.y ? item.encode.y[0] : 0],
            color: item.color
          };
        }).filter(Boolean);

        items.sort(function(a, b) {
          return b.value - a.value;
        });

        var uniqueItems = new Set();

        items.forEach(function(item) {
          var fmt = new Intl.NumberFormat(locale, options);
          var groupValueFormatted = fmt.format(item.value);

          if (!uniqueItems.has(item.seriesName) && item.color !== '#d3d3d3') {
            uniqueItems.add(item.seriesName);
            tooltipContent += '<div style=\"display:inline-block;width:10px;height:10px;margin-right:5px;background-color:' + item.color + ';\"></div>' +
                              '<b>' + item.seriesName + ':</b> ' + groupValueFormatted + '<br/>';
          }
        });

        return tooltipContent;
      }"
    )

    chart <- dt_plot |>
      group_by(country) |>
      e_charts(x = date) |>
      e_line(
        serie = value,
        legend = list(show = FALSE),
        lineStyle = list(width = 0.5),
        color = "#d3d3d3",
        symbol = "none"
      ) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name)
      ) |>
      e_line(serie = value, symbolSize = 2) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name) |>
          dplyr::filter(date == max(date))
      ) |>
      e_line(serie = value, symbolSize = 5) |>
      e_tooltip(trigger = "axis", formatter = formatter) |>
      e_toolbox_feature(feature = "saveAsImage") |>
      e_color(color = relative_palette) |>
      e_y_axis(
        min = y_axis_min,
        max = y_axis_max,
        interval = y_axis_interval,
        axisLabel = list(
          formatter = htmlwidgets::JS("function(value) { return value.toFixed(0); }"),
          textStyle = list(fontSize = 8)
        ),
        nameTextStyle = list(fontSize = 8)
      ) |>
      e_x_axis(
        axisLabel = list(textStyle = list(fontSize = 8)),
        nameTextStyle = list(fontSize = 8)
      ) |>
      e_title(
        text = main_title,
        top = "2%",
        left = "center",
        textStyle = list(fontSize = 10, fontWeight = "bold")
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

    if (any(!grepl("Othrs", selected_countries_vector))) {
      chart <- chart |>
        e_legend(
          top = "3%",
          left = "center",
          orient = "horizontal",
          textStyle = list(fontSize = 10),
          itemWidth = 15,
          itemHeight = 7.5,
          padding = c(15, 10),
          show = TRUE
        )
    } else {
      chart <- chart |> e_legend(show = FALSE)
    }

    if (isTRUE(input$show_annotations)) {
      ann <- annotations_filtered()
      if (nrow(ann) > 0L) {
        category_colors <- c(
          "supply shock" = "#E67E22",
          "policy" = "#8E5AE6",
          "market" = "#2F80ED",
          "seasonality" = "#27AE60",
          "other" = "#A0A0A0"
        )

        country_palette <- relative_palette
        names(country_palette) <- selected_countries_vector

        for (i in seq_len(nrow(ann))) {
          category_name <- trimws(as.character(ann$category[i]))
          category_key <- tolower(if (is.na(category_name) || category_name == "") "other" else category_name)
          category_color <- unname(category_colors[category_key])
          if (is.null(category_color) || length(category_color) == 0L || is.na(category_color)) {
            category_color <- "#A0A0A0"
          }

          country_name <- as.character(ann$country[i])
          country_color <- country_palette[[country_name]]
          if (is.null(country_color) || length(country_color) == 0L || is.na(country_color)) {
            country_color <- "#A0A0A0"
          }

          note_text <- if (is.na(ann$note[i]) || ann$note[i] == "") "Market note" else as.character(ann$note[i])
          gold_line_color <- "#D8B569"

          chart <- chart |>
            e_mark_line(
              data = list(
                xAxis = as.character(ann$date[i]),
                name = paste0(country_name, ': ', note_text),
                country_name = country_name,
                country_color = country_color,
                note_text = note_text,
                category_name = category_name,
                category_color = category_color,
                tooltip = list(
                  trigger = "item",
                  backgroundColor = "#1F2430",
                  borderColor = category_color,
                  borderWidth = 1,
                  textStyle = list(color = "#FFFFFF"),
                  extraCssText = "box-shadow: 0 4px 14px rgba(0,0,0,0.2); border-radius: 8px; padding: 8px 10px;",
                  formatter = htmlwidgets::JS(
                    "function(params) {
                      var countryColor = params.data && params.data.country_color ? params.data.country_color : '#A0A0A0';
                      var categoryColor = params.data && params.data.category_color ? params.data.category_color : '#A0A0A0';
                      var countryName = params.data && params.data.country_name ? params.data.country_name : (params.seriesName || params.name || 'country');
                      var noteText = params.data && params.data.note_text ? params.data.note_text : (params.name || 'Market note');
                      var label = params.data && params.data.category_name ? params.data.category_name : 'Market note';
                      return '<div style=\"font-size: 12px; line-height: 1.4; max-width: 260px; white-space: normal; word-break: break-word; overflow-wrap: anywhere;\"><div style=\"font-weight: 700; margin-bottom: 4px; color: #FFFFFF;\">Market note: <span style=\"color: ' + countryColor + ';\">' + countryName + '</span></div><div><span style=\"color: ' + categoryColor + '; font-weight: 700;\">' + label + '</span><br/>' + noteText + '</div></div>';
                    }"
                  )
                ),
                trigger = "click"
              ),
              symbol = "none",
              lineStyle = list(
                color = gold_line_color,
                width = 1.8,
                type = "dashed"
              ),
              label = list(show = FALSE)
            )
        }
      }
    }

    chart
  })

  output$bar_chart <- renderPlot({
    req(input$global_fuel)
    req(input$bar_date)

    fuel_data <- oil_bulletin_long[variable == input$global_fuel & !is.na(value)]
    if (nrow(fuel_data) == 0L) {
      return(NULL)
    }

    ref_date <- normalize_to_week_monday(input$bar_date)
    if (!ref_date %in% fuel_data$date) {
      ref_date <- max(fuel_data$date)
    }

    snapshot <- fuel_data[date == ref_date, .(country_name, current_price = value)]
    snapshot[, country_group := vapply(country_name, function(x) {
      if (x %in% names(country_group_lookup)) country_group_lookup[[x]] else "EU"
    }, character(1L))]
    snapshot <- snapshot[order(-current_price)]
    snapshot[, country_name := factor(country_name, levels = country_name)]

    x_max <- ceiling(max(snapshot$current_price, na.rm = TRUE) / 0.25) * 0.25 + 0.25

    ggplot(snapshot, aes(x = current_price, y = reorder(country_name, current_price), fill = country_group)) +
      geom_col(width = 0.8) +
      geom_text(
        aes(label = sprintf("€%.2f", current_price)),
        hjust = -0.15,
        size = 3,
        color = "#2d2d2d"
      ) +
      scale_fill_manual(values = country_group_colors, name = "Region") +
      scale_x_continuous(
        limits = c(0, x_max),
        breaks = seq(0, x_max, by = 0.25),
        minor_breaks = seq(0, x_max, by = 0.25),
        labels = function(x) paste0("€", sprintf("%.2f", x)),
        expand = expansion(mult = c(0, 0.08)),
        position = "top"
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = paste0("Price of ", input$global_fuel, " by country / zone on ", format(ref_date, "%B %d, %Y"))
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(color = "#bdbdbd", linewidth = 0.35, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        plot.margin = margin(10, 30, 10, 10),
        legend.position = "bottom",
        axis.title = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })

  output$heatmap_chart <- renderPlot({
    req(input$global_fuel)

    fuel_data <- oil_bulletin_long[variable == input$global_fuel & !is.na(value)]
    if (nrow(fuel_data) == 0L) {
      return(NULL)
    }

    latest_date <- max(fuel_data$date)
    ref_date <- latest_date

    get_nearest_value <- function(target_date) {
      available <- fuel_data[date <= target_date]
      if (nrow(available) == 0L) {
        return(data.table(country_name = unique(fuel_data$country_name), value = NA_real_))
      }
      available[order(date), .(value = last(value)), by = country_name]
    }

    current_values <- fuel_data[date == ref_date, .(country_name, current_price = value)]
    previous_week <- get_nearest_value(ref_date - 7)
    setnames(previous_week, "value", "prev_week")
    previous_month <- get_nearest_value(ref_date %m-% months(1))
    setnames(previous_month, "value", "prev_month")
    previous_six_months <- get_nearest_value(ref_date %m-% months(6))
    setnames(previous_six_months, "value", "prev_six_months")
    ytd_values <- get_nearest_value(as.Date(paste0(year(ref_date), "-01-01")))
    setnames(ytd_values, "value", "ytd_value")
    previous_year_values <- get_nearest_value(ref_date - years(1))
    setnames(previous_year_values, "value", "prev_year")

    snapshot <- current_values[previous_week, on = "country_name"]
    snapshot <- snapshot[previous_month, on = "country_name"]
    snapshot <- snapshot[previous_six_months, on = "country_name"]
    snapshot <- snapshot[ytd_values, on = "country_name"]
    snapshot <- snapshot[previous_year_values, on = "country_name"]

    snapshot[, `:=`(
      pct_prev_week = ifelse(!is.na(prev_week) & prev_week != 0, (current_price - prev_week) / prev_week, NA_real_),
      pct_prev_month = ifelse(!is.na(prev_month) & prev_month != 0, (current_price - prev_month) / prev_month, NA_real_),
      pct_prev_six_months = ifelse(!is.na(prev_six_months) & prev_six_months != 0, (current_price - prev_six_months) / prev_six_months, NA_real_),
      pct_ytd = ifelse(!is.na(ytd_value) & ytd_value != 0, (current_price - ytd_value) / ytd_value, NA_real_),
      pct_prev_year = ifelse(!is.na(prev_year) & prev_year != 0, (current_price - prev_year) / prev_year, NA_real_)
    )]

    long_snapshot <- melt(
      snapshot,
      id.vars = "country_name",
      measure.vars = c("pct_prev_week", "pct_prev_month", "pct_prev_six_months", "pct_ytd", "pct_prev_year"),
      variable.name = "period",
      value.name = "pct_change"
    )

    long_snapshot <- long_snapshot[country_name != "United Kingdom"]

    long_snapshot[, period := factor(
      period,
      levels = c("pct_prev_week", "pct_prev_month", "pct_prev_six_months", "pct_ytd", "pct_prev_year"),
      labels = c("Prev week", "Prev month", "Prev 6M", "YTD", "Prev year")
    )]

    country_levels <- rev(unique(long_snapshot$country_name))
    long_snapshot[, country_name := factor(country_name, levels = country_levels)]

    ggplot(long_snapshot, aes(x = period, y = country_name, fill = pct_change)) +
      geom_tile(color = "white", linewidth = 0.4) +
      geom_text(
        aes(label = ifelse(is.na(pct_change), "", sprintf("%.1f%%", pct_change * 100))),
        size = 2.5,
        color = "#2d2d2d"
      ) +
      scale_fill_gradientn(
        colors = wesanderson::wes_palette("Zissou1", 100, type = "continuous"),
        na.value = "#f0f0f0",
        limits = c(min(long_snapshot$pct_change, na.rm = TRUE), max(long_snapshot$pct_change, na.rm = TRUE))
      ) +
      scale_x_discrete(position = "top") +
      labs(
        x = NULL,
        y = NULL,
        title = paste("Relative change by country / zone -", input$global_fuel)
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 7, hjust = 0),
        axis.title = element_blank(),
        legend.position = "none",
        plot.margin = margin(20, 10, 10, 10)
      )
  })

  output$chart <- renderEcharts4r({
    selected_countries_vector <- unlist(selected_country())

    countries_letters <- data.table(
      country_name = selected_countries_vector,
      color = letters[seq_along(selected_countries_vector)]
    )

    dt_plot <- react_df()[countries_letters, on = "country_name", nomatch = 0L]
    dt_plot[, country_name := factor(country_name, levels = selected_countries_vector)]

    y_axis <- dt_plot[dt_plot[, .I[1], by = country_name]$V1, value]
    if (any(na_ind <- is.na(y_axis))) {
      y_axis <- y_axis[!na_ind]
      dt_plot <- dt_plot[!country_name %in% countries_letters[na_ind][[1]]]
    }

    main_title <- paste(
      "Price of", input$global_fuel, "in Europe until",
      format(max(react_df()[["date"]]), "%d %B %Y")
    )

    caption_title <- paste0(
      "Data source: Oil Bulletin, European Commission, ",
      max(year(oil_bulletin_long$date)),
      "\nAuthor: Szymon Lisowski"
    )

    formatter <- htmlwidgets::JS(
      "function(params) {
        if (!params || !params.length) return '';

        var markLineNote = null;
        for (var i = 0; i < params.length; i++) {
          var param = params[i];
          if (param && param.componentType === 'markLine' && param.name) {
            markLineNote = param.name;
            break;
          }
        }

        if (markLineNote) {
          return '<b>Market note</b><br/>' + markLineNote;
        }

        var firstSeries = null;
        for (var j = 0; j < params.length; j++) {
          var item = params[j];
          if (item && item.data && item.data.value && item.seriesName) {
            firstSeries = item;
            break;
          }
        }

        if (!firstSeries) {
          return '';
        }

        var tooltipContent = '<b>Date:</b> ' + (firstSeries.axisValueLabel || firstSeries.name || '') + '<br/>';
        var locale = 'en-US';
        var options = { style: 'currency', currency: 'EUR', minimumFractionDigits: 1 };

        var items = params.map(function(item) {
          if (!item || !item.data || !item.data.value || !item.seriesName) return null;
          return {
            seriesName: item.seriesName,
            value: item.data.value[item.encode && item.encode.y ? item.encode.y[0] : 0],
            color: item.color
          };
        }).filter(Boolean);

        items.sort(function(a, b) {
          return b.value - a.value;
        });

        var uniqueItems = new Set();

        items.forEach(function(item) {
          var fmt = new Intl.NumberFormat(locale, options);
          var groupValueFormatted = fmt.format(item.value);

          if (!uniqueItems.has(item.seriesName) && item.color !== '#d3d3d3') {
            uniqueItems.add(item.seriesName);
            tooltipContent += '<div style=\"display: inline-block; width: 10px; height: 10px; margin-right: 5px; background-color: ' + item.color + '\"></div>' +
                              '<b>' + item.seriesName + ':</b> ' + groupValueFormatted + '<br/>';
          }
        });

        return tooltipContent;
      }"
    )

    chart <- react_df() |>
      group_by(country) |>
      e_charts(x = date) |>
      e_line(
        serie = value,
        legend = list(show = FALSE),
        lineStyle = list(width = 0.5),
        color = "#d3d3d3",
        symbol = "none"
      ) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name)
      ) |>
      e_line(serie = value, symbolSize = 2) |>
      e_data(
        data = dt_plot |>
          dplyr::filter(!country_name %in% c("Other")) |>
          group_by(country_name) |>
          dplyr::filter(date == max(date))
      ) |>
      e_line(serie = value, symbolSize = 5) |>
      e_tooltip(trigger = "axis", formatter = formatter) |>
      e_toolbox_feature(feature = "saveAsImage") |>
      e_color(color = palette()) |>
      e_y_axis(
        formatter = htmlwidgets::JS(
          "function(value) {
            return '€' + value.toFixed(1);
          }"
        ),
        axisLabel = list(textStyle = list(fontSize = 8)),
        nameTextStyle = list(fontSize = 8)
      ) |>
      e_x_axis(
        axisLabel = list(textStyle = list(fontSize = 8)),
        nameTextStyle = list(fontSize = 8)
      ) |>
      e_title(
        text = main_title,
        top = "2%",
        left = "center",
        textStyle = list(fontSize = 10, fontWeight = "bold")
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

    if (any(!grepl("Othrs", selected_countries_vector))) {
      chart <- chart |>
        e_legend(
          top = "3%",
          left = "center",
          orient = "horizontal",
          textStyle = list(fontSize = 10),
          itemWidth = 15,
          itemHeight = 7.5,
          padding = c(15, 10),
          show = TRUE
        )
    } else {
      chart <- chart |> e_legend(show = FALSE)
    }

    if (isTRUE(input$show_annotations)) {
      ann <- annotations_filtered()
      if (nrow(ann) > 0L) {
        category_colors <- c(
          "supply shock" = "#E67E22",
          "policy" = "#8E5AE6",
          "market" = "#2F80ED",
          "seasonality" = "#27AE60",
          "other" = "#A0A0A0"
        )

        country_palette <- palette()
        names(country_palette) <- selected_country()

        for (i in seq_len(nrow(ann))) {
          category_name <- trimws(as.character(ann$category[i]))
          category_key <- tolower(if (is.na(category_name) || category_name == "") "other" else category_name)
          category_color <- unname(category_colors[category_key])
          if (is.null(category_color) || length(category_color) == 0L || is.na(category_color)) {
            category_color <- "#A0A0A0"
          }

          country_name <- as.character(ann$country[i])
          country_color <- country_palette[[country_name]]
          if (is.null(country_color) || length(country_color) == 0L || is.na(country_color)) {
            country_color <- "#A0A0A0"
          }

          note_text <- if (is.na(ann$note[i]) || ann$note[i] == "") "Market note" else as.character(ann$note[i])
          gold_line_color <- "#D8B569"

          chart <- chart |>
            e_mark_line(
              data = list(
                xAxis = as.character(ann$date[i]),
                name = paste0(country_name, ': ', note_text),
                country_name = country_name,
                country_color = country_color,
                note_text = note_text,
                category_name = category_name,
                category_color = category_color,
                tooltip = list(
                  trigger = "item",
                  backgroundColor = "#1F2430",
                  borderColor = category_color,
                  borderWidth = 1,
                  textStyle = list(color = "#FFFFFF"),
                  extraCssText = "box-shadow: 0 4px 14px rgba(0,0,0,0.2); border-radius: 8px; padding: 8px 10px;",
                  formatter = htmlwidgets::JS(
                    "function(params) {
                      var countryColor = params.data && params.data.country_color ? params.data.country_color : '#A0A0A0';
                      var categoryColor = params.data && params.data.category_color ? params.data.category_color : '#A0A0A0';
                      var countryName = params.data && params.data.country_name ? params.data.country_name : (params.seriesName || params.name || 'country');
                      var noteText = params.data && params.data.note_text ? params.data.note_text : (params.name || 'Market note');
                      var label = params.data && params.data.category_name ? params.data.category_name : 'Market note';
                      return '<div style=\"font-size: 12px; line-height: 1.4; max-width: 260px; white-space: normal; word-break: break-word; overflow-wrap: anywhere;\"><div style=\"font-weight: 700; margin-bottom: 4px; color: #FFFFFF;\">Market note: <span style=\"color: ' + countryColor + ';\">' + countryName + '</span></div><div><span style=\"color: ' + categoryColor + '; font-weight: 700;\">' + label + '</span><br/>' + noteText + '</div></div>';
                    }"
                  )
                ),
                trigger = "click"
              ),
              symbol = "none",
              lineStyle = list(
                color = gold_line_color,
                width = 1.8,
                type = "dashed"
              ),
              label = list(show = FALSE)
            )
        }
      }
    }

    chart
  })

  output$annotations_table <- renderTable({
    if (!isTRUE(input$show_annotations)) {
      return(NULL)
    }

    ann <- annotations_filtered()
    if (nrow(ann) == 0L) {
      return(NULL)
    }

    ann[, .(
      Date = format(date, "%Y-%m-%d"),
      Country = country,
      Label = short_label,
      Category = ifelse(is.na(category) | category == "", "General", category),
      Note = note
    )]
  }, striped = TRUE, hover = TRUE, width = "100%", digits = 0)

  output$map_chart <- renderPlot({
    req(input$global_fuel)
    req(input$map_date)
    req(input$map_reference_date)

    selected_date <- as.Date(input$map_date)
    reference_date <- as.Date(input$map_reference_date)

    iso_lookup <- data.table(
      country_name = c(
        "Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
        "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
        "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
        "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta",
        "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Norway",
        "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia",
        "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom"
      ),
      iso3 = c(
        "ALB", "AUT", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
        "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "XKX", "LVA",
        "LTU", "LUX", "MLT", "MDA", "MNE", "NLD", "MKD", "NOR", "POL", "PRT",
        "ROU", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "UKR", "GBR"
      )
    )

    country_values <- oil_bulletin_long[
      variable == input$global_fuel & country_name %in% iso_lookup$country_name,
      .(country_name, date, value)
    ]

    if (nrow(country_values) == 0) {
      return(NULL)
    }

    nearest_value_by_country <- function(target_date) {
      available <- country_values[date <= target_date]
      if (nrow(available) == 0L) {
        return(data.table(country_name = unique(country_values$country_name), value = NA_real_))
      }
      available[order(date), .(value = last(value)), by = country_name]
    }

    selected_values <- nearest_value_by_country(selected_date)
    reference_values <- nearest_value_by_country(reference_date)

    mapped_data <- iso_lookup[selected_values, on = "country_name"]
    setnames(mapped_data, "value", "selected_value")
    mapped_data <- mapped_data[reference_values, on = "country_name"]
    setnames(mapped_data, "value", "reference_value")

    mapped_data <- mapped_data[!country_name %in% c("European Union", "Euro Zone")]
    mapped_data[, pct_change := ifelse(
      !is.na(reference_value) & reference_value != 0,
      100 * (selected_value - reference_value) / reference_value,
      NA_real_
    )]

    if (nrow(mapped_data) == 0) {
      return(NULL)
    }

    selected_country_names <- if (is.null(input$map_selected_country)) character(0) else input$map_selected_country
    selected_country_names <- intersect(selected_country_names, iso_lookup$country_name)

    metric <- if (input$map_metric == "price") "selected_value" else "pct_change"
    mapped_data[, metric_value := get(metric)]
    mapped_data[, is_selected_country := if (length(selected_country_names) == 0L) TRUE else country_name %in% selected_country_names]
    mapped_data[, metric_value := ifelse(is_selected_country, metric_value, NA_real_)]

    metric_label <- if (input$map_metric == "price") "Price in EUR" else "Percent change vs reference date"
    metric_unit <- if (input$map_metric == "price") " EUR" else " %"

    europe_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf", continent = "Europe")
    europe_sf <- europe_sf[, c("iso_a3", "adm0_a3", "geometry")]
    europe_sf$country_name <- iso_lookup$country_name[match(europe_sf$adm0_a3, iso_lookup$iso3)]
    europe_sf$country_selected <- if (length(selected_country_names) == 0L) TRUE else europe_sf$country_name %in% selected_country_names
    europe_sf$metric_value <- mapped_data$metric_value[match(europe_sf$adm0_a3, mapped_data$iso3)]
    europe_sf$metric_value[is.na(europe_sf$country_selected) | !europe_sf$country_selected] <- NA_real_
    europe_sf$label_text <- ifelse(
      !is.na(europe_sf$metric_value) & europe_sf$country_selected,
      paste0(europe_sf$country_name, "\n", sprintf("%.2f", europe_sf$metric_value), if (input$map_metric == "price") " €" else " %"),
      NA_character_
    )

    map_colors <- viridis::viridis(100, option = "magma", alpha = 0.8, begin = 0.1, end = 0.9, direction = -1)

    map_plot <- ggplot(europe_sf) +
      geom_sf(aes(fill = metric_value), color = "white", linewidth = 0.25)

    if (input$map_labels && any(!is.na(europe_sf$label_text))) {
      map_plot <- suppressWarnings(
        map_plot +
          geom_sf_label(
            data = subset(europe_sf, !is.na(label_text)),
            aes(label = label_text),
            color = "#2b2b2b",
            fill = "white",
            size = 2.7,
            label.padding = unit(0.12, "lines"),
            label.r = unit(0.08, "lines"),
            label.size = 0.1,
            check_overlap = FALSE,
            lineheight = 0.9
          )
      )
    }

    map_plot <- map_plot +
      coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE) +
      scale_fill_gradientn(
        colors = map_colors,
        na.value = "#d9d9d9",
        name = metric_label,
        guide = guide_colorbar(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(10, "lines"),
          barheight = unit(0.18, "inches"),
          direction = "horizontal"
        )
      ) +
      labs(
        title = paste0("Price of ", input$global_fuel, " in Europe on ", format(selected_date, "%d %B %Y")),
        caption = "Source: Oil Bulletin, European Commission"
      ) +
      theme_map() +
      theme(
        legend.position = "bottom",
        legend.justification = "center",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.caption = element_text(size = 8, color = "#666666"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.margin = unit(c(0.3, 0.2, 0.1, 0.2), "cm")
      )

    map_plot
  })
}

shinyApp(ui, server)

