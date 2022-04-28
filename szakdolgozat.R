# borders: https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json
# data: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv

library(shiny)
library(leaflet)
library(RColorBrewer)
library(geojsonio)
library(scales)
library(ggplot2)
library(data.table)

borders_url <-
  "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
data_url <-
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

if (length(grep(paste0("^owid-covid-data_", Sys.Date(), ".csv"), list.files())) > 0) {
  data <- fread(paste0("owid-covid-data_", Sys.Date(), ".csv"))
} else {
  for (fileIndex in rev(grep(
    "^owid-covid-data_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv",
    list.files()
  ))) {
    print(paste(list.files()[fileIndex], "removed"))
    file.remove(list.files()[fileIndex])
  }
  download.file(
    url = data_url,
    destfile = paste0(getwd(), "/owid-covid-data_", Sys.Date(), ".csv")
  )
  data <- fread(paste0("owid-covid-data_", Sys.Date(), ".csv"))
}

if (file.exists("countries.geo.json")) {
  WorldCountry <- geojson_read("countries.geo.json", what = "sp")
} else {
  download.file(url = borders_url,
                destfile = paste0(getwd(), "/countries.geo.json"))
  WorldCountry <- geojson_read("countries.geo.json", what = "sp")
}

vars <- c(
  "New cases" = "new_cases",
  "Total cases" = "total_cases",
  "Total deaths per million" = "total_deaths_per_million",
  "Test positivity rate" = "positive_rate",
  "People fully vaccinated per hundred" = "people_fully_vaccinated_per_hundred"
)


ui <- navbarPage(
  "COVID-19 data",
  tabPanel(
    "Interactive map",
    tags$style(
      type = "text/css",
      ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
    ),
    div(
      class = "outer",
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(
        top = 10,
        right = 10,
        draggable = TRUE,
        selectInput("map_variable", "Variable", vars),
        dateInput(
          "date",
          strong("Date"),
          value = max(data$date),
          min = min(data$date),
          max = max(data$date)
        ),
        checkboxInput("map_legend", "Show legend", TRUE)
      )
    )
  ),
  
  tabPanel(
    "Total cases per continent",
    titlePanel("Covid-19 Total cases per million over time by continent"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "total_cases_continents",
          strong("Continents"),
          choices = unique(data$continent)[unique(data$continent) != ""],
          selected = "Europe"
        ),
        
        dateRangeInput(
          "total_cases_date",
          strong("Date range"),
          start = min(data$date),
          end = max(data$date),
          min = min(data$date),
          max = max(data$date)
        )
      ),
      mainPanel(plotOutput(outputId = "total_cases_lineplot", height = "300px"))
    )
  ),
  
  navbarMenu(
    "Scatter plots",
    tabPanel(
      "COVID-19 daily new cases",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            "scatter_daily_date",
            strong("Date range"),
            start = min(data$date),
            end = max(data$date),
            min = min(data$date),
            max = max(data$date)
          ),
          checkboxGroupInput(
            "scatter_daily_countries",
            strong("Countries"),
            choices = unique(data[data$continent == "South America", ]$location),
            selected = c("Argentina", "Bolivia")
          )
          
        ),
        mainPanel(plotOutput(outputId = "scatter_daily_plot", height = "300px"))
      )
    ),
    
    tabPanel(
      "Deaths, cases, people fully vaccinated",
      sidebarLayout(sidebarPanel(
        helpText(
          "This graph shows the correlation between the total cases per million, total deaths per million, people fully vaccinated per hundred variables by country on the given day"
        ),
        dateInput(
          "scatter_country_date",
          strong("Date"),
          value = max(data$date),
          min = min(data$date),
          max = max(data$date)
        )
      ),
      mainPanel(
        plotOutput(outputId = "scatter_country_plot", height = "300px")
      ))
    ),
    
    tabPanel(
      "Deaths, cases, people fully vaccinated, location",
      sidebarLayout(sidebarPanel(
        helpText(
          "This graph shows the correlation between the total cases per million, total deaths per million, people fully vaccinated per hundred variables by income level on the given day"
        ),
        dateInput(
          "scatter_income_date",
          strong("Date"),
          value = max(data$date),
          min = min(data$date),
          max = max(data$date)
        )
      ),
      mainPanel(
        plotOutput(outputId = "scatter_income_plot", height = "300px")
      ))
    ),
    tabPanel(
      "COVID-19 total cases",
      sidebarLayout(
        sidebarPanel(
          dateInput(
            "scatter_total_date",
            strong("Date"),
            value = max(data$date),
            min = min(data$date),
            max = max(data$date)
          ),
          checkboxGroupInput(
            "scatter_total_continents",
            strong("Continents"),
            choices = unique(data[data$continent != '',]$continent),
            selected = c("Europe")
          )
          
        ),
        mainPanel(plotOutput(outputId = "scatter_total_plot", height = "300px"))
      )
    )
  ),
  
  navbarMenu(
    "Other plots",
    tabPanel("Histogram",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   "histogram_date",
                   strong("Date range"),
                   start = as.Date(max(data$date)) -
                     30,
                   end = max(data$date),
                   min = min(data$date),
                   max = max(data$date)
                 ),
                 checkboxGroupInput(
                   "histogram_continents",
                   strong("Continents"),
                   choices = unique(data$continent)[unique(data$continent) != ""],
                   selected = unique(data$continent)[unique(data$continent) != ""]
                 )
                 
               ),
               mainPanel(plotOutput(outputId = "histogram_plot"))
             )),
    tabPanel("Bar chart",
             sidebarLayout(
               sidebarPanel(
                 dateInput(
                   "bar_date",
                   strong("Date"),
                   value = max(data$date),
                   min = min(data$date),
                   max = max(data$date)
                 ),
                 checkboxGroupInput(
                   "bar_continents",
                   strong("Continents"),
                   choices = unique(data$continent)[unique(data$continent) != ""],
                   selected = unique(data$continent)[unique(data$continent) != ""]
                 )
               ),
               mainPanel(plotOutput(outputId = "bar_plot"))
             )),
    tabPanel("Box plot",
             sidebarLayout(
               sidebarPanel(
                 dateInput(
                   "box_date",
                   strong("Date"),
                   value = max(data$date),
                   min = min(data$date),
                   max = max(data$date)
                 ),
                 checkboxGroupInput(
                   "box_continents",
                   strong("Continents"),
                   choices = unique(data$continent)[unique(data$continent) != ""],
                   selected = unique(data$continent)[unique(data$continent) != ""]
                 )
               ),
               mainPanel(plotOutput(outputId = "box_plot"))
             ))
  ),
  
  tabPanel(
    "Data explorer",
    fluidRow(
      column(
        3,
        conditionalPanel(
          "input.data_explorer_other_locations == ''",
          selectInput(
            "data_explorer_continents",
            "Continents",
            choices = unique(data$continent),
            multiple = TRUE
          )
        )
      ),
      column(
        3,
        conditionalPanel(
          "input.data_explorer_other_locations == ''",
          selectInput(
            "data_explorer_countries",
            "Countries",
            choices = unique(data[data$continent != "", ]$location),
            multiple = TRUE
          )
        )
      ),
      column(
        4,
        dateRangeInput(
          "data_explorer_date",
          strong("Date range"),
          start = min(data$date),
          end = max(data$date),
          min = min(data$date),
          max = max(data$date)
        )
      ),
      column(
        2,
        conditionalPanel(
          "input.data_explorer_continents == '' & input.data_explorer_countries == ''",
          selectInput(
            "data_explorer_other_locations",
            "Other locations",
            choices = unique(data[data$continent == "", ]$location),
            multiple = TRUE
          )
        )
      )
    ),
    dataTableOutput("explore_data_output")
    
  )
)


server <- function(input, output) {
  #a WorldCountry$id és data$iso_code-oknak meg kell egyezniük, és a megfelelő sorrendben kell lenniük
  #különben a térképen egy polygonhoz nem a megfelelő értékek fognak tartozni
  countries <- reactive({
    intersection <-
      intersect(WorldCountry$id, data[data$date == input$date,]$iso_code)
    tmp_WorldCountry <-
      WorldCountry[WorldCountry$id %in% intersection, ]
    tmp_WorldCountry <-
      tmp_WorldCountry[order(tmp_WorldCountry$id),]
    tmp_WorldCountry
  })
  
  filtered_data <- reactive({
    intersection <-
      intersect(WorldCountry$id, data[data$date == input$date,]$iso_code)
    tmp_data <-
      data[data$iso_code %in% intersection &
             data$date == input$date, ]
    tmp_data <- tmp_data[order(tmp_data$iso_code), ]
    tmp_data
  })
  
  selected_variable <- reactive({
    input$map_variable
  })
  
  colorpal <- reactive({
    if (max(filtered_data()[[selected_variable()]], na.rm = TRUE) == -Inf) {
      colorNumeric(palette = "Blues",
                   domain = NULL)
    } else {
      colorNumeric(palette = "Blues",
                   domain = filtered_data()[[selected_variable()]])
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 10,
              lng = 0,
              zoom = 2)
  })
  
  #a dátum változásakor nem az egész térképet, csak a polygonokat rajzoljuk újra
  observe({
    pal <- colorpal()
    proxy <- leafletProxy("map", data = filtered_data())
    
    proxy %>% clearShapes() %>%
      addPolygons(
        data = countries(),
        fillColor = ~ pal(filtered_data()[[selected_variable()]]),
        weight = 1,
        opacity = 0.7,
        color = "blue",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#fff",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~ paste0(
          "Country: ",
          filtered_data()$location,
          br(),
          names(vars)[vars == selected_variable()],
          ": ",
          filtered_data()[[selected_variable()]]
        )
      )
  })
  
  observe({
    pal <- colorpal()
    proxy <- leafletProxy("map", data = filtered_data())
    
    proxy %>% clearControls()
    if (input$map_legend) {
      proxy %>% addLegend(
        title = names(vars)[vars == selected_variable()],
        position = "bottomright",
        pal = pal,
        values = filtered_data()[[selected_variable()]]
      )
    }
  })
  
  
  # TOTAL CASES PER CONTINENT #################################################################
  
  total_cases_data <- reactive({
    shiny::validate(
      need(
        input$total_cases_date[[1]] < input$total_cases_date[[2]],
        "Error: The starting date must be earlier than the ending date."
      )
    )
    tmp <- data[data$location %in% input$total_cases_continents &
                  data$date >= input$total_cases_date[[1]] &
                  data$date <= input$total_cases_date[[2]] &
                  !is.na(data$total_cases_per_million),]
    transform(tmp, date = as.Date(date, format = "%Y-%m-%d"))
  })
  
  
  output$total_cases_lineplot <- renderPlot({
    par(mar = c(4, 4, 1, 1))
    ggplot(data = total_cases_data(),
           aes(
             date,
             total_cases_per_million,
             group = location,
             color = location
           )) +
      geom_line(size = 1) +
      labs(y = "total COVID-19 cases per million people",
           x = "Date") +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_date(breaks = breaks_width("1 month"), labels = date_format("%Y-%m")) +
      scale_y_continuous(labels = comma)
  })
  
  # SCATTER PLOTS ###############################################################################
  
  scatter_daily_data <- reactive({
    shiny::validate(
      need(
        input$scatter_daily_date[[1]] <= input$scatter_daily_date[[2]],
        "Error: The starting date must be the same as or be earlier than the ending date."
      )
    )
    tmp <- data[data$location %in% input$scatter_daily_countries &
                  data$date >= input$scatter_daily_date[[1]] &
                  data$date <= input$scatter_daily_date[[2]] &
                  !is.na(data$new_cases), ]
    transform(tmp, date = as.Date(date, format = "%Y-%m-%d"))
  })
  
  output$scatter_daily_plot <- renderPlot({
    ggplot(data = scatter_daily_data(), aes(date, new_cases, color = iso_code)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      labs(title = "New confirmed COVID-19 cases in South America",
           y = "New cases",
           x = "Date") +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_date(breaks = breaks_width("1 month"), labels = date_format("%Y-%m")) +
      scale_y_continuous(labels = comma)
  })
  
  scatter_country_data <- reactive({
    data[data$date == input$scatter_country_date &
           !is.na(data$total_cases_per_million) &
           !is.na(data$total_deaths_per_million) &
           !is.na(data$people_fully_vaccinated_per_hundred) &
           data$continent != "",]
  })
  
  output$scatter_country_plot <- renderPlot({
    ggplot(
      data = scatter_country_data(),
      aes(
        total_cases_per_million,
        total_deaths_per_million,
        color = people_fully_vaccinated_per_hundred
      )
    ) +
      geom_point(size = 2.5) +
      labs(y = "Total deaths per million",
           x = "Total cases per million",
           color = "People fully vaccinated per hundred") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma)
  })
  
  scatter_income_data <- reactive({
    data[data$date == input$scatter_income_date &
           data$location %in% c("High income",
                                "Upper middle income",
                                "Lower middle income",
                                "Low income"),]
  })
  
  output$scatter_income_plot <- renderPlot({
    ggplot(data = scatter_income_data(),
           aes(total_cases_per_million, total_deaths_per_million)) +
      geom_point(aes(size = people_fully_vaccinated_per_hundred, color = location)) +
      labs(
        y = "Total deaths per million",
        x = "Total cases per million",
        size = "People fully vaccinated per hundred",
        color = "Income level"
      ) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma)
  })
  
  scatter_total_data <- reactive({
    data[data$date == input$scatter_total_date &
           data$continent %in% input$scatter_total_continents &
           !is.na(data$total_cases_per_million),]
  })
  
  output$scatter_total_plot <- renderPlot({
    ggplot(data = scatter_total_data(),
           aes(continent, total_cases_per_million, color = continent)) +
      coord_flip() +
      geom_jitter(size = 4,
                  alpha = 0.2,
                  width = 0.05) +
      stat_summary(fun = mean,
                   geom = "point",
                   size = 8) +
      geom_hline(aes(yintercept = mean(total_cases_per_million)), color = "black", size =
                   0.9) +
      labs(title = "Total cases per million people by continents on a given day.",
           y = "Total cases per million",
           x = "Continent") +
      scale_y_continuous(labels = comma)
  })
  
  
  # HISTOGRAM ################################################################
  
  hist_data <- reactive({
    shiny::validate(
      need(
        input$histogram_date[[1]] <= input$histogram_date[[2]],
        "Error: The starting date must be the same as or be earlier than the ending date."
      )
    )
    tmp <- data[data$continent %in% input$histogram_continents &
                  data$date >= input$histogram_date[[1]] &
                  data$date <= input$histogram_date[[2]] &
                  !is.na(data$new_cases), ]
    transform(tmp, date = as.Date(date, format = "%Y-%m-%d"))
  })
  
  output$histogram_plot <- renderPlot({
    colors <- c(
      "Asia" = "#7aff7a",
      "Europe" = "#3838ff",
      "Africa" = "#ffa64d",
      "North America" = "#ffff00",
      "South America" = "#ff80ff",
      "Oceania" = "#ff0000"
    )
    
    ggplot(data = hist_data(), aes(date, new_cases)) +
      geom_col(aes(fill = continent)) +
      labs(title = "Daily new cases by continent on a given day",
           y = "New cases",
           x = "Date") +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_manual(values = colors) +
      scale_x_date(breaks = breaks_width("1 month"), labels = date_format("%Y-%m")) +
      scale_y_continuous(labels = comma)
  })
  
  # BAR CHART ##################################################################
  
  bar_data <- reactive({
    data[data$date == input$bar_date &
           data$location %in% input$bar_continents &
           !is.na(data$people_fully_vaccinated_per_hundred),]
  })
  
  output$bar_plot <- renderPlot({
    ggplot(data = bar_data(),
           aes(location, people_fully_vaccinated_per_hundred)) +
      geom_bar(
        stat = "identity",
        width = 0.8,
        color = "red",
        fill = "pink"
      ) +
      geom_text(
        aes(label = people_fully_vaccinated_per_hundred),
        vjust = 1.6,
        size = 5
      ) +
      geom_hline(
        yintercept = mean(bar_data()$people_fully_vaccinated_per_hundred),
        linetype = "dashed",
        size = 1
      ) +
      geom_text(aes(
        1,
        mean(bar_data()$people_fully_vaccinated_per_hundred),
        label = "World average",
        vjust = -0.5
      )) +
      labs(title = "People fully vaccinated by continent on a given day",
           y = "People fully vaccinated per hundred",
           x = "Continent")
  })
  
  # BOX PLOT ###################################################################
  
  box_data <- reactive({
    data[data$date == input$box_date &
           data$continent %in% input$box_continents &
           !is.na(data$total_cases_per_million),]
  })
  
  output$box_plot <- renderPlot({
    ggplot(data = box_data(), aes(continent, total_cases_per_million)) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(0.2)) +
      labs(title = "Total cases per million people by continents on a given day.",
           y = "Total cases per million",
           x = "Continent") +
      scale_y_continuous(labels = comma)
  })
  
  # DATA EXPLORER ##############################################################
  
  selected_continents <- reactive({
    if (is.null(input$data_explorer_continents)) {
      updateSelectInput(inputId = "data_explorer_countries",
                        choices = unique(data[data$continent != "", ]$location))
      unique(data$continent)
    } else {
      updateSelectInput(inputId = "data_explorer_countries",
                        choices = unique(data[data$continent %in% input$data_explorer_continents, ]$location))
      input$data_explorer_continents
    }
  })
  
  selected_countries <- reactive({
    if (is.null(input$data_explorer_countries)) {
      unique(data[data$continent != "", ]$location)
    } else {
      input$data_explorer_countries
    }
  })
  
  selected_other_location <- reactive({
    if (is.null(input$data_explorer_other_locations)) {
      unique(data[data$continent == "", ]$location)
    } else {
      input$data_explorer_other_locations
    }
  })
  
  selected_date <- reactive({
    shiny::validate(
      need(
        input$data_explorer_date[[1]] <= input$data_explorer_date[[2]],
        "Error: The starting date must be the same as or be earlier than the ending date."
      )
    )
    input$data_explorer_date
  })
  
  
  explore_data_data <- reactive({
    if (length(selected_continents()) == length(unique(data$continent)) &
        length(selected_countries()) == length(unique(data[data$continent != "", ]$location)) &
        length(selected_other_location()) == length(unique(data[data$continent == "", ]$location))) {
      data[data$date >= selected_date()[[1]] &
             data$date <= selected_date()[[2]] &
             ((
               data$continent %in% selected_continents() &
                 data$location %in% selected_countries()
             ) |
               data$location %in% selected_other_location()
             ), ]
    } else if (length(selected_continents()) != length(unique(data$continent)) |
               length(selected_countries()) != length(unique(data[data$continent != "", ]$location))) {
      data[data$date >= selected_date()[[1]] &
             data$date <= selected_date()[[2]] &
             data$continent %in% selected_continents() &
             data$location %in% selected_countries(), ]
    } else if (length(selected_other_location()) != length(unique(data[data$continent == "", ]$location))) {
      data[data$date >= selected_date()[[1]] &
             data$date <= selected_date()[[2]] &
             data$location %in% selected_other_location(), ]
    }
  })
  
  output$explore_data_output <- renderDataTable({
    explore_data_data()
  },
  options = list(
    pageLength = 10,
    lengthMenu = list(c(10, 25, 50, 100, 500), c('10', '25', '50', '100', '500')),
    searching = FALSE,
    scrollX = TRUE
  ))
}

shinyApp(ui = ui, server = server)