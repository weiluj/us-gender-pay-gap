#--------------------
# Objective: PPHA 30536 Final Project-Shiny Dashboard
# Date: 29th Nov, 2022
#--------------------

# Clear Global Environment
rm(list = ls())
options(
  scipen = 999,
  digits = 3
)

# Load packages
library(tidyverse)
library(dplyr)
library(scales)
library(ggplot2) # Plot
library(sf)
library(plotly)
library(leaflet)
library(statebins)
library(ggthemes) # Set Theme
library(ggthemr)
library(colorspace)
library(bslib)
library(shiny) # Shiny
library(shinyWidgets)

# User Interface
ui <- navbarPage(
  theme = bs_theme(version = 3, "sandstone"),
  strong("US Gender Pay Gap Dashboard"),
  tabPanel(
    "State Level",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(em(strong("Research Question"))),
        p(h5("Gender wage gap is an issue shared worldwide. In the US, women are only paid 80 cents for 1 dollar paid for men.")),
        p(h5("Research indicates that the wage gap does not narrow down in the US despite increse of women's labor force participation starting late 1990s.")),
        p(h5("This reseaerch project looks at Current Population Survey data from 2000 to 2020 by US Census to Discorve whether the Gender Wage Gap is Still Expanding.")),
        br(),
        sliderInput("year", "Select a Year", value = 2000, min = 2000, max = 2020, step = 1),
        br(),
        submitButton("Refresh")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Yearly",
            fluidRow(
              column(
                width = 8,
                h4(strong("Yearly Sate Level Wage Ratio")),
                leafletOutput("map", height = 620)
              ),
              fluidRow(
                column(
                  width = 4,
                  h4(strong("Highest Pay Gap States")),
                  plotlyOutput("stgap_h", height = 300)
                ),
                column(
                  width = 4,
                  h4(strong("Lowest Pay Gap States")),
                  plotlyOutput("stgap_l", height = 300)
                ),
              )
            )
          ),
          tabPanel(
            "Overall",
            fluidRow(
              column(
                6,
                h4(strong("Gap by State Fertility and Partisanship(2003-2020)")),
                plotlyOutput("ferpar", height = 450)
              ),
              column(
                6,
                h4(strong("Overall Gap by State(2000-2020)")),
                plotOutput("map_al", height = 400)
            )
          )
        )
      )
    )
    )
  ),
  tabPanel(
    "Individual Level",
    fluidRow(
      column(
        6,
        h4(strong("Gender Wage Gap by Race")),
        plotlyOutput("prace", height = 300)
      ),
      column(
        6,
        h4(strong("Gender Wage Gap by Education")),
        plotlyOutput("pedu", height = 300)
      )
    ),
    fluidRow(
      column(
        12,
        h4(strong("Gender Wage Gap by Marital Status and Child")),
        plotlyOutput("pmarchil", height = 330)
      )
    )
  ),
  tabPanel(
    "Time Series",
    fluidRow(
      column(
        width = 12,
        plotlyOutput("ts_mwage", height = 350)
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotlyOutput("ts_gap", height = 350)
      )
    )
  )
)

# Server
server <- function(input, output) {
  df_ts <- read_csv("ts.csv") # Time series wage change data
  df_p <- readRDS("personal.rds") # Personal characteristics data
  df_st <- st_read("sf.shp") # State level data
  df_fp <- read_csv("fertility_partisan.csv") # Fertility and Partisan Correlation Data

  data_fp <- reactive({ # reactive data
    df_fp %>%
      filter(year == input$year)
  })

  data_st <- reactive({ # reactive data
    df_st %>%
      filter(year == input$year)
  })

  # Define plot theme
  theme <-
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      plot.caption = element_text(size = 6, face = "italic", hjust = 0),
      axis.text = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    )
  theme_a <- theme + theme_economist()
  ggthemr("dust")
  # State Level Choropleth Overall
  output$map_al <- renderPlot({
    data_st() %>%
      group_by(state) %>%
      summarise(pct = mean(pct)) %>%
    statebins(value_col = "pct",
              palette = "RdPu",
              name = "Wage Ratio %",
              direction = 1) +
      theme_statebins(legend_position = "top") +
      theme(legend.justification = c(0.3, 0.2),
            legend.key.size = unit(1, "cm"))
  })
  # State Level Choropleth by Year
  output$map <- renderLeaflet({
    palette <- colorBin(c('#fee0d2',
                          '#fcbba1',
                          '#fc9272',
                          '#fb6a4a',
                          '#a50f15',
                          '#67000d'),
                        bins = c(0.57, 0.625, 0.691, 0.757, 0.823, 0.9))
    popup1 <- paste0("<span style='color: #7f0000'><strong>Wage Ratio</strong></span>",
                     "<br><span style='color: salmon;'><strong>state: </strong></span>", 
                     data_st()$NAME, 
                     "<br><span style='color: salmon;'><strong>percentage: </strong></span>",
                     format(data_st()$pct, digits = 3)
    )
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(-98.35, 39.7, 4) %>%
      addPolygons(
        data = data_st(),
        fillColor = ~ palette(data_st()$pct),
        fillOpacity = 0.6,
        weight = 1.5,
        smoothFactor = 0.2,
        stroke = TRUE,
        color = "white",
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        popup = popup1
      ) %>%
      addLegend(position = "bottomleft",
                pal = palette,
                values = range(data_st()$pct),
                opacity = 0.6, 
                title = "Wage Ratio")
  })

  # States with Highest pay Gap Each Year
  output$stgap_h <- renderPlotly({
    plt <- data_st() %>%
      arrange(value) %>%
      head(5) %>%
      ggplot() +
      geom_col(aes(state, value),
        fill = "#9A8A76"
      ) +
      scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                         limits = c(0, 0.876)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text("Wage Ratio"))
    ggplotly(plt)
  })

  # States with Lowest pay Gap Each Year
  output$stgap_l <- renderPlotly({
    plt <- data_st() %>%
      arrange(desc(value)) %>%
      head(5) %>%
      ggplot() +
      geom_col(aes(state, value)) +
      scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                         limits = c(0, 0.876)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text("Wage Ratio"))
    ggplotly(plt)
  })

  # Plotting Correlation of State Fertility Rate and Partisanship
  output$ferpar <- renderPlotly({
    plt <- ggplot(
      data_fp(),
      aes(fertility_rate, pct)
    ) +
      geom_point(aes(color = party_fct), alpha = 0.8) +
      geom_smooth(
        se = F, size = 0.8,
        method = "lm"
      ) +
      scale_color_manual(
        name = "Elected Party",
        values = c("#4e79a7", "#7A6752", "#e15759"),
        labels = c("Dem", "Neutral", "Rep")
      ) +
      labs(
        x = "Fertility Rate",
        y = "Wage Ratio"
      ) +
      theme +
      legend_bottom()
    ggplotly(plt) %>%
      layout(legend = list(
        orientation = "h",
        y = 1.1
      ))
  })
  # Gender Wage Ratio by Education Level
  output$pedu <- renderPlotly({
    df_p[[1]] %>%
      ggplot() +
      geom_line(
        aes(year, pct,
          color = factor(grade92_fct)
        )
      ) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      labs(
        x = "Year",
        y = "Wage Ratio"
      ) +
      theme +
      theme(legend.position = "none")
  })

  # Gender Wage Ratio By race
  output$prace <- renderPlotly({
    df_p[[2]] %>%
      ggplot() +
      geom_line(
        aes(year, pct,
          color = factor(race)
        )
      ) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      labs(
        x = "Year",
        y = "Wage Ratio"
      ) +
      theme +
      theme(legend.position = "none")
  })

  # Gender Wage Ratio by Marital and Children Number
  output$pmarchil <- renderPlotly({
    df_p[[3]] %>%
      ggplot(aes(year, pct,
        color = factor(marital),
        group = interaction(marital, ownchild_fct)
      )) +
      geom_line() +
      geom_point(aes(pch = ownchild_fct)) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      labs(
        x = "Year",
        y = "Wage Ratio"
      ) +
      theme +
      theme(legend.position = "none")
  })

  # Time Series Wage Gap
  output$ts_gap <- renderPlotly({
    df_ts %>%
      ggplot(aes(year, pct)) +
      geom_line(size = 1) +
      labs(
        title = "Women's-to-Men's Earning Ratio",
        x = "Year",
        y = "Wage Ratio"
      ) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      scale_y_continuous(
        breaks = seq(0.65, 0.8, 0.05),
        limits = c(0.65, 0.82)
      ) +
      theme_a
  })

  # Time Series Median Wage
  output$ts_mwage <- renderPlotly({
    df_ts %>%
      ggplot(aes(
        x = year, y = median_wage,
        color = sex
      )) +
      geom_line(size = 1) +
      scale_y_continuous(labels = label_dollar()) +
      labs(
        title = "Median Wages Trend 2000 to 2020",
        x = "Year",
        y = "Median Wage",
      ) +
      scale_x_continuous(breaks = seq(2000, 2020, 2)) +
      scale_y_continuous(
        breaks = seq(600, 1200, 200),
        limits = c(500, 1200)
      ) +
      theme_a +
      scale_color_economist()
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)