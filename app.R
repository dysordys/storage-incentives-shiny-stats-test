library(shiny)
library(tidyverse)


source("download_clean.R") # Functions to download and clean data
source("price_model.R") # Functions for tracking price oracle


dat <- read_rds("data.rds") # Initialize data


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "tab", label = "Which table?",
                  choices = c("Revealers", "Skipped rounds")),
      sliderInput(inputId = "ip", label = "Initial price",
                  min = 2^10, max = 2^13, value = 2^10, step = 1),
      actionButton("downloadData", "Refresh data")
    ),
    mainPanel(
      textOutput("outMessage"),
      plotOutput("outFig"),
      tableOutput("outTab")
    ),
  )
)


server <- function(input, output) {
  output$outTab <- renderTable({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(round) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `number of honest revealers` = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(`number of honest revealers`, initPrice = input$ip))
    } else {
      tibble(round = min(dat$round):max(dat$round)) %>%
        anti_join(distinct(select(dat, round)), by = "round")
    }
  })
  output$outFig <- renderPlot({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(round) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(nHonest = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(nHonest, initPrice = input$ip)) %>%
        ggplot(aes(x = round, y = price)) +
        geom_line(colour = "steelblue") +
        theme_bw(base_size = 16)
    } else {
      NULL
    }
  })
  observe({
    renderText("Downloading data (this may take a while)...")
    fetchJsonAll() %>% cleanData() %>% write_rds("data.rds", compress = "xz")
    dat <- read_rds("data.rds")
    renderText("Downloading and updating finished.")
  }) %>%
    bindEvent(input$downloadData)
}


shinyApp(ui = ui, server = server)
