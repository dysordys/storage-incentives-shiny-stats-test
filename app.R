library(shiny)
library(tidyverse)

source("download_data.R") # Functions to download data
source("clean_data.R") # Functions to clean up the data
source("price_model.R") # Functions for tracking price oracle

dat <- read_rds("data.rds") # Initialize data



ui <- fluidPage(
  verticalLayout(
    selectInput(inputId = "tab", label = "Which table?",
                choices = c("Revealers", "Skipped rounds")),
    sliderInput(inputId = "ip", label = "Initial price", min = 2^10,
                max = 2^13, value = 2^10, step = 1, width = "80%"),
    sliderInput(inputId = "roundRange", label = "Rounds to consider",
                min = min(dat$round), max = max(dat$round),
                value = range(dat$round), width = "80%"),
    actionButton(inputId = "downloadData", label = "Refresh data"),
    plotOutput("outFig"),
    tableOutput("outTab")
  )
)



server <- function(input, output) {
  output$outFig <- renderPlot({
    if (input$tab == "Revealers") {
      dat %>%
        filter(round %in% reduce(input$roundRange, `:`)) %>%
        group_by(round) %>%
        mutate(honest = (id == id[event == "won"])) %>%
        filter(event == "revealed") %>%
        summarise(nHonest = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(nHonest, initPrice = input$ip)) %>%
        ggplot(aes(x = round, y = price)) +
        geom_line(colour = "steelblue") +
        theme_bw(base_size = 16)
    } else {
      tibble(round = reduce(input$roundRange, `:`)) %>%
        anti_join(distinct(select(dat, round)), by = "round") %>%
        ggplot(aes(x = round, y = 0)) +
        geom_point(colour = "steelblue", alpha = 0.7) +
        theme_bw(base_size = 16) +
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
    }
  })
  output$outTab <- renderTable({
    if (input$tab == "Revealers") {
      dat %>%
        filter(round %in% reduce(input$roundRange, `:`)) %>%
        group_by(round) %>%
        mutate(honest = (id == id[event == "won"])) %>%
        filter(event == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `honest revealers` = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(`honest revealers`, initPrice = input$ip))
    } else {
      tibble(round = min(dat$round):max(dat$round)) %>%
        anti_join(distinct(select(dat, round)), by = "round") %>%
        filter(round %in% reduce(input$roundRange, `:`))
    }
  })
  observe({
    dat <<-
      fetchJsonAll(minRound = max(dat$round)) %>%
      cleanData() %>%
      mergeData(dat) %>%
      write_rds("data.rds", compress = "xz")
  }) %>%
    bindEvent(input$downloadData)
}



shinyApp(ui = ui, server = server)
