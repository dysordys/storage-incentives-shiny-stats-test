# Ideas for stats dashboard
#
# What is the distribution of rewards?
#  * distribution of rewards (total)
#  * distribution of rewards across neighborhoods (how evenly distributed?)
#  * profitability by neighborhood (how evenly distributed?)
#
# What is the distribution of number of overlays (nodes) per neighborhood?
#  * distribution of honest revealers per neighbourhood
#  * distribution of playing and staked nodes per neighbourhood
#  * distribution of ‘dishonest’ nodes per neighbourhood
#
# Other
#  * In the last x rounds, what was the average number of revealers by round
#  * Skipped rounds distribution over time
#  * Distribution of reported depth and are there any seeming anomalies
#  * How often are 'honest' nodes misreporting their rc
#  * How many rounds are controversial (have some disagreement in hashes)
#  * Distribution of freezes over time (how many nodes frozen at any given point)
#  * Distribution of freezes over neighborhoods
#
# Staking
#  * Distribution of staked nodes not playing
#  * Distribution of stakes by neighbourhood

library(shiny)
library(tidyverse)
library(rdrop2)

source("download_data.R")
source("clean_data.R")
source("price_model.R")
source("dropbox_comm.R")


loadDataFromDropbox()
dat <- read_rds("data.rds")



ui <- fluidPage(
  verticalLayout(
    selectInput(inputId = "tab", label = "Which data?",
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
        group_by(round) %>%
        mutate(honest = (id == id[event == "won"])) %>%
        filter(event == "revealed") %>%
        summarise(nHonest = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(nHonest, initPrice = input$ip)) %>%
        filter(round %in% reduce(input$roundRange, `:`)) %>%
        ggplot(aes(x = round, y = price)) +
        geom_line(colour = "steelblue") +
        theme_bw(base_size = 16) +
        theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
    } else {
      tibble(round = reduce(input$roundRange, `:`)) %>%
        anti_join(distinct(select(dat, round)), by = "round") %>%
        ggplot(aes(x = round, y = 0)) +
        geom_point(colour = "steelblue", alpha = 0.7) +
        theme_bw(base_size = 16) +
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
    }
  })
  output$outTab <- renderTable({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(round) %>%
        mutate(honest = (id == id[event == "won"])) %>%
        filter(event == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `honest revealers` = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(`honest revealers`, initPrice = input$ip)) %>%
        filter(round %in% reduce(input$roundRange, `:`))
    } else {
      tibble(round = min(dat$round):max(dat$round)) %>%
        anti_join(distinct(select(dat, round)), by = "round") %>%
        filter(round %in% reduce(input$roundRange, `:`))
    }
  })
  observe({
    fetchJsonAll(minRound = max(dat$round)) %>%
      cleanData() %>%
      mergeData(dat) %>%
      saveDataToDropbox()
    loadDataFromDropbox()
    dat <<- read_rds("data.rds")
  }) %>%
    bindEvent(input$downloadData)
}



shinyApp(ui = ui, server = server)
