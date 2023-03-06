# Ideas for stats dashboard
#
# What is the distribution of rewards?
#  * distribution of rewards (total)
#  * distribution of rewards across neighborhoods (how evenly distributed?)
#  * profitability by neighborhood (how evenly distributed?)
#
# What is the distribution of number of overlays (nodes) per neighborhood?
#  * distribution of honest revealers per neighborhood
#  * distribution of playing and staked nodes per neighborhood
#  * distribution of ‘dishonest’ nodes per neighborhood
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

source("download_clean.R")
source("price_model.R")
source("dropbox_conn.R")
source("display_items.R")


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
      revealerPerRoundFig(dat, initPrice = input$ip, roundRange = input$roundRange)
    } else {
      missedRoundsFig(dat, roundRange = input$roundRange)
    }
  })
  output$outTab <- renderTable({
    if (input$tab == "Revealers") {
      revealerPerRoundTab(dat, initPrice = input$ip, roundRange = input$roundRange)
    } else {
      missedRoundsTab(dat, roundRange = input$roundRange)
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
