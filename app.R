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
#  * distribution of 'dishonest' nodes per neighborhood
#
# Staking
#  * Distribution of staked nodes not playing
#  * Distribution of stakes by neighbourhood
#
# Other
#  * In the last x rounds, what was the average number of revealers by round
#  * Skipped rounds distribution over time
#  * Distribution of reported depth and are there any seeming anomalies
#  * How often are 'honest' nodes misreporting their rc
#  * How many rounds are controversial (have some disagreement in hashes)
#  * Distribution of freezes over time (how many nodes frozen at any given point)
#  * Distribution of freezes over neighborhoods
#  * Distribution of anchors

library(shiny)
library(tidyverse)
library(rdrop2)

source("download_clean.R")
#source("dropbox_conn.R")
source("display_items.R")


#token <- drop_auth()
#loadDataFromDropbox(dtoken = token)
dat <- read_rds("data.rds")



ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      title = "Graph",
      verticalLayout(
        selectInput(inputId = "tabGraph", label = "Which data?",
                    choices = c("Revealers", "Skipped rounds")),
        sliderInput(inputId = "roundRangeGraph", label = "Range of rounds",
                    min = min(dat$round), max = max(dat$round),
                    value = range(dat$round), width = "80%"),
        #actionButton(inputId = "downloadData", label = "Refresh data"),
        plotOutput("outFig"),
      )
    ),
    tabPanel(
      title = "Table",
      verticalLayout(
        selectInput(inputId = "tabTab", label = "Which data?",
                    choices = c("Revealers", "Skipped rounds")),
        sliderInput(inputId = "roundRangeTab", label = "Range of rounds",
                    min = min(dat$round), max = max(dat$round),
                    value = range(dat$round), width = "80%"),
        tableOutput("outTab")
      )
    )
  )
)



server <- function(input, output) {
  output$outFig <- renderPlot({
    if (input$tabGraph == "Revealers") {
      revealerPerRoundFig(dat, roundRange = input$roundRangeGraph)
    } else {
      missedRoundsFig(dat, roundRange = input$roundRangeGraph)
    }
  })
  output$outTab <- renderTable({
    if (input$tabTab == "Revealers") {
      revealersPerRound(dat, roundRange = input$roundRangeTab)
    } else {
      missedRounds(dat, roundRange = input$roundRangeTab)
    }
  })
  # observe({
  #   fetchJsonAll(minRound = max(dat$round)) %>%
  #     cleanData() %>%
  #     mergeData(dat) %>%
  #     write_rds("data.rds")
  #     #saveDataToDropbox(dtoken = token)
  #   #loadDataFromDropbox(dtoken = token)
  #   dat <<- read_rds("data.rds")
  # }) %>%
  #   bindEvent(input$downloadData)
}



shinyApp(ui = ui, server = server)
