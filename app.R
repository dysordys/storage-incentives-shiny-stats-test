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
#library(rdrop2)

source("download_clean.R")
#source("dropbox_conn.R")
source("figures.R")


#token <- drop_auth()
#loadDataFromDropbox(dtoken = token)
dat <- read_rds("data.rds")



ui <- fluidPage(
  navbarPage(
    title = "View data on:",
    sliderInput(inputId = "roundRange",
                label = "Range of rounds",
                min = min(dat$round), max = max(dat$round),
                value = range(dat$round), width = "80%"),
    tabPanel(
      title = "Price",
      tabsetPanel(
        tabPanel(
          title = "Figure",
          verticalLayout(plotOutput("outPriceFig"))
        ),
        tabPanel(
          title = "Table",
          verticalLayout(
            radioButtons(inputId = "dishonestFilter", label = NULL,
                         choices = c("Show all rounds",
                                     "Show only rounds with inaccurate revealers")),
            tableOutput("outPriceTab")
          )
        )
      )
    ),
    tabPanel(
      title = "Skipped rounds",
      tabsetPanel(
        tabPanel(
          title = "Figure",
          verticalLayout(plotOutput("outSkippedFig"))
        ),
        tabPanel(
          title = "Table",
          verticalLayout(tableOutput("outSkippedTab"))
        )
      )
    ),
    tabPanel(
      title = "Reward amount",
      verticalLayout(
        radioButtons(inputId = "rewardFigLogY", label = NULL, selected = TRUE,
                     choices = c("Linear y-axis" = FALSE,
                                 "Pseudo-logarithmic y-axis" = TRUE)),
        plotOutput("outRewardFig")
      )
    )
  )
)



server <- function(input, output) {
  output$outPriceFig <- renderPlot(
    priceFig(dat, roundRange = input$roundRange)
  )
  output$outPriceTab <- renderTable(
    pricePerRound(dat) %>%
      restrictRounds(input$roundRange) %>%
      { if (input$dishonestFilter == "Show all rounds") . else
        filter(., `inaccurate revealers` > 0)
      }
  )
  output$outSkippedFig <- renderPlot(
    missedRoundsFig(dat, roundRange = input$roundRange)
  )
  output$outSkippedTab <- renderTable(
    missedRounds(dat) %>%
      restrictRounds(input$roundRange) %>%
      rename(`Skipped rounds:` = round)
  )
  output$outRewardFig <- renderPlot(
    rewardDistrFig(dat, log.y = input$rewardFigLogY, roundRange = input$roundRange)
  )
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
