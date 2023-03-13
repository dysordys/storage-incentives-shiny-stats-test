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
#  * Distribution of stakes by neighborhood
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

source("download_clean.R")
source("statistics.R")


dat <- read_rds("data.rds") %>% calculateNhoodsDec()



ui <- fluidPage(
  navbarPage(
    title = "View data on:",
    sliderInput(inputId = "roundRange",
                label = "Range of rounds",
                min = min(dat$roundNumber), max = max(dat$roundNumber),
                value = range(dat$roundNumber), width = "80%"),
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
      title = "Rewards",
      tabsetPanel(
        tabPanel(
          title = "Reward distribution",
          verticalLayout(
            radioButtons(inputId = "rewardFigLogY", label = NULL, selected = TRUE,
                         choices = c("Linear y-axis" = FALSE,
                                     "Pseudo-logarithmic y-axis" = TRUE)),
            plotOutput("outRewardFig")
          )
        ),
        tabPanel(
          title = "Wins across neighbourhoods",
          verticalLayout(
            plotOutput("outWinNhoodHistFig"),
            plotOutput("outWinNhoodQuantileFig")
          )
        ),
        tabPanel(
          title = "Total reward across neighbourhoods",
          verticalLayout(
            plotOutput("outRewardNhoodFig")
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  output$outPriceFig <- renderPlot(
    dat %>%
      pricePerRound() %>%
      restrictRounds(input$roundRange) %>%
      priceFig()
  )
  output$outPriceTab <- renderTable(
    pricePerRound(dat) %>%
      restrictRounds(input$roundRange) %>%
      { if (input$dishonestFilter == "Show all rounds") . else
        filter(., `inaccurate revealers` > 0)
      } %>%
      rename(round = roundNumber)
  )
  output$outSkippedFig <- renderPlot(
    dat %>%
      missedRounds() %>%
      restrictRounds(input$roundRange) %>%
      roundsFig(title = pull(., roundNumber) %>%
                  chisqUnif() %>%
                  `$`(p.value) %>%
                  round(5) %>%
                  str_c("p = ", ., " (Chi-squared uniformity test)"))
  )
  output$outSkippedTab <- renderTable(
    missedRounds(dat) %>%
      restrictRounds(input$roundRange) %>%
      rename(`Skipped rounds:` = roundNumber)
  )
  output$outRewardFig <- renderPlot(
    dat %>%
      skippedRounds() %>%
      mutate(skip = as_factor(skip)) %>%
      restrictRounds(input$roundRange) %>%
      rewardDistrFig(log.y = input$rewardFigLogY)
  )
  output$outWinNhoodHistFig <- renderPlot(
    participationNhoodHistFig(dat)
  )
  output$outWinNhoodQuantileFig <- renderPlot(
    participationNhoodQuantileFig(dat)
  )
  output$outRewardNhoodFig <- renderPlot(
    rewardNhoodFig(dat)
  )
}



shinyApp(ui = ui, server = server)

# fetchJsonAll(minRound = max(dat$roundNumber)) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds")) %>%
#   write_rds("data.rds", compress = "xz")

# tictoc::tic()
# downloadAllData() %>% write_rds("data.rds", compress = "xz")
# tictoc::toc()
