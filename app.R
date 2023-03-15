# Ideas for stats dashboard
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
          verticalLayout(
            textOutput("outChiSqUnifTxt"),
            plotOutput("outSkippedFig")
          )
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
            radioButtons(inputId = "winNhoodChoice", label = NULL,
                         choices = c("Number of wins across neighbourhoods" = TRUE,
                                     "Distribution of number of win events" = FALSE)),
            plotOutput("outWinNhoodFig")
          )
        ),
        tabPanel(
          title = "Total reward across neighbourhoods",
          verticalLayout(
            plotOutput("outRewardNhoodFig")
          )
        )
      )
    ),
    tabPanel(
      title = "Nodes",
      tabsetPanel(
        tabPanel(
          title = "Nodes per neighbourhood",
          verticalLayout(
            radioButtons(inputId = "nodesFigChoice", label = NULL,
                         choices = c("Number of nodes across neighbourhoods" = TRUE,
                                     "Distribution of number of nodes" = FALSE)),
            plotOutput("outNodesPerNhoodFig")
          )
        ),
        tabPanel(
          title = "Revealers per node",
          verticalLayout(
            radioButtons(inputId = "revealerSortType",
                         label = "Sort neighbourhoods in increasing order of:",
                         choices = c("Honest revealers" = TRUE,
                                     "Inaccurate revealers" = FALSE)),
            plotOutput("outRevealersPerNhoodFig")
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
  output$outChiSqUnifTxt <- renderText(
    dat %>%
      restrictRounds(input$roundRange) %>%
      pull(roundNumber) %>%
      chisqUnif() %>%
      `$`(p.value) %>%
      round(5) %>%
      str_c("Chi-squared test of uniformity: p = ", ., " ", case_when(
        . < 0.01 ~ "(i.e., skipped rounds are not uniformly distributed)",
        . < 0.05 ~ "(i.e., skipped rounds are unlikely to be uniformly distributed)",
        . < 0.1  ~ str_c("(i.e., skipped rounds may not be uniformly distributed, ",
                         "but it is difficult to say)"),
        TRUE ~ "(i.e., assumption of uniformity cannot be rejected)",
      ))
  )
  output$outSkippedFig <- renderPlot(
    dat %>%
      missedRounds() %>%
      restrictRounds(input$roundRange) %>%
      roundsFig()
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
  output$outWinNhoodFig <- renderPlot(
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$winNhoodChoice) participationNhoodQuantileFig(.) else
        participationNhoodHistFig(.)
      }
  )
  output$outRewardNhoodFig <- renderPlot(
    dat %>%
      restrictRounds(input$roundRange) %>%
      rewardNhoodFig()
  )
  output$outNodesPerNhoodFig <- renderPlot(
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$nodesFigChoice) nodesPerNhoodQuantileFig(.) else
        nodesPerNhoodHistFig(.)
      }
  )
  output$outRevealersPerNhoodFig <- renderPlot(
    dat %>%
      restrictRounds(input$roundRange) %>%
      revealersPerNhoodFig(input$revealerSortType)
  )
}



shinyApp(ui = ui, server = server)

# fetchJsonAll(minRound = max(dat$roundNumber)) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds")) %>%
#   write_rds("data.rds", compress = "xz")

# tictoc::tic(); downloadAllData() %>% write_rds("data.rds",compress="xz"); tictoc::toc()
