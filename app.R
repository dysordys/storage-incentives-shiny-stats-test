library(shiny)
library(tidyverse)

source("statistics.R")
source("figures.R")


dat <- read_rds("data.rds") %>% calculateNhoodsDec()



ui <- fluidPage(
  navbarPage(
    title = "View data on:",
    sliderInput(inputId = "roundRange",
                label = "Range of rounds",
                min = min(dat$roundNumber),
                max = max(dat$roundNumber),
                value = range(dat$roundNumber),
                width = "90%"),
    tabPanel(
      title = "Price",
      tabsetPanel(
        tabPanel(
          title = "Graph of price change",
          verticalLayout(plotOutput("outPriceFig"))
        ),
        tabPanel(
          title = "Table of price change and revealers",
          verticalLayout(
            radioButtons(inputId = "inaccFilt", label = NULL,
                         choices = c("Show all rounds",
                                     "Show only rounds with inaccurate revealers")),
            textOutput("outInacc"),
            tableOutput("outPriceTab")
          )
        ),
        tabPanel(
          title = "Revealers per neighbourhood",
          verticalLayout(
            splitLayout(
              radioButtons(inputId = "revealerSortType",
                           label = "Sort neighbourhoods in increasing order of:",
                           choices = c("Honest revealers" = TRUE,
                                       "Inaccurate revealers" = FALSE)),
              selectInput(inputId = "revealerNhoodDepth", label = "Depth:",
                          choices = depthDistr(dat) %>% filter(depth>0) %>% pull(depth),
                          selected = 8)
            ),
            plotOutput("outRevealersPerNhoodFig")
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
            textOutput("outChiSqUnifTxt1"),
            plotOutput("outSkippedFig")
          )
        ),
        tabPanel(
          title = "Table",
          verticalLayout(
            textOutput("outChiSqUnifTxt2"),
            tableOutput("outSkippedTab")
          )
        )
      )
    ),
    tabPanel(
      title = "Rewards",
      tabsetPanel(
        tabPanel(
          title = "Reward distribution",
          verticalLayout(
            splitLayout(
              radioButtons(inputId = "rewardFigLogX",
                           label = "x-axis:",
                           selected = TRUE,
                           choices = c("Linear" = FALSE,
                                       "Logarithmic" = TRUE)),
              radioButtons(inputId = "rewardFigLogY",
                           label = "y-axis:",
                           selected = TRUE,
                           choices = c("Linear" = FALSE,
                                       "Pseudo-logarithmic" = TRUE))
            ),
            plotOutput("outRewardFig"),
            sliderInput(inputId = "rewardRange",
                        label = "Range of rewards",
                        min = 0.9 * min(dat$rewardAmount, na.rm = TRUE),
                        max = 1.1 * max(dat$rewardAmount, na.rm = TRUE),
                        value = c(0.9 * min(dat$rewardAmount, na.rm = TRUE),
                                  1.1 * max(dat$rewardAmount, na.rm = TRUE)),
                        ticks = FALSE,
                        width = "90%")
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
          title = "Total reward",
          verticalLayout(
            radioButtons(inputId = "totalRewardChoice", label = NULL,
                         choices = c("Total reward across neighbourhoods" = TRUE,
                                     "Total reward across nodes" = FALSE)),
            plotOutput("outRewardAcrossFig")
          )
        )
      )
    ),
    tabPanel(
      title = "Nodes",
      tabsetPanel(
        tabPanel(
          title = "Depths",
          navbarPage(
            title = "",
            tabPanel(
              title = "Table",
              tableOutput("outDepthTab")
            ),
            tabPanel(
              title = "Figure",
              verticalLayout(
                radioButtons(inputId = "depthFigLogY", label = NULL, selected = TRUE,
                             choices = c("Linear y-axis" = FALSE,
                                         "Logarithmic y-axis" = TRUE)),
                plotOutput("outDepthFig")
              )
            )
          )
        ),
        tabPanel(
          title = "Nodes per neighbourhood",
          verticalLayout(
            radioButtons(inputId = "nodesFigChoice", label = NULL,
                         choices = c("Number of nodes across neighbourhoods" = TRUE,
                                     "Distribution of number of nodes" = FALSE)),
            plotOutput("outNodesPerNhoodFig")
          )
        )#,
        # tabPanel(
        #   title = "Revealers per node",
        #   verticalLayout(
        #     radioButtons(inputId = "revealerNodeSortType",
        #                  label = "Sort neighbourhoods in increasing order of:",
        #                  choices = c("Honest revealers" = TRUE,
        #                              "Inaccurate revealers" = FALSE)),
        #     plotOutput("outRevealersPerNodeFig")
        #   )
        # )
      )
    ),
    tabPanel(
      title = "Stakes",
      verticalLayout(
        radioButtons(inputId = "stakesFigChoice", label = NULL,
                     choices = c("Sum of stakes across neighbourhoods" = TRUE,
                                 "Distribution of sum of stakes" = FALSE)),
        plotOutput("outStakesNhoodFig")
      )
    )
  )
)



server <- function(input, output) {
  output$outPriceFig <- renderPlot({
    dat %>%
      pricePerRound() %>%
      mutate(inaccurate = revealers - honest) %>%
      mutate(price = price / first(price)) %>%
      restrictRounds(input$roundRange) %>%
      priceFig()
  })
  output$outInacc <- renderText({
    s <- dat %>%
      restrictRounds(input$roundRange) %>%
      inaccurateRevealerStats()
    str_c("Rounds with inaccurate revealers: ", s$n, " out of ", s$rounds,
          ", or ", round(100 * s$p, 4), "%")
  })
  output$outPriceTab <- renderTable({
    dat %>%
      pricePerRound() %>%
      mutate(inaccurate = revealers - honest, price = price / first(price)) %>%
      { if (input$inaccFilt == "Show all rounds") . else filter(., inaccurate > 0) } %>%
      restrictRounds(input$roundRange) %>%
      transmute(round = roundNumber,
                `price (in units of the initial value)` = price,
                `number of revealers` = revealers,
                `inaccurate revealers` = inaccurate)
  })
  output$outRevealersPerNhoodFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      filter(depth == input$revealerNhoodDepth) %>%
      filter(!(roundNumber %in% roundsWithoutWinner(.))) %>%
      revealerNhoodSummary(.f = mean) %>%
      revealersPerNhoodFig(input$revealerSortType)
  })
  output$outChiSqUnifTxt1 <- renderText({
    dat %>%
      restrictRounds(input$roundRange) %>%
      chisqUnifMissedRounds()
  })
  output$outChiSqUnifTxt2 <- renderText({
    dat %>%
      restrictRounds(input$roundRange) %>%
      chisqUnifMissedRounds()
  })
  output$outSkippedFig <- renderPlot({
    dat %>%
      missedRounds() %>%
      restrictRounds(input$roundRange) %>%
      roundsFig()
  })
  output$outSkippedTab <- renderTable({
    missedRounds(dat) %>%
      restrictRounds(input$roundRange) %>%
      rename(`Skipped rounds:` = roundNumber)
  })
  output$outRewardFig <- renderPlot({
    dat %>%
      skippedRounds() %>%
      mutate(skip = as_factor(skip)) %>%
      restrictRounds(input$roundRange) %>%
      rewardDistrFig(xrange = input$rewardRange,
                     log.x = input$rewardFigLogX, log.y = input$rewardFigLogY)
  })
  output$outWinNhoodFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$winNhoodChoice) participationNhoodQuantileFig(winsByNhood(.)) else
        participationNhoodHistFig(winEventsTab(.))
      }
  })
  output$outRewardAcrossFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$totalRewardChoice) rewardNhoodFig(rewardNhoodDistr(.)) else
        rewardPerNodeFig(rewardPerNode(.))
      }
  })
  output$outDepthTab <- renderTable({
    dat %>%
      restrictRounds(input$roundRange) %>%
      depthDistr() %>%
      rename(`number of nodes` = n)
  })
  output$outDepthFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      depthDistr() %>%
      depthDistrFig(log.y = input$depthFigLogY)
  })
  output$outNodesPerNhoodFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$nodesFigChoice) nodesPerNhoodQuantileFig(nodesByNhoodRank(.)) else
        nodesPerNhoodHistFig(nodesByNhood(.))
      }
  })
  # output$outRevealersPerNodeFig <- renderPlot({
  #   dat %>%
  #     restrictRounds(input$roundRange) %>%
  #     revealerNhoodSummary() %>%
  #     revealersPerNhoodFig(input$revealerNodeSortType)
  # })
  output$outStakesNhoodFig <- renderPlot({
    dat %>%
      restrictRounds(input$roundRange) %>%
      { if (input$stakesFigChoice) stakesNhoodQuantileFig(rewardNhoodDistr(.)) else
        stakesNhoodHistFig(rewardNhoodDistr(.))
      }
  })
}



shinyApp(ui = ui, server = server)

# source("download_clean.R")
# fetchJsonAll(minRound = max(dat$roundNumber)) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds")) %>%
#   write_rds("data.rds", compress = "xz")

# tictoc::tic(); write_rds(downloadAllData(), "data.rds", compress="xz"); tictoc::toc()
