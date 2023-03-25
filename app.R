library(shiny)
library(tidyverse)

source("statistics.R")
source("figures.R")
source("serverfunc.R")
source("uifunc.R")


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
                                     "Only show rounds with inaccurate revealers")),
            textOutput("outInacc"),
            tableOutput("outPriceTab")
          )
        ),
        tabPanel(
          title = "Revealers per neighbourhood",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "revealerNhoodDepth",
                                            filter(depthDistr(dat), depth > 0)$depth)),
              column(width = 6, heightSlider(inputId = "revealerNhoodFigHeight")),
              column(width = 4, radioButtons(inputId = "revealerSortType",
                                             label = "Sort neighbourhoods by:",
                                             choices = c("Honest revealers",
                                                         "Inaccurate revealers",
                                                         "Numerical order")))
            ),
            plotOutput("outRevealersPerNhoodFig")
          )
        )
      )
    ),
    tabPanel(
      title = "Skipped rounds",
      verticalLayout(
        textOutput("outNumSkipped"),
        textOutput("outChiSqUnifTxt"),
        plotOutput("outSkippedFig"),
        tableOutput("outSkippedTab")
      )
    ),
    tabPanel(
      title = "Rewards",
      tabsetPanel(
        tabPanel(
          title = "Reward distribution",
          verticalLayout(
            splitLayout(
              radioButtons(inputId = "rewardFigLogX", label = "x-axis:", inline = TRUE,
                           selected = "Logarithmic",
                           choices = c("Linear", "Logarithmic")),
              radioButtons(inputId = "rewardFigLogY", label = "y-axis:", inline = TRUE,
                           selected = "Pseudo-logarithmic",
                           choices = c("Linear", "Pseudo-logarithmic"))
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
          title = "Wins across nhoods",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "depthWins",
                                            filter(depthDistr(dat), depth > 0)$depth)),
              column(width = 6, heightSlider(inputId = "winNhoodFigHeight"))
            ),
            plotOutput("outWinNhoodFig")
          )
        ),
        tabPanel(
          title = "Distribution of wins",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "depthWD",
                                            filter(depthDistr(dat), depth > 0)$depth))
            ),
            plotOutput("outWinDistrFig")
          )
        ),
        tabPanel(
          title = "Total reward (nhoods)",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "depthTR",
                                            filter(depthDistr(dat), depth > 0)$depth)),
              column(width = 6, heightSlider(inputId = "rewardNhoodFigHeight"))
            ),
            plotOutput("outRewardNhoodFig")
          )
        ),
        tabPanel(
          title = "Total reward (nodes)",
          verticalLayout(
            fluidRow(
              column(width = 6, heightSlider(inputId = "rewardNodeFigHeight", min = 400))
            ),
            plotOutput("outRewardNodeFig")
          )
        )
      )
    ),
    tabPanel(
      title = "Nodes",
      tabsetPanel(
        tabPanel(
          title = "Depths",
          fluidRow(
            column(width = 4, tableOutput("outDepthTab")),
            column(width = 8, verticalLayout(
              radioButtons(inputId = "depthLogY", label = NULL,
                           selected = "Logarithmic y-axis", inline = TRUE,
                           choices = c("Linear y-axis", "Logarithmic y-axis")),
              plotOutput("outDepthFig")
            ))
          )
        ),
        tabPanel(
          title = "Nodes per neighbourhood",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "depthNodes",
                                            filter(depthDistr(dat), depth > 0)$depth)),
              column(width = 6, heightSlider(inputId = "nodeFigHeight"))
            ),
            plotOutput("outNodesPerNhoodFig")
          )
        ),
        tabPanel(
          title = "Distribution of nodes",
          verticalLayout(
            fluidRow(
              column(width = 2, depthSelect(inputId = "depthNodes2",
                                            filter(depthDistr(dat), depth > 0)$depth))
            ),
            plotOutput("outNodeDistrFig")
          )
        )
      )
    ),
    tabPanel(
      title = "Stakes",
      tabsetPanel(
        tabPanel(
          title = "Sum of stakes across neighbourhoods",
          fluidRow(
            column(width = 2, depthSelect(inputId = "depthStakes",
                                          filter(depthDistr(dat), depth > 0)$depth)),
            column(width = 6, heightSlider(inputId = "stakeFigHeight"))
          ),
          plotOutput("outStakesNhoodFig")
        ),
        tabPanel(
          title = "Distribution of sum of stakes",
          fluidRow(
            column(width = 2, depthSelect(inputId = "depthStakes2",
                                          filter(depthDistr(dat), depth > 0)$depth))
          ),
          plotOutput("outStakesNodeFig")
        )
      )
    )
  )
)



server <- function(input, output) {
  output$outPriceFig <- renderPlot(outPriceFig(dat, input$roundRange))
  output$outInacc <- renderText(outInaccurate(dat, input$roundRange))
  output$outPriceTab <- renderTable(outPriceTab(dat, input$roundRange, input$inaccFilt),
                                    na = "")
  output$outRevealersPerNhoodFig <- renderPlot(outRevealersPerNhoodFig(
    dat, input$roundRange, input$revealerNhoodDepth, input$revealerSortType),
    height = reactive(input$revealerNhoodFigHeight))
  output$outNumSkipped <- renderText(outNumSkipped(dat, input$roundRange))
  output$outChiSqUnifTxt <- renderText(outChiSqUnifTxt(dat, input$roundRange))
  output$outSkippedFig <- renderPlot(outSkippedFig(dat, input$roundRange))
  output$outSkippedTab <- renderTable(outSkippedTab(dat, input$roundRange))
  output$outRewardFig <- renderPlot(outRewardFig(
    dat, input$roundRange, input$rewardRange, input$rewardFigLogX, input$rewardFigLogY))
  output$outWinNhoodFig <- renderPlot(outWinNhoodFig(
    dat, input$roundRange, input$depthWins),
    height = reactive(input$winNhoodFigHeight))
  output$outWinDistrFig <- renderPlot(outWinDistrFig(
    dat, input$roundRange, input$depthWD))
  output$outRewardNhoodFig <- renderPlot(outRewardNhoodFig(
    dat, input$roundRange, input$depthTR),
    height = reactive(input$rewardNhoodFigHeight))
  output$outRewardNodeFig <- renderPlot(outRewardNodeFig(dat, input$roundRange),
                                        height = reactive(input$rewardNodeFigHeight))
  output$outDepthTab <- renderTable(outDepthTab(dat, input$roundRange))
  output$outDepthFig <- renderPlot(outDepthFig(dat, input$roundRange, input$depthLogY))
  output$outNodesPerNhoodFig <- renderPlot(outNodesPerNhoodFig(
    dat, input$roundRange, input$depthNodes),
    height = reactive(input$nodeFigHeight))
  output$outNodeDistrFig <- renderPlot(outNodeDistrFig(
    dat, input$roundRange, input$depthNodes2))
  output$outStakesNhoodFig <- renderPlot(outStakesNhoodFig(
    dat, input$roundRange, input$depthStakes),
    height = reactive(input$stakeFigHeight))
  output$outStakesNodeFig <- renderPlot(outStakesNodeFig(
    dat, input$roundRange, input$depthStakes2))
}



shinyApp(ui = ui, server = server)

# source("download_clean.R")
# fetchJsonAll(minRound = max(read_rds("data.rds")$roundNumber)) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds")) %>%
#   write_rds("data.rds", compress = "xz")

# tictoc::tic(); write_rds(downloadAllData(), "data.rds", compress="xz"); tictoc::toc()
