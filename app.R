library(shiny)
library(tidyverse)

source("utils.R")
source("figures.R")
source("serverfunc.R")
source("uifunc.R")


dataOrig <- read_rds("data.rds")


ui <- fluidPage(
  titlePanel("Redistribution game dashboard"),
  navbarPage(
    title = "View data on:",
    roundsSlider(inputId = "roundRange",
                 min = min(dataOrig$roundNumber), max = max(dataOrig$roundNumber),
                 # 3300 rounds is approximately 30 days:
                 value = c(max(dataOrig$roundNumber) - 3300, max(dataOrig$roundNumber))),
    tabPanel(title = "Revealers", revealerTabset(depths(dataOrig))),
    tabPanel(title = "Skipped rounds", skippedRoundsTabset()),
    tabPanel(title = "Rewards", rewardTabset(depths(dataOrig), rewardRange(dataOrig))),
    tabPanel(title = "Nodes", nodeTabset(depths(dataOrig))),
    tabPanel(title = "Stakes", stakeTabset(depths(dataOrig)))
  )
)


server <- function(input, output) {

  dat <- reactive(restrictRounds(dataOrig, input$roundRange))


  # Elements of tabset "Reveal":
  output$outRevealersPerNhoodFig <- renderPlot(
    outRevealersPerNhoodFig(dat(), input$roundRange, input$depthSelRevealersPerNhood,
                            .f = mean, input$revealerSortType,
                            input$nhoodSelRevealersPerNhood),
    width = reactive(input$revealerNhoodFigWidth), height = 500
  )
  output$revealerNhoodDepth <- renderUI(
    depthSelect(inputId = "depthSelRevealersPerNhood", depths = depths(dat()))
  )
  output$revealerNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelRevealersPerNhood",
                nhoods = nhoodList(dat(), as.integer(input$depthSelRevealersPerNhood)))
  )

  output$outRevealersPerNhoodFig2 <- renderPlot(
    outRevealersPerNhoodFig(dat(), input$roundRange, input$depthSelRevealersPerNhood2,
                            .f = sum, input$revealerSortType2,
                            input$nhoodSelRevealersPerNhood2),
    width = reactive(input$revealerNhoodFigWidth2), height = 500
  )
  output$revealerNhoodDepth2 <- renderUI(
    depthSelect(inputId = "depthSelRevealersPerNhood2", depths = depths(dat()))
  )
  output$revealerNhoodSelect2 <- renderUI(
    nhoodSelect(inputId = "nhoodSelRevealersPerNhood2",
                nhoods = nhoodList(dat(), as.integer(input$depthSelRevealersPerNhood2)))
  )

  output$outInacc <- renderText(outInaccurate(dat(), input$roundRange))

  output$outPriceFig <- renderPlot(outPriceFig(dat(), input$roundRange))

  output$outRevealCommitTab <- renderTable(
    outRevealCommitTab(dat(), input$roundRange, input$inaccFilt), na = ""
  )


  # Elements of tabset "Skipped rounds":
  output$outNumSkipped <- renderText(
    outNumSkipped(dat(), input$roundRange)
  )

  output$outSkippedFig <- renderPlot(
    outSkippedFig(dat(), input$roundRange)
  )

  output$outSkippedTab <- renderTable(
    outSkippedTab(dat(), input$roundRange)
  )

  output$outSkipDistrTab <- renderTable(
    outSkippedRoundDistrTab(dat(), input$roundRange)
  )

  output$outSkipDistrFig <- renderPlot(
    outSkippedRoundDistrFig(dat(), input$roundRange)
  )


  # Elements of tabset "Rewards":
  output$outRewardFig <- renderPlot(
    outRewardFig(dat(), input$roundRange, input$rewardRange, input$rewardFigLogX,
                 input$rewardFigLogY)
  )

  output$outWinNhoodFig <- renderPlot(
    outWinNhoodFig(dat(), input$roundRange, input$depthSelWinNhood, input$nhoodSelWinNhood),
    width = reactive(input$winNhoodFigWidth), height = 500
  )
  output$depthWins <- renderUI(
    depthSelect(inputId = "depthSelWinNhood", depths = depthsWinDistr(dat()))
  )
  output$winNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelWinNhood",
                nhoods = nhoodList(filter(dat(), event == "won"),
                                   as.integer(input$depthSelWinNhood)))
  )

  output$outWinDistrFig <- renderPlot(
    outWinDistrFig(dat(), input$roundRange, input$depthSelWinDistr)
  )
  output$depthWD <- renderUI(
    depthSelect(inputId = "depthSelWinDistr", depths = depthsWinDistr(dat()))
  )

  output$outRewardNhoodFig <- renderPlot(
    outRewardNhoodFig(dat(), input$roundRange, input$depthSelTotalReward,
                      input$nhoodSelTotalReward),
    width = reactive(input$rewardNhoodFigWidth), height = 500
  )
  output$depthTR <- renderUI(
    depthSelect(inputId = "depthSelTotalReward", depths = depthsWinDistr(dat()))
  )
  output$rewardNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelTotalReward",
                nhoods = nhoodList(filter(dat(), event == "won"),
                                   as.integer(input$depthSelTotalReward)))
  )

  output$outRewardNodeFig <- renderPlot(
    outRewardNodeFig(dat(), input$roundRange),
    width = reactive(input$rewardNodeFigWidth), height = 450
  )


  # Elements of tabset "Nodes":
  output$outDepthTab <- renderTable(
    outDepthTab(dat(), input$roundRange)
  )

  output$outDepthFig <- renderPlot(
    outDepthFig(dat(), input$roundRange, input$depthLogY)
  )

  output$outNodesPerNhoodFig <- renderPlot(
    outNodesPerNhoodFig(dat(), input$roundRange, input$depthSelNodesPerNhood,
                        input$nhoodSelNodesPerNhood),
    width = reactive(input$nodeFigWidth), height = 500
  )
  output$depthNodesPerNhood <- renderUI(
    depthSelect(inputId = "depthSelNodesPerNhood", depths = depths(dat()))
  )
  output$nodesPerNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelNodesPerNhood",
                nhoods = nhoodList(dat(), as.integer(input$depthSelNodesPerNhood)))
  )

  output$outWinsNodesPerNhoodFig <- renderPlot(
    outWinNodeNhoodFig(dat(), input$roundRange, input$depthSelWinNode,
                       input$sortWinNode, input$nhoodSelWinNode),
    width = reactive(input$winNodeFigWidth), height = 500
  )
  output$depthWinsNodes <- renderUI(
    depthSelect(inputId = "depthSelWinNode", depths = depths(dat()))
  )
  output$winNodeNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelWinNode",
                nhoods = nhoodList(dat(), as.integer(input$depthSelWinNode)))
  )

  output$outNodeDistrFig <- renderPlot(
    outNodeDistrFig(dat(), input$roundRange, input$depthSelNodesDistr)
  )
  output$depthNodesDistr <- renderUI(
    depthSelect(inputId = "depthSelNodesDistr", depths = depths(dat()))
  )


  # Elements of tabset "Stakes":
  output$outStakesNhoodFig <- renderPlot(
    outStakesNhoodFig(dat(), input$roundRange, input$depthSelStake, input$nhoodSelStake),
    width = reactive(input$stakeFigWidth), height = 500
  )
  output$depthStakes <- renderUI(
    depthSelect(inputId = "depthSelStake", depths = depthsWinDistr(dat()))
  )
  output$stakeNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelStake",
                nhoods = nhoodList(filter(dat(), event == "won"),
                                   as.integer(input$depthSelStake)))
  )

  output$outStakesNodeFig <- renderPlot(
    outStakesNodeFig(dat(), input$roundRange, input$depthSelStakeDistr)
  )
  output$depthStakeDistr <- renderUI(
    depthSelect(inputId = "depthSelStakeDistr", depths = depths(dat()))
  )

  output$outStakedNodesFig <- renderPlot(
    outStakedNodesFig(dat(), input$roundRange, input$depthSelStakedNodes,
                      input$nhoodSelStakedNodes),
    width = reactive(input$stakedNodesFigWidth), height = 500
  )
  output$depthStakedNodes <- renderUI(
    depthSelect(inputId = "depthSelStakedNodes", depths = depths(dat()))
  )
  output$stakedNodesNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelStakedNodes",
                nhoods = nhoodList(dat(), as.integer(input$depthSelStakedNodes)))
  )

}


shinyApp(ui = ui, server = server)

# source("download_clean.R")
# fetchJsonAll(minRound = max(read_rds("data.rds")$roundNumber)) %>%
#   cleanData() %>%
#   calculateNhoodsDec() %>%
#   mergeData(read_rds("data.rds")) %>%
#   write_rds("data.rds", compress = "xz")

# paste0("aws s3 --profile rw-research-team cp ",
#        "projects/SWARM/storage-incentives-shiny-stats/data.rds ",
#        "s3://ethswarm-research-team/data.rds")

# https://api.swarmscan.io/v1/network/stats
