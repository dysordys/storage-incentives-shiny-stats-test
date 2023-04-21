library(shiny)
library(tidyverse)

source("utils.R")
source("output_items.R")
source("uifunc.R")


dataOrig <- read_rds("data.rds")



ui <- fluidPage(
  titlePanel("Redistribution data dashboard"),
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


  # Elements of tab "Reveal":
  output$outRevealersPerNhoodFig <- renderPlot(
    revealersPerNhoodFig(dat(), input$depthSelRevealersPerNhood, .f = mean,
                         input$revealerSortType, input$nhoodSelRevealersPerNhood),
    height = 500
  )
  output$revealerNhoodDepth <- renderUI(
    depthSelect(inputId = "depthSelRevealersPerNhood", depths = depths(dat()))
  )
  output$revealerNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelRevealersPerNhood",
                nhoods = nhoodList(dat(), as.integer(input$depthSelRevealersPerNhood)))
  )

  output$outRevealersPerNhoodFig2 <- renderPlot(
    revealersPerNhoodFig(dat(), input$depthSelRevealersPerNhood2, .f = sum,
                         input$revealerSortType2, input$nhoodSelRevealersPerNhood2),
    height = 500
  )
  output$revealerNhoodDepth2 <- renderUI(
    depthSelect(inputId = "depthSelRevealersPerNhood2", depths = depths(dat()))
  )
  output$revealerNhoodSelect2 <- renderUI(
    nhoodSelect(inputId = "nhoodSelRevealersPerNhood2",
                nhoods = nhoodList(dat(), as.integer(input$depthSelRevealersPerNhood2)))
  )

  output$outInacc <- renderText(
    inaccurateStats(dat())
  )

  output$outPriceFig <- renderPlot(
    priceFig(dat())
  )

  output$outRevealCommitTab <- renderTable(
    revealCommitTab(dat(), input$inaccFilt),
    na = ""
  )


  # Elements of tab "Skipped rounds":
  output$outNumSkipped <- renderText(
    numSkipped(dat())
  )

  output$outSkippedFig <- renderPlot(
    roundsFig(dat())
  )

  output$outSkippedTab <- renderTable(
    skippedRoundsTab(dat())
  )

  output$outSkipDistrTab <- renderTable(
    skippedRoundDistrTab(dat())
  )

  output$outSkipDistrFig <- renderPlot(
    skippedRoundDistrFig(dat())
  )


  # Elements of tab "Rewards":
  output$outRewardFig <- renderPlot(
    rewardDistrFig(dat(), input$rewardFigLogX, input$rewardFigLogY)
  )

  output$outWinNhoodFig <- renderPlot(
    winNhoodQuantileFig(dat(), input$depthSelWinNhood, input$nhoodSelWinNhood),
    height = 500
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
    winNhoodHistFig(dat(), input$depthSelWinDistr)
  )
  output$depthWD <- renderUI(
    depthSelect(inputId = "depthSelWinDistr", depths = depthsWinDistr(dat()))
  )

  output$outRewardNhoodFig <- renderPlot(
    rewardNhoodFig(dat(), input$depthSelTotalReward, input$nhoodSelTotalReward),
    height = 500
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
    rewardPerNodeFig(dat()),
    height = 450
  )


  # Elements of tab "Nodes":
  output$outDepthTab <- renderTable(
    depthTab(dat())
  )

  output$outDepthFig <- renderPlot(
    depthDistrFig(dat(), input$depthLogY)
  )

  output$outNodesPerNhoodFig <- renderPlot(
    nodesPerNhoodQuantileFig(dat(), input$depthSelNodesPerNhood,
                             input$nhoodSelNodesPerNhood),
    height = 500
  )
  output$depthNodesPerNhood <- renderUI(
    depthSelect(inputId = "depthSelNodesPerNhood", depths = depths(dat()))
  )
  output$nodesPerNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelNodesPerNhood",
                nhoods = nhoodList(dat(), as.integer(input$depthSelNodesPerNhood)))
  )

  output$outWinsNodesPerNhoodFig <- renderPlot(
    winNodeNhoodFig(dat(), input$depthSelWinNode, input$sortWN, input$nhoodSelWinNode),
    height = 500
  )
  output$depthWinsNodes <- renderUI(
    depthSelect(inputId = "depthSelWinNode", depths = depths(dat()))
  )
  output$winNodeNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelWinNode",
                nhoods = nhoodList(dat(), as.integer(input$depthSelWinNode)))
  )

  output$outNodeDistrFig <- renderPlot(
    nodesPerNhoodHistFig(dat(), input$depthSelNodesDistr)
  )
  output$depthNodesDistr <- renderUI(
    depthSelect(inputId = "depthSelNodesDistr", depths = depths(dat()))
  )


  # Elements of tab "Stakes":
  output$outStakesNhoodFig <- renderPlot(
    stakesNhoodQuantileFig(dat(), input$depthSelStake, input$nhoodSelStake),
    height = 500
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
    stakesNhoodHistFig(dat(), input$depthSelStakeDistr)
  )
  output$depthStakeDistr <- renderUI(
    depthSelect(inputId = "depthSelStakeDistr", depths = depths(dat()))
  )

  output$outStakedNodesFig <- renderPlot(
    stakedNodesFig(dat(), input$depthSelStakedNodes, input$nhoodSelStakedNodes),
    height = 500
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
