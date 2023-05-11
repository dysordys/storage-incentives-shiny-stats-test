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
    timeSlider(inputId = "timeRange", min=min(dataOrig$date), max=max(dataOrig$date)),
    tabPanel(title = "Revealers", revealerTabset()),
    tabPanel(title = "Skipped rounds", skippedRoundsTabset()),
    tabPanel(title = "Rewards", rewardTabset(rewardRange(dataOrig))),
    tabPanel(title = "Nodes", nodeTabset()),
    tabPanel(title = "Stakes", stakeTabset()),
    tabPanel(title = "Freezes", freezeTabset())
  )
)



server <- function(input, output) {

  dat <- reactive(restrictDate(dataOrig, input$timeRange))


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
                nhoods = nhoodList(dat(), input$depthSelRevealersPerNhood))
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
                nhoods = nhoodList(dat(), input$depthSelRevealersPerNhood2))
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
    depthSelect(inputId = "depthSelWinNhood", depths = depthsWith(dat()))
  )
  output$winNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelWinNhood",
                nhoods = nhoodList(dat(), input$depthSelWinNhood))
  )

  output$outWinDistrFig <- renderPlot(
    winNhoodHistFig(dat(), input$depthSelWinDistr)
  )
  output$depthWD <- renderUI(
    depthSelect(inputId = "depthSelWinDistr", depths = depthsWith(dat()))
  )

  output$outRewardNhoodFig <- renderPlot(
    rewardNhoodFig(dat(), input$depthSelTotalReward, input$nhoodSelTotalReward),
    height = 500
  )
  output$depthTR <- renderUI(
    depthSelect(inputId = "depthSelTotalReward", depths = depthsWith(dat()))
  )
  output$rewardNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelTotalReward",
                nhoods = nhoodList(dat(), input$depthSelTotalReward))
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
                nhoods = nhoodList(dat(), input$depthSelNodesPerNhood))
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
                nhoods = nhoodList(dat(), input$depthSelWinNode))
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
    depthSelect(inputId = "depthSelStake", depths = depthsWith(dat()))
  )
  output$stakeNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelStake",
                nhoods = nhoodList(dat(), input$depthSelStake))
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
                nhoods = nhoodList(dat(), input$depthSelStakedNodes))
  )


  # Elements of tab "Freezes":
  output$outFrozenNodesFig <- renderPlot(
    frozenNodesFig(dat(), input$depthSelFrozenNodes, input$nhoodSelFrozenNodes),
    height = 500
  )
  output$depthFrozenNodes <- renderUI(
    depthSelect(inputId = "depthSelFrozenNodes", depths = depthsWith(dat(), frozenNodes))
  )
  output$frozenNodesNhoodSelect <- renderUI(
    nhoodSelect(inputId = "nhoodSelFrozenNodes",
                nhoods = frozenNodes(dat()) %>% nhoodList(input$depthSelFrozenNodes))
  )

  output$outFreezeThroughTimeFig <- renderPlot(
    freezesThroughTimeFig(dat())
  )

}



shinyApp(ui = ui, server = server)

# source("download_clean.R")
# fetchJsonAll(minRound = max(read_rds("data.rds")$roundNumber)) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds")) %>%
#   mutate(date = roundsToDatetime(roundNumber, lubridate::now())) %>%
#   write_rds("data.rds", compress = "xz")
