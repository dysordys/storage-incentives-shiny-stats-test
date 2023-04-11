library(shiny)
library(tidyverse)

source("utils.R")
source("figures.R")
source("serverfunc.R")
source("uifunc.R")


dat <- read_rds("data.rds") %>%
  mutate(rewardAmount = rewardAmount / 1e16, stake = stake / 1e16) # Convert PLUR to BZZ


ui <- fluidPage(
  navbarPage(
    title = "View data on:",
    roundsSlider(inputId = "roundRange",
                 min = min(dat$roundNumber), max = max(dat$roundNumber),
                 # 672 rounds = 1 week:
                 value = c(max(dat$roundNumber) - 672, max(dat$roundNumber))),
    tabPanel(title = "Revealers", revealerTabset(depths(dat))),
    tabPanel(title = "Skipped rounds", skippedRoundsTabset()),
    tabPanel(title = "Rewards", rewardTabset(depths(dat), rewardRange(dat))),
    tabPanel(title = "Nodes", nodeTabset(depths(dat))),
    tabPanel(title = "Stakes", stakeTabset(depths(dat)))
  )
)


server <- function(input, output) {
  output$outRevealersPerNhoodFig <- renderPlot(outRevealersPerNhoodFig(
    dat, input$roundRange, input$revealerNhoodDepth, .f = mean, input$revealerSortType,
    input$depthSelectRevealersPerNhood), height = reactive(input$revealerNhoodFigHeight))
  output$revealerNhoodSelect <- renderUI(nhoodSelect(
    inputId = "depthSelectRevealersPerNhood", as.integer(input$revealerNhoodDepth)))
  output$outRevealersPerNhoodFig2 <- renderPlot(outRevealersPerNhoodFig(
    dat, input$roundRange, input$revealerNhoodDepth2, .f = sum, input$revealerSortType2),
    height = reactive(input$revealerNhoodFigHeight2))
  output$outInacc <- renderText(outInaccurate(dat, input$roundRange))
  output$outPriceTab <- renderTable(outPriceTab(dat, input$roundRange, input$inaccFilt),
                                    na = "")
  output$outPriceFig <- renderPlot(outPriceFig(dat, input$roundRange))
  output$outRevealCommitTab <- renderTable(outRevealCommitTab(dat, input$roundRange))
  output$outNumSkipped <- renderText(outNumSkipped(dat, input$roundRange))
  output$outChiSqUnifTxt <- renderText(outChiSqUnifTxt(dat, input$roundRange))
  output$outSkippedFig <- renderPlot(outSkippedFig(dat, input$roundRange))
  output$outSkippedTab <- renderTable(outSkippedTab(dat, input$roundRange))
  output$outSkipDistrTab <- renderTable(outSkippedRoundDistrTab(dat, input$roundRange))
  output$outSkipDistrFig <- renderPlot(outSkippedRoundDistrFig(dat, input$roundRange))
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
  output$outWinsNodesPerNhoodFig <- renderPlot(outWinNodeNhoodFig(
    dat, input$roundRange, input$depthWinsNodes, input$sortWinNode),
    height = reactive(input$winNodeFigHeight))
  output$outNodeDistrFig <- renderPlot(outNodeDistrFig(
    dat, input$roundRange, input$depthNodes2))
  output$outStakesNhoodFig <- renderPlot(outStakesNhoodFig(
    dat, input$roundRange, input$depthStakes),
    height = reactive(input$stakeFigHeight))
  output$outStakesNodeFig <- renderPlot(outStakesNodeFig(
    dat, input$roundRange, input$depthStakes2))
  output$outStakedNodesFig <- renderPlot(outStakedNodesFig(
    dat, input$roundRange, input$depthStakes3),
    height = reactive(input$stakedNodesFigHeight))
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

# tictoc::tic(); write_rds(downloadAllData(), "data.rds", compress="xz"); tictoc::toc()

# https://api.swarmscan.io/v1/network/stats
