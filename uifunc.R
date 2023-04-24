roundsSlider <- function(inputId = "roundRange", min, max, value) {
  sliderInput(inputId, label = "Range of rounds", step = 1,
              min = min, max = max, value = value, width = "95%")
}


heightSlider <- function(inputId, min = 200) {
  sliderInput(inputId, label = "Figure height", min = min, max = 2500,
              value = 500, round = TRUE, width = "150%")
}


widthSlider <- function(inputId) {
  sliderInput(inputId, label = "Figure width", min = 200, max = 4000,
              value = 850, round = TRUE, width = "150%")
}


depthSelect <- function(inputId, depths = 1:9) {
  selectInput(inputId, label = "Depth:", choices = depths, selected = 8)
}


nhoodSelect <- function(inputId, nhoods) {
  selectInput(inputId, label = "Highlight nhood", choices = c("-", nhoods))
}


revealerTabset <- function(depths) {
  tabsetPanel(
    tabPanel(
      title = "Mean revealers per neighbourhood",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "revealerNhoodDepth")),
          column(width = 2, uiOutput(outputId = "revealerNhoodSelect")),
          column(width = 3, radioButtons(inputId = "revealerSortType",
                                         label = "Sort neighbourhoods by:",
                                         choices = c("Honest revealers",
                                                     "Inaccurate revealers")))
        ),
        plotOutput("outRevealersPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Total revealers per neighbourhood",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "revealerNhoodDepth2")),
          column(width = 2, uiOutput(outputId = "revealerNhoodSelect2")),
          column(width = 3, radioButtons(inputId = "revealerSortType2",
                                         label = "Sort neighbourhoods by:",
                                         choices = c("Honest revealers",
                                                     "Inaccurate revealers")))
        ),
        plotOutput("outRevealersPerNhoodFig2")
      )
    ),
    tabPanel(
      title = "Price change",
      verticalLayout(plotOutput("outPriceFig"))
    ),
    tabPanel(
      title = "Reveals vs. commits",
      verticalLayout(
        radioButtons(inputId = "inaccFilt", label = "Filter rounds for:",
                     choices = c(
                       "No filter",
                       "Inaccurate revealers",
                       "Reveal-commit mismatch",
                       "Inaccurate revealers or reveal-commit mismatch"
                     )),
        textOutput("outInacc"),
        tableOutput("outRevealCommitTab")
      )
    ),
  )
}


skippedRoundsTabset <- function() {
  tabsetPanel(
    tabPanel(
      title = "Skipped rounds through time",
      verticalLayout(
        textOutput("outNumSkipped"),
        plotOutput("outSkippedFig"),
        tableOutput("outSkippedTab")
      )
    ),
    tabPanel(
      title = "Distribution of skipped rounds",
      fluidRow(
        column(width = 4, tableOutput("outSkipDistrTab")),
        column(width = 8, plotOutput("outSkipDistrFig"))
      )
    )
  )
}


rewardTabset <- function(depths, rewardRange) {
  tabsetPanel(
    tabPanel(
      title = "Reward distribution",
      verticalLayout(
        fluidRow(
          column(width = 2,
                 radioButtons(inputId = "rewardFigLogX", label = "x-axis:",
                              selected = "Linear",
                              choices = c("Linear", "Logarithmic"))),
          column(width = 3,
                 radioButtons(inputId = "rewardFigLogY", label = "y-axis:",
                              selected = "Square-root transformed",
                              choices = c("Linear", "Square-root transformed")))
        ),
        plotOutput("outRewardFig")
      )
    ),
    tabPanel(
      title = "Wins across neighbourhoods",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "depthWins")),
          column(width = 2, uiOutput(outputId = "winNhoodSelect"))
        ),
        plotOutput("outWinNhoodFig")
      )
    ),
    tabPanel(
      title = "Distribution of wins",
      verticalLayout(
        fluidRow(column(width = 2, uiOutput(outputId = "depthWD"))),
        plotOutput("outWinDistrFig")
      )
    ),
    tabPanel(
      title = "Total reward (neighbourhoods)",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "depthTR")),
          column(width = 2, uiOutput(outputId = "rewardNhoodSelect"))
        ),
        plotOutput("outRewardNhoodFig")
      )
    ),
    tabPanel(
      title = "Total reward (nodes)",
      verticalLayout(
        plotOutput("outRewardNodeFig")
      )
    )
  )
}


nodeTabset <- function(depths) {
  tabsetPanel(
    tabPanel(
      title = "Depths",
      fluidRow(
        column(width = 4, tableOutput("outDepthTab")),
        column(width = 8, verticalLayout(
          radioButtons(inputId = "depthLogY", label = "y-axis: ",
                       selected = "Logarithmic", inline = TRUE,
                       choices = c("Linear", "Logarithmic")),
          plotOutput("outDepthFig")
        ))
      )
    ),
    tabPanel(
      title = "Nodes per neighbourhood",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "depthNodesPerNhood")),
          column(width = 2, uiOutput(outputId = "nodesPerNhoodSelect"))
        ),
        plotOutput("outNodesPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Wins & nodes per neighbourhood",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "depthWinsNodes")),
          column(width = 2, uiOutput(outputId = "winNodeNhoodSelect")),
          column(width = 3,  radioButtons(inputId = "sortWN",
                                          label = "Sort by number of:",
                                          selected = "wins",
                                          choices = c("wins", "nodes")))
        ),
        plotOutput("outWinsNodesPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Distribution of nodes",
      verticalLayout(
        fluidRow(
          column(width = 2, uiOutput(outputId = "depthNodesDistr"))
        ),
        plotOutput("outNodeDistrFig")
      )
    )
  )
}


stakeTabset <- function(depths) {
  tabsetPanel(
    tabPanel(
      title = "Sum of stakes across neighbourhoods",
      fluidRow(
        column(width = 2, uiOutput(outputId = "depthStakes")),
        column(width = 2, uiOutput(outputId = "stakeNhoodSelect"))
      ),
      plotOutput("outStakesNhoodFig")
    ),
    tabPanel(
      title = "Distribution of sum of stakes",
      fluidRow(column(width = 2, uiOutput(outputId = "depthStakeDistr"))),
      plotOutput("outStakesNodeFig")
    ),
    tabPanel(
      title = "Staked nodes",
      fluidRow(
        column(width = 2, uiOutput(outputId = "depthStakedNodes")),
        column(width = 2, uiOutput(outputId = "stakedNodesNhoodSelect"))
      ),
      plotOutput("outStakedNodesFig")
    )
  )
}
