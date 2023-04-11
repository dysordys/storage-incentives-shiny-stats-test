roundsSlider <- function(inputId = "roundRange", min, max, value) {
  sliderInput(inputId, label = "Range of rounds",
              min = min, max = max, value = value, width = "90%")
}


heightSlider <- function(inputId, min = 200) {
  sliderInput(inputId, label = "Figure height", min = min, max = 2500,
              value = 450, round = TRUE, width = "150%")
}


depthSelect <- function(inputId, depths = 1:9) {
  selectInput(inputId, label = "Depth:", choices = depths, selected = 8)
}


nhoodSelect <- function(inputId, depth) {
  selectInput(inputId, label = "Highlight nhood",
              choices = c(NA, R.utils::intToBin(0:(2^depth - 1))))
}


revealerTabset <- function(depths) {
  tabsetPanel(
    tabPanel(
      title = "Mean revealers per nhood",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "revealerNhoodDepth", depths)),
          column(width = 5, heightSlider(inputId = "revealerNhoodFigHeight")),
          column(width = 3, radioButtons(inputId = "revealerSortType",
                                         label = "Sort neighbourhoods by:",
                                         choices = c("Honest revealers",
                                                     "Inaccurate revealers",
                                                     "Numerical order"))),
          column(width = 2, uiOutput(outputId = "revealerNhoodSelect"))
        ),
        plotOutput("outRevealersPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Total revealers per nhood",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "revealerNhoodDepth2", depths)),
          column(width = 6, heightSlider(inputId = "revealerNhoodFigHeight2")),
          column(width = 4, radioButtons(inputId = "revealerSortType2",
                                         label = "Sort neighbourhoods by:",
                                         choices = c("Honest revealers",
                                                     "Inaccurate revealers",
                                                     "Numerical order")))
        ),
        plotOutput("outRevealersPerNhoodFig2")
      )
    ),
    tabPanel(
      title = "Revealer table",
      verticalLayout(
        radioButtons(inputId = "inaccFilt", label = NULL,
                     choices = c("Show all rounds",
                                 "Only show rounds with inaccurate revealers")),
        textOutput("outInacc"),
        tableOutput("outPriceTab")
      )
    ),
    tabPanel(
      title = "Price change",
      verticalLayout(plotOutput("outPriceFig"))
    ),
    tabPanel(
      title = "Reveals vs. commits",
      tags$html(tags$body(p(str_c("Table showing rounds where there is a mismatch ",
                                  "between revealers and committers")))),
      verticalLayout(tableOutput("outRevealCommitTab"))
    ),
  )
}


skippedRoundsTabset <- function() {
  tabsetPanel(
    tabPanel(
      title = "Skipped rounds through time",
      verticalLayout(
        textOutput("outNumSkipped"),
        textOutput("outChiSqUnifTxt"),
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
        splitLayout(
          radioButtons(inputId = "rewardFigLogX", label = "x-axis:", inline = TRUE,
                       selected = "Logarithmic",
                       choices = c("Linear", "Logarithmic")),
          radioButtons(inputId = "rewardFigLogY", label = "y-axis:", inline = TRUE,
                       selected = "Pseudo-logarithmic",
                       choices = c("Linear", "Pseudo-logarithmic"))
        ),
        plotOutput("outRewardFig"),
        sliderInput(inputId = "rewardRange", label = "Range of rewards",
                    min = 0, max = ceiling(1.1 * rewardRange[2]),
                    value = ceiling(c(0, 1.1) * rewardRange),
                    round = -2, step = 0.01, width = "90%")
      )
    ),
    tabPanel(
      title = "Wins across nhoods",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthWins", depths)),
          column(width = 6, heightSlider(inputId = "winNhoodFigHeight"))
        ),
        plotOutput("outWinNhoodFig")
      )
    ),
    tabPanel(
      title = "Distribution of wins",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthWD", depths))
        ),
        plotOutput("outWinDistrFig")
      )
    ),
    tabPanel(
      title = "Total reward (nhoods)",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthTR", depths)),
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
}


nodeTabset <- function(depths) {
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
      title = "Nodes per nhood",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthNodes", depths)),
          column(width = 6, heightSlider(inputId = "nodeFigHeight"))
        ),
        plotOutput("outNodesPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Nodes & wins per nhood",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthWinsNodes", depths)),
          column(width = 6, heightSlider(inputId = "winNodeFigHeight")),
          column(width = 4,  radioButtons(inputId = "sortWinNode",
                                          label = "Sort by number of:",
                                          selected = "wins", inline = TRUE,
                                          choices = c("wins", "nodes")))
        ),
        plotOutput("outWinsNodesPerNhoodFig")
      )
    ),
    tabPanel(
      title = "Distribution of nodes",
      verticalLayout(
        fluidRow(
          column(width = 2, depthSelect(inputId = "depthNodes2", depths))
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
        column(width = 2, depthSelect(inputId = "depthStakes", depths)),
        column(width = 6, heightSlider(inputId = "stakeFigHeight"))
      ),
      plotOutput("outStakesNhoodFig")
    ),
    tabPanel(
      title = "Distribution of sum of stakes",
      fluidRow(
        column(width = 2, depthSelect(inputId = "depthStakes2", depths))
      ),
      plotOutput("outStakesNodeFig")
    ),
    tabPanel(
      title = "Staked nodes",
      fluidRow(
        column(width = 2, depthSelect(inputId = "depthStakes3", depths)),
        column(width = 6, heightSlider(inputId = "stakedNodesFigHeight"))
      ),
      plotOutput("outStakedNodesFig")
    )
  )
}
