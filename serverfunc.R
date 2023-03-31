outPriceFig <- function(dat, roundRange = NA) {
  pricePerRound(dat) %>%
    mutate(inaccurate = revealers - honest) %>%
    mutate(price = price / first(price)) %>%
    restrictRounds(roundRange) %>%
    priceFig()
}


outInaccurate <- function(dat, roundRange = NA) {
  s <- inaccurateRevealerStats(restrictRounds(dat, roundRange))
  str_c("Rounds with inaccurate revealers: ", s$n, " out of ", s$rounds,
        ", or ", round(100 * s$p, 2), "%")
}


outPriceTab <- function(dat, roundRange = NA, inaccFilt = "Show all rounds") {
  pricePerRound(dat) %>%
    mutate(inaccurate = revealers - honest, price = price / first(price)) %>%
    { if (inaccFilt == "Show all rounds") . else filter(., inaccurate > 0) } %>%
    restrictRounds(roundRange) %>%
    transmute(round = roundNumber,
              `price (in units of the initial value)` = price,
              `number of revealers` = revealers,
              `inaccurate revealers` = inaccurate)
}


outRevealersPerNhoodFig <- function(dat, roundRange = NA, depth = 8,
                                    revealerSortType = TRUE) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    filter(!(roundNumber %in% roundsWithoutWinner(.))) %>%
    revealerNhoodSummary(.f = mean) %>%
    revealersPerNhoodFig(revealerSortType)
}


outChiSqUnifTxt <- function(dat, roundRange = NA) {
  chisqUnifMissedRounds(restrictRounds(dat, roundRange))
}


outNumSkipped <- function(dat, roundRange = NA) {
  datRestricted <- restrictRounds(dat, roundRange)
  roundsMissed <- nrow(missedRounds(datRestricted))
  totalRounds <- max(datRestricted$roundNumber) - min(datRestricted$roundNumber) + 1
  str_c("Skipped rounds: ", roundsMissed, " out of ", totalRounds,
        ", or ", round(100 * roundsMissed / totalRounds, 2), "%")
}


outSkipped <- function(dat, roundRange = NA) {
  restrictRounds(missedRounds(dat), roundRange)
}


outSkippedFig <- function(dat, roundRange = NA) {
  roundsFig(outSkipped(dat, roundRange))
}


outSkippedTab <- function(dat, roundRange = NA) {
  rename(outSkipped(dat, roundRange), `Skipped rounds:` = roundNumber)
}


outSkippedRoundDistrTab <- function(dat, roundRange = NA) {
  restrictRounds(dat, roundRange) %>%
    skippedRoundDistr() %>%
    filter(skip > 0) %>%
    rename(`Skipped in a row` = skip, `count` = n)
}


outSkippedRoundDistrFig <- function(dat, roundRange = NA) {
  restrictRounds(dat, roundRange) %>%
    skippedRoundDistr() %>%
    filter(skip > 0) %>%
    skippedRoundDistrFig()
}


outRewardFig <- function(dat, roundRange = NA, xrange = range(dat$rewardAmount),
                         log.x = "Logarithmic", log.y = "Pseudo-logarithmic") {
  skippedRounds(dat) %>%
    mutate(skip = as_factor(skip)) %>%
    restrictRounds(roundRange) %>%
    rewardDistrFig(xrange, log.x, log.y)
}


outWinNhoodFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winsByNhood() %>%
    participationNhoodQuantileFig()
}


outWinNodeNhoodFig <- function(dat, roundRange = NA, depth = 8, sortBy = "wins") {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winsNodesByNhood(roundRange, depth) %>%
    winNodeNhoodFig(sortBy)
}


outWinDistrFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winEventsTab() %>%
    participationNhoodHistFig()
}


outRewardNhoodFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    rewardNhoodFig()
}


outRewardNodeFig <- function(dat, roundRange = NA) {
  restrictRounds(dat, roundRange) %>%
    rewardPerNode() %>%
    rewardPerNodeFig()
}


outDepth <- function(dat, roundRange = NA) {
  depthDistr(restrictRounds(dat, roundRange))
}


outDepthTab <- function(dat, roundRange = NA) {
  rename(outDepth(dat, roundRange), `number of nodes` = n)
}


outDepthFig <- function(dat, roundRange = NA, log.y = "Logarithmic y-axis") {
  depthDistrFig(outDepth(dat, roundRange), log.y = log.y)
}


outNodesPerNhoodFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    nodesByNhoodRank() %>%
    nodesPerNhoodQuantileFig()
}


outNodeDistrFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    nodesByNhood() %>%
    nodesPerNhoodHistFig()
}


outStakesNhoodFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    stakesNhoodQuantileFig()
}


outStakesNodeFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    stakesNhoodHistFig()
}


outStakedNodesFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    stakedNodesPerNhood() %>%
    arrange(stakedNodes) %>%
    rowid_to_column("rank") %>%
    stakedNodesFig()
}
