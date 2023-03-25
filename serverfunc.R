outPriceFig <- function(dat, roundRange = NA) {
  dat %>%
    pricePerRound() %>%
    mutate(inaccurate = revealers - honest) %>%
    mutate(price = price / first(price)) %>%
    restrictRounds(roundRange) %>%
    priceFig()
}


outInaccurate <- function(dat, roundRange = NA) {
  s <- dat %>%
    restrictRounds(roundRange) %>%
    inaccurateRevealerStats()
  str_c("Rounds with inaccurate revealers: ", s$n, " out of ", s$rounds,
        ", or ", round(100 * s$p, 2), "%")
}


outPriceTab <- function(dat, roundRange = NA, inaccFilt = "Show all rounds") {
  dat %>%
    pricePerRound() %>%
    mutate(inaccurate = revealers - honest, price = price / first(price)) %>%
    { if (inaccFilt == "Show all rounds") . else filter(., inaccurate > 0) } %>%
    restrictRounds(roundRange) %>%
    transmute(round = roundNumber,
              `price (in units of the initial value)` = price,
              `number of revealers` = revealers,
              `inaccurate revealers` = inaccurate)
}


outRevealersPerNhoodFig <- function(dat, roundRange = NA, revealerNhoodDepth = 8,
                                    revealerSortType = TRUE) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == revealerNhoodDepth) %>%
    filter(!(roundNumber %in% roundsWithoutWinner(.))) %>%
    revealerNhoodSummary(.f = mean) %>%
    revealersPerNhoodFig(revealerSortType)
}


outChiSqUnifTxt <- function(dat, roundRange = NA) {
  dat %>%
    restrictRounds(roundRange) %>%
    chisqUnifMissedRounds()
}


outNumSkipped <- function(dat, roundRange = NA) {
  datRestricted <- restrictRounds(dat, roundRange)
  roundsMissed <- nrow(missedRounds(datRestricted))
  totalRounds <- max(datRestricted$roundNumber) - min(datRestricted$roundNumber) + 1
  str_c("Skipped rounds: ", roundsMissed, " out of ", totalRounds,
        ", or ", round(100 * roundsMissed / totalRounds, 2), "%")
}


outSkipped <- function(dat, roundRange = NA) {
  dat %>%
    missedRounds() %>%
    restrictRounds(roundRange)
}


outSkippedFig <- function(dat, roundRange = NA) {
  outSkipped(dat, roundRange) %>%
    roundsFig()
}


outSkippedTab <- function(dat, roundRange = NA) {
  outSkipped(dat, roundRange) %>%
    rename(`Skipped rounds:` = roundNumber)
}


outRewardFig <- function(dat, roundRange = NA, xrange = range(dat$rewardAmount),
                         log.x = "Logarithmic", log.y = "Pseudo-logarithmic") {
  dat %>%
    skippedRounds() %>%
    mutate(skip = as_factor(skip)) %>%
    restrictRounds(roundRange) %>%
    rewardDistrFig(xrange, log.x, log.y)
}


outWinNhoodFig <- function(dat, roundRange = NA, winDepth = 8) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == winDepth) %>%
    winsByNhood() %>%
    participationNhoodQuantileFig()
}


outWinDistrFig <- function(dat, roundRange = NA, winDepth = 8) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == winDepth) %>%
    winEventsTab() %>%
    participationNhoodHistFig()
}


outRewardNhoodFig <- function(dat, roundRange = NA, rewardDepth = 8) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == rewardDepth) %>%
    rewardNhoodDistr() %>%
    rewardNhoodFig()
}


outRewardNodeFig <- function(dat, roundRange = NA) {
  dat %>%
    restrictRounds(roundRange) %>%
    rewardPerNode() %>%
    rewardPerNodeFig()
}


outDepth <- function(dat, roundRange = NA) {
  dat %>%
    restrictRounds(roundRange) %>%
    depthDistr()
}


outDepthTab <- function(dat, roundRange = NA) {
  outDepth(dat, roundRange) %>%
    rename(`number of nodes` = n)
}


outDepthFig <- function(dat, roundRange = NA, log.y = "Logarithmic y-axis") {
  outDepth(dat, roundRange) %>%
    depthDistrFig(log.y = log.y)
}


outNodesPerNhoodFig <- function(dat, roundRange = NA, nodeDepth = 8) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == nodeDepth) %>%
    nodesByNhoodRank() %>%
    nodesPerNhoodQuantileFig()
}


outNodeDistrFig <- function(dat, roundRange = NA, nodeDepth = 8) {
  dat %>%
    restrictRounds(roundRange) %>%
    filter(depth == nodeDepth) %>%
    nodesByNhood() %>%
    nodesPerNhoodHistFig()
}


outStakes <- function(dat, roundRange = NA, stakeDepth = 8) {
  restrictRounds(dat, roundRange) %>%
    filter(depth == stakeDepth) %>%
    rewardNhoodDistr()
}


outStakesNhoodFig <- function(dat, roundRange = NA, stakeDepth = 8) {
  outStakes(dat, roundRange, stakeDepth) %>%
    stakesNhoodQuantileFig()
}


outStakesNodeFig <- function(dat, roundRange = NA, stakeDepth = 8) {
  outStakes(dat, roundRange, stakeDepth) %>%
    stakesNhoodHistFig()
}
