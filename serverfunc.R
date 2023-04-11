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


outRevealersPerNhoodFig <- function(dat, roundRange = NA, depth = 8, .f = mean,
                                    revealerSortType = "Numerical order",
                                    highlight = NA) {
  fname <- case_when(
    identical(.f, mean) ~ "mean ",
    identical(.f, sum) ~ "total ",
    TRUE ~ ""
  )
  xlab <- str_c(fname, "number of revealers")
  restrictRoundsDepth(dat, roundRange, depth) %>%
    filter(!(roundNumber %in% roundsWithoutWinner(.))) %>%
    revealerNhoodSummary(.f) %>%
    revealersPerNhoodFig(revealerSortType, xlab, highlight)
}


outRevealCommitTab <- function(dat, roundRange = NA) {
  restrictRounds(dat, roundRange) %>%
    select(roundNumber, event, overlay) %>%
    filter(event != "won") %>%
    nest(overlays = !roundNumber & !event) %>%
    mutate(overlays = map(overlays, ~.$overlay)) %>%
    pivot_wider(names_from = event, values_from = overlays) %>%
    mutate(`revealed but not committed` = map2(revealed, committed, setdiff)) %>%
    mutate(`committed but not revealed` = map2(committed, revealed, setdiff)) %>%
    mutate(l1 = map_int(`revealed but not committed`, length)) %>%
    mutate(l2 = map_int(`committed but not revealed`, length)) %>%
    filter(l1 != 0 | l2 != 0) %>%
    select(-l1, -l2) %>%
    mutate(revealed = map_int(revealed, length)) %>%
    mutate(committed = map_int(committed, length)) %>%
    mutate(`revealed but not committed` = map_chr(`revealed but not committed`, str_c,
                                                  collapse = "\n")) %>%
    mutate(`committed but not revealed` = map_chr(`committed but not revealed`, str_c,
                                                  collapse = "\n"))
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
