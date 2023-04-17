outPriceFig <- function(dat, roundRange = NA) {
  restrictRounds(dat, roundRange) %>%
    pricePerRound() %>%
    mutate(inaccurate = revealers - honest) %>%
    mutate(price = price / first(price)) %>%
    priceFig()
}


outInaccurate <- function(dat, roundRange = NA) {
  s <- inaccurateRevealerStats(restrictRounds(dat, roundRange))
  str_c("Rounds with inaccurate revealers: ", s$n, " out of ", s$rounds,
        ", or ", round(100 * s$p, 2), "%")
}


outRevealersPerNhoodFig <- function(dat, roundRange = NA, depth = 8, .f = mean,
                                    revealerSortType = "Numerical order",
                                    highlightNhood = NA) {
  fname <- case_when(
    identical(.f, mean) ~ "mean ",
    identical(.f, sum) ~ "total ",
    TRUE ~ ""
  )
  ylab <- str_c(fname, "number of revealers")
  restrictRoundsDepth(dat, roundRange, depth) %>%
    filter(!(roundNumber %in% roundsWithoutWinner(.))) %>%
    revealerNhoodSummary(.f) %>%
    filter(!is.na(nhood)) %>%
    revealersPerNhoodFig(revealerSortType, ylab, highlightNhood)
}


outRevealCommitTab <- function(dat, roundRange = NA, inaccFilt = "") {
  restrictRounds(dat, roundRange) %>%
    select(roundNumber, event, overlay) %>%
    filter(event != "won") %>%
    nest(overlays = !roundNumber & !event) %>%
    mutate(overlays = map(overlays, ~.$overlay)) %>%
    pivot_wider(names_from = event, values_from = overlays) %>%
    mutate(revealNoCommit = map2(revealed, committed, setdiff)) %>%
    mutate(commitNoReveal = map2(committed, revealed, setdiff)) %>%
    mutate(numRevealNoCommit = map_int(revealNoCommit, length)) %>%
    mutate(numCommitNoReveal = map_int(commitNoReveal, length)) %>%
    mutate(revealed = map_int(revealed, length)) %>%
    mutate(committed = map_int(committed, length)) %>%
    mutate(revealNoCommit = map_chr(revealNoCommit, str_c, collapse = "\n")) %>%
    mutate(commitNoReveal = map_chr(commitNoReveal, str_c, collapse = "\n")) %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(inaccurate = revealed - honest) %>%
    { if (inaccFilt == "Inaccurate revealers") {
      filter(., inaccurate > 0)
    } else if (inaccFilt == "Reveal-commit mismatch") {
      filter(., numRevealNoCommit != 0 | numCommitNoReveal != 0)
    } else if (inaccFilt=="Inaccurate revealers or reveal-commit mismatch") {
      filter(., inaccurate > 0 | (numRevealNoCommit != 0 | numCommitNoReveal != 0))
    } else .
    } %>%
    transmute(`round` = roundNumber,
              `reveals` = revealed,
              `commits` = committed,
              `inaccurate reveals` = inaccurate,
              `reveals without commit` = revealNoCommit,
              `commits without reveal` = commitNoReveal)
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
                         xtrans = "Logarithmic", ytrans = "Square-root transformed") {
  skippedRounds(dat) %>%
    mutate(skip = as_factor(skip)) %>%
    restrictRounds(roundRange) %>%
    rewardDistrFig(xrange, xtrans, ytrans)
}


outWinNhoodFig <- function(dat, roundRange = NA, depth = 8, highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winsByNhood() %>%
    participationNhoodQuantileFig(highlightNhood)
}


outWinNodeNhoodFig <- function(dat, roundRange = NA, depth = 8, sortBy = "wins",
                               highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winsNodesByNhood(roundRange, depth) %>%
    winNodeNhoodFig(sortBy, highlightNhood)
}


outWinDistrFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    winEventsTab() %>%
    participationNhoodHistFig()
}


outRewardNhoodFig <- function(dat, roundRange = NA, depth = 8, highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    rewardNhoodFig(highlightNhood)
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


outNodesPerNhoodFig <- function(dat, roundRange = NA, depth = 8, highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    nodesByNhoodRank() %>%
    nodesPerNhoodQuantileFig(highlightNhood)
}


outNodeDistrFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    nodesByNhood() %>%
    nodesPerNhoodHistFig()
}


outStakesNhoodFig <- function(dat, roundRange = NA, depth = 8, highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    stakesNhoodQuantileFig(highlightNhood)
}


outStakesNodeFig <- function(dat, roundRange = NA, depth = 8) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    rewardNhoodDistr() %>%
    stakesNhoodHistFig()
}


outStakedNodesFig <- function(dat, roundRange = NA, depth = 8, highlightNhood = NA) {
  restrictRoundsDepth(dat, roundRange, depth) %>%
    stakedNodesPerNhood() %>%
    arrange(stakedNodes) %>%
    rowid_to_column("rank") %>%
    stakedNodesFig(highlightNhood)
}
