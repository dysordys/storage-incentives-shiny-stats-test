adjustPrice <- function(currentPrice, honestRevealers) {
  minimumPrice <- 2^10
  v <- 0.00294
  #increaseRate <- c(1036, 1031, 1027, 1025, 1024, 1023, 1021, 1017, 1012) / minimumPrice
  targetRedundancy <- 4
  maxConsideredExtraRedundancy <- 4
  usedRedundancy <- min(honestRevealers, targetRedundancy + maxConsideredExtraRedundancy)
  #max(increaseRate[usedRedundancy + 1] * currentPrice, minimumPrice)
  max(currentPrice * 2^(v * (targetRedundancy - usedRedundancy)), minimumPrice)
}


accumPrice <- function(redundancyVec, initPrice) {
  Reduce(function(x, y) adjustPrice(x, y), redundancyVec, init = initPrice,
         accumulate = TRUE)[-1]
}


isValidRoundRange <- function(roundRange) {
  (length(roundRange) == 2) && (roundRange[1] <= roundRange[2])
}


restrictRounds <- function(dat, roundRange) {
  if (isValidRoundRange(roundRange))
    filter(dat, roundNumber %in% reduce(round(roundRange), `:`)) else dat
}


roundsToPlot <- function(roundRange, maxPoints) {
  if (isValidRoundRange(roundRange)) {
    diff <- round(roundRange[2]) - round(roundRange[1])
    if (diff > maxPoints) {
      floor(seq(round(roundRange)[1], round(roundRange)[2], l = maxPoints))
    } else {
      round(roundRange[1]):round(roundRange[2])
    }
  } else NULL
}


revealersPerRound <- function(dat) {
  dat %>%
    group_by(roundNumber) %>%
    mutate(honest = (id == id[event == "won"])) %>%
    filter(event == "revealed") %>%
    summarise(revealers = n(), honest = sum(honest)) %>%
    ungroup()
}


revealerNhoodSummary <- function(dat, .f = mean) {
  dat %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(inaccurate = revealers - honest) %>%
    group_by(nhood) %>%
    summarise(honest = .f(honest), inaccurate = .f(inaccurate)) %>%
    ungroup()
}


pricePerRound <- function(dat, initPrice = 2048) {
  tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(honest = replace_na(honest, 0)) %>%
    mutate(price = accumPrice(honest, initPrice))
}


missedRounds <- function(dat) {
  tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
    anti_join(distinct(select(dat, roundNumber)), by = "roundNumber")
}


inaccurateRevealerStats <- function(dat) {
  tab <- dat %>% pricePerRound()
  rounds <- nrow(tab)
  roundsWithInaccurates <- nrow(filter(tab, revealers != honest))
  list(rounds = rounds, n = roundsWithInaccurates, p = roundsWithInaccurates / rounds)
}


chisqUnif <- function(vec) {
  spgs::chisq.unif.test(x = vec, interval = range(vec))
}


chisqUnifMissedRounds <- function(dat) {
  chisqUnif(pull(dat, roundNumber))$p.value %>%
    round(5) %>%
    str_c("Chi-squared test of uniformity: p = ", ., " ", case_when(
      . < 0.01 ~ "(i.e., skipped rounds are not uniformly distributed)",
      . < 0.05 ~ "(i.e., skipped rounds are unlikely to be uniformly distributed)",
      . < 0.1  ~ str_c("(i.e., skipped rounds may not be uniformly distributed, ",
                       "but it is difficult to say)"),
      TRUE ~ "(i.e., assumption of uniformity cannot be rejected)",
    ))
}


skippedRounds <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    select(roundNumber, rewardAmount) %>%
    mutate(skip = roundNumber - lag(roundNumber, default = first(roundNumber) - 1) - 1)
}


nhoodBinStr <- function(overlay, depth = 8L) {
  if (!is.na(depth) & depth > 0) {
    numHexDigits <- ceiling(depth / 4) # 4: four bits = 1 hex digit
    str_sub(R.utils::intToBin(str_sub(overlay, 1, numHexDigits + 2)), 1, depth)
  } else NA
}


nhoodDec <- function(overlay, depth = 8L) {
  strtoi(nhoodBinStr(overlay, depth), base = 2)
}


calculateNhoodsDec <- function(dat) {
  mutate(dat, nhood = map2_int(overlay, depth, nhoodDec), .after = overlay)
}


proximity <- function(fst, snd) {
  maxPO <- 16L
  b <- min((maxPO - 1) %/% 8 + 1, length(fst))
  m <- 8L
  for (i in 1:b) {
    oxo <- bitwXor(fst[i], snd[i])
    for (j in 1:m) {
      if (bitwAnd(bitwShiftR(oxo, 8L - j), 0x01) != 0) return(8L*(i - 1L) + j - 1L)
    }
  }
  return(maxPO)
}


rewardNhoodDistr <- function(dat) {
  dat %>%
    filter(event == "won", !is.na(nhood)) %>%
    group_by(nhood) %>%
    summarise(winEvents = n(),
              totalReward = sum(rewardAmount),
              totalStake = sum(stake)) %>%
    ungroup()
}


unifQuantileNull <- function(p, rounds, nhoods) {
  qbinom(p, size = rounds, prob = 1 / nhoods)
}


unifDistrNull <- function(x, rounds, nhoods) {
  dbinom(x, size = rounds, prob = 1 / nhoods)
}


nodesPerNhood <- function(dat) {
  dat %>%
    filter(!is.na(nhood)) %>%
    group_by(nhood) %>%
    count(overlay) %>%
    ungroup()
}


rewardPerNode <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    group_by(rewardTo) %>%
    summarise(reward = sum(rewardAmount)) %>%
    ungroup()
}


depthDistr <- function(dat) {
  dat %>%
    filter(!is.na(depth)) %>%
    count(depth)
}


nhoods <- function(dat) {
  length(unique(dat$nhood[!is.na(dat$nhood)]))
}


nodes <- function(dat) {
  dat %>%
    nodesPerNhood() %>%
    count(nhood) %>%
    pull(n) %>%
    sum()
}


rounds <- function(dat) {
  length(unique(dat$roundNumber[!is.na(dat$roundNumber)]))
}


winEventsTab <- function(dat) {
  dat %>%
    rewardNhoodDistr() %>%
    count(winEvents, name = "observed") %>%
    right_join(tibble(winEvents = 1:max(.$winEvents)), by = "winEvents") %>%
    mutate(predicted = unifDistrNull(winEvents, rounds(dat), nhoods(dat)) *
             sum(observed, na.rm = TRUE))
}


winsByNhood <- function(dat) {
  dat %>%
    rewardNhoodDistr() %>%
    arrange(winEvents) %>%
    rowid_to_column("rank") %>%
    left_join(tibble(
      rank = 1:nhoods(dat),
      predict = unifQuantileNull(seq(0.1, 0.99, l=nhoods(dat)), rounds(dat), nhoods(dat))
    ), by = "rank")
}


nodesByNhood <- function(dat) {
  dat %>%
    nodesPerNhood() %>%
    count(nhood, name = "num") %>%
    count(num) %>%
    right_join(tibble(num = 1:max(.$num)), by = "num") %>%
    mutate(predict = unifDistrNull(num, nodes(dat), nhoods(dat))*sum(n, na.rm=TRUE))
}


nodesByNhoodRank <- function(dat) {
  dat %>%
    nodesPerNhood() %>%
    count(nhood) %>%
    arrange(n) %>%
    rowid_to_column("rank") %>%
    left_join(tibble(
      rank = 1:nhoods(dat),
      predict = unifQuantileNull(seq(0.1, 0.99, l=nhoods(dat)), nodes(dat), nhoods(dat))
    ), by = "rank")
}


roundsWithoutWinner <- function(dat) {
  dat %>%
    group_by(roundNumber) %>%
    summarise(winner = length(event[event == "won"])) %>%
    ungroup() %>%
    filter(winner == 0) %>%
    pull(roundNumber)
}


roundsWithDepthMismatch <- function(dat) {
  dat %>%
    filter(event != "committed") %>%
    group_by(roundNumber) %>%
    mutate(wdepth = depth[event == "won"]) %>%
    mutate(eq = depth != wdepth) %>%
    summarise(diff = sum(eq)) %>%
    filter(diff != 0) %>%
    pull(roundNumber)
}
