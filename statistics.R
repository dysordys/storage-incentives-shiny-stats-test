adjustPrice <- function(currentPrice, redundancy) {
  minimumPrice <- 2^10
  increaseRate <- c(1036, 1027, 1025, 1024, 1023, 1021, 1017, 1012)
  targetRedundancy <- 4
  maxConsideredExtraRedundancy <- 4
  usedRedundancy <- min(redundancy, targetRedundancy + maxConsideredExtraRedundancy)
  max((increaseRate[usedRedundancy] * currentPrice) / minimumPrice, minimumPrice)
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
    summarise(`number of revealers` = n(),
              `honest revealers` = sum(honest)) %>%
    ungroup()
}


pricePerRound <- function(dat, initPrice = 2048) {
  dat %>%
    revealersPerRound() %>%
    mutate(price = accumPrice(`honest revealers`, initPrice)) %>%
    transmute(`roundNumber`,
              `price (in units of the initial value)` = price / initPrice,
              `number of revealers`,
              `inaccurate revealers` = `number of revealers` - `honest revealers`)
}


missedRounds <- function(dat) {
  tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
    anti_join(distinct(select(dat, roundNumber)), by = "roundNumber")
}


chisqUnif <- function(vec) {
  spgs::chisq.unif.test(x = vec, interval = range(vec))
}


skippedRounds <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    select(roundNumber, rewardAmount) %>%
    mutate(skip = roundNumber - lag(roundNumber, default = first(roundNumber) - 1) - 1)
}


nhoodBinStr <- function(overlay, depth = 8L) {
  numHexDigits <- ceiling(depth / 4) # 4: four bits = 1 hex digit
  str_sub(R.utils::intToBin(str_sub(overlay, 1, numHexDigits + 2)), 1, depth)
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
    summarise(winEvents = n(), totalReward = sum(rewardAmount)) %>%
    ungroup()
}


participationNhoodQuantileNull <- function(p, rounds, nhoods) {
  qbinom(p, size = rounds, prob = 1 / nhoods)
}


participationNhoodDistrNull <- function(x, rounds, nhoods) {
  dbinom(x, size = rounds, prob = 1 / nhoods)
}


nodesPerNhood <- function(dat) {
  dat %>%
    group_by(nhood) %>%
    count(overlay) %>%
    ungroup()
}


priceFig <- function(dat, maxPoints = 3001) {
  dat %>%
    filter(roundNumber %in% roundsToPlot(range(dat$roundNumber), maxPoints)) %>%
    ggplot(aes(x = roundNumber, y = `price (in units of the initial value)`)) +
    geom_line(colour = "steelblue") +
    labs(x = "round") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


roundsFig <- function(dat) {
  dat %>%
    ggplot(aes(x = roundNumber)) +
    geom_rug(colour = "steelblue", alpha = 0.6) +
    geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, bins = 30) +
    labs(x = "round", y = "no. of skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


rewardDistrFig <- function(dat, log.y = TRUE) {
  plt <- dat %>%
    ggplot(aes(x = rewardAmount, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100, position = "stack") +
    scale_x_log10(name = "reward") +
    scale_fill_manual(values = rcartocolor::carto_pal(name = "Safe"),
                      name = "skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  ymax <- layer_scales(plt, 1, 1)$y$range$range[2]
  if (log.y) plt + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                                      breaks = 10^(0:ceiling(log10(ymax)))) else plt
}


participationNhoodHistFig <- function(dat) {
  rounds <- length(unique(dat$roundNumber))
  nhoods <- length(unique(dat$nhood[!is.na(dat$nhood)]))
  dat %>%
    rewardNhoodDistr() %>%
    count(winEvents, name = "observed") %>%
    mutate(predicted = participationNhoodDistrNull(winEvents, rounds, nhoods) *
             sum(observed)) %>%
    pivot_longer(cols = !winEvents) %>%
    ggplot(aes(x = winEvents, y = value, colour = name, fill = name)) +
    geom_col(alpha = 0.2, position = "identity") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    labs(x = "number of win events", y = "number of nhoods with given wins") +
    theme_bw(base_size = 16)
}


participationNhoodQuantileFig <- function(dat) {
  rounds <- length(unique(dat$roundNumber))
  nhoods <- length(unique(dat$nhood[!is.na(dat$nhood)]))
  dat %>%
    rewardNhoodDistr() %>%
    arrange(winEvents) %>%
    rowid_to_column("rank") %>%
    left_join(tibble(
      rank = 1:nhoods,
      predict = participationNhoodQuantileNull(seq(0.1, 0.99, l=nhoods), rounds, nhoods)
    ), by = "rank") %>%
    pivot_longer(cols = c(winEvents, predict)) %>%
    mutate(name = recode(name, "winEvents" = "observed", "predict" = "predicted")) %>%
    ggplot(aes(x = rank, y = value, colour = name)) +
    geom_step() +
    labs(x = "neighbourhoods", y = "number of win events") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


rewardNhoodFig <- function(dat) {
  rounds <- length(unique(dat$roundNumber))
  nhoods <- length(unique(dat$nhood[!is.na(dat$nhood)]))
  dat %>%
    rewardNhoodDistr() %>%
    rename(observed = totalReward) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    #mutate(predicted = participationNhoodQuantileNull(seq(0.1, 0.99, l=nrow(.)), rounds,
    #                                                  nhoods) * mean(observed)) %>%
    #pivot_longer(cols = c(observed, predicted)) %>%
    ggplot(aes(x = rank, y = observed)) +
    geom_step(colour = "steelblue") +
    labs(x = "neighbourhoods", y = "sum of rewards") +
    #scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


nodesPerNhoodFig <- function(dat, log.y = TRUE) {
  dat %>%
    nodesPerNhood() %>%
    count(nhood) %>%
    arrange(n) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = n)) +
    geom_step(colour = "steelblue") +
    labs(x = "neighbourhoods", y = "number of nodes") +
    { if (log.y) scale_y_log10() else scale_y_continuous() } +
    theme_bw(base_size = 16)
}


revealersPerNhoodFig <- function(dat) {
  dat %>%
    select(roundNumber, event, nhood) %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(`inaccurate revealers` = `number of revealers` - `honest revealers`) %>%
    group_by(nhood) %>%
    summarise(mh = mean(`honest revealers`), md = mean(`inaccurate revealers`)) %>%
    ungroup() %>%
    arrange(mh) %>%
    rowid_to_column("rank") %>%
    pivot_longer(cols = c(mh, md)) %>%
    mutate(`revealer type` = recode(name, "mh" = "honest", "md" = "inaccurate")) %>%
    ggplot(aes(x = rank, y = value, colour = `revealer type`)) +
    geom_step() +
    labs(x = "neighbourhoods", y = "mean number of revealers") +
    scale_colour_manual(values = c("steelblue", "goldenrod")) +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}
