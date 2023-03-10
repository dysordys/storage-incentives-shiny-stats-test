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
    filter(dat, round %in% reduce(round(roundRange), `:`)) else dat
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
    group_by(round) %>%
    mutate(honest = (id == id[event == "won"])) %>%
    filter(event == "revealed") %>%
    summarise(`number of revealers` = n(),
              `honest revealers` = sum(honest)) %>%
    ungroup()
}


pricePerRound <- function(dat, initPrice = 1024) {
  dat %>%
    revealersPerRound() %>%
    mutate(price = accumPrice(`honest revealers`, initPrice)) %>%
    transmute(round, price, `number of revealers`,
              `inaccurate revealers` = `number of revealers` - `honest revealers`)
}


missedRounds <- function(dat) {
  tibble(round = min(dat$round):max(dat$round)) %>%
    anti_join(distinct(select(dat, round)), by = "round")
}


skippedRounds <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    select(round, reward) %>%
    mutate(skip = round - lag(round, default = first(round) - 1) - 1)
}


nhoodBinStr <- function(overlay, depth = 8L) {
  nibble <- 4L
  numHexDigits <- ceiling(depth / nibble)
  str_sub(R.utils::intToBin(str_sub(overlay, 1, numHexDigits + 2)), 1, depth)
}


nhoodDec <- function(overlay, depth = 8L) {
  strtoi(nhoodBinStr(overlay, depth), base = 2)
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
    summarise(n = n(), totalReward = sum(reward)) %>%
    ungroup()
}


participationNhoodQuantileNull <- function(p, rounds, nhoods) {
  rev(qbinom(p, size = rounds, prob = 1 / nhoods))
}


participationNhoodDistrNull <- function(x, rounds, nhoods) {
  dbinom(x, size = rounds, prob = 1 / nhoods)
}


participationNhoodRand <- function(nhoods, rounds) {
  sample.int(n = nhoods, size = rounds, replace = TRUE) %>%
    table() %>%
    as_tibble() %>%
    rename(nhood = ".")
}


priceFig <- function(dat, initPrice = 1024, maxPoints = 3001) {
  dat %>%
    filter(round %in% roundsToPlot(range(dat$round), maxPoints)) %>%
    ggplot(aes(x = round, y = price)) +
    geom_line(colour = "steelblue") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


roundsFig <- function(dat) {
  dat %>%
    ggplot(aes(x = round)) +
    geom_rug(colour = "steelblue", alpha = 0.6) +
    geom_density(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(y = "density of skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


rewardDistrFig <- function(dat, log.y = TRUE) {
  plt <- dat %>%
    ggplot(aes(x = reward, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100) +
    scale_x_log10(name = "reward") +
    scale_fill_discrete(name = "skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  ymax <- layer_scales(plt, 1, 1)$y$range$range[2]
  if (log.y) plt + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                                      breaks = 10^(0:ceiling(log10(ymax)))) else plt
}


participationNhoodHistFig <- function(dat) {
  rounds <- length(unique(dat$round))
  nhoods <- length(unique(dat$nhood[!is.na(dat$nhood)]))
  particip <- dat %>% rewardNhoodDistr()
  nmax <- max(particip$n)
  particip %>%
    ggplot() +
    geom_bar(aes(x = n), colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    geom_col(data = tibble(
      n = 1:nmax,
      pred = participationNhoodDistrNull(n, rounds, nhoods) * rounds / 9) %>%
        filter(pred > 1e-6),
      aes(x = n, y = pred), colour = "firebrick", fill = "firebrick", alpha = 0.2
    ) +
    theme_bw()
}


rewardNhoodQuantileFig <- function(dat) {
  rounds <- length(unique(dat$round))
  nhoods <- length(unique(dat$nhood[!is.na(dat$nhood)]))
  dat %>%
    rewardNhoodDistr() %>%
    arrange(desc(n)) %>%
    rowid_to_column("rank") %>%
    left_join(tibble(
      rank = 1:nhoods,
      predict = participationNhoodQuantileNull(seq(0.1, 0.99, l=nhoods), rounds, nhoods)
    ), by = "rank") %>%
    ggplot(aes(x = rank)) +
    geom_col(aes(y = n), fill = "steelblue", colour = "steelblue", alpha = 0.2) +
    geom_step(aes(y = predict)) +
    labs(x = "neighbourhood ID", y = "number of win events") +
    theme_bw() +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


rewardNhoodQuantileRandFig <- function(nhoods, rounds) {
  participationNhoodRand(nhoods, rounds) %>%
    arrange(desc(n)) %>%
    rowid_to_column("rank") %>%
    left_join(tibble(
      rank = 1:nhoods,
      predict = participationNhoodNull(seq(0.1, 0.99, l = nhoods), rounds, nhoods)
    ), by = "rank") %>%
    ggplot(aes(x = rank)) +
    geom_col(aes(y = n), fill = "steelblue", colour = "steelblue", alpha = 0.2) +
    geom_step(aes(y = predict)) +
    labs(x = "neighbourhood ID", y = "number of win events") +
    theme_bw() +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}
