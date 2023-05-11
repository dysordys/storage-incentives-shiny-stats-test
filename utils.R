`%ni%` <- function(x, y) !(x %in% y)


adjustPrice <- function(currentPrice, honestRevealers) {
  minimumPrice <- 2^10
  v <- 0.00294
  targetRedundancy <- 4
  maxConsideredExtraRedundancy <- 4
  usedRedundancy <- min(honestRevealers, targetRedundancy + maxConsideredExtraRedundancy)
  max(currentPrice * 2^(v * (targetRedundancy - usedRedundancy)), minimumPrice)
}


accumPrice <- function(redundancyVec, initPrice) {
  Reduce(function(x, y) adjustPrice(x, y), redundancyVec, init = initPrice,
         accumulate = TRUE)[-1]
}


isValidRoundRange <- function(minRound, maxRound) {
  (length(minRound) == 1) && (length(maxRound) == 1) && (minRound <= maxRound)
}


restrictRounds <- function(dat, minRound, maxRound) {
  if (isValidRoundRange(minRound, maxRound))
    filter(dat, roundNumber <= round(minRound) & roundNumber >= round(maxRound)) else dat
}


restrictDate <- function(dat, minDate, maxDate) {
  filter(dat, date >= minDate & date <= maxDate)
}


depthFilter <- function(dat, .depth, defaultDepth = 8L) {
  filter(dat, depth == { if (length(.depth) == 1L) .depth else defaultDepth })
}


restrictDepth <- function(dat, depths) {
  dat %>%
    group_by(roundNumber) %>%
    filter(length(intersect(depths, depth)) > 0) %>%
    ungroup()
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
    mutate(honest = (reserveCommitmentHash == reserveCommitmentHash[event == "won"])) %>%
    filter(event == "revealed") %>%
    summarise(revealers = n(), honest = sum(honest), .groups = "drop")
}


reveals <- function(dat) {
  dat %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    filter(event == "revealed") %>%
    group_by(roundNumber) %>%
    count(reserveCommitmentHash) %>%
    ungroup()
}


revealStats <- function(dat, index = invsimpson) {
  dat %>%
    reveals() %>%
    group_by(roundNumber) %>%
    summarise(totalReveals = sum(n),
              majorityReveals = max(n),
              majorityFraction = majorityReveals / totalReveals,
              richness = n(),
              diversity = index(n),
              .groups = "drop")
}


revealerNhoodSummary <- function(dat, .f = mean, .depth = 8L) {
  dat %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(inaccurate = revealers - honest) %>%
    depthFilter(.depth) %>%
    group_by(nhood) %>%
    summarise(honest = .f(honest), inaccurate = .f(inaccurate), .groups = "drop")
}


pricePerRound <- function(dat, initPrice = 2048) {
  tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
    left_join(revealersPerRound(dat), by = "roundNumber") %>%
    mutate(honest = replace_na(honest, 0)) %>%
    mutate(price = accumPrice(honest, initPrice))
}


priceTab <- function(dat) {
  dat %>%
    pricePerRound() %>%
    mutate(inaccurate = revealers - honest) %>%
    mutate(price = price / first(price))
}


missedRounds <- function(dat) {
  tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
    anti_join(distinct(select(dat, roundNumber)), by = "roundNumber")
}


inaccurateRevealerFrequency <- function(dat) {
  tab <- dat %>% pricePerRound()
  rounds <- nrow(tab)
  roundsWithInaccurates <- nrow(filter(tab, revealers != honest))
  list(rounds = rounds, n = roundsWithInaccurates, p = roundsWithInaccurates / rounds)
}


skippedRounds <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    select(roundNumber, rewardAmount) %>%
    mutate(skip = roundNumber - lag(roundNumber, default = first(roundNumber) - 1L) - 1L)
}


skippedRoundDistr <- function(dat) {
  dat %>%
    skippedRounds() %>%
    count(skip)
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
    filter(!is.na(nhood)) %>%
    group_by(depth, nhood) %>%
    summarise(winEvents = n(),
              totalReward = sum(rewardAmount, na.rm = TRUE),
              totalStake = sum(stake),
              .groups = "drop")
}


nodesPerNhood <- function(dat) {
  dat %>%
    filter(!is.na(nhood)) %>%
    group_by(depth, nhood) %>%
    count(overlay) %>%
    ungroup() %>%
    count(depth, nhood, name = "nodes")
}


stakedNodesPerNhood <- function(dat) {
  dat %>%
    filter(!is.na(nhood)) %>%
    filter(stake > 0) %>%
    count(depth, nhood, name = "stakedNodes")
}


rewardPerNode <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    group_by(rewardTo) %>%
    summarise(reward = sum(rewardAmount), .groups = "drop")
}


depthDistr <- function(dat) {
  count(dat, depth)
}


depths <- function(dat) {
  depthDistr(dat) %>%
    filter(depth > 0) %>%
    pull(depth)
}


depthsWith <- function(dat, .f = rewardNhoodDistr) {
  .f(dat) %>%
    pull(depth) %>%
    unique() %>%
    sort()
}


nhoods <- function(dat) {
  length(unique(dat$nhood[!is.na(dat$nhood)]))
}


nhoodList <- function(dat, .depth, na.rm = TRUE) {
  dat %>%
    depthFilter(.depth) %>%
    filter(if (na.rm) !is.na(nhood) else TRUE) %>%
    select(nhood) %>%
    distinct() %>%
    arrange(nhood) %>%
    mutate(nhoodBin = R.utils::intToBin(nhood)) %>%
    mutate(nhoodHex = R.utils::intToHex(nhood)) %>%
    mutate(nhoodOut = str_c(nhoodBin, " / 0x", nhoodHex)) %>%
    pull(nhoodOut)
}


nodes <- function(dat) {
  dat %>%
    nodesPerNhood() %>%
    pull(nodes) %>%
    sum()
}


rounds <- function(dat) {
  length(unique(dat$roundNumber[!is.na(dat$roundNumber)]))
}


nodeNhoodDistr <- function(dat, .depth) {
  dat %>%
    nodesPerNhood() %>%
    depthFilter(.depth) %>%
    count(nodes)
}


winsNodesByNhood <- function(dat) {
  nodesPerNhood(dat) %>%
    full_join(rewardNhoodDistr(dat), by = c("depth", "nhood"))
}


sortNhoodBy <- function(datByNhood, .sortby) {
  datByNhood %>%
    arrange(if (length(substitute(.sortby)) > 0) {{.sortby}} else NA) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank))
}


roundsWithoutWinner <- function(dat) {
  dat %>%
    group_by(roundNumber) %>%
    summarise(winner = length(event[event == "won"]), .groups = "drop") %>%
    filter(winner == 0) %>%
    pull(roundNumber)
}


roundsWithDepthMismatch <- function(dat) {
  dat %>%
    filter(event != "committed") %>%
    group_by(roundNumber) %>%
    mutate(wdepth = depth[event == "won"]) %>%
    mutate(eq = depth != wdepth) %>%
    summarise(diff = sum(eq), .groups = "drop") %>%
    filter(diff != 0) %>%
    pull(roundNumber)
}


shannon <- function(vec) {
  p <- vec / sum(vec)
  sum(-p[p > 0] * log(p[p > 0]))
}


invsimpson <- function(vec) {
  p <- vec / sum(vec)
  1 / sum(p^2)
}


frozenNodes <- function(dat) {
  dat %>%
    unnest(stakeFrozen) %>%
    filter(!is.na(slashed)) %>%
    select(roundNumber, depth, nhood, slashed) %>%
    rename(frozen = slashed)
}


countFrozenNodes <- function(dat) {
  dat %>%
    frozenNodes() %>%
    group_by(depth, nhood) %>%
    summarise(nFrozen = n(), .groups = "drop")
}


freezesThroughTime <- function(dat) {
  dat %>%
    frozenNodes() %>%
    group_by(roundNumber) %>%
    summarise(nFrozen = n(), .groups = "drop")
}


jsonNhoodTable <- function(jsonNhood) {
  jsonNhood %>%
    unnest(neighborhoods) %>%
    mutate(across(everything(), compose(list, unlist))) %>%
    pivot_longer(cols = everything(), names_to = "depth", values_to = "nhoods") %>%
    mutate(depth = strtoi(depth)) %>%
    mutate(nhoods = map(nhoods, ~tibble(nhood = names(.x), nodes = unname(.x)))) %>%
    unnest(nhoods) %>%
    mutate(nhood = str_remove(nhood, "0b")) %>% # Knock off starting "0b" from binary
    mutate(nhood = strtoi(nhood, base = 2)) %>% # Convert to decimal integer
    arrange(depth, nhood)
}
