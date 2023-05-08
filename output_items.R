themeApp <- function(size = 18, margin = c(0.2, 2, 0.2, 0.2), nhood.x = FALSE) {
  theme_bw(base_size = size) +
    { if (nhood.x) {
      theme(axis.text.x = element_text(size = 8, family = "mono",
                                       angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top", plot.margin = unit(margin, "cm"),
            panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
    } else {
      theme(plot.margin = unit(margin, "cm"))
    }
    }
}


nhoodHighlight <- function(xintercept = NA, alpha = 0.5, linetype = "dashed", ...) {
  geom_vline(xintercept = str_remove(xintercept, pattern = " /.*"),
             alpha = alpha, linetype = linetype)
}


revealersPerNhoodFig <- function(dat, .depth, .f = mean, sortBy = "Honest revealers",
                                 highlightNhood = NA) {
  fname <- case_when(identical(.f,mean) ~ "mean", identical(.f,sum) ~ "total", TRUE ~ "")
  ylab <- str_c(fname, " number of revealers")
  dat %>%
    restrictDepth(.depth) %>%
    revealerNhoodSummary(.f, .depth) %>%
    filter(!is.na(nhood)) %>%
    sortNhoodBy(if (sortBy == "Honest revealers") honest else
      if (sortBy == "Inaccurate revealers") inaccurate else NA) %>%
    pivot_longer(cols = c(honest, inaccurate), names_to = "revealer type") %>%
    ggplot(aes(x = nhood, y = value, colour = `revealer type`, fill = `revealer type`)) +
    geom_point(alpha = 0.75, size = 2) +
    geom_col(colour = NA, alpha = 0.2, position = "identity") +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = ylab) +
    scale_colour_manual(values = c("steelblue", "goldenrod")) +
    scale_fill_manual(values = c("steelblue", "goldenrod"),
                      guide = guide_legend(override.aes = list(alpha = 1))) +
    themeApp(nhood.x = TRUE)
}


priceFig <- function(dat, maxPoints = 10001) {
  pdat <- priceTab(dat)
  yscale <- max(pdat$price) / max(pdat$honest)
  pdat %>%
    # Restrict the number of points to plot on the graph:
    #filter(roundNumber %in% roundsToPlot(range(pdat$roundNumber), maxPoints)) %>%
    ggplot(aes(x = roundNumber)) +
    geom_line(aes(y = price), colour = "steelblue", alpha = 0.4) +
    geom_line(aes(y = honest * yscale), colour = "goldenrod", alpha = 0.2) +
    geom_smooth(aes(y = price), colour = "steelblue", se = FALSE) +
    geom_smooth(aes(y = honest * yscale), colour = "goldenrod", se = FALSE) +
    labs(x = "round", y = "relative price change (blue)") +
    scale_y_continuous(sec.axis = sec_axis(name = "honest revealers (yellow)",
                                           function(y) y / yscale,
                                           breaks = seq(0, max(pdat$honest), by = 2))) +
    themeApp()
}


inaccurateStats <- function(dat) {
  s <- inaccurateRevealerStats(dat)
  str_c("Rounds with inaccurate revealers: ", s$n, " out of ", s$rounds,
        ", or ", round(100 * s$p, 2), "%")
}


revealCommitTab <- function(dat, inaccFilt = "") {
  dat %>%
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
              `commits` = committed,
              `reveals` = revealed,
              `inaccurate reveals` = inaccurate,
              `commits without reveal` = commitNoReveal,
              `reveals without commit` = revealNoCommit)
}


numSkipped <- function(dat) {
  roundsMissed <- nrow(missedRounds(dat))
  totalRounds <- max(dat$roundNumber) - min(dat$roundNumber) + 1
  str_c("Skipped rounds: ", roundsMissed, " out of ", totalRounds,
        ", or ", round(100 * roundsMissed / totalRounds, 2), "%")
}


roundsFig <- function(dat) {
  dat %>%
    missedRounds() %>%
    ggplot(aes(x = roundNumber)) +
    geom_rug(colour = "steelblue", alpha = 0.6) +
    geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, bins = 30) +
    labs(x = "round", y = "number of skipped rounds") +
    themeApp()
}


skippedRoundsTab <- function(dat) {
  missedRounds(dat) %>%
    rename(`List of skipped rounds:` = roundNumber)
}


skippedRoundDistrTab <- function(dat) {
  dat %>%
    skippedRoundDistr() %>%
    filter(skip > 0) %>%
    rename(`Skipped in a row` = skip, `count` = n)
}


skippedRoundDistrFig <- function(dat) {
  dat %>%
    skippedRoundDistr() %>%
    filter(skip > 0) %>%
    ggplot(aes(x = skip, y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "Number of consecutive rounds skipped", y = "Number of occurrences") +
    themeApp()
}


rewardDistrFig <- function(dat, xtrans = "Logarithmic",
                           ytrans = "Square-root transformed") {
  dat %>%
    skippedRounds() %>%
    mutate(skip = as_factor(skip)) %>%
    ggplot(aes(x = rewardAmount, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100, position = "stack") +
    { if (xtrans == "Logarithmic") scale_x_log10() } +
    { if (ytrans == "Square-root transformed") scale_y_sqrt() } +
    labs(x = "reward (BZZ)") +
    scale_fill_manual(values = rcartocolor::carto_pal(name = "Safe"),
                      name = "skipped\nrounds") +
    themeApp()
}


winNhoodHistFig <- function(dat, .depth) {
  dat %>%
    rewardNhoodDistr() %>%
    depthFilter(.depth) %>%
    select(nhood, winEvents) %>%
    count(winEvents) %>%
    ggplot(aes(x = winEvents, y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2,
             position = "identity", na.rm = TRUE) +
    labs(x = "number of win events", y = "number of nhoods with given # of wins") +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    themeApp(margin = rep(0.2, 4))
}


winNhoodQuantileFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    rewardNhoodDistr() %>%
    depthFilter(.depth) %>%
    sortNhoodBy(winEvents) %>%
    ggplot(aes(x = nhood, y = winEvents)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of win events") +
    themeApp(nhood.x = TRUE)
}


rewardNhoodFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    rewardNhoodDistr() %>%
    depthFilter(.depth) %>%
    sortNhoodBy(totalReward) %>%
    ggplot(aes(x = nhood, y = totalReward)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "sum of rewards (BZZ)") +
    themeApp(nhood.x = TRUE)
}


rewardPerNodeFig <- function(dat) {
  dat %>%
    rewardPerNode() %>%
    arrange(reward) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = reward)) +
    geom_step(colour = "steelblue") +
    scale_x_continuous(name = "nodes (in increasing order of total reward)") +
    scale_y_log10(name = "sum of rewards (BZZ)") +
    themeApp() +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


nodesPerNhoodQuantileFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    nodesPerNhood() %>%
    depthFilter(.depth) %>%
    sortNhoodBy(nodes) %>%
    ggplot(aes(x = nhood, y = nodes)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of nodes") +
    themeApp(nhood.x = TRUE)
}


depthTab <- function(dat) {
  depthDistr(dat) %>%
    filter(depth > 0) %>%
    rename(`number of nodes` = n)
}


depthDistrFig <- function(dat, log.y = "Logarithmic") {
  dat %>%
    depthDistr() %>%
    filter(depth > 0) %>%
    ggplot(aes(x = as_factor(depth), y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "depth", y = "number of nodes") +
    { if (log.y == "Logarithmic") scale_y_log10() else scale_y_continuous() } +
    themeApp()
}


winNodeNhoodFig <- function(dat, .depth, sortBy = "wins", highlightNhood = NA) {
  dat %>%
    winsNodesByNhood() %>%
    depthFilter(.depth) %>%
    arrange({ if (sortBy == "wins") winEvents else nodes }) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    rename(`win events` = winEvents) %>%
    pivot_longer(cols = c(nodes, `win events`)) %>%
    mutate(name = fct_relevel(name, "win events")) %>%
    ggplot(aes(x = nhood, y = value, colour = name, fill = name)) +
    geom_point(alpha = 0.75, size = 2) +
    geom_col(colour = NA, alpha = 0.2, position = "identity") +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of nodes / wins") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod"),
                        guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    themeApp(nhood.x = TRUE)
}


nodesPerNhoodHistFig <- function(dat, .depth) {
  dat %>%
    nodeNhoodDistr(.depth) %>%
    ggplot(aes(x = nodes, y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2,
             position = "identity", na.rm = TRUE) +
    labs(x = "number of nodes", y = "number of nhoods with given # of nodes") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    themeApp(margin = rep(0.2, 4))
}


stakesNhoodQuantileFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    rewardNhoodDistr() %>%
    depthFilter(.depth) %>%
    sortNhoodBy(totalStake) %>%
    ggplot(aes(x = nhood, y = totalStake)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "sum of stakes (BZZ)") +
    scale_y_log10() +
    themeApp(nhood.x = TRUE)
}


stakesNhoodHistFig <- function(dat, .depth) {
  dat %>%
    rewardNhoodDistr() %>%
    depthFilter(.depth) %>%
    ggplot(aes(x = totalStake)) +
    geom_histogram(colour = NA, fill = "steelblue", alpha = 0.8, bins = 80) +
    scale_x_log10(name = "sum of stakes (BZZ)") +
    themeApp()
}


stakedNodesFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    stakedNodesPerNhood() %>%
    depthFilter(.depth) %>%
    sortNhoodBy(stakedNodes) %>%
    ggplot(aes(x = nhood, y = stakedNodes)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of staked nodes") +
    themeApp(nhood.x = TRUE)
}


frozenNodesFig <- function(dat, .depth, highlightNhood = NA) {
  dat %>%
    depthFilter(.depth) %>%
    countFrozenNodes() %>%
    sortNhoodBy(nFrozen) %>%
    ggplot(aes(x = nhood, y = nFrozen)) +
    geom_point(colour = "steelblue", alpha = 0.75, size = 2) +
    geom_col(colour = NA, fill = "steelblue", alpha = 0.2) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "cumulative number of frozen nodes") +
    themeApp(nhood.x = TRUE)
}


freezesThroughTimeFig <- function(dat) {
  dat %>%
    freezesThroughTime() %>%
    ggplot(aes(x = roundNumber, y = nFrozen)) +
    geom_line(colour = "steelblue", alpha = 0.4) +
    geom_smooth(colour = "steelblue", se = FALSE) +
    labs(x = "round", y = "number of freezing events") +
    themeApp()
}
