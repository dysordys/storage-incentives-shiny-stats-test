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
  geom_vline(xintercept = xintercept, alpha = alpha, linetype = linetype)
}


priceFig <- function(dat, maxPoints = 3001) {
  yscale = max(dat$price) / max(dat$honest)
  dat %>%
    filter(roundNumber %in% roundsToPlot(range(dat$roundNumber), maxPoints)) %>%
    ggplot(aes(x = roundNumber)) +
    geom_line(aes(y = price), colour = "steelblue", alpha = 0.4) +
    geom_line(aes(y = honest * yscale), colour = "goldenrod", alpha = 0.2) +
    geom_smooth(aes(y = price), colour = "steelblue", se = FALSE) +
    geom_smooth(aes(y = honest * yscale), colour = "goldenrod", se = FALSE) +
    labs(x = "round", y = "relative price change (blue)") +
    scale_y_continuous(sec.axis = sec_axis(name = "honest revealers (yellow)",
                                           function(y) y / yscale,
                                           breaks = seq(0, max(dat$honest), by = 2))) +
    themeApp()
}


roundsFig <- function(dat) {
  dat %>%
    ggplot(aes(x = roundNumber)) +
    geom_rug(colour = "steelblue", alpha = 0.6) +
    geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, bins = 30) +
    labs(x = "round", y = "number of skipped rounds") +
    themeApp()
}


skippedRoundDistrFig <- function(dat) {
  dat %>%
    ggplot(aes(x = skip, y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "Number of consecutive rounds skipped", y = "Number of occurrences") +
    themeApp()
}


rewardDistrFig <- function(dat, xrange = c(NA, NA), xtrans = "Logarithmic",
                           ytrans = "Square-root transformed") {
  xScale <- function(dat, xtrans, xrange) {
    if (xtrans == "Logarithmic") {
      if (xrange[1] == 0) xrange[1] <- 0.9 * min(dat$rewardAmount)
      scale_x_log10(name = "reward (BZZ)", limits = xrange)
    } else {
      scale_x_continuous(name = "reward (BZZ)", limits = xrange)
    }
  }
  dat %>%
    ggplot(aes(x = rewardAmount, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100, position = "stack") +
    xScale(dat, xtrans, xrange) +
    scale_fill_manual(values = rcartocolor::carto_pal(name = "Safe"),
                      name = "skipped\nrounds") +
    { if (ytrans == "Square-root transformed") scale_y_sqrt() } +
    themeApp()
}


participationNhoodHistFig <- function(dat) {
  dat %>%
    mutate(winEvents = { if (max(winEvents)>8) winEvents else as_factor(winEvents) }) %>%
    pivot_longer(cols = !winEvents) %>%
    ggplot(aes(x = winEvents, y = value, colour = name, fill = name, alpha = name)) +
    geom_col(position = "identity", na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_alpha_manual(name = NULL, values = c(0.2, 0.1)) +
    labs(x = "number of win events", y = "number of nhoods with given # of wins") +
    themeApp(margin = rep(0.2, 4))
}


participationNhoodQuantileFig <- function(dat, highlightNhood = NA) {
  dat %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    pivot_longer(cols = c(winEvents, predict)) %>%
    mutate(name = recode(name, "winEvents" = "observed",
                         "predict" = "null expectation")) %>%
    mutate(name = fct_relevel(name, "observed")) %>%
    ggplot(aes(x = nhood, y = value, colour = name, group = name)) +
    geom_step() +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of win events") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    themeApp(nhood.x = TRUE)
}


rewardNhoodFig <- function(dat, highlightNhood = NA) {
  dat %>%
    rename(observed = totalReward) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = nhood, y = observed, group = 0)) +
    geom_step(colour = "steelblue") +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "sum of rewards (BZZ)") +
    themeApp(nhood.x = TRUE)
}


nodesPerNhoodHistFig <- function(dat) {
  dat %>%
    pivot_longer(cols = !num) %>%
    mutate(name = recode(name, "n" = "observed", "predict" = "null expectation")) %>%
    mutate(name = fct_relevel(name, "observed")) %>%
    ggplot(aes(x = as_factor(num), y = value,
               colour = name, fill = name, alpha = name)) +
    geom_col(position = "identity", na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_alpha_manual(name = NULL, values = c(0.2, 0.1)) +
    labs(x = "number of nodes", y = "number of nhoods with given # of nodes") +
    themeApp(margin = rep(0.2, 4))
}


nodesPerNhoodQuantileFig <- function(dat, highlightNhood = NA) {
  dat %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    pivot_longer(cols = c(n, predict)) %>%
    mutate(name = recode(name, "n" = "observed", "predict" = "null expectation")) %>%
    mutate(name = fct_relevel(name, "observed")) %>%
    ggplot(aes(x = nhood, y = value, colour = name, group = name)) +
    geom_step() +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of nodes") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    themeApp(nhood.x = TRUE)
}


winNodeNhoodFig <- function(dat, sortBy = "wins", highlightNhood = NA) {
  dat %>%
    mutate(rank = { if (sortBy == "wins") rankWin else rankNode }) %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    rename(`win events` = winEvents) %>%
    pivot_longer(cols = c(nodes, `win events`)) %>%
    ggplot(aes(x = nhood, y = value, colour = name, group = name)) +
    geom_point(alpha = 0.5) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of nodes / wins") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod"),
                        guide = guide_legend(override.aes = list(alpha = 1))) +
    themeApp(nhood.x = TRUE)
}


revealersPerNhoodFig <- function(dat, sortBy = "Honest revealers", ylab = NA,
                                 highlightNhood = NA) {
  dat %>%
    arrange(if (sortBy == "Honest revealers") honest else
      if (sortBy == "Inaccurate revealers") inaccurate else NA) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    pivot_longer(cols = c(honest, inaccurate), names_to = "revealer type") %>%
    ggplot(aes(x = nhood, y = value, colour = `revealer type`, fill = `revealer type`)) +
    geom_col(alpha = 0.5) +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = ylab) +
    scale_colour_manual(values = c("steelblue", "goldenrod")) +
    scale_fill_manual(values = c("steelblue", "goldenrod")) +
    themeApp(nhood.x = TRUE)
}


stakesNhoodHistFig <- function(dat) {
  dat %>%
    rename(observed = totalStake) %>%
    ggplot(aes(x = observed)) +
    geom_histogram(colour = NA, fill = "steelblue", alpha = 0.8, bins = 80) +
    scale_x_log10(name = "sum of stakes (BZZ)") +
    themeApp()
}


stakesNhoodQuantileFig <- function(dat, highlightNhood = NA) {
  dat %>%
    rename(observed = totalStake) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = nhood, y = observed, group = 0)) +
    geom_step(colour = "steelblue") +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "sum of stakes (BZZ)") +
    scale_y_log10() +
    themeApp(nhood.x = TRUE)
}


rewardPerNodeFig <- function(dat) {
  dat %>%
    arrange(reward) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = reward)) +
    geom_step(colour = "steelblue") +
    scale_x_continuous(name = "nodes (in increasing order of total reward)") +
    scale_y_log10(name = "sum of rewards (BZZ)") +
    themeApp() +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


depthDistrFig <- function(dat, log.y = "Logarithmic y-axis") {
  dat %>%
    ggplot(aes(x = as_factor(depth), y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "depth", y = "number of nodes") +
    { if (log.y == "Logarithmic y-axis") scale_y_log10() else scale_y_continuous() } +
    themeApp()
}


stakedNodesFig <- function(dat, highlightNhood = NA) {
  dat %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = nhood, y = stakedNodes, group = 0)) +
    geom_step(colour = "steelblue") +
    nhoodHighlight(xintercept = highlightNhood) +
    labs(x = "neighbourhood", y = "number of staked nodes") +
    themeApp(nhood.x = TRUE)
}
