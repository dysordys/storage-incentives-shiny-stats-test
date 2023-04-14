themeApp <- function(size = 18, margin = c(0.2, 2, 0.2, 0.2), nhood.y = FALSE) {
  theme_bw(base_size = size) +
    { if (nhood.y) {
      theme(axis.text.y = element_text(size = 9, family = "mono"),
            legend.position = "top", plot.margin = unit(margin, "cm"),
            panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())
    } else {
      theme(plot.margin = unit(margin, "cm"))
    }
    }
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


rewardDistrFig <- function(dat, xrange = c(NA, NA),
                           log.x = "Logarithmic", log.y = "Pseudo-logarithmic") {
  xScale <- function(dat, log.x, xrange) {
    if (log.x == "Logarithmic") {
      if (xrange[1] == 0) xrange[1] <- 0.9 * min(dat$rewardAmount)
      scale_x_log10(name = "reward (BZZ)", limits = xrange, labels = scales::label_log())
    } else {
      scale_x_continuous(name = "reward (BZZ)", limits = xrange)
    }
  }
  plt <- dat %>%
    ggplot(aes(x = rewardAmount, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100, position = "stack") +
    xScale(dat, log.x, xrange) +
    scale_fill_manual(values = rcartocolor::carto_pal(name = "Safe"),
                      name = "skipped\nrounds") +
    themeApp()
  ymax <- layer_scales(plt, 1, 1)$y$range$range[2]
  if (log.y == "Pseudo-logarithmic") {
    plt + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                             breaks = 10^(0:ceiling(log10(ymax))))
  } else {
    plt
  }
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
    ggplot(aes(x = value, y = nhood, colour = name, group = name)) +
    geom_step() +
    geom_hline(yintercept = highlightNhood, alpha = 0.5, linetype = "dashed") +
    labs(x = "number of win events", y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    themeApp(nhood.y = TRUE)
}


rewardNhoodFig <- function(dat) {
  dat %>%
    rename(observed = totalReward) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = observed, y = nhood, group = 0)) +
    geom_step(colour = "steelblue") +
    labs(x = "sum of rewards (BZZ)", y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    themeApp(nhood.y = TRUE)
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


nodesPerNhoodQuantileFig <- function(dat) {
  dat %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    pivot_longer(cols = c(n, predict)) %>%
    mutate(name = recode(name, "n" = "observed", "predict" = "null expectation")) %>%
    mutate(name = fct_relevel(name, "observed")) %>%
    ggplot(aes(x = value, y = nhood, colour = name, group = name)) +
    geom_step() +
    labs(x = "number of nodes", y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    themeApp(nhood.y = TRUE)
}


winNodeNhoodFig <- function(dat, sortBy = "wins") {
  dat %>%
    mutate(rank = { if (sortBy == "wins") rankWin else rankNode }) %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    rename(`win events` = winEvents) %>%
    pivot_longer(cols = c(nodes, `win events`)) %>%
    ggplot(aes(x = value, y = nhood, colour = name, group = name)) +
    geom_point(alpha = 0.5) +
    labs(x = "number of nodes / wins", y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod"),
                        guide = guide_legend(override.aes = list(alpha = 1))) +
    themeApp(nhood.y = TRUE)
}


revealersPerNhoodFig <- function(dat, sortBy = "Honest revealers", xlab = NA,
                                 highlightNhood = NA) {
  dat %>%
    arrange(if (sortBy == "Honest revealers") honest else
      if (sortBy == "Inaccurate revealers") inaccurate else NA) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    pivot_longer(cols = c(honest, inaccurate), names_to = "revealer type") %>%
    ggplot(aes(x = value, y = nhood, colour = `revealer type`, fill = `revealer type`)) +
    geom_col(alpha = 0.5) +
    geom_hline(yintercept = highlightNhood, alpha = 0.5, linetype = "dashed") +
    labs(x = xlab, y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_colour_manual(values = c("steelblue", "goldenrod")) +
    scale_fill_manual(values = c("steelblue", "goldenrod")) +
    themeApp(nhood.y = TRUE)
}


stakesNhoodHistFig <- function(dat) {
  dat %>%
    rename(observed = totalStake) %>%
    ggplot(aes(x = observed)) +
    geom_histogram(colour = NA, fill = "steelblue", alpha = 0.8, bins = 80) +
    scale_x_log10(name = "sum of stakes (BZZ)", labels = scales::label_log()) +
    themeApp()
}


stakesNhoodQuantileFig <- function(dat) {
  dat %>%
    rename(observed = totalStake) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = observed, y = nhood, group = 0)) +
    geom_step(colour = "steelblue") +
    labs(x = "sum of stakes (BZZ)", y = "neighbourhood") +
    scale_x_log10(expand = expansion(mult = c(0.01, 0.05)),
                  labels = scales::label_log()) +
    themeApp(nhood.y = TRUE)
}


rewardPerNodeFig <- function(dat) {
  dat %>%
    arrange(reward) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = reward, y = rank)) +
    geom_step(colour = "steelblue") +
    scale_x_log10(name = "sum of rewards (BZZ)", labels = scales::label_log()) +
    scale_y_continuous(name = "nodes (in increasing order of total reward)") +
    themeApp()
}


depthDistrFig <- function(dat, log.y = "Logarithmic y-axis") {
  dat %>%
    ggplot(aes(x = as_factor(depth), y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "depth", y = "number of nodes") +
    { if (log.y == "Logarithmic y-axis") scale_y_log10() else scale_y_continuous() } +
    themeApp()
}


stakedNodesFig <- function(dat) {
  dat %>%
    mutate(nhood = fct_reorder(R.utils::intToBin(nhood), rank)) %>%
    ggplot(aes(x = stakedNodes, y = nhood, group = 0)) +
    geom_step(colour = "steelblue") +
    labs(x = "number of staked nodes", y = "neighbourhood") +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    themeApp(nhood.y = TRUE)
}
