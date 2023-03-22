priceFig <- function(dat, maxPoints = 3001) {
  dat %>%
    filter(roundNumber %in% roundsToPlot(range(dat$roundNumber), maxPoints)) %>%
    ggplot(aes(x = roundNumber, y = price)) +
    geom_line(colour = "steelblue") +
    labs(x = "round", y = "price (in units of the initial value)") +
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


rewardDistrFig <- function(dat, xrange = c(NA, NA), log.x = TRUE, log.y = TRUE) {
  plt <- dat %>%
    ggplot(aes(x = rewardAmount, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100, position = "stack") +
    { if (log.x) scale_x_log10(name = "reward", limits = xrange) else
      scale_x_continuous(name = "reward", limits = xrange)
    } +
    scale_fill_manual(values = rcartocolor::carto_pal(name = "Safe"),
                      name = "skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  ymax <- layer_scales(plt, 1, 1)$y$range$range[2]
  if (log.y) plt + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                                      breaks = 10^(0:ceiling(log10(ymax)))) else plt
}


participationNhoodHistFig <- function(dat) {
  dat %>%
    pivot_longer(cols = !winEvents) %>%
    ggplot(aes(x = winEvents, y = value, colour = name, fill = name, alpha = name)) +
    geom_col(position = "identity", na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_alpha_manual(name = NULL, values = c(0.2, 0.1)) +
    labs(x = "number of win events", y = "number of nhoods with given # of wins") +
    theme_bw(base_size = 16)
}


participationNhoodQuantileFig <- function(dat) {
  dat %>%
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
  dat %>%
    rename(observed = totalReward) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = observed)) +
    geom_step(colour = "steelblue") +
    labs(x = "neighbourhoods", y = "sum of rewards") +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


nodesPerNhoodHistFig <- function(dat) {
  dat %>%
    pivot_longer(cols = !num) %>%
    mutate(name = recode(name, "n" = "observed", "predict" = "predicted")) %>%
    ggplot(aes(x = num, y = value, colour = name, fill = name, alpha = name)) +
    geom_col(position = "identity", na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_fill_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    scale_alpha_manual(name = NULL, values = c(0.2, 0.1)) +
    labs(x = "number of nodes", y = "number of nhoods with given # of nodes") +
    theme_bw(base_size = 16)
}


nodesPerNhoodQuantileFig <- function(dat) {
  dat %>%
    pivot_longer(cols = c(n, predict)) %>%
    mutate(name = recode(name, "n" = "observed", "predict" = "predicted")) %>%
    ggplot(aes(x = rank, y = value, colour = name)) +
    geom_step() +
    labs(x = "neighbourhoods", y = "number of nodes") +
    scale_colour_manual(name = NULL, values = c("steelblue", "goldenrod")) +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


revealersPerNhoodFig <- function(dat, sortByHonest = TRUE) {
  dat %>%
    arrange(if (sortByHonest) honest else inaccurate) %>%
    rowid_to_column("rank") %>%
    pivot_longer(cols = c(honest, inaccurate), names_to = "revealer type") %>%
    ggplot(aes(x = rank, y = value, colour = `revealer type`)) +
    geom_line() +
    labs(x = "neighbourhoods", y = "mean number of revealers") +
    scale_colour_manual(values = c("steelblue", "goldenrod")) +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


stakesNhoodHistFig <- function(dat) {
  dat %>%
    rename(observed = totalStake) %>%
    ggplot(aes(x = observed)) +
    geom_histogram(colour = NA, fill = "steelblue", alpha = 0.8, bins = 80) +
    labs(x = "sum of stakes") +
    scale_x_log10() +
    theme_bw(base_size = 16)
}


stakesNhoodQuantileFig <- function(dat) {
  dat %>%
    rename(observed = totalStake) %>%
    arrange(observed) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = observed)) +
    geom_step(colour = "steelblue") +
    labs(x = "neighbourhoods", y = "sum of stakes") +
    scale_y_log10() +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


rewardPerNodeFig <- function(dat) {
  dat %>%
    arrange(reward) %>%
    rowid_to_column("rank") %>%
    ggplot(aes(x = rank, y = reward)) +
    geom_step(colour = "steelblue") +
    scale_x_continuous(name = "nodes (sorted in increasing order of total reward)") +
    scale_y_log10(name = "sum of rewards") +
    theme_bw(base_size = 16) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
}


depthDistrFig <- function(dat, log.y = TRUE) {
  dat %>%
    ggplot(aes(x = as_factor(depth), y = n)) +
    geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
    labs(x = "depth", y = "number of nodes") +
    { if (log.y) scale_y_log10() else scale_y_continuous() } +
    theme_bw(base_size = 16)
}
