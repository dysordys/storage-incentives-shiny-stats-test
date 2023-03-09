source("utils.R")


priceFig <- function(dat, initPrice = 1024, roundRange = NA) {
  dat %>%
    pricePerRound(initPrice) %>%
    restrictRounds(roundRange) %>%
    ggplot(aes(x = round, y = price)) +
    geom_line(colour = "steelblue") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


missedRoundsFig <- function(dat, roundRange = NA) {
  dat %>%
    missedRounds() %>%
    restrictRounds(roundRange) %>%
    ggplot(aes(x = round, y = 0)) +
    geom_point(colour = "steelblue", alpha = 0.7) +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


rewardDistrFig <- function(dat, log.y = TRUE, roundRange = NA) {
  plt <- dat %>%
    skippedRounds() %>%
    mutate(skip = as_factor(skip)) %>%
    restrictRounds(roundRange) %>%
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
