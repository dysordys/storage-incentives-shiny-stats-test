source("utils.R")


revealerPerRoundFig <- function(dat, initPrice = 1024, roundRange = NA) {
  dat %>%
    revealersPerRound(initPrice, roundRange) %>%
    ggplot(aes(x = round, y = price)) +
    geom_line(colour = "steelblue") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


missedRoundsFig <- function(dat, roundRange = NA) {
  dat %>%
    missedRounds(roundRange) %>%
    ggplot(aes(x = round, y = 0)) +
    geom_point(colour = "steelblue", alpha = 0.7) +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


rewardDistrFig <- function(dat, log.y = TRUE, roundRange = NA) {
  dat %>%
    skippedRounds() %>%
    mutate(skip = as_factor(skip)) %>%
    restrictRounds(roundRange) %>%
    ggplot(aes(x = reward, fill = skip)) +
    geom_histogram(colour = NA, alpha = 0.8, bins = 100) +
    scale_x_log10(name = "reward") +
    { if (log.y) scale_y_log10() } +
    scale_fill_discrete(name = "skipped rounds") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
}
