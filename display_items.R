restrictRounds <- function(dat, roundRange) {
  validRange <- length(roundRange) == 2 && (roundRange[1] <= roundRange[2])
  if (validRange) filter(dat, round %in% reduce(round(roundRange), `:`)) else dat
}


revealerPerRoundTab <- function(dat, initPrice, roundRange = NA) {
  dat %>%
    group_by(round) %>%
    mutate(honest = (id == id[event == "won"])) %>%
    filter(event == "revealed") %>%
    summarise(`number of revealers` = n(),
              `honest revealers` = sum(honest)) %>%
    ungroup() %>%
    mutate(price = accumPrice(`honest revealers`, initPrice)) %>%
    restrictRounds(roundRange)
}


revealerPerRoundFig <- function(dat, initPrice = 1024, roundRange = NA) {
  dat %>%
    revealerPerRoundTab(initPrice, roundRange) %>%
    ggplot(aes(x = round, y = price)) +
    geom_line(colour = "steelblue") +
    theme_bw(base_size = 16) +
    theme(plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}


missedRoundsTab <- function(dat, roundRange = NA) {
  tibble(round = min(dat$round):max(dat$round)) %>%
    anti_join(distinct(select(dat, round)), by = "round") %>%
    restrictRounds(roundRange)
}


missedRoundsFig <- function(dat, roundRange = NA) {
  dat %>%
    missedRoundsTab(roundRange) %>%
    ggplot(aes(x = round, y = 0)) +
    geom_point(colour = "steelblue", alpha = 0.7) +
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = unit(c(0.2, 2, 0.2, 0.2), "cm"))
}
