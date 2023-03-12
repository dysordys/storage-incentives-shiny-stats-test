library(tidyverse)

dat <- read_rds("data.rds")


# Distribution of claimed depths
dat %>% filter(!is.na(depth)) %>% count(depth) # Everyone (revealers + winners)
dat %>% filter(event == "revealed") %>% count(depth) # Revealers without the winners


# Distribution of number of revealers:
dat %>%
  filter(event == "revealed") %>%
  group_by(roundNumber) %>%
  summarise(revealers = n(), depths = length(unique(depth))) %>%
  ungroup() %>%
  mutate(depths = if_else(depths == 1, "one depth", "multiple depths")) %>%
  mutate(revealers = as_factor(revealers),
         depths = fct_relevel(depths, "one depth")) %>%
  ggplot(aes(x = revealers)) +
  geom_bar(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  facet_wrap(~ depths, scales = "free_y") +
  theme_bw()


# Distribution of stakes:
dat %>% filter(!is.na(stake)) %>% count(stake)
dat %>%
  filter(!is.na(stake)) %>%
  group_by(roundNumber) %>%
  summarise(m = mean(stake), s = sd(stake), .groups = "drop") %>%
  count(`Within-round variation in stakes across revealers` = s == 0)
dat %>%
  count(stake) %>%
  filter(!is.na(stake)) %>%
  ggplot(aes(x = stake, y = n)) +
  geom_col(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


# Honest revealers:
dat %>%
  group_by(roundNumber) %>% # In each round: check whether revealed id = that of winner
  mutate(honest = (id == id[event == "won"])) %>%
  ungroup() %>%
  filter(event == "revealed") %>%
  pull(honest) %>%
  # Add up honest revealers, and determine their proportion as well:
  { tibble(`total number of honest revealers` = sum(.),
           `total fraction of honest revealers` = sum(.) / length(.)) }
dat %>%
  group_by(roundNumber) %>% # In each round: check whether revealed id = that of winner
  mutate(honest = (id == id[event == "won"])) %>%
  filter(event == "revealed") %>% # Restrict to revealers
  summarise(nHonest = sum(honest)) %>%
  ungroup() %>%
  count(nHonest)


# In every round with depth 8, all events in a round come from same neighborhood:
dat %>%
  filter(depth == 8) %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  group_by(roundNumber) %>%
  summarise(events = n(), nhoods = length(unique(nhood))) %>%
  ungroup() %>%
  count(nhoods)
# Are neighborhoods (first 2 digits of overlay in depth=8 cases) uniformly distributed?
dat %>%
  filter(depth == 8, event == "won") %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  ggplot(aes(x = nhood)) +
  geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, binwidth = 8) +
  theme_bw()
# Kolmogorov-Smirnov test:
dat %>%
  filter(depth == 8, event == "won") %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  pull(nhood) %>%
  `-`(min(.)) %>%
  `/`(max(.)) %>%
  `+`(runif(length(.), 0, 1e-5)) %>% # To break ties in KS test
  ks.test("punif") # No evidence of non-uniformity
# Chi-squared test:
dat %>%
  filter(depth == 8, event == "won") %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  pull(nhood) %>%
  spgs::chisq.unif.test(interval = c(0, 255))
