library(tidyverse)

dat <- read_rds("data.rds") %>%
  select(round, type, type, id, reward = rewardAmount,
         commits = countCommits, reveals = countReveals, depth, stake, overlay)


# Distribution of claimed depths
dat %>% filter(!is.na(depth)) %>% count(depth) # Everyone (revealers + winners)
dat %>% filter(type == "revealed") %>% count(depth) # Revealers without the winners
# The single claim of depth = 7 happened at:
dat %>% filter(depth == 7)
# The two claims of depth = 9 happened at:
dat %>% filter(depth == 9)
# Both are from the same round, and depth = 9 was the truth!


# Distribution of rewards:
dat %>%
  select(reward) %>%
  drop_na() %>%
  mutate(reward_dbl = as.double(reward)) %>%
  # Check whether converting reward to double works; i.e., loss of
  # precision doesn't create equal-looking but really unequal rewards:
  { print(str_c("Unique reward values: ", length(unique(.$reward))))
    print(str_c("And after conversion: ", length(unique(.$reward_dbl))))
  }
dat %>% # The above means we can safely convert the reward to double:
  mutate(reward = as.double(reward)) %>%
  filter(type == "won") %>%
  ggplot(aes(x = reward)) +
  geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, bins = 100) +
  theme_bw()
dat %>%
  mutate(doubled = case_when(
    round %in% (dat %>%
                  mutate(reward = as.double(reward)) %>%
                  filter(reward > 3.45e13) %>% # There's only one reward that large
                  pull(round)) ~ "2 times",
    round %in% (tibble(round = min(dat$round):max(dat$round)) %>%
                  anti_join(distinct(select(dat, round)), by = "round") %>%
                  pull(round) %>%
                  `+`(1)) ~ "1 times",
    TRUE ~ "0 times"
  )) %>%
  mutate(reward = as.double(reward)) %>%
  filter(type == "won") %>%
  ggplot(aes(x = reward, fill = doubled)) +
  geom_histogram(alpha = 0.8, bins = 80) +
  scale_x_continuous(breaks = c(1.15e13, 2.3e13, 3.45e13)) +
  scale_fill_manual(values = c("plum3", "steelblue", "goldenrod")) +
  theme_bw()


# Distribution of # of revealers:
dat %>%
  filter(!is.na(reveals)) %>%
  mutate(reveals = as_factor(reveals)) %>%
  ggplot(aes(x = reveals)) +
  geom_bar(colour = "steelblue", fill = "steelblue", alpha = 0.2) +
  theme_bw()
# Distribution of # of revealers per depth not too interesting because the only
# depth values are
dat %>% filter(depth == 7)
# The two claims of depth = 9 happened at:
dat %>% filter(depth == 9)


# Distribution of stakes:
dat %>% filter(!is.na(stake)) %>% count(stake)
dat %>%
  filter(!is.na(stake)) %>%
  group_by(round) %>%
  summarise(m = mean(stake), s = sd(stake), .groups = "drop") %>%
  # It is rare to have within-round variation in stakes across revealers:
  count(`Within-round variation in stakes across revealers` = s == 0)


# Honest revealers:
dat %>%
  group_by(round) %>% # Within each round, check if revealed id == that of winner:
  mutate(honest = (id == id[type == "won"])) %>%
  ungroup() %>%
  filter(type == "revealed") %>% # Restrict to revealers
  pull(honest) %>%
  sum() # Add up honest revealers; it's exactly = the number of rounds!
dat %>%
  group_by(round) %>% # Within each round, check if revealed id == that of winner:
  mutate(honest = (id == id[type == "won"])) %>%
  filter(type == "revealed") %>% # Restrict to revealers
  summarise(nHonest = sum(honest)) %>%
  ungroup() %>%
  count(nHonest) # Indeed, always 1 honest revealer per round
dat %>%
  group_by(round) %>% # Within each round,
  # check the following: is the revealed id equal to that of the winner?
  mutate(honest = (id == id[type == "won"])) %>%
  ungroup() %>%
  # Restrict to revealers:
  filter(type == "revealed") %>%
  pull(honest) %>%
  # Add up honest revealers, and determine their proportion as well:
  { tibble(`total number of honest revealers` = sum(.),
           `total fraction of honest revealers` = sum(.) / length(.)) } %>%
  knitr::kable()
dat %>%
  group_by(round) %>%
  mutate(honest = (id == id[type == "won"])) %>%
  filter(type == "revealed") %>%
  summarise(number_of_revealers = n(),
            number_of_honest_revealers = sum(honest)) %>%
  ungroup()


# In every round with depth 8, all events in a round come from same neighborhood:
dat %>%
  filter(depth == 8) %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  group_by(round) %>%
  summarise(events = n(), nhoods = length(unique(nhood))) %>%
  ungroup() %>%
  count(nhoods)
# Are neighborhoods (first 2 digits of overlay in depth=8 cases) uniformly distributed?
dat %>%
  filter(depth == 8, type == "won") %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  ggplot(aes(x = nhood)) +
  geom_histogram(colour = "steelblue", fill = "steelblue", alpha = 0.2, binwidth = 16) +
  theme_bw()
# Kolmogorov-Smirnov test:
dat %>%
  filter(depth == 8, type == "won") %>%
  mutate(nhood = strtoi(str_sub(overlay, 1, 4))) %>%
  pull(nhood) %>%
  `-`(min(.)) %>%
  `/`(max(.)) %>%
  `+`(runif(length(.), 0, 1e-5)) %>% # To break ties in KS test
  ks.test("punif") # No evidence of non-uniformity
