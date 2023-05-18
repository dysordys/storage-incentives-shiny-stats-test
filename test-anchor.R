dat %>%
  group_by(roundNumber) %>%
  filter("current-reveal-anchor" %in% event) %>%
  ungroup() %>%
  filter(event %in% c("won", "current-reveal-anchor")) %>%
  left_join(skippedRoundsTibble(dat), by = "roundNumber") %>%
  filter(skip == 0) %>%
  select(roundNumber, event, reserveCommitmentHash, anchor) ->
    anchorDat

anchorDat %>% print(n = 20)
