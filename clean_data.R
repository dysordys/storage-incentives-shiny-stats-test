jsonToTibble <- function(jsonDat) {
  as_tibble(jsonDat) %>%
    select(rounds) %>% # This will drop the unneeded "next" column, if it exists
    unnest(rounds) %>%
    # Sometimes a round is corrupted, e.g. has no winner yet - remove these:
    rowwise() %>% filter(ncol(events) >= 20) %>% ungroup() %>%
    mutate(events = map(events, reshapeEvents)) %>%
    unnest(events)
}


reshapeEvents <- function(events) {
  select(events, -contains("roundNumber")) %>%
    unnest(winner) %>%
    rename_with(~str_c("winner.", .x), c(overlay, stake, stakeDensity, depth)) %>%
    unnest(data) %>%
    mutate(id = if_else(type == "event", reserveCommitment, hash)) %>%
    mutate(type = if_else(type == "event", name, "won")) %>%
    mutate(overlay = if_else(type == "won", winner.overlay, overlay)) %>%
    mutate(stake = if_else(type == "won", winner.stake, stake)) %>%
    mutate(depth = if_else(type == "won", winner.depth, depth)) %>%
    mutate(stakeDensity = if_else(type == "won", winner.stakeDensity, stakeDensity)) %>%
    select(-contains("winner"), -contains("stakeFrozen"), -contains("roundNumber"),
           -reserveCommitment, -hash, -name, -topics)
}


cleanData <- function(rawDataTable) {
  distinct(rawDataTable) %>% # Remove occasional repeated records across files
    filter(roundNumber != min(roundNumber)) %>% # 1st round might be corrupted
    arrange(roundNumber) %>%
    rename(round = roundNumber)
}


mergeData <- function(data1, data2) {
  bind_rows(data1, data2) %>%
    distinct() %>%
    arrange(round)
}
