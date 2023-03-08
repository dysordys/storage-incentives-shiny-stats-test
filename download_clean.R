fetchJson <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  jsonlite::stream_in(url(url))
}


fetchJsonAll <- function(start = "https://api.swarmscan.io/v1/redistribution/rounds",
                         minRound = 0) {
  dat <- tibble()
  url <- start
  roundPtr <- .Machine$integer.max
  while (!is.null(roundPtr) && roundPtr >= minRound) {
    jsonDat <- fetchJson(url)
    dat <- bind_rows(dat, jsonToTibble(jsonDat))
    roundPtr <- jsonDat$"next"
    url <- str_c("https://api.swarmscan.io/v1/redistribution/rounds?start=", roundPtr)
  }
  return(dat)
}


downloadAllData <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  fetchJsonAll(start = url) %>%
    cleanData() %>%
    filter(round != min(round)) # Sometimes 1st round in data is corrupted; remove it
}


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
    mutate(event = if_else(type == "event", name, "won")) %>%
    mutate(overlay = if_else(type == "won", winner.overlay, overlay)) %>%
    mutate(stake = if_else(type == "won", winner.stake, stake)) %>%
    mutate(depth = if_else(type == "won", winner.depth, depth)) %>%
    mutate(stakeDensity = if_else(type == "won", winner.stakeDensity, stakeDensity)) %>%
    select(-contains("winner"), -contains("stakeFrozen"), -contains("roundNumber"),
           -contains("count"), -reserveCommitment, -hash, -name, -topics, -type)
}


cleanData <- function(rawDataTable) {
  distinct(rawDataTable) %>% # Remove occasional repeated records across files
    arrange(roundNumber) %>%
    rename(round = roundNumber, reward = rewardAmount) %>%
    relocate(round, event, id, reward, depth, stake, overlay)
}


mergeData <- function(data1, data2) {
  bind_rows(data1, data2) %>%
    distinct() %>%
    arrange(round)
}