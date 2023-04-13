fetchJson <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  jsonlite::stream_in(url(url))
}


fetchJsonAll <- function(start = "https://api.swarmscan.io/v1/redistribution/rounds",
                         minRound = 0) {
  jsonDat <- list()
  url <- start
  roundPtr <- .Machine$integer.max
  while (!is.null(roundPtr) && roundPtr >= minRound) {
    newJson <- fetchJson(url)
    jsonDat <- append(jsonDat, list(newJson))
    roundPtr <- newJson$"next"
    url <- str_c("https://api.swarmscan.io/v1/redistribution/rounds?start=", roundPtr)
  }
  return(jsonDat)
}


downloadAllData <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  fetchJsonAll(start = url) %>%
    cleanData() %>% # Sometimes 1st round is corrupted and last isn't done yet,
    filter(!(roundNumber %in% range(roundNumber))) # so remove these
}


cleanData <- function(jsonDat) {
  tibble(json = jsonDat) %>%
    unnest(json) %>%
    select(rounds) %>% # This will drop the unneeded "next" column, if it exists
    unnest(rounds) %>%
    rowwise() %>% # Sometimes a round is corrupted, e.g. has no winner yet;
    filter(ncol(events) >= 20) %>% # remove these - they have less than 20 columns
    ungroup() %>%
    mutate(events = map(events, reshapeEvents)) %>%
    unnest(events) %>%
    distinct() %>% # Remove occasional repeated records across files
    arrange(roundNumber)
}


reshapeEvents <- function(events) {
  events %>%
    select(-contains("roundNumber")) %>%
    unnest(winner) %>%
    { if ("depth" %in% names(.)) . else mutate(., depth = NA_integer_) } %>%
    rename_with(~str_c("winner.", .x), c(overlay, stake, stakeDensity, depth)) %>%
    unnest(data) %>%
    # Very occasionally, some columns are missing; add if necessary:
    { if ("depth" %in% names(.)) . else mutate(., depth = NA_integer_) } %>%
    { if ("stake" %in% names(.)) . else mutate(., stake = NA_real_) } %>%
    { if ("stakeDensity" %in% names(.)) . else mutate(., stakeDensity = NA_real_) } %>%
    { if ("reserveCommitment" %in% names(.)) . else
      mutate(., reserveCommitment = NA_character_)
    } %>%
    mutate(id = if_else(type == "event", reserveCommitment, hash)) %>%
    mutate(event = if_else(type == "event", name, "won")) %>%
    mutate(overlay = if_else(event == "won", winner.overlay, overlay)) %>%
    mutate(stake = if_else(event == "won", winner.stake, stake)) %>%
    mutate(depth = if_else(event == "won", winner.depth, depth)) %>%
    mutate(stakeDensity = if_else(event == "won", winner.stakeDensity, stakeDensity)) %>%
    relocate(event, id, rewardAmount, depth, stake, overlay) %>%
    #bind_cols(ifelse("stakeFrozen" %in% names(.),
    #                 cleanStakeFrozen(.$stakeFrozen),
    #                 tibble(slashed = rep(NA, nrow(.), account = NA, time = NA)))) %>%
    select(-contains("winner"), -contains("stakeFrozen"), -contains("roundNumber"),
           -starts_with("count"), -reserveCommitment, -hash, -name, -topics, -type)
}


cleanStakeFrozen <- function(stakeFrozenColumn) {
  tibble(sf = stakeFrozenColumn) %>%
    rowid_to_column("n") %>%
    unnest(sf) %>%
    right_join(tibble(n = 1:length(stakeFrozenColumn)), by = "n") %>%
    select(-n)
}


mergeData <- function(data1, data2) {
  bind_rows(data1, data2) %>%
    distinct() %>%
    arrange(roundNumber)
}


fetchNhoodJson <- function(url = "https://api.swarmscan.io/v1/network/stats") {
  fetchJson(url)
}
