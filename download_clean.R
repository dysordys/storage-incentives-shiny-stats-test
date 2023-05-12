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
    cleanData()
}


nhoodBinStr <- function(overlay, depth = 8L) {
  if (!is.na(depth) & depth > 0) {
    numHexDigits <- ceiling(depth / 4) # 4: four bits = 1 hex digit
    str_sub(R.utils::intToBin(str_sub(overlay, 1, numHexDigits + 2)), 1, depth)
  } else NA
}


nhoodDec <- function(overlay, depth = 8L) {
  strtoi(nhoodBinStr(overlay, depth), base = 2)
}


roundsToDate <- function(rounds, today) {
  maxRound <- max(rounds)
  todayNum <- as.numeric(lubridate::as_datetime(today))
  map_dbl(rounds, function(x) todayNum - 760 * (maxRound - x)) %>%
    lubridate::as_datetime() %>%
    lubridate::as_date()
}


cleanStakeFrozen <- function(stakeFrozenElement) {
  if (is.null(stakeFrozenElement)) tibble(value = NA) else as_tibble(stakeFrozenElement)
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
      mutate(., reserveCommitment = NA_character_) } %>%
    { if ("stakeFrozen" %in% names(.)) . else
      mutate(., stakeFrozen = NA) } %>%
    mutate(reserveCommitmentHash = if_else(type == "event", reserveCommitment, hash)) %>%
    mutate(event = if_else(type == "event", name, "won")) %>%
    mutate(overlay = if_else(event == "won", winner.overlay, overlay)) %>%
    mutate(stake = if_else(event == "won", winner.stake, stake)) %>%
    mutate(depth = if_else(event == "won", winner.depth, depth)) %>%
    mutate(stakeDensity = if_else(event == "won", winner.stakeDensity, stakeDensity)) %>%
    mutate(stakeFrozen = map(stakeFrozen, cleanStakeFrozen)) %>%
    relocate(event, reserveCommitmentHash, rewardAmount, depth, stake,
             overlay, stakeFrozen) %>%
    select(-contains("winner"), -contains("roundNumber"), -starts_with("count"),
           -reserveCommitment, -hash, -name, -topics, -type)
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
    mutate(nhood = map2_int(overlay, depth, nhoodDec),
           .after = overlay) %>% # Pre-calculate neighborhoods
    mutate(rewardAmount = rewardAmount / 1e16,
           stake = stake / 1e16) %>% # Convert PLUR to BZZ
    arrange(roundNumber)
}


mergeData <- function(oldData, newData) {
  duplicateRounds <- intersect(unique(oldData$roundNumber), unique(newData$roundNumber))
  oldData %>%
    filter(!(roundNumber %in% duplicateRounds)) %>%
    bind_rows(newData) %>%
    distinct() %>%
    arrange(roundNumber)
}


fetchNhoodJson <- function(url = "https://api.swarmscan.io/v1/network/neighborhoods") {
  fetchJson(url)
}
