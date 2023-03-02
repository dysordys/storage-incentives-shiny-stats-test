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
