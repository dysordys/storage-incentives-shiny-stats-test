library(tidyverse)
library(fs)

source("download_clean.R")

if (file_access("data.rds", mode = "write")) {
  minRound <- max(read_rds("data.rds")$roundNumber)
} else {
  minRound <- 0
}

fetchJsonAll(minRound = minRound) %>%
  cleanData() %>%
  mergeData(read_rds("data.rds") %>% select(-date)) %>%
  mutate(date = roundsToDate(roundNumber, lubridate::today())) %>%
  write_rds("data.rds")
