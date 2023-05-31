clargs <- commandArgs(trailingOnly = TRUE)
if (length(clargs) > 0) short <- clargs[1] else short <- NA

dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))  # add to the path
install.packages(c("jsonlite", "R.utils"))

# suppressPackageStartupMessages({
#   library(tidyverse)
#   library(fs)
# })
#
# source("download_clean.R")
#
# if (file_access("data.rds", mode = "write"))
#   minRound <- max(read_rds("data.rds")$roundNumber) else minRound <- 0
#
# if (short == "short")
#   fetch <- function(start, minRound) fetchJson() else fetch <- fetchJsonAll
#
# fetch(minRound = minRound) %>%
#   cleanData() %>%
#   mergeData(read_rds("data.rds") %>% select(-date)) %>%
#   mutate(date = roundsToDate(roundNumber)) %>%
#   write_rds("data.rds")
