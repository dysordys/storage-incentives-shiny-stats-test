adjustPrice <- function(currentPrice, redundancy) {
  minimumPrice <- 2^10
  increaseRate <- c(1036, 1027, 1025, 1024, 1023, 1021, 1017, 1012)
  targetRedundancy <- 4
  maxConsideredExtraRedundancy <- 4
  usedRedundancy <- min(redundancy, targetRedundancy + maxConsideredExtraRedundancy)
  max((increaseRate[usedRedundancy] * currentPrice) / minimumPrice, minimumPrice)
}


accumPrice <- function(redundancyVec, initPrice) {
  Reduce(function(x, y) adjustPrice(x, y), redundancyVec, init = initPrice,
         accumulate = TRUE)[-1]
}


restrictRounds <- function(dat, roundRange) {
  validRange <- length(roundRange) == 2 && (roundRange[1] <= roundRange[2])
  if (validRange) filter(dat, round %in% reduce(round(roundRange), `:`)) else dat
}


revealersPerRound <- function(dat, initPrice = 1024, roundRange = NA) {
  dat %>%
    group_by(round) %>%
    mutate(honest = (id == id[event == "won"])) %>%
    filter(event == "revealed") %>%
    summarise(`number of revealers` = n(),
              `honest revealers` = sum(honest)) %>%
    ungroup() %>%
    mutate(price = accumPrice(`honest revealers`, initPrice)) %>%
    restrictRounds(roundRange)
}


missedRounds <- function(dat, roundRange = NA) {
  tibble(round = min(dat$round):max(dat$round)) %>%
    anti_join(distinct(select(dat, round)), by = "round") %>%
    restrictRounds(roundRange)
}


skippedRounds <- function(dat) {
  dat %>%
    filter(event == "won") %>%
    select(round, reward) %>%
    mutate(skip = round - lag(round, default = first(round) - 1) - 1)
}


takeWhile <- function(vec, cond) {
  for (i in seq_along(vec) - 1) if (!cond(vec[i + 1])) break
  if (i == 0) return(vec[0]) else return(vec[1:i])
}


nhood <- function(overlay, depth = 8L) {
  byte <- 8L
  noBytes <- ceiling(depth / byte)
  totBytes <- (str_length(overlay) - 2L) / 2L # Subtract 2 because of the starting "0x"
  map_int(seq(3, 2 * min(noBytes, totBytes) + 2, by = 2),
          function(x) strtoi(str_c("0x", str_sub(overlay, x, x + 1))))
}


proximity <- function(fst, snd) {
  maxPO <- 16L
  b <- min((maxPO - 1) %/% 8 + 1, length(fst))
  m <- 8L
  for (i in 1:b) {
    oxo <- bitwXor(fst[i], snd[i])
    for (j in 1:m) {
      if (bitwAnd(bitwShiftR(oxo, 8 - j), 0x01) != 0) return(8 * (i - 1) + j - 1)
    }
  }
  return(maxPO)
}

# proximity(255, 0)

# package main
#
# import (
#   "fmt"
# )
#
# const MaxPO = 16
#
# func Proximity(one, other []byte) (ret int) {
#   b := (MaxPO-1)/8 + 1
#   if b > len(one) {
#     b = len(one)
#   }
#   m := 8
#   for i := 0; i < b; i++ {
#     oxo := one[i] ^ other[i]
#     for j := 0; j < m; j++ {
#       if (oxo>>uint8(7-j))&0x01 != 0 {
#         return i*8 + j
#       }
#     }
#   }
#   return MaxPO
# }
#
# func main() {
#   nhood1 := []byte{124}
#   nhood2 := []byte{123}
#   po := Proximity(nhood1, nhood2)
#   fmt.Println(po)
# }
