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
