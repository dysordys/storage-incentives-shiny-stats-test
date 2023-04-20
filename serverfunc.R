outWinNodeNhoodFig <- function(dat, roundRange = NA, depthVal = 8, sortBy = "wins",
                               highlightNhood = NA) {
  restrictRounds(dat, roundRange) %>%
    winsNodesByNhood() %>%
    filter(depth == depthVal) %>%
    winNodeNhoodFig(sortBy, highlightNhood)
}


outNodesPerNhoodFig <- function(dat, roundRange = NA, depthVal = 8,
                                highlightNhood = NA) {
  dat %>%
    nodesPerNhood() %>%
    filter(depth == depthVal) %>%
    nodesPerNhoodQuantileFig(highlightNhood)
}


outNodeDistrFig <- function(dat, roundRange = NA, depthVal = 8) {
  dat %>%
    nodeNhoodDistr(depthVal) %>%
    nodesPerNhoodHistFig()
}


outStakesNhoodFig <- function(dat, roundRange = NA, depthVal = 8, highlightNhood = NA) {
  dat %>%
    rewardNhoodDistr() %>%
    filter(depth == depthVal) %>%
    stakesNhoodQuantileFig(highlightNhood)
}


outStakesNodeFig <- function(dat, roundRange = NA, depthVal = 8) {
  restrictRoundsDepth(dat, roundRange, depthVal) %>%
    rewardNhoodDistr() %>%
    stakesNhoodHistFig()
}


outStakedNodesFig <- function(dat, roundRange = NA, depthVal = 8, highlightNhood = NA) {
  dat %>%
    stakedNodesPerNhood() %>%
    filter(depth == depthVal) %>%
    stakedNodesFig(highlightNhood)
}
