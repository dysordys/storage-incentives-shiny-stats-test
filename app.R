library(shiny)
library(tidyverse)



# Functions to download and clean data ----------------------------------------

fetchJson <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  jsonlite::stream_in(url(url))
}

fetchJsonAll <- function(start = "https://api.swarmscan.io/v1/redistribution/rounds") {
  dat <- tibble()
  url <- start
  roundPtr <- 0
  while (!is.null(roundPtr)) {
    jsonDat <- fetchJson(url)
    dat <- bind_rows(dat, jsonToTibble(jsonDat))
    roundPtr <- jsonDat$"next"
    url <- str_c("https://api.swarmscan.io/v1/redistribution/rounds?start=", roundPtr)
  }
  return(dat)
}

cleanData <- function(rawDataTable) {
  distinct(rawDataTable) %>% # Remove occasional repeated records across files
    filter(roundNumber != min(roundNumber)) # 1st round might be corrupted
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

jsonToTibble <- function(jsonDat) {
  as_tibble(jsonDat) %>%
    select(rounds) %>%
    unnest(rounds) %>%
    rowwise() %>% filter(ncol(events) >= 20) %>% ungroup() %>%
    mutate(events = map(events, reshapeEvents)) %>%
    unnest(events)
}



# Functions for tracking price oracle -----------------------------------------

adjustPrice <- function(currentPrice, redundancy) {
  minimumPrice <- 1024
  increaseRate <- c(1036, 1027, 1025, 1024, 1023, 1021, 1017, 1012)
  targetRedundancy <- 4
  maxConsideredExtraRedundancy <- 4
  usedRedundancy <- min(redundancy, targetRedundancy + maxConsideredExtraRedundancy)
  max((increaseRate[usedRedundancy] * currentPrice) / minimumPrice, minimumPrice)
}

accumPrice <- function(redundancyVec, initPrice) {
  accumulate(redundancyVec, function(x, y) adjustPrice(x, y), .init = initPrice)[-1]
}



# Initialize data -------------------------------------------------------------

dat <- read_rds("data.rds")



# Shiny app interface ---------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "tab", label = "Which table?",
                  choices = c("Revealers", "Skipped rounds")),
      sliderInput(inputId = "ip", label = "Initial price",
                  min = 2^10, max = 2^13, value = 2^10, step = 1),
      actionButton("downloadData", "Refresh data")
    ),
    mainPanel(
      plotOutput("outFig"),
      tableOutput("outTab")
    ),
  )
)


server <- function(input, output) {
  output$outTab <- renderTable({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(roundNumber) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `number of honest revealers` = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(`number of honest revealers`, initPrice = input$ip))
    } else {
      tibble(roundNumber = min(dat$roundNumber):max(dat$roundNumber)) %>%
        anti_join(distinct(select(dat, roundNumber)), by = "roundNumber")
    }
  })
  output$outFig <- renderPlot({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(roundNumber) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(nHonest = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(nHonest, initPrice = input$ip)) %>%
        ggplot(aes(x = roundNumber, y = price)) +
        geom_line(colour = "steelblue") +
        theme_bw(base_size = 16)
    } else {
      NULL
    }
  })
  observe({
    fetchJsonAll() %>% cleanData() %>% write_rds("data.rds", compress = "xz")
    dat <- read_rds("data.rds")
  }) %>%
    bindEvent(input$downloadData, once = TRUE)
}


shinyApp(ui = ui, server = server)
