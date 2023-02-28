library(shiny)
library(tidyverse)


fetchJson <- function(url = "https://api.swarmscan.io/v1/redistribution/rounds") {
  rawJsonStr <- system(str_c("curl -s ", url), intern = TRUE)
  jsonlite::parse_json(rawJsonStr[1])
}

fetchJsonAll <- function(start = "https://api.swarmscan.io/v1/redistribution/rounds") {
  dat <- tibble()
  url <- start
  roundPointer <- 0
  while (!is.null(roundPointer)) {
    jsonDat <- fetchJson(url)
    dat <- bind_rows(dat, jsonToTibble(jsonDat))
    roundPointer <- jsonDat$"next"
    url <- str_c("https://api.swarmscan.io/v1/redistribution/",
                 "rounds\\?start\\=", roundPointer)
  }
}

cleanData <- function(rawDataTibble) {
  rawDataTibble %>%
    distinct() %>% # Occasionally repeated records across files; remove duplicates
    mutate(stake = as.numeric(stake), # Convert some fields to numbers
           stakeDensity = as.numeric(stakeDensity),
           depth = as.integer(depth),
           blockNumber = as.integer(blockNumber),
           countCommits = as.integer(countCommits),
           countReveals = as.integer(countReveals),
           rewardAmount = as.double(rewardAmount))
}

reshapeEvent <- function(eventStr) {
  eventStr %>%
    enframe(name = "quantity") %>%
    # Remove entries beginning with "stakeFrozen":
    filter(str_detect(quantity, "stakeFrozen", negate = TRUE)) %>%
    # Remove prefixes before ".", as these always refer to the same basic data:
    mutate(quantity = str_remove(quantity, "^[^\\.]*\\.")) %>%
    # Remove roundNumber, as that is already in the overall data:
    filter(quantity != "roundNumber") %>%
    pivot_wider(names_from = "quantity")
}

jsonToTibble <- function(jsonDat) {
  as_tibble(jsonDat) %>%
    select(rounds) %>%
    unnest(rounds) %>%
    mutate(name = names(rounds)) %>%
    pivot_wider(names_from = name, values_from = rounds, values_fn = list) %>%
    unnest(c(roundNumber, events)) %>%
    unnest(roundNumber) %>%
    arrange(roundNumber) %>%
    mutate(events = map(events, ~map(.x, function(y) unlist(y)) %>%
                          enframe(name = "event"))) %>%
    unnest(events) %>%
    mutate(value = map(value, reshapeEvent)) %>%
    unnest(value) %>%
    mutate(id = if_else(type == "event", reserveCommitment, hash)) %>%
    mutate(type = if_else(type == "event", name, "won")) %>%
    select(-c(reserveCommitment, hash, name))
}

#jsonToTibble(fetchJson()) %>% select(roundNumber) %>% distinct()
#fetchJsonAll()

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

dat <- read_rds("../data/data_parsed.rds") %>%
  mutate(id = if_else(type == "event", reserveCommitment, hash)) %>%
  mutate(type = if_else(type == "event", name, "won")) %>%
  select(round = roundNumber, event, type, id, reward = rewardAmount,
         commits = countCommits, reveals = countReveals, depth, stake, overlay)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "tab", label = "Which table?",
                  choices = c("Revealers", "Skipped rounds")),
      sliderInput(inputId = "ip", label = "Initial price",
                  min = 2^10, max = 2^13, value = 2^10, step = 1)
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
        group_by(round) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `number of honest revealers` = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(`number of honest revealers`, initPrice = input$ip))
    } else {
      tibble(round = min(dat$round):max(dat$round)) %>%
        anti_join(distinct(select(dat, round)), by = "round")
    }
  })
  output$outFig <- renderPlot({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(round) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(nHonest = sum(honest)) %>%
        ungroup() %>%
        mutate(price = accumPrice(nHonest, initPrice = input$ip)) %>%
        ggplot(aes(x = round, y = price)) +
        geom_line(colour = "steelblue") +
        theme_bw(base_size = 16)
    } else {
      NULL
    }
  })
}


shinyApp(ui = ui, server = server)
