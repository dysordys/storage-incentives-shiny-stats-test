library(shiny)
library(tidyverse)


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
      tableOutput("out")
    )
  )
)


server <- function(input, output) {
  output$out <- renderTable({
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
}


shinyApp(ui = ui, server = server)
