library(shiny)
library(tidyverse)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("tab", "Which table?", c("Revealers", "Skipped rounds"))
    ),
    mainPanel(
      tableOutput("out")
    )
  )
)


server <- function(input, output) {

  dat <- read_rds("../data/data_parsed.rds") %>%
    mutate(id = if_else(type == "event", reserveCommitment, hash)) %>%
    mutate(type = if_else(type == "event", name, "won")) %>%
    select(round = roundNumber, event, type, id, reward = rewardAmount,
           commits = countCommits, reveals = countReveals, depth, stake)

  output$out <- renderTable({
    if (input$tab == "Revealers") {
      dat %>%
        group_by(round) %>%
        mutate(honest = (id == id[type == "won"])) %>%
        filter(type == "revealed") %>%
        summarise(`number of revealers` = n(),
                  `number of honest revealers` = sum(honest)) %>%
        ungroup()
    } else {
      tibble(round = min(dat$round):max(dat$round)) %>%
        anti_join(distinct(select(dat, round)), by = "round")
    }
  })
}


shinyApp(ui = ui, server = server)
