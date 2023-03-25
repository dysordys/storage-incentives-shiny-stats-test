depthSelect <- function(inputId, depths = 1:9) {
  selectInput(inputId, label = "Depth:", choices = depths, selected = 8)
}


heightSlider <- function(inputId, min = 200) {
  sliderInput(inputId, label = "Figure height", min = min, max = 2000,
              value = 450, round = TRUE, width = "150%")
}
