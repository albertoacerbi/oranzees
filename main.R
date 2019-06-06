library(tidyverse)

set_oranzees_environment <- function() {
  list_pop <- c('Uossob', 'Iat Forest', 'Ebmog', 'Elaham', 'Elabik', 'Ognodub')
  output <- tibble(
    population = rep(list_pop, each = 38),
    behaviour = as.factor(rep(1:38, 6)),
    type = rep(c(rep("social", 16), rep("food-related", 22)), 6),
    category = rep(c(
      rep("play", 4), rep("display", 4), rep("groom", 4), rep("courthsip", 4),
      "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "D", "E", "E", "F", "F", "G", "H", "I", "J"
    ), 6),
    nutrient = rep(c(rep(NA, 16), "Y", "Y", "Y", "Y", "Z", "Z", "Z", "Z", "Y", "Y", "Y", "Z", "Z", "Z", "Y", "Y", "Z", "Z", "Y", "Z", "Y", "Z"), 6),
    p_g = rep(NA, 38 * 6),
    p_e = rep(NA, 38 * 6)
  )
  
  env_or_x <- c(220, 230, 700, 705, 710, 750)
  env_or_y <- c(660, 610, 450, 430, 510, 550)
  
  # genetic predispositions:
  x_g <- sample(1:1000, 38)
  y_g <- sample(1:1000, 38)
  
  # ecological availability:
  x_e <- sample(1:1000, 38)
  y_e <- sample(1:1000, 38)
  
  for (i in 1:6) {
    for (behav in 1:38) {
      output[output$population == list_pop[i] & output$behaviour == behav, ]$p_g <- 1 - sqrt((x_g[behav] - env_or_x[i])^2 + (y_g[behav] - env_or_y[i])^2) / 1000
      
      if (behav > 16) { # only for food-related behaviours:
        output[output$population == list_pop[i] & output$behaviour == behav, ]$p_e <- 1 - sqrt((x_e[behav] - env_or_x[i])^2 + (y_e[behav] - env_or_y[i])^2) / 500
      }
    }
  }
  
  # replace negative values with 0s:
  if (dim(output[output$p_g < 0, ])[1] > 0) {
    output[output$p_g < 0, ]$p_g <- 0
  }
  if (dim(output[output$p_e < 0 & !is.na(output$p_e), ])[1] > 0) {
    output[output$p_e < 0 & !is.na(output$p_e), ]$p_e <- 0
  }
  
  # return the tibble:
  output
}

oranzees_environment <- set_oranzees_environment()
