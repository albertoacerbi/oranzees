library(tidyverse)

set_oranzees_environment <- function() {
  
  output <- tibble( population = as.factor(rep(1:6, each = 38)),
                          behaviour = as.factor(rep(1:38, 6)),
                          p_g = rep(NA, 38 * 6), 
                          p_e = rep(NA, 38 * 6))
  
  env_or_x <- c(220, 230, 700, 705, 710, 750)
  env_or_y <- c(660, 610, 450, 430, 510, 550)
  
  # genetic predispositions for the 38 behaviours:
  x_g <- sample(1:1000, 38)
  y_g <- sample(1:1000, 38)
  
  # ecological availability for the 38 behaviours:
  x_e <- sample(1:1000, 38)
  y_e <- sample(1:1000, 38)
  
  for(oranzees_pop in 1:6){
    for(behav in 1:38) {
      
      output[output$population==oranzees_pop & output$behaviour == behav,]$p_g <- 1 - sqrt((x_g[behav] - env_or_x[oranzees_pop])^2 + (y_g[behav] - env_or_y[oranzees_pop])^2)/1000
      
      output[output$population==oranzees_pop & output$behaviour == behav,]$p_e <- 1 - sqrt((x_e[behav] - env_or_x[oranzees_pop])^2 + (y_e[behav] - env_or_y[oranzees_pop])^2)/500
    }
  }
  
  # replace negative values with 0s:
  if(dim(output[output$p_g < 0, ])[1] > 0 )
    output[output$p_g < 0, ]$p_g = 0
  if(dim(output[output$p_e < 0, ])[1] > 0 )
    output[output$p_e < 0, ]$p_e = 0
  
  # return the tibble:
  output
}

oranzees_environment <- set_oranzees_environment()
