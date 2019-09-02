library(tidyverse)
library(scales)

set_oranzees_world <- function(alpha_g, alpha_e) {
  list_pop <- c("Uossob", "Iat Forest", "Ebmog", "Elaham", "Elabik", "Ognodub")
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
  
  for(behav in 1:38){
    output[output$behaviour == behav,]$p_g <- 1 - rescale(sqrt((x_g[behav] - env_or_x)^2 + (y_g[behav] - env_or_y)^2), to = c(1 - alpha_g, alpha_g))
    
    if(behav > 16){
      output[output$behaviour == behav,]$p_e <- 1 - rescale(sqrt((x_e[behav] - env_or_x)^2 + (y_e[behav] - env_or_y)^2), to = c(1 - alpha_e, alpha_e))
    }
  }
  # return the tibble:
  output
}

# example call:
# alpha_g = 0.7
# alpha_e = 0.9
# oranzees_world <- set_oranzees_world(alpha_g, alpha_e)

update_demography <- function(pop) {
  pop[, 39] <- pop[, 39] + 1
  pop[pop[, 39] >= 720, ] <- 0
  old <- which(pop[, 39] > 300)
  dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
  pop[old[dead], ] <- 0
  pop
}

update_social_behaviours <- function(pop, test_world, sd_peering) {
  N <- dim(pop)[1]
  state <- ((rowSums(pop[, 1:4]) >= 1) + (rowSums(pop[, 5:8]) >= 1) + (rowSums(pop[, 9:12]) >= 1) + (rowSums(pop[, 13:16]) >= 1)) / 4
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(16, mean = colSums(pop[, 1:16]), sd = sd_peering)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(1:16, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i]  * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}  

update_food_behaviours <- function(pop, test_world, sd_peering) {
  N <- dim(pop)[1]
  nut_y <- (rowSums(pop[, 17:20])>=1) + (rowSums(pop[, 25:27])>=1) + (rowSums(pop[, 31:32])>=1) + pop[, 35] + pop[, 37]
  nut_z <- (rowSums(pop[, 21:24])>=1) + (rowSums(pop[, 28:30])>=1) + (rowSums(pop[, 33:34])>=1) + pop[, 36] + pop[, 38]
  state <- (nut_y + nut_z - abs(nut_y - nut_z)) / 10 
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = sd_peering)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(17:38, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i] * test_world$p_e[innovation_i] * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}

run_oranzees <- function(t_max, alpha_g, alpha_e, sd_peering, init_world, n_run) {
  
  N <- c(50, 100, 150, 50, 100, 150)
  
  oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
  
  output <- tibble(run = rep(1:n_run, each= 38 * 6),
                    population = rep(oranzees_world$population, n_run),
                   behaviour = rep(1:38, 6 * n_run),
                    code = rep("NA", 38 * 6 * n_run))
  
  for(run in 1:n_run){
    
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
    }
    
    for(current_population in 1:6){ 
      pop <- matrix(c(rep(0, 38 * N[current_population]), 
                      sample(1:300, N[current_population], replace = TRUE)), 
                    nrow = N[current_population], byrow = FALSE)
      current_world <- oranzees_world %>%
        filter(population == unique(oranzees_world$population)[current_population])
      # start simulation here:
      for (t in 1:t_max) {
        pop <- update_demography(pop)
        pop <- update_social_behaviours(pop, current_world, sd_peering)
        pop <- update_food_behaviours(pop, current_world, sd_peering)
      }
      
      # calculate output:
    
      # age classes:
      adults = pop[,39]/12 > 16
      subadults = pop[,39]/12 > 8 & pop[,39]/12 <= 16
      juveniles = pop[,39]/12 <= 8
    
      # customary:
      customary_adults <- colSums(pop[adults,1:38])>(sum(adults)/2)
      customary_subadults <- colSums(pop[subadults,1:38])>(sum(subadults)/2)
      customary_juveniles <- colSums(pop[juveniles,1:38])>(sum(juveniles)/2)
      customary <- customary_adults | customary_subadults | customary_juveniles
      output[output$population==current_world$population & output$run == run & customary==TRUE,4] <- "customary"
    
      # habitual:
      habitual <- colSums(pop[,1:38])>=2 & !customary
      output[output$population==current_world$population & output$run == run & habitual==TRUE,4] <- "habitual"
      
      # present:
      present <- colSums(pop[,1:38])==1
      output[output$population==current_world$population & output$run == run & present==TRUE,4] <- "present"
      
      # absent or ecological explanation:
      all_absent <- !(customary | habitual | present)
      absent <- all_absent & (current_world$type == "social" | current_world$p_e > 0)
      output[output$population==current_world$population & output$run == run & absent==TRUE,4] <- "absent"
      ecological_explanation <- all_absent & (current_world$type == "food-related" & current_world$p_e == 0)
      output[output$population==current_world$population & output$run == run & ecological_explanation==TRUE,4] <- "ecological explanation"
    }
  }
  output
}



## sandbox:::
my_output <- run_oranzees(6000,1,1,1,1,1)
# plot (only for one run):
my_output$population <- as.factor(my_output$population)
levels(my_output$population) <- unique(my_output$population) # to plot in right order
# social behaviour
my_output %>%
  filter(behaviour <= 16) %>%
  ggplot( aes(x = population, y = behaviour, colour = code)) +
  geom_point(size = 10) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("Social behaviours") +
  theme_bw()

# food-related behaviour
my_output %>%
  filter(behaviour > 16) %>%
  ggplot( aes(x = population, y = behaviour, colour = code)) +
  geom_point(size = 10) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  ggtitle("Food-related behaviours") +
  theme_bw()





