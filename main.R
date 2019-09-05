library(tidyverse)
library(scales)
library(reshape2)
library(viridis)

#############################
# COMMON FUNCTIONS:
#############################

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

update_demography <- function(pop) {
  pop[, 39] <- pop[, 39] + 1
  pop[pop[, 39] >= 720, ] <- 0
  old <- which(pop[, 39] > 300)
  dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
  pop[old[dead], ] <- 0
  pop
}

update_social_behaviours <- function(pop, test_world) {
  N <- dim(pop)[1]
  state <- ((rowSums(pop[, 1:4]) >= 1) + (rowSums(pop[, 5:8]) >= 1) + (rowSums(pop[, 9:12]) >= 1) + (rowSums(pop[, 13:16]) >= 1)) / 4
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(16, mean = colSums(pop[, 1:16]), sd = 1)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(1:16, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i]  * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}  

update_food_behaviours <- function(pop, test_world) {
  N <- dim(pop)[1]
  nut_y <- (rowSums(pop[, 17:20])>=1) + (rowSums(pop[, 25:27])>=1) + (rowSums(pop[, 31:32])>=1) + pop[, 35] + pop[, 37]
  nut_z <- (rowSums(pop[, 21:24])>=1) + (rowSums(pop[, 28:30])>=1) + (rowSums(pop[, 33:34])>=1) + pop[, 36] + pop[, 38]
  state <- (nut_y + nut_z - abs(nut_y - nut_z)) / 10 
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = 1)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(17:38, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i] * test_world$p_e[innovation_i] * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}

use_behaviour <- function(pop, optimisation){
  N <- dim(pop)[1]
  optimise <- sample( c(TRUE, FALSE), N , prob = c(optimisation, 1-optimisation), replace = TRUE)
  for( i in (1:N)[optimise]){
    if(sum(pop[i, 1 : 4]) > 1){
      pop[i, sample(which(pop[i, 1 : 4] == 1), 1)] = 0
    }
    if(sum(pop[i, 5 : 8]) > 1){
      pop[i, sample(which(pop[i, 5 : 8] == 1), 1)+4] = 0
    }
    if(sum(pop[i, 9 : 12]) > 1){
      pop[i, sample(which(pop[i, 9 : 12] == 1), 1)+8] = 0
    }
    if(sum(pop[i, 13 : 16]) > 1){
      pop[i, sample(which(pop[i, 13 : 16] == 1), 1)+12] = 0
    }
    if(sum(pop[i, 17 : 20]) > 1){
      pop[i, sample(which(pop[i, 17 : 20] == 1), 1)+16] = 0
    }
    if(sum(pop[i, 21 : 24]) > 1){
      pop[i, sample(which(pop[i, 21 : 24] == 1), 1)+20] = 0
    }
    if(sum(pop[i, 25 : 27]) > 1){
      pop[i, sample(which(pop[i, 25 : 27] == 1), 1)+24] = 0
    }
    if(sum(pop[i, 28 : 30]) > 1){
      pop[i, sample(which(pop[i, 28 : 30] == 1), 1)+27] = 0
    }
    if(sum(pop[i, 31 : 32]) > 1){
      pop[i, sample(which(pop[i, 31 : 32] == 1), 1)+30] = 0
    }
    if(sum(pop[i, 33 : 34]) > 1){
      pop[i, sample(which(pop[i, 33 : 34] == 1), 1)+32] = 0
    }
  }
  pop
}

###########################
# MAIN FUNCTIONS:
###########################
run_oranzees <- function(t_max, optimisation, alpha_g, init_world, n_run) {
  
  N <- c(50, 100, 150, 50, 100, 150)
  
  oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
  
  output <- tibble(run = rep(1:n_run, each= 38 * 6),
                    population = rep(oranzees_world$population, n_run),
                   behaviour = rep(1:38, 6 * n_run),
                    code = rep("NA", 38 * 6 * n_run))
  
  for(run in 1:n_run){
    
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
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
        pop <- update_social_behaviours(pop, current_world)
        pop <- update_food_behaviours(pop, current_world)
        if(optimisation)
          pop <- use_behaviour(pop, optimisation)
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

# TEST FUNCTION 1 (RUN ONLY ON ONE POPULATION AND PRODUCES A RICHER OUTPUT):

test_oranzees_1 <- function(t_max, optimisation, alpha_g, init_world, n_run) {
  
  N <- 100
  
  if(n_run == 1){
    output <- matrix(nrow = t_max, ncol = 38)
  }
  else{
    output <- matrix(nrow = n_run, ncol = 38)
  }
  
  oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
  test_world <- oranzees_world %>%
    filter(population == "Uossob")
  
  for(run in 1:n_run){
    pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
      test_world <- oranzees_world %>%
        filter(population == "Uossob")
    }
    # start simulation here:
    for (t in 1:t_max) {
      if(n_run == 1){
        output[t,] <- colSums(pop[, 1:38])
      }
      pop <- update_demography(pop)
      pop <- update_social_behaviours(pop, test_world)
      pop <- update_food_behaviours(pop, test_world)
      if(optimisation)
        pop <- use_behaviour(pop, optimisation)
    }
    if( n_run > 1){
      output[run, ] <- colSums(pop[, 1:38])
    }  
  }
  output
}

# TEST FUNCTION 2 (RUN ONLY ON ONE POPULATION):

test_oranzees_2 <- function(t_max, optimisation, alpha_g, init_world, n_run) {
  
  N <- 100
  
  output <- matrix(nrow = n_run, ncol = 5)
  
  oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
  test_world <- oranzees_world %>%
    filter(population == "Uossob")
  
  for(run in 1:n_run){
    pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e = 1)
      test_world <- oranzees_world %>%
        filter(population == "Uossob")
    }
    # start simulation here:
    for (t in 1:t_max) {
      pop <- update_demography(pop)
      pop <- update_social_behaviours(pop, test_world)
      pop <- update_food_behaviours(pop, test_world)
      if(optimisation)
        pop <- use_behaviour(pop, optimisation)
    }
    # calculate codes values:
    
    # age classes:
    adults = pop[,39]/12 > 16
    subadults = pop[,39]/12 > 8 & pop[,39]/12 <= 16
    juveniles = pop[,39]/12 <= 8
    
    # customary:
    customary_adults <- colSums(pop[adults,1:38])>(sum(adults)/2)
    customary_subadults <- colSums(pop[subadults,1:38])>(sum(subadults)/2)
    customary_juveniles <- colSums(pop[juveniles,1:38])>(sum(juveniles)/2)
    customary <- customary_adults | customary_subadults | customary_juveniles
    output[run, 1] <- sum(customary)
    
    # habitual:
    habitual <- colSums(pop[,1:38])>=2 & !customary
    output[run, 2] <- sum(habitual)
    
    # present:
    present <- colSums(pop[,1:38])==1
    output[run, 3] <- sum(present)
    
    # absent or ecological explanation:
    all_absent <- !(customary | habitual | present)
    absent <- all_absent & (test_world$p_e > 0 | test_world$type == "social") 
    output[run, 4] <- sum(absent)
    ecological_explanation <- all_absent & (test_world$p_e == 0 & test_world$type == "food-related" ) 
    output[run, 5] <- sum(ecological_explanation)
  }
  output
}

#######################
# VISUALISATIONS:
#######################
# TO USE WITH test_oranzees_1():

plot_one_run <- function(my_test) {
  t_max = dim(my_test)[1]
  my_test <- gather(as_tibble(my_test), 1:38, key = "behaviour", value = "frequency")
  data_to_plot <- tibble(
    behaviour = my_test$behaviour,
    frequency = my_test$frequency,
    time = rep(1:t_max, 38),
    category = as_factor(c(
      rep("play", t_max * 4), rep("display", t_max * 4), rep("groom", t_max * 4), rep("courthsip", t_max * 4),
      rep("A", t_max * 4), rep("B", t_max * 4), rep("C", t_max * 3), rep("D", t_max * 3),
      rep("E", t_max * 2), rep("F", t_max * 2), rep("G", t_max), rep("H", t_max),
      rep("I", t_max), rep("J", t_max)
    ))
  )
  ggplot(data = data_to_plot) +
    geom_line(aes(x = time, y = frequency, color = behaviour)) +
    facet_wrap(~category) +
    theme_bw() +
    theme(legend.position = "none")
}

plot_multiple_runs <- function(my_test) {
  n_run = dim(my_test)[1]
  as_tibble(melt(my_test, varnames = c("run", "behaviour"), value.name = "frequency")) %>%
    mutate(run = as_factor(run), behaviour = as_factor(behaviour)) %>%
    add_column(category = as_factor(c(
      rep("play", 4 * n_run), rep("display", 4 * n_run), rep("groom", 4 * n_run), rep("courthsip", 4 * n_run),
      rep("A", 4 * n_run), rep("B", 4 * n_run), rep("C", 3 * n_run), rep("D", 3 * n_run),
      rep("E", 2 * n_run), rep("F", 2 * n_run), rep("G", n_run), rep("H", n_run),
      rep("I", n_run), rep("J", n_run)
    ))) %>%
    ggplot() +
    geom_raster(aes(x = behaviour, y = run, fill = frequency)) +
    facet_wrap(~category, scales = "free") +
    scale_fill_gradient(low = "grey90", high = "red") +
    theme_bw()
}

# TO USE WITH test_oranzees_2():

plot_codes_distribution <- function(my_test) {
  n_run = dim(my_test)[1]
  tibble(code = as_factor(rep(c("customary", "habitual", "present", "absent", "ecological"), each=n_run)),
         behaviours = as.vector(my_test)) %>%
  ggplot(aes( x = code, y = behaviours, fill = code)) +
    geom_boxplot() +
    geom_jitter(width=0.05, alpha=0.5) +
    theme_bw()  
}

# TO USE WITH run_oranzees() - only for single runs:

plot_oranzees <- function(my_test, social) {
  my_test$population <- as.factor(my_test$population)
  levels(my_test$population) <- unique(my_test$population) # to plot in right order
  if(social){
    my_test %>%
      filter(behaviour <= 16) %>%
      ggplot( aes(x = population, y = behaviour, colour = code)) +
        geom_point(size = 10) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        ggtitle("Social behaviours") +
        theme_bw()
  }
  else{
    my_test %>%
      filter(behaviour > 16) %>%
      ggplot( aes(x = population, y = behaviour, colour = code)) +
        geom_point(size = 10) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        ggtitle("Food-related behaviours") +
        theme_bw()
  }
}

