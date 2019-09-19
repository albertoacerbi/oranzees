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
    population = rep(list_pop, each = 64),
    behaviour = as.factor(rep(1:64, 6)),
    type = rep(c(rep("social", 32), rep("food-related", 32)), 6),
    category = rep(c(
      rep("play", 8), rep("display", 8), rep("groom", 8), rep("courthsip", 8),
      "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B","C", "C", "C", "C", "D", "D", "D", "D", "E", "E", "F", "F", "G", "H", "I", "J"
    ), 6),
    nutrient = rep(c(rep(NA, 32), "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Y", "Y", "Y", "Y", "Z", "Z", "Z", "Z", "Y", "Y", "Z", "Z", "Y", "Z", "Y", "Z"), 6),
    p_g = rep(NA, 64 * 6),
    p_e = rep(NA, 64 * 6)
  )

  env_or_x <- c(220, 230, 700, 705, 710, 750)
  env_or_y <- c(660, 610, 450, 430, 510, 550)

  # genetic predispositions:
  p_g <- runif(64)
  x_g <- sample(1:1000, 64)
  y_g <- sample(1:1000, 64)
  

  # ecological availability:
  p_e <- runif(64)
  x_e <- sample(1:1000, 64)
  y_e <- sample(1:1000, 64)
  

  for(behav in 1:64){
    if(runif(1) < alpha_g){
      output[output$behaviour == behav,]$p_g <- 1 - rescale(sqrt((x_g[behav] - env_or_x)^2 + (y_g[behav] - env_or_y)^2))
    }
    else{
      output[output$behaviour == behav,]$p_g <- p_g[behav]
    }
    if(behav > 32){
      if(runif(1) < alpha_e){
        output[output$behaviour == behav,]$p_e <- 1 - rescale(sqrt((x_e[behav] - env_or_x)^2 + (y_e[behav] - env_or_y)^2))
      }
      else{
        output[output$behaviour == behav,]$p_e <- p_e[behav]
      }
    }
  }
  # return the tibble:
  output
}

update_demography <- function(pop) {
  pop[, 65] <- pop[, 65] + 1
  pop[pop[, 65] >= 720, ] <- 0
  old <- which(pop[, 65] > 300)
  dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
  pop[old[dead], ] <- 0
  pop
}

update_social_behaviours <- function(pop, test_world) {
  N <- dim(pop)[1]
  state <- ((rowSums(pop[, 1:8]) >= 1) + (rowSums(pop[, 9:16]) >= 1) + (rowSums(pop[, 17:24]) >= 1) + (rowSums(pop[, 25:32]) >= 1)) / 4
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(32, mean = colSums(pop[, 1:32]), sd = 1)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(1:32, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i]  * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}  

update_food_behaviours <- function(pop, test_world) {
  N <- dim(pop)[1]
  nut_y <- (rowSums(pop[, 33:40])>=1) + (rowSums(pop[, 49:52])>=1) + (rowSums(pop[, 57:58])>=1) + pop[, 61] + pop[, 64]
  nut_z <- (rowSums(pop[, 41:48])>=1) + (rowSums(pop[, 53:56])>=1) + (rowSums(pop[, 59:60])>=1) + pop[, 62] + pop[, 63]
  state <- (nut_y + nut_z - abs(nut_y - nut_z)) / 10 
  p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
  p_peering <- rnorm(32, mean = colSums(pop[, 33:64]), sd = 1)
  p_peering[p_peering < 0] <- 0
  innovation_i <- sample(33:64, N, prob = p_peering, replace = TRUE)
  p_innovate <- runif(N) < test_world$p_g[innovation_i] * test_world$p_e[innovation_i] * p_state
  for (i in (1:N)[p_innovate]) {
    pop[i, innovation_i[i]] <- 1
  }
  pop
}

###########################
# MAIN FUNCTIONS:
###########################
# `run_oranzees()` runs the simulation on the six oranzees populations, 
# and produces as output the code for each behaviour in each population
run_oranzees <- function(t_max, alpha_g, alpha_e, init_world, n_run) {
  
  N <- c(20, 76, 50, 95, 42, 49)
  
  oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
  
  output <- tibble(run = rep(1:n_run, each= 64 * 6),
                    population = rep(oranzees_world$population, n_run),
                   behaviour = rep(1:64, 6 * n_run),
                    code = rep("NA", 64 * 6 * n_run))
  
  for(run in 1:n_run){
    
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
    }
    
    for(current_population in 1:6){ 
      pop <- matrix(c(rep(0, 64 * N[current_population]), 
                      sample(1:300, N[current_population], replace = TRUE)), 
                    nrow = N[current_population], byrow = FALSE)
      current_world <- oranzees_world %>%
        filter(population == unique(oranzees_world$population)[current_population])
      # start simulation here:
      for (t in 1:t_max) {
        pop <- update_demography(pop)
        pop <- update_social_behaviours(pop, current_world)
        pop <- update_food_behaviours(pop, current_world)
      }
      
      # calculate output:
    
      # age classes:
      adults = pop[,65]/12 > 16
      subadults = pop[,65]/12 > 8 & pop[,65]/12 <= 16
      juveniles = pop[,65]/12 <= 8
    
      # customary:

      customary_adults <- rep(FALSE, 64)
      if(sum(adults) >= 3){
        customary_adults <- colSums(pop[adults,1:64])>(sum(adults)/2) 
      }
      customary_subadults <- rep(FALSE, 64)
      if(sum(subadults) >= 3){
        customary_subadults <- colSums(pop[subadults,1:64])>(sum(subadults)/2)
      }
      customary_juveniles <- rep(FALSE, 64)
      if(sum(juveniles) >= 3){
        customary_juveniles <- colSums(pop[juveniles,1:64])>(sum(juveniles)/2) 
      }
      customary <- customary_adults | customary_subadults | customary_juveniles
      output[output$population==current_world$population & output$run == run & customary==TRUE,4] <- "customary"
    
      # habitual:
      habitual <- colSums(pop[,1:64])>=2 & !customary
      output[output$population==current_world$population & output$run == run & habitual==TRUE,4] <- "habitual"
      
      # present:
      present <- colSums(pop[,1:64])==1
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

# `test_oranzees_1()` runs the simulation only on one population, and produces a richer output, 
# consisting in the frequencies of all behaviours at each time step when `n_run=1`, 
# and in the final frequencie of all behaviours when `n_run>1`.
test_oranzees_1 <- function(t_max, alpha_g, alpha_e, init_world, n_run) {

  N <- 100

  if(n_run == 1){
    output <- matrix(nrow = t_max, ncol = 64)
  }
  else{
    output <- matrix(nrow = n_run, ncol = 64)
  }

  oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
  test_world <- oranzees_world %>%
    filter(population == "Uossob")

  for(run in 1:n_run){
    pop <- matrix(c(rep(0, 64 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
      test_world <- oranzees_world %>%
        filter(population == "Uossob")
    }
    # start simulation here:
    for (t in 1:t_max) {
      if(n_run == 1){
        output[t,] <- colSums(pop[, 1:64])
      }
      pop <- update_demography(pop)
      pop <- update_social_behaviours(pop, test_world)
      pop <- update_food_behaviours(pop, test_world)
    }
    if( n_run > 1){
      output[run, ] <- colSums(pop[, 1:64])
    }
  }
  output
}

# `test_oranzees_2()` also runs the simulation on one population, but gives as output 
# the behavioural codes as described in Whiten et al., 1999
test_oranzees_2 <- function(t_max, alpha_g, alpha_e, init_world, n_run) {

  N <- 100

  output <- matrix(nrow = n_run, ncol = 5)

  oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
  test_world <- oranzees_world %>%
    filter(population == "Uossob")

  for(run in 1:n_run){
    pop <- matrix(c(rep(0, 64 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
    if (init_world) {
      oranzees_world <- set_oranzees_world(alpha_g, alpha_e)
      test_world <- oranzees_world %>%
        filter(population == "Uossob")
    }
    # start simulation here:
    for (t in 1:t_max) {
      pop <- update_demography(pop)
      pop <- update_social_behaviours(pop, test_world)
      pop <- update_food_behaviours(pop, test_world)
    }
    # calculate codes values:

    # age classes:
    adults = pop[,65]/12 > 16
    subadults = pop[,65]/12 > 8 & pop[,65]/12 <= 16
    juveniles = pop[,65]/12 <= 8

    # customary:
    customary_adults <- rep(FALSE, 64)
    if(sum(adults) >= 3){
      customary_adults <- colSums(pop[adults,1:64])>(sum(adults)/2)
    }
    customary_subadults <- rep(FALSE, 64)
    if(sum(subadults) >= 3){
      customary_subadults <- colSums(pop[subadults,1:64])>(sum(subadults)/2)
    }
    customary_juveniles <- rep(FALSE, 64)
    if(sum(juveniles) >= 3){
      customary_juveniles <- colSums(pop[juveniles,1:64])>(sum(juveniles)/2)
    }
    customary <- customary_adults | customary_subadults | customary_juveniles
    output[run, 1] <- sum(customary)

    # habitual:
    habitual <- colSums(pop[,1:64])>=2 & !customary
    output[run, 2] <- sum(habitual)

    # present:
    present <- colSums(pop[,1:64])==1
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
# PLOTTING FUNCTIONS:
#######################
# TO USE WITH test_oranzees_1():

plot_one_run <- function(my_test) {
  t_max = dim(my_test)[1]
  my_test <- gather(as_tibble(my_test), 1:64, key = "behaviour", value = "frequency")
  data_to_plot <- tibble(
    behaviour = my_test$behaviour,
    frequency = my_test$frequency,
    time = rep(1:t_max, 64),
    category = as_factor(c(
      rep("play", t_max * 8), rep("display", t_max * 8), rep("groom", t_max * 8), rep("courthsip", t_max * 8),
      rep("A", t_max * 8), rep("B", t_max * 8), rep("C", t_max * 4), rep("D", t_max * 4),
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
      rep("play", 8 * n_run), rep("display", 8 * n_run), rep("groom", 8 * n_run), rep("courthsip", 8 * n_run),
      rep("A", 8 * n_run), rep("B", 8 * n_run), rep("C", 4 * n_run), rep("D", 4 * n_run),
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
      filter(behaviour <= 32) %>%
      ggplot( aes(x = population, y = behaviour, colour = code)) +
        geom_point(size = 10) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        ggtitle("Social behaviours") +
        theme_bw()
  }
  else{
    my_test %>%
      filter(behaviour > 32) %>%
      ggplot( aes(x = population, y = behaviour, colour = code)) +
        geom_point(size = 10) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        ggtitle("Food-related behaviours") +
        theme_bw()
  }
}

