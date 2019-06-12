library(tidyverse)

set_oranzees_environment <- function() {
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

##
##
##
##
# TEST FOR ONE RUN:

test_oranzees1 <- function(t_max){

  oranzees_environment <- set_oranzees_environment()
  test_environment <- oranzees_environment %>%
    filter(population == 'Uossob')
  
  N <- 100
  
  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
  
  output <- matrix( nrow = t_max, ncol = 16)
  
  for (t in 1:t_max) {
    
    output[t, ] <- colSums(pop[, 1:16])
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    for(i in 1:N){
      state <- ((sum(pop[i, 1:4]) >= 1) + (sum(pop[i, 5:8]) >= 1) + (sum(pop[i, 9:12]) >= 1) + (sum(pop[i, 13:16]) >= 1)) / 4
      p_state <- rnorm(1, mean = 1 - state, sd = 0.05)
      if(runif(1) < p_state){
        p_peering <- rnorm(16, mean = colSums(pop[, 1:16]))
        p_peering[p_peering < 0] = 0
        innovation_i <- sample(1:16, 1, prob = p_peering)
        if( runif(1) < test_environment$p_g[innovation_i]){
          if(innovation_i <= 4){
            pop[i, 1:4] = 0
            pop[i, innovation_i] = 1
          } else if(innovation_i > 4 & innovation_i <= 8){
            pop[i, 5:8] = 0
            pop[i, innovation_i] = 1
          } else if(innovation_i > 8 & innovation_i <= 12){
            pop[i, 9:12] = 0
            pop[i, innovation_i] = 1
          } else{
            pop[i, 13:16] = 0
            pop[i, innovation_i] = 1
          }
        }
      }
    }
  }
  output
}

t_max = 12000
my_test <- test_oranzees1(t_max)
# matplot(my_test[,1:4], type = "l")

# PLOT

my_test <- gather(as_tibble(my_test), 1:16, key = "behaviour", value = "frequency")
data_to_plot = tibble(behaviour = my_test$behaviour, 
                      frequency = my_test$frequency, 
                      time = rep(1:t_max ,16), 
                      category = c(rep("play", t_max*4), rep("display", t_max*4), rep("groom", t_max*4), rep("courthsip", t_max*4)))

ggplot(data = data_to_plot) +
  geom_line(aes(x = time, y = frequency, color = behaviour)) +
  facet_wrap(~category) +
  theme_bw() +
  theme(legend.position = "none")

##
##
##
##
# TEST FOR MULTIPLE RUNS:
  
test_oranzees2 <- function(t_max){
  
  N <- 100
  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
  
  for (t in 1:t_max) {
    
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    for(i in 1:N){
      state <- ((sum(pop[i, 1:4]) >= 1) + (sum(pop[i, 5:8]) >= 1) + (sum(pop[i, 9:12]) >= 1) + (sum(pop[i, 13:16]) >= 1)) / 4
      p_state <- rnorm(1, mean = 1 - state, sd = 0.05)
      if(runif(1) < p_state){
        p_peering <- rnorm(16, mean = colSums(pop[, 1:16]))
        p_peering[p_peering < 0] = 0
        innovation_i <- sample(1:16, 1, prob = p_peering)
        if( runif(1) < test_environment$p_g[innovation_i]){
          if(innovation_i <= 4){
            pop[i, 1:4] = 0
            pop[i, innovation_i] = 1
          } else if(innovation_i > 4 & innovation_i <= 8){
            pop[i, 5:8] = 0
            pop[i, innovation_i] = 1
          } else if(innovation_i > 8 & innovation_i <= 12){
            pop[i, 9:12] = 0
            pop[i, innovation_i] = 1
          } else{
            pop[i, 13:16] = 0
            pop[i, innovation_i] = 1
          }
        }
      }
    }
  }
  output <- colSums(pop[, 1:16])
}


oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == 'Uossob')

n_run <- 10
t_max <- 12000
gene_test1 <- matrix(nrow = n_run, ncol = 16)
for(run in 1:n_run){
  gene_test1[run,] <- test_oranzees2(t_max)
}

write(t(gene_test1), file = "output/gene_test1.csv", ncolumns = 16)
write(test_environment$p_g[1:16], file = "output/gene_test1_p_g.csv", ncolumns = 1)





