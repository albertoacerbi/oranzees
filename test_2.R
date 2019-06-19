library(tidyverse)

#
# set_oranzees_environment()
# function commons to all the tests
#

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

### NEW SUB-FUNCTIONS:

add_social_behaviour <- function(oranzee, b){
  if (b <= 4) {
    oranzee[1:4] <- 0
    oranzee[b] <- 1
  } else if (b > 4 & b <= 8) {
    oranzee[5:8] <- 0
    oranzee[b] <- 1
  } else if (b > 8 & b <= 12) {
    oranzee[9:12] <- 0
    oranzee[b] <- 1
  } else {
    oranzee[13:16] <- 0
    oranzee[b] <- 1
  }
  oranzee
}

add_food_behaviour <- function(oranzee, b){
  if (b <= 20) {
    oranzee[17:20] <- 0
    oranzee[b] <- 1
  } else if (b > 20 & b <= 24) {
    oranzee[21:24] <- 0
    oranzee[b] <- 1
  } else if (b > 24 & b <= 27) {
    oranzee[25:27] <- 0
    oranzee[b] <- 1
  } else if (b > 27 & b <= 30) {
    oranzee[28:30] <- 0
    oranzee[b] <- 1
  } else if (b > 30 & b <= 32) {
    oranzee[31:32] <- 0
    oranzee[b] <- 1
  } else if (b > 32 & b <= 34) {
    oranzee[33:34] <- 0
    oranzee[b] <- 1
  } else {
    oranzee[b] <- 1
  }
  oranzee
}

#############################################################################
#
# TEST FOR ONE RUN (only social behaviours)
#

test_oranzees1 <- function(t_max) {

  N <- 100

  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)

  output <- matrix(nrow = t_max, ncol = 16)

  for (t in 1:t_max) {
    output[t, ] <- colSums(pop[, 1:16])
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    for (i in 1:N) {
      state <- ((sum(pop[i, 1:4]) >= 1) + (sum(pop[i, 5:8]) >= 1) + (sum(pop[i, 9:12]) >= 1) + (sum(pop[i, 13:16]) >= 1)) / 4
      p_state <- rnorm(1, mean = 1 - state, sd = 0.05)
      if (runif(1) < p_state) {
        p_peering <- rnorm(16, mean = colSums(pop[, 1:16]), sd = 1)
        p_peering[p_peering < 0] <- 0
        innovation_i <- sample(1:16, 1, prob = p_peering)
        if (runif(1) < test_environment$p_g[innovation_i]) {
          if (innovation_i <= 4) {
            pop[i, 1:4] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 4 & innovation_i <= 8) {
            pop[i, 5:8] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 8 & innovation_i <= 12) {
            pop[i, 9:12] <- 0
            pop[i, innovation_i] <- 1
          } else {
            pop[i, 13:16] <- 0
            pop[i, innovation_i] <- 1
          }
        }
      }
    }
  }
  output
}

## OPTIMISED:
test_oranzees1_better <- function(t_max) {
  N <- 100
  
  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
  
  output <- matrix(nrow = t_max, ncol = 16)
  
  for (t in 1:t_max) {
    output[t, ] <- colSums(pop[, 1:16])
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    state <- ((rowSums(pop[, 1:4]) >= 1) + (rowSums(pop[, 5:8]) >= 1) + (rowSums(pop[, 9:12]) >= 1) + (rowSums(pop[, 13:16]) >= 1)) / 4
    p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
    p_peering <- rnorm(16, mean = colSums(pop[, 1:16]), sd = 1)
    p_peering[p_peering < 0] <- 0
    innovation_i <- sample(1:16, N, prob = p_peering, replace = TRUE)
    p_innovate <- runif(N) < test_environment$p_g[innovation_i]  * p_state
    for(i in 1:N){
      if(p_innovate[i]){
        pop[i,] <- add_social_behaviour(pop[i,], innovation_i[i])
      }
    }  
  }
  output
}

oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == "Uossob")

t_max <- 1000

my_test <- test_oranzees1(t_max)

# PLOT

my_test <- gather(as_tibble(my_test), 1:16, key = "behaviour", value = "frequency")
data_to_plot <- tibble(
  behaviour = my_test$behaviour,
  frequency = my_test$frequency,
  time = rep(1:t_max, 16),
  category = c(rep("play", t_max * 4), rep("display", t_max * 4), rep("groom", t_max * 4), rep("courthsip", t_max * 4))
)

ggplot(data = data_to_plot) +
  geom_line(aes(x = time, y = frequency, color = behaviour)) +
  facet_wrap(~category) +
  theme_bw() +
  theme(legend.position = "none")


#############################################################################
#
# TEST FOR ONE RUN (only food behaviours)
#

test_oranzees3 <- function(t_max) {
  
  N <- 100
  
  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
  
  output <- matrix(nrow = t_max, ncol = 22)
  
  for (t in 1:t_max) {
    output[t, ] <- colSums(pop[, 17:38])
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    for (i in 1:N) {
      nut_y <- (sum(pop[1, 17:20])>=1) + (sum(pop[1, 25:27])>=1) + (sum(pop[1, 31:32])>=1) + pop[1, 35] + pop[1, 37]
      nut_z <- (sum(pop[1, 21:24])>=1) + (sum(pop[1, 28:30])>=1) + (sum(pop[1, 33:34])>=1) + pop[1, 36] + pop[1, 38]
      state <- (nut_y + nut_z - abs(nut_y - nut_z)) / 10 
      p_state <- rnorm(1, mean = 1 - state, sd = .05)
      if (runif(1) < p_state) {
        p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = 1)
        p_peering[p_peering < 0] <- 0
        innovation_i <- sample(17:38, 1, prob = p_peering)
        if (runif(1) < (test_environment$p_g[innovation_i] * test_environment$p_e[innovation_i])) {
          if (innovation_i <= 20) {
            pop[i, 17:20] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 20 & innovation_i <= 24) {
            pop[i, 21:24] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 24 & innovation_i <= 27) {
            pop[i, 25:27] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 27 & innovation_i <= 30) {
            pop[i, 28:30] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 30 & innovation_i <= 32) {
            pop[i, 31:32] <- 0
            pop[i, innovation_i] <- 1
          } else if (innovation_i > 32 & innovation_i <= 34) {
            pop[i, 33:34] <- 0
            pop[i, innovation_i] <- 1
          } else {
            pop[i, innovation_i] <- 1
          }
        }
      }
    }
  }
  output
}
## OPTIMISED:

test_oranzees3_better <- function(t_max) {
  N <- 100
  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)
  output <- matrix(nrow = t_max, ncol = 22)
  
  for (t in 1:t_max) {
    output[t, ] <- colSums(pop[, 17:38])
    # demographic bit:
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    nut_y <- (rowSums(pop[, 17:20])>=1) + (rowSums(pop[, 25:27])>=1) + (rowSums(pop[, 31:32])>=1) + pop[, 35] + pop[, 37]
    nut_z <- (rowSums(pop[, 21:24])>=1) + (rowSums(pop[, 28:30])>=1) + (rowSums(pop[, 33:34])>=1) + pop[, 36] + pop[, 38]
    state <- (nut_y + nut_z - abs(nut_y - nut_z)) / 10 
    p_state <- runif(N) < rnorm(N, mean = 1 - state, sd = 0.05)
    p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = 1)
    p_peering[p_peering < 0] <- 0
    innovation_i <- sample(17:38, N, prob = p_peering, replace = TRUE)
    p_innovate <- runif(N) < test_environment$p_g[innovation_i] * test_environment$p_e[innovation_i] * p_state
    for(i in 1:N){
      if(p_innovate[i]){
        pop[i,] <- add_food_behaviour(pop[i,], innovation_i[i])
      }
    }
  }
  output
}

oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == "Uossob")

t_max <- 5000
tic()
my_test <- test_oranzees3(t_max)
toc()
# PLOT

my_test <- gather(as_tibble(my_test), 1:22, key = "behaviour", value = "frequency")
data_to_plot <- tibble(
  behaviour = my_test$behaviour,
  frequency = my_test$frequency,
  time = rep(1:t_max, 22),
  category = as_factor(c(
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

