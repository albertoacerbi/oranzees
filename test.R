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

#############################################################################
#
# TEST FOR ONE RUN (only social behaviours)
#

test_oranzees1 <- function(t_max) {
  oranzees_environment <- set_oranzees_environment()
  test_environment <- oranzees_environment %>%
    filter(population == "Uossob")

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

t_max <- 12000
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
# TEST FOR MULTIPLE RUNS WITH SAME INITIALISATION (only social behaviours)
#

test_oranzees2 <- function(t_max) {
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
    for (i in 1:N) {
      state <- ((sum(pop[i, 1:4]) >= 1) + (sum(pop[i, 5:8]) >= 1) + (sum(pop[i, 9:12]) >= 1) + (sum(pop[i, 13:16]) >= 1)) / 4
      p_state <- rnorm(1, mean = 1 - state, sd = 0.05)
      if (runif(1) < p_state) {
        p_peering <- rnorm(16, mean = colSums(pop[, 1:16]))
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
  output <- colSums(pop[, 1:16])
}


oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == "Uossob")

n_run <- 10
t_max <- 12000
gene_test1 <- matrix(nrow = n_run, ncol = 16)
for (run in 1:n_run) {
  gene_test1[run, ] <- test_oranzees2(t_max)
}

write(t(gene_test1), file = "output/social_test1.csv", ncolumns = 16)
write(test_environment$p_g[1:16], file = "output/social_test1_p_g.csv", ncolumns = 1)

# PLOT:
library(reshape2)
results <- as.matrix(read.table("output/gene_test1.csv"))
colnames(results) <- 1:16

as_tibble(melt(results, varnames = c("run", "behaviour"), value.name = "frequency")) %>%
  mutate(run = as_factor(run), behaviour = as_factor(behaviour)) %>%
  add_column(category = as_factor(c(rep("play", 40), rep("display", 40), rep("groom", 40), rep("courthsip", 40)))) %>%
  ggplot() +
  geom_raster(aes(x = behaviour, y = run, fill = frequency)) +
  facet_wrap(~category, scales = "free") +
  scale_fill_gradient(low = "grey90", high = "red") +
  theme_bw()

#############################################################################
#
# TEST FOR ONE RUN (only food behaviours)
#

test_oranzees3 <- function(t_max) {
  oranzees_environment <- set_oranzees_environment()
  test_environment <- oranzees_environment %>%
    filter(population == "Uossob")

  N <- 100

  pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)

  output <- matrix(nrow = t_max, ncol = 22)

  for (t in 1:t_max) {
    # demographic bit:
    output[t, ] <- colSums(pop[, 17:38])
    pop[, 39] <- pop[, 39] + 1
    pop[pop[, 39] >= 720, ] <- 0
    old <- which(pop[, 39] > 300)
    dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
    pop[old[dead], ] <- 0
    # innovation bit:
    for (i in 1:N) {
      food_behaviours <- which(pop[i, 17:38] > 0) + 16
      if (length(food_behaviours) > 1) {
        amount <- test_environment$p_e[food_behaviours]
        nutrients <- test_environment$nutrient[food_behaviours]
        if (length(unique(nutrients)) > 1) {
          state <- ((1 - abs(sum(amount[nutrients == "Y"]) - sum(amount[nutrients == "Z"])) / 5) + sum(amount) / 10) / 2
        } else {
          state <- 0
        }
      } else {
        state <- 0
      }
      p_state <- rnorm(1, mean = 1 - state, sd = .05)
      if (runif(1) < p_state) {
        p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = 1)
        p_peering[p_peering < 0] <- 0
        innovation_i <- sample(17:38, 1, prob = p_peering)
        if (runif(1) < test_environment$p_g[innovation_i]) {
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

t_max <- 1000
my_test <- test_oranzees3(t_max)

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

#############################################################################
#
# TEST FOR MULTIPLE RUNS WITH SAME INITIALISATION (only food behaviours)
#

test_oranzees4 <- function(t_max) {
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
    for (i in 1:N) {
      food_behaviours <- which(pop[i, 17:38] > 0) + 16
      if (length(food_behaviours) > 1) {
        amount <- test_environment$p_e[food_behaviours]
        nutrients <- test_environment$nutrient[food_behaviours]
        if (length(unique(nutrients)) > 1) {
          state <- ((1 - abs(sum(amount[nutrients == "Y"]) - sum(amount[nutrients == "Z"])) / 5) + sum(amount) / 10) / 2
        } else {
          state <- 0
        }
      } else {
        state <- 0
      }
      p_state <- rnorm(1, mean = 1 - state, sd = .05)
      if (runif(1) < p_state) {
        p_peering <- rnorm(22, mean = colSums(pop[, 17:38]), sd = 1)
        p_peering[p_peering < 0] <- 0
        innovation_i <- sample(17:38, 1, prob = p_peering)
        if (runif(1) < test_environment$p_g[innovation_i]) {
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
  output <- colSums(pop[, 17:38])
}

oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == "Uossob")

n_run <- 10
t_max <- 12000
food_test1 <- matrix(nrow = n_run, ncol = 22)
for (run in 1:n_run) {
  food_test1[run, ] <- test_oranzees4(t_max)
  print(run)
}
write(t(food_test1), file = "output/food_test5.csv", ncolumns = 22)
write(test_environment$p_g[17:38], file = "output/food_test5_p_g.csv", ncolumns = 1)
write(test_environment$p_e[17:38], file = "output/food_test5_p_e.csv", ncolumns = 1)


# PLOT:

library(reshape2)
results <- as.matrix(read.table("output/food_test5.csv"))
colnames(results) <- 17:38

as_tibble(melt(results, varnames = c("run", "behaviour"), value.name = "frequency")) %>%
  mutate(run = as_factor(run), behaviour = as_factor(behaviour)) %>%
  add_column(category = as_factor(c(
    rep("A", 40), rep("B", 40), rep("C", 30), rep("D", 30),
    rep("E", 20), rep("F", 20), rep("G", 10), rep("H", 10),
    rep("I", 10), rep("J", 10)
  ))) %>%
  ggplot() +
  geom_raster(aes(x = behaviour, y = run, fill = frequency)) +
  facet_wrap(~category, scales = "free") +
  scale_fill_gradient(low = "grey90", high = "red") +
  theme_bw()





###### SANDBOX #########

oranzees_environment <- set_oranzees_environment()
test_environment <- oranzees_environment %>%
  filter(population == "Uossob")

N <- 100
pop <- matrix(c(rep(0, 38 * N), sample(1:300, N, replace = TRUE)), nrow = N, byrow = FALSE)

output <- matrix(nrow = t_max, ncol = 22)
output_state <- rep(NA, t_max)

for (t in 1:t_max) {
  output_state_t <- 0
  # demographic bit:
  output[t, ] <- colSums(pop[, 17:38])
  pop[, 39] <- pop[, 39] + 1
  pop[pop[, 39] >= 720, ] <- 0
  old <- which(pop[, 39] > 300)
  dead <- sample(c(TRUE, FALSE), length(old), prob = c(.01, .99), replace = TRUE)
  pop[old[dead], ] <- 0
  # innovation bit:
  for (i in 1:N) {
    nut_y <- sum(pop[i, 17:20]) + sum(pop[i, 25:27]) + sum(pop[i, 31:32]) + pop[i, 35] + pop[i, 37]
    nut_z <- sum(pop[i, 21:24]) + sum(pop[i, 28:30]) + sum(pop[i, 33:34]) + pop[i, 36] + pop[i, 38]
    state <- sum(nut_y + nut_z)/10 - abs(nut_y - nut_z)/5 
    p_state <- rnorm(1, mean = 1 - state, sd = .05)
    output_state_t <- output_state_t + state
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
  output_state[t] <- output_state_t / N
}


my_test <- gather(as_tibble(output), 1:22, key = "behaviour", value = "frequency")
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
