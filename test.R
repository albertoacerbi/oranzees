library(tictoc)

# USE FUNCTIONS IN main.R
source("main.R")

# function for test
analyse_patterns <- function(my_test){
  output <- tibble(pattern = rep(c("A", "B", "C", "D"), each = max(my_test$run)), 
                   proportion = rep(NA, max(my_test$run) * 4))
  process <- my_test %>%
    group_by(behaviour, run) %>%
    summarise(A = !("absent" %in% code) && !("ecological explanation" %in% code), 
              B = !("habitual" %in% code) && !("customary" %in% code),
              C = !("absent" %in% code) && ("ecological explanation" %in% code)) %>%
    ungroup() %>%
    mutate (D = !(A | B | C)) %>%
    group_by(run)
  
  temp <- count(process, A) %>%
    filter(A == FALSE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="A",]$proportion <- (64 - out) / 64
  
  temp <- count(process, B) %>%
    filter(B == FALSE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="B",]$proportion <- (64 - out) / 64
  
  temp <- count(process, C) %>%
    filter(C == FALSE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="C",]$proportion <- (64 - out) / 64
  
  temp <- count(process, D) %>%
    filter(D == FALSE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="D",]$proportion <- (64 - out) / 64
  
  output
}

# TESTS HERE:

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 0, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_0_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_0_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 1, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_0_1.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 0, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_05_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_05_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 1, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_05_1.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 0, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_1_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_1_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 1, init_world = 1, n_run = 20)
toc()
write_csv(test, "output_test/test_1_1.csv")


#####


data_to_plot <- analyse_patterns(test)
whiten <- tibble(pattern = 1:4, 
                 proportion = c(7/65, 16/65, 3/65, 38/65), 
                 data = rep("Whiten", 4)) 
ggplot(data = data_to_plot, aes(x = pattern, y = proportion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.05, alpha = 0.5) +
  theme_bw() +
  geom_line(data = whiten, colour = "red") +
  geom_point(data = whiten, colour = "red") 

