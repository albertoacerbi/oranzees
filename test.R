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
  output[output$pattern=="A",]$proportion <- (38 - temp$n) / 38
  
  temp <- count(process, B) %>%
    filter(B == FALSE)
  output[output$pattern=="B",]$proportion <- (38 - temp$n) / 38
  
  temp <- count(process, C) %>%
    filter(C == FALSE)
  output[output$pattern=="C",]$proportion <- (38 - temp$n) / 38
  
  temp <- count(process, D) %>%
    filter(D == FALSE)
  output[output$pattern=="D",]$proportion <- (38 - temp$n) / 38
  
  output
}

# TESTS HERE:

tic()
test <- run_oranzees(t_max = 6000, opt = 0, alpha_g = 0.5, init_world = 1, n_run = 5)
toc()

data_to_plot <- analyse_patterns(test)
whiten <- tibble(pattern = 1:4, 
                 proportion = c(7/65, 16/65, 3/65, 38/65), 
                 data = rep("Whiten", 4)) 
ggplot(data = data_to_plot, aes(x = pattern, y = proportion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.05, alpha=0.5) +
  theme_bw() +
  geom_line(data = whiten, colour="red") +
  geom_point(data = whiten, colour="red") 


####
# initial tests:

tic()
test_0_05 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test_0_05, "output_test/test_0_0.5.csv")

tic()
test_0_06 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.6, init_world = 1, n_run = 20)
toc()
write_csv(test_0_06, "output_test/test_0_0.6.csv")

tic()
test_0_07 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
write_csv(test_0_07, "output_test/test_0_0.7.csv")

tic()
test_01_05 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test_01_05, "output_test/test_0.1_0.5.csv")

tic()
test_01_06 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.6, init_world = 1, n_run = 20)
toc()
write_csv(test_01_06, "output_test/test_0.1_0.6.csv")

tic()
test_01_07 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
write_csv(test_01_07, "output_test/test_0.1_0.7.csv")


