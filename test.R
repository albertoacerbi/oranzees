library(tictoc)

# USE FUNCTIONS IN main.R
source("main.R")

# function for test
analyse_patterns <- function(my_test){
  output <- tibble(pattern = rep(c("A", "B", "C", "D"), each = max(my_test$run)), 
                   n = rep(NA, max(my_test$run) * 4))
  process <- my_test %>%
    group_by(behaviour, run) %>%
    summarise(A = !("absent" %in% code) && !("ecological explanation" %in% code), 
              B = !("habitual" %in% code) && !("customary" %in% code),
              C = !("absent" %in% code) && ("ecological explanation" %in% code)) %>%
    ungroup() %>%
    mutate (D = !(A | B | C)) %>%
    group_by(run)
  
  temp <- count(process, A) %>%
    filter(A == TRUE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="A",]$n <- out  
  
  temp <- count(process, B) %>%
    filter(B == TRUE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="B",]$n <- out  
  
  temp <- count(process, C) %>%
    filter(C == TRUE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="C",]$n <- out 
  
  temp <- count(process, D) %>%
    filter(D == TRUE)
  out <- temp$n
  if(length(temp$n) < max(my_test$run)){
    out <- c(temp$n, rep(0, max(my_test$run)-length(temp$n)))
  }
  output[output$pattern=="D",]$n <- out 
  
  output
}


# TEST 1

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 0, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_0_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 0.5, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_0_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0, alpha_g = 1, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_0_1.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 0, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_05_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 0.5, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_05_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 0.5, alpha_g = 1, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_05_1.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 0, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_1_0.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 0.5, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_1_05.csv")

tic()
test <- run_oranzees(t_max = 6000, alpha_e = 1, alpha_g = 1, init_world = TRUE, n_run = 20)
toc()
write_csv(test, "output_test/test_1_1.csv")


# TEST 2

alpha_e_test = seq(0.5, 1, by = 0.1)
alpha_g_test = seq(0, 0.5, by = 0.1)

output <- tibble(alpha_e = rep(alpha_e_test, each = 30), 
                 alpha_g = rep(rep(alpha_g_test, each = 5), 6),
                 n = rep(NA, 180))

for(e in alpha_e_test){
  for(g in alpha_g_test) {
    test <- run_oranzees(t_max = 6000, alpha_e = e, alpha_g = g, init_world = TRUE, n_run = 5)
    out <- analyse_patterns(test)
    output[output$alpha_e == e & output$alpha_g == g, ]$n <- out$n[16:20]
    print(e)
  }
}
write_csv(output, "output_test/test_many.csv")


