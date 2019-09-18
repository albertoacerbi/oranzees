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
test_0_0.5 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test_0_0.5, "output_test/test_0_0.5.csv")

tic()
test_0_0.6 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.6, init_world = 1, n_run = 20)
toc()
write_csv(test_0_0.6, "output_test/test_0_0.6.csv")

tic()
test_0_0.7 <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
write_csv(test_0_0.7, "output_test/test_0_0.7.csv")

tic()
test_0.1_0.5 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(test_0.1_0.5, "output_test/test_0.1_0.5.csv")

tic()
test_0.1_0.6 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.6, init_world = 1, n_run = 20)
toc()
write_csv(test_0.1_0.6, "output_test/test_0.1_0.6.csv")

tic()
test_0.1_0.7 <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
write_csv(test_0.1_0.7, "output_test/test_0.1_0.7.csv")


#
# TO DO
#
# PLOT SUMMARY:

test_0_05 <-analyse_table(read_csv("output_test/test_0_0.5.csv"))
test_0_06 <-analyse_table(read_csv("output_test/test_0_0.6.csv"))
test_0_07 <-analyse_table(read_csv("output_test/test_0_0.7.csv"))
test_01_05 <-analyse_table(read_csv("output_test/test_0.1_0.5.csv"))
test_01_06 <-analyse_table(read_csv("output_test/test_0.1_0.6.csv"))
test_01_07 <-analyse_table(read_csv("output_test/test_0.1_0.7.csv"))

palette_1 <- c("#FFDB6D", "#F4EDCA", "#C4961A",
               "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

tibble(opt = rep(c("0", "0.1"), each = 60), 
       alpha_g = rep(c("0.5", "0.6", "0.7", "0.5", "0.6", "0.7"), each = 20),
      behaviours = c(test_0_05, test_0_06, test_0_07, test_01_05, test_01_06, test_01_07)) %>%
  ggplot(aes( x = opt, y = behaviours, fill = alpha_g)) +
    geom_boxplot() +
    geom_point(pch=21, position=position_jitterdodge(jitter.width=0.1)) +
  scale_fill_manual(values = c("#FFDB6D", "#F4EDCA", "#D16103")) +
    theme_bw() +
    labs(y = "proportion of traits considered 'cultural'") 
    






