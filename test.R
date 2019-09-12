library(tictoc)

# USE FUNCTIONS IN main.R
source("main.R")

# function for test
analyse_table <- function(my_test){
  output <- my_test %>%
    group_by(behaviour, run) %>%
    summarise(A = !("absent" %in% code) && !("ecological explanation" %in% code), 
              B = !("habitual" %in% code) && !("customary" %in% code),
              C = !("absent" %in% code) && ("ecological explanation" %in% code)) %>%
    ungroup() %>%
    mutate (D = !(A | B | C)) %>%
    group_by(run) %>%
    count(D) %>%
    filter(D == TRUE) 
  output <- output$n/38
}


# test-1 (BASELINE)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.01, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
# write_csv(my_test, "output_test/test-1.csv")
# my_test <- read_csv("output_test/test-1.csv")

# test-2 (NO GENETIC)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.01, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
# write_csv(my_test, "output_test/test-2.csv")
# my_test <- read_csv("output_test/test-2.csv")

# test-3 (NO OPTIMISATION)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
# write_csv(my_test, "output_test/test-3.csv")
# my_test <- read_csv("output_test/test-2.csv")

# test-4 (STRONGER OPTIMISATION)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.1, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
# write_csv(my_test, "output_test/test-4.csv")
# my_test <- read_csv("output_test/test-2.csv")

# test-5 (STRONG GENES)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.01, alpha_g = 1, init_world = 1, n_run = 20)
toc()
# write_csv(my_test, "output_test/test-5.csv")
# my_test <- read_csv("output_test/test-2.csv")

# test-6 (NO GENES, NO OPTIMISATION)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
write_csv(my_test, "output_test/test-6.csv")

# PLOT SUMMARY:

# only BASELINE - NO GENES - NO GENES/NO OPTIMISATION - STRONG GENES - STRONG OPTIMISATION
test_1 <-analyse_table(read_csv("output_test/test-1.csv"))
test_2 <-analyse_table(read_csv("output_test/test-2.csv"))
test_4 <-analyse_table(read_csv("output_test/test-4.csv"))
test_5 <-analyse_table(read_csv("output_test/test-5.csv"))
test_6 <-analyse_table(read_csv("output_test/test-6.csv"))

palette_1 <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
               "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

tibble(condition = rep(c("baseline","no genes", "no genes/no optimisation", "strong optimisation","strong genes"), each = 20), 
      behaviours = c(test_1, test_2, test_6, test_4, test_5)) %>%
  ggplot(aes( x = condition, y = behaviours, fill = condition)) +
    geom_boxplot() +
    geom_jitter(width = 0.1) +
    scale_fill_manual(values = palette_2) +
    theme_bw() +
    labs(y = "proportion of traits considered 'cultural'", title = "'Culture' in oranzees (20 runs each)") +
    theme(legend.position = "none") +
    ggsave("output_test/second-test.pdf", width = 7, height = 4)
    






