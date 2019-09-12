library(tictoc)

# USE FUNCTIONS IN main.R
source("main.R")

# test-1
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.01, alpha_g = 0.7, init_world = 1, n_run = 20)
toc()
# 365.1 sec elapsed
# write_csv(my_test, "output_test/test-1.csv")
# my_test <- read_csv("output_test/test-1.csv")

# analyse:
output <- matrix(data = "D", nrow = max(my_test$run), ncol = 38)
for(r in unique(my_test$run)){
  temp <- filter(my_test, run == r)
  for(b in 1:38){
    temp_b <- filter(temp, behaviour == b)
    if(!("absent" %in% temp_b$code) && !("ecological explanation" %in% temp_b$code)) {
      output[r,b] <- "A"
    }
    if(!("habitual" %in% temp_b$code) && !("customary" %in% temp_b$code)){
      output[r,b] <- "B"
    }
    if(!("absent" %in% temp_b$code) && ("ecological explanation" %in% temp_b$code)){
      output[r,b] <- "C"
    }
  }
}
rowSums(output=="D")/38
# [1] 0.8421053 0.7894737 0.9736842 0.8947368 0.9473684 0.9473684 0.8684211 0.8684211 0.9210526 0.7631579 0.8421053 0.9210526 0.7368421 0.8157895
# [15] 0.8684211 0.8684211 0.8684211 0.8157895 0.9473684 0.8684211

# test-2 (NO GENETIC)
tic()
my_test <- run_oranzees(t_max = 12000, opt = 0.01, alpha_g = 0.5, init_world = 1, n_run = 20)
toc()
# 365.1 sec elapsed
# write_csv(my_test, "output_test/test-2.csv")
# my_test <- read_csv("output_test/test-2.csv")

# analyse:
output <- matrix(data = "D", nrow = max(my_test$run), ncol = 38)
for(r in unique(my_test$run)){
  temp <- filter(my_test, run == r)
  for(b in 1:38){
    temp_b <- filter(temp, behaviour == b)
    if(!("absent" %in% temp_b$code) && !("ecological explanation" %in% temp_b$code)) {
      output[r,b] <- "A"
    }
    if(!("habitual" %in% temp_b$code) && !("customary" %in% temp_b$code)){
      output[r,b] <- "B"
    }
    if(!("absent" %in% temp_b$code) && ("ecological explanation" %in% temp_b$code)){
      output[r,b] <- "C"
    }
  }
}
rowSums(output=="D")/38