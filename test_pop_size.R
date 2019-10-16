library(tidyverse)
data <- read_csv("sims_for_ms/data/figure_2.csv")
pop_sizes <- c(20, 76, 50, 95, 42, 49)
list_pop <- c("Uossob", "Iat Forest", "Ebmog", "Elaham", "Elabik", "Ognodub")

customary <- c()
habitual <- c()
present <- c()

for( r in 1:20){
  for(pop in list_pop){
    customary <- c(customary, dim(data[data$run==r & data$population==pop & data$code=="customary",])[1])
    habitual <- c(habitual, dim(data[data$run==r & data$population==pop & data$code=="habitual",])[1])
    present <- c(present, dim(data[data$run==r & data$population==pop & data$code=="present",])[1])
  }
}
out <- tibble(behaviours = customary + present + habitual, 
              pop_size = rep(pop_sizes, 20))
ggplot(data = out, aes(x=pop_size, y=behaviours)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = lm)