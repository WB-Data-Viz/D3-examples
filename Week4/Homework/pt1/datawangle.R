library(tidyverse)

unemp = read.csv("all-states2.csv")
unemp_sum = unemp %>% group_by(end) %>% summarize(value = sum(value))
unemp_sum$date = unemp_sum$end
unemp_sum = unemp_sum %>% select(-end)
write.csv(unemp_sum, "all-states2_sum.csv")
