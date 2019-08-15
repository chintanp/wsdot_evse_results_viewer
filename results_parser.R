rm(list = ls())
library(here)
library(dplyr)

# out_of_charge_file <- read.csv(here("out_of_charge", "out_of_charge_2019-07-27-12-37-04.csv"))
# power_draw_file <- read.csv(here("power_draw", "power_draw_2019-07-27-12-37-04.csv"))
finished_file <- read.csv(here("finished", "finished_2019-07-27-12-37-04.csv"), header = FALSE)

hist(finished_file$V5, breaks = 20, xlim = c(0, 100))
plot(finished_file$V5)
