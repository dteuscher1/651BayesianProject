library(baseballr)
library(tidyverse)
# Alternatively, using the devtools package:
#devtools::install_github(repo = "BillPetti/baseballr")
data_2016 <- fg_pitch_leaders(2016, 2016, qual = 40, ind = 0)
data_2017 <- fg_pitch_leaders(2017, 2017, qual = 40, ind = 0)
data <- data_2016 %>% 
    bind_rows(data_2017) %>% 
    mutate(Season = as.numeric(Season))
savant <- read.csv("Data/stats.csv") %>% 
    mutate(Name = str_trim(paste(first_name, last_name)))
combined <- savant %>% inner_join(data %>% select(Name, FIP, Season), by = c("Name", "year" = "Season"))
