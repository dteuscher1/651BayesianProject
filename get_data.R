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

head(combined)
small <- combined %>% select(Name, FIP, year, p_formatted_ip, z_swing_percent, z_swing_miss_percent, 
                             oz_swing_percent, oz_swing_miss_percent, edge_percent, n_fastball_formatted, 
                             n_breaking_formatted, n_offspeed_formatted) %>% replace(is.na(.), 0) 
small <- small[1:25, ]
model <- lm(FIP ~.-Name-year-p_formatted_ip, data = small)

N <- nrow(small)
X <- model.matrix(FIP ~.-1-Name-year-p_formatted_ip, data = small) %>% scale()
K <- ncol(X)
y <- small$FIP
data <- list(N = N, K = K, x = X, y = y)   # Set alpha and beta without needing to recompile!

library(rstan)
### Run the model and examine results
nCores <- parallel::detectCores()
options(mc.cores = nCores)          # Use all available cores
rstan_options(auto_write = TRUE)    # Cache compiled code.

fit <- stan(model_code = readLines("MLR_Model.stan"),
            data = data, iter = 10000, warmup = 1000, thin = 2, chains = 2)


samples <- rstan::extract(fit)
library(shinystan)
launch_shinystan(fit)
