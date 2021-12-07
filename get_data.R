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
small <- combined %>% select(-X, -player_id, -xba, -xslg, -xwoba, -xobp, -xiso, -wobacon, 
                             -exit_velocity_avg, -barrel_batted_rate, -hard_hit_percent) %>%
    filter(year == 2016) %>% 
    filter(p_formatted_ip > 90) %>%
    filter(pitch_hand == "R") %>%
    select(-p_formatted_ip, -pitch_hand) %>%
    replace(is.na(.), 0) 
#small <- small[1:25, ]
X <- model.matrix(FIP ~.-1-Name-year-last_name-first_name, data = small) %>% scale()
y <- small$FIP

library(glmnet)
cv.out <- cv.glmnet(X,y,alpha=0, standardize = TRUE)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
mod <- glmnet(X, y , alpha = 0, lambda = bestlam, standardize = TRUE)
predict(mod,type="coefficients",s=bestlam)

N <- nrow(small)
X <- model.matrix(FIP ~.-1-Name-year-last_name-first_name, data = small) %>% scale() %>% bind_cols(intercept = 1)
K <- ncol(X)
y <- small$FIP
data <- list(N = N, K = K, X = X, y = y, a = 30, b = 15)   # Set alpha and beta without needing to recompile!

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


data <- list(N = N, K = K, X = X, y = y, a = 1, b = 1)   # Set alpha and beta without needing to recompile!

library(rstan)
### Run the model and examine results
nCores <- parallel::detectCores()
options(mc.cores = nCores)          # Use all available cores
rstan_options(auto_write = TRUE)    # Cache compiled code.

fit2 <- stan(model_code = readLines("MLR_Model.stan"),
            data = data, iter = 10000, warmup = 1000, thin = 2, chains = 2)


samples2 <- rstan::extract(fit2)
library(shinystan)
launch_shinystan(fit2)


data <- list(N = N, K = K, X = X, y = y, a = .1, b = 10)   # Set alpha and beta without needing to recompile!

library(rstan)
### Run the model and examine results
nCores <- parallel::detectCores()
options(mc.cores = nCores)          # Use all available cores
rstan_options(auto_write = TRUE)    # Cache compiled code.

fit3 <- stan(model_code = readLines("MLR_Model.stan"),
            data = data, iter = 10000, warmup = 1000, thin = 2, chains = 2)


samples3 <- rstan::extract(fit3)
library(shinystan)
launch_shinystan(fit3)
