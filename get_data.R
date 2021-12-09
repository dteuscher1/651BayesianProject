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

N <- nrow(small)
X <- model.matrix(FIP ~.-1-Name-year-last_name-first_name, data = small) %>% scale() %>% bind_cols(intercept = 1)
K <- ncol(X)
y <- small$FIP
data <- list(N = N, K = K, X = X, y = y, a = 36, b = 12)   # Set alpha and beta without needing to recompile!


# Hand draws 

ndraws <- 1000
beta.draws <- matrix(0, nrow = ndraws, ncol = ncol(X))
alpha.draws <- numeric(length = ndraws)
sigma2 <- numeric(length = ndraws)
sigma2_beta <- numeric(length = ndraws)
alpha.draws[1] <- 4
sigma2[1] <- 1
sigma2_beta[1] <- 1
a <- 40
b <- 8
X_mat <- as.matrix(X)
accept <- 0
library(mvtnorm)
for(i in 2:ndraws){
    r <- y - alpha.draws[i-1]
    sigma_mat <- solve((t(X_mat) %*% X_mat)/sigma2[i-1] + 1/sigma2_beta[i-1] *diag(50))
    mu_vec <- sigma_mat %*% ((t(X) %*% r)/sigma2[i-1])
    
    beta.draws[i, ] <- rmvnorm(1, mean = mu_vec, sigma = sigma_mat)
    a_star <- a + nrow(X_mat)/2
    b_star <- b + .5*sum((y - (X_mat %*% beta.draws[i, ]))^2)
    sigma2[i] <- 1/rgamma(1, a_star, b_star)
    r <- y - (X_mat %*% beta.draws[i, ])
    alpha_mat <- matrix(1, ncol = 1, nrow = nrow(X))
    sigma_mat <- solve((t(alpha_mat) %*% alpha_mat)/sigma2[i-1] + 1/sigma2_beta[i-1] *diag(1))
    mu_vec <- sigma_mat %*% ((t(alpha_mat) %*% r)/sigma2[i-1])
    alpha.draws[i] <- rmvnorm(1, mean = mu_vec, sigma = sigma_mat)
    proposed_value <- rnorm(1, mean = sigma2_beta[i-1], sd = .1)
    if(proposed_value > 0){
        mh <- sum(dnorm(beta.draws[i, ], 0, proposed_value, log = T)) + 
            sum(dgamma(proposed_value, 40, 8, log = T)) - 
            sum(dnorm(beta.draws[i, ], 0, sigma2_beta[i-1], log = T)) -
            sum(dgamma(sigma2_beta, 40, 8, log = T))
        if(log(runif(1)) < mh){
            sigma2_beta[i] <- proposed_value
            accept <- accept + 1
        } else{
            sigma2_beta[i] <- sigma2_beta[i-1]
        }
    } else{
        sigma2_beta[i] <- sigma2_beta[i-1]
    }    
}

plot(sigma2_beta, type = "l")
plot(alpha.draws, type = "l")
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


data <- list(N = N, K = K, X = X, y = y, a = 40, b = 8)   # Set alpha and beta without needing to recompile!

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


chains <- cbind(samples3[[1]],samples3[[2]],samples3[[3]], samples3[[4]])
sims <- as.mcmc(chains)
r_l <- raftery.diag(sims)

chains_frame <- data.frame(chains, iter = 1:nrow(chains))
library(tidyverse)
ggplot(chains_frame, aes(x = iter, y = X1)) +
    geom_line() + 
    theme_minimal() +
    labs(x = "Iteration", y = "Beta 1")

ggplot(chains_frame, aes(x = iter, y = X5)) +
    geom_line() + 
    theme_minimal() +
    labs(x = "Iteration", y = "Beta 5")

library(latex2exp)
ggplot(chains_frame, aes(x = iter, y = X51)) +
    geom_line() + 
    theme_minimal() +
    labs(x = "Iteration", y = TeX(r'($\sigma^2$)'))

ggplot(chains_frame, aes(x = iter, y = X52)) +
    geom_line() + 
    theme_minimal() +
    labs(x = "Iteration", y = TeX(r'($\sigma_b^2$)'))

ggplot(chains_frame, aes(x = iter, y = X53)) +
    geom_line() + 
    theme_minimal() +
    labs(x = "Iteration", y = TeX(r'($\alpha$)'))

posterior_means <- apply(chains[, 1:50], 2, mean)
# Frequentist
library(glmnet)
cv.out <- cv.glmnet(as.matrix(X),y,alpha=0, standardize = TRUE)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
mod <- glmnet(X, y , alpha = 0, lambda = bestlam, standardize = TRUE)
predict(mod,type="coefficients",s=bestlam)

bootstrap_coef <- matrix(0, nrow = 500, ncol = 50)
for(i in 1:500){
    n <- nrow(small)
    obs <- sample(1:n, n, replace = TRUE)
    boot_sample <- small[obs, ]
    x <- model.matrix(FIP ~.-1-Name-year-last_name-first_name, data = boot_sample)
    y <- boot_sample$FIP
    model <- glmnet(x, y, alpha = 0, lambda = bestlam)
    coef <- as.vector(predict(model,type="coefficients",s=bestlam))
    bootstrap_coef[i, ] <- coef
}
cis <- apply(bootstrap_coef, 2, quantile, c(.025, .975))
cis
boot_est <- apply(bootstrap_coef, 2, mean)
vars <- rownames(predict(model,type="coefficients",s=bestlam))
ridge_freq <- data.frame(Variable = vars, Lower = cis[1, ], 
                         Estimate = boot_est, Upper = cis[2,]) %>% 
    arrange(desc(Estimate))

ridge_freq %>% filter((Lower > 0) | Upper < 0) %>% arrange(desc(Estimate))
