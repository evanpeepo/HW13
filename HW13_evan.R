#HW13, Evan Peepo


# Load packages -----------------------------------------------------------
library(ggplot2)

# Load data ---------------------------------------------------------------

dragons <- read.csv('dragon_data.csv')

# Plot --------------------------------------------------------------------

plot(dragons$size, dragons$acres_on_fire) #linear
hist(dragons$size) #normal
hist(dragons$acres_on_fire) #normal

# Make linear model -------------------------------------------------------

dragons_lm <- lm(acres_on_fire ~ size, data = dragons) 
coef(dragons_lm) #for comparison

# Objective 1 -------------------------------------------------------------

y <- dragons$acres_on_fire
x_matrix <- model.matrix(~ size, data = dragons)

#A. Analytical solution
matrix_par <- solve(t(x_matrix) %*% x_matrix) %*% (t(x_matrix) %*% y) #same as lm()

# Objective 2 -------------------------------------------------------------

#A. Grid search
beta0 <- seq(from = -2, to = 1, by = 0.1)
beta1 <- seq(from = 1, to = 3, by = 0.1)
beta0 <- round(beta0, 1) 
beta1 <- round(beta1, 1)

#Empty matrix
ss_matrix <- matrix(NA, ncol = length(beta0), nrow = length(beta1))
rownames(ss_matrix) <- beta1
colnames(ss_matrix) <- beta0

#Nested for loop filling matrix with sum of squares
for (i in seq_along(beta0)) {
  for (j in seq_along(beta1)) {
    y_hat = beta0[i] + beta1[j] * dragons$size
    ss <- sum((dragons$acres_on_fire - y_hat)^2)
    ss_matrix[j, i] <- ss
  }
}

min_ss <- min(ss_matrix) #find min value

which(ss_matrix == min_ss, arr.ind = TRUE) #find the index with min value
colnames(ss_matrix)[23] #this is off from lm(), lets check formula
rownames(ss_matrix)[4] #matches lm() if rounded down


#Check intercept based on slope
mean_size <- mean(dragons$size, na.rm = TRUE)
mean_acres <- mean(dragons$acres_on_fire, na.rm = TRUE)

b1_grid_choice <- 1.3
b0_opt_for_b1 <- mean_acres - b1_grid_choice * mean_size

b0_opt_for_b1 #same as what is in grid search, I guess for a slope of 1.3, the optimal intercept is 0.2. 
#It appears going to the closest 0.1 is not enough detail to get the best parameter fit

#B. Optimatization with optim()

min_ss <- function(par, data) {
  y_hat = par[1] + par[2] * data$size
  ss <- sum((data$acres_on_fire - y_hat)^2)
  return(ss)
}

start_parameter <- c(-1, 1)


result1 <- optim(par = start_parameter, fn = min_ss, data = dragons) 
result1 #closely matches lm(), intercept slightly different

#C
#Model converges, #convergence = 0

parameter_test <- c(-5, 4) #test different starting values
result2 <- optim(par = parameter_test, fn = min_ss, data = dragons) #closely matches lm(), intercept slightly different
result2 #gets roughly the same values

# Objective 3 -------------------------------------------------------------

#A. Grid search
beta0 <- seq(from = -2, to = 1, by = 0.1)
beta1 <- seq(from = 1, to = 3, by = 0.1)
beta0 <- round(beta0, 1) 
beta1 <- round(beta1, 1)

#Empty matrix
ss_matrix <- matrix(NA, ncol = length(beta0), nrow = length(beta1))
rownames(ss_matrix) <- beta1
colnames(ss_matrix) <- beta0

#Nested for loop filling matrix with sum of squares
for (i in seq_along(beta0)) {
  for (j in seq_along(beta1)) {
    y_hat = beta0[i] + beta1[j] * dragons$size
    ss = sum((dragons$acres_on_fire - y_hat)^2)
    resid_var = ss / 48
    lnL = (-50 / 2) * log(2 * pi * resid_var) - (1 / 2 * resid_var) * sum((dragons$size - mean_size)^2)
    ss_matrix[j, i] <- lnL
  }
}

max_lnL <- max(ss_matrix) #find max value

which(ss_matrix == max_lnL, arr.ind = TRUE) #find the index with max value
colnames(ss_matrix)[23] #same as grid search using sum of squares
rownames(ss_matrix)[4] #matches lm() if rounded down

#Plot 

ggplot(dragons, aes(size, acres_on_fire)) +
  geom_point() +
  geom_abline(intercept = 0.2, slope = 1.3, color = 'blue') + #from grid search
  geom_abline(intercept = -1.375551, slope = 1.346694, color = 'red') #from lm()
#Intercept is different from other methods, but visually is not much different

#B. Optimatization with optim()

max_likelihood <- function(par, data) {
  y_hat = par[1] + par[2] * data$size
  ss = sum((data$acres_on_fire - y_hat)^2)
  resid_var = ss / 48
  #plug in calculated residual variance as sigma^2, using xbar as mu
  lnL = (-50 / 2) * log(2 * pi * resid_var) - (1 / 2 * resid_var) * sum((data$size - mean(data$size))^2) 
  return(lnL)
}

neg_log_L <- function(par, data) { #use negative log likelihood for optim() since it minimizes by default
  -max_likelihood(par, data)
}

start_parameter <- c(-1, 1)

result3 <- optim(par = start_parameter, fn = neg_log_L, data = dragons) 
result3 #closely matches lm(), intercept slightly different

#C
#Model converges, #convergence = 0

parameter_test <- c(-5, 4) #test different starting values
result4 <- optim(par = parameter_test, fn = neg_log_L, data = dragons) #closely matches lm(), intercept slightly different
result4 #gets roughly the same values

# Objective 4 -------------------------------------------------------------
matrix_par_vec <- as.vector(matrix_par)
grid_search_ols <- c(0.2, 1.3)
optim_ols <- result1$par
grid_search_lnL <- c(0.2, 1.3)
optim_lnL <- result3$par
lm_coef <- coef(dragons_lm)

method_compare_df <- data.frame(matrix_par_vec, grid_search_ols, optim_ols, grid_search_lnL, optim_lnL, lm_coef)
method_compare_df

#All close except the grid search intercept