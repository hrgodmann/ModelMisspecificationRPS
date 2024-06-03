



library(rstan)
library(ggplot2)


model <- "
// Rock-Paper-Scissors Multinomial Processing Tree over time
data {
  int<lower=1> N; // Number of games
  int<lower=1, upper=3> choices[N]; // Player's choices: 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> prev_choices[N]; // Player's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> opp_choices[N]; // Opponent's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
}
parameters {
  vector<lower=0, upper=1>[N] g; // Probability of guessing over time
  real<lower=0> sigma_g; // Standard deviation of random walk for g
}
model {
  // Fixed parameters
  real r = 0.3; // Probability of repeating own choice
  real c = 0.7; // Probability of copying opponent's choice
  
  // Priors
  sigma_g ~ normal(0, 0.1);
  g[1] ~ beta(1, 1);

  // Random walk for g
  for (t in 2:N) {
    g[t] ~ normal(g[t-1], sigma_g);
  }
  
  // Model for making choices
  for (i in 1:N) {
    real p_rock = g[i] / 3 + (1 - g[i]) * ((r * (prev_choices[i] == 1)) + ((1 - r) * (c * (opp_choices[i] == 1))));
    real p_paper = g[i] / 3 + (1 - g[i]) * ((r * (prev_choices[i] == 2)) + ((1 - r) * (c * (opp_choices[i] == 2))));
    real p_scissors = g[i] / 3 + (1 - g[i]) * ((r * (prev_choices[i] == 3)) + ((1 - r) * (c * (opp_choices[i] == 3))));

    real sum_p = p_rock + p_paper + p_scissors;
    vector[3] choice_probs = to_vector([p_rock, p_paper, p_scissors]) / sum_p;

    choices[i] ~ categorical(choice_probs);
  }
}
"


stan_model <- stan_model(model_code = model)


dat <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_variant_linear_g.csv")


stan_data <- list(N = nrow(dat), 
                  choices = dat$PlayerChoice,
                  prev_choices = dat$PlayerPrevChoice,
                  opp_choices = dat$OpponentPrevChoice
)



# Fit model
fit <- sampling(stan_model, data = stan_data, chains = 2, iter = 2000, cores = 2, warmup = 500, control = list(adapt_delta = 0.99, max_treedepth = 17))

traceplot(fit)

# Extract the posterior samples


posterior_samples_g <- extract(fit)$g

# Calculate the mean of the posterior samples at each time point
mean_g_over_time <- apply(posterior_samples_g, 2, mean)

# Plotting
time_points <- 1:nrow(dat)


# Calculate the 2.5th and 97.5th percentiles for c at each time point
lower_ci_g <- apply(posterior_samples_g, 2, quantile, probs = 0.2)
upper_ci_g <- apply(posterior_samples_g, 2, quantile, probs = 0.8)

#################

plot_data_simplified <- data.frame(
  Time = rep(1:nrow(dat), 3),
  Estimate = c(mean_g_over_time),
  Lower = c(lower_ci_g),
  Upper = c(upper_ci_g),
  Parameter = factor(rep(c("g"), each = nrow(dat)))
)


source("/Users/henrikgodmann/Desktop/workspace/GitHub/hrgodmann/Rcolors.R")

# Linear decrease for g
start_g <- 0.2
end_g <- 0.8
g <- seq(from = start_g, to = end_g, length.out = nrow(dat))
# Assuming Time is numeric and we want it to span the same range as g
time_sequence <- seq(min(plot_data_simplified$Time), max(plot_data_simplified$Time), length.out = nrow(dat))
g_data <- data.frame(Time = time_sequence, g = g)

# Plot
ggplot(plot_data_simplified, aes(x = Time, y = Estimate, colour = Parameter)) +
  geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Parameter), alpha = 0.2) +
  scale_colour_manual(values = c("g" = my_blue)) +
  scale_fill_manual(values = c("g" = my_blue)) +
  labs(title = "Parameter Estimates over Time with Credible Intervals",
       x = "Time",
       y = "Estimate",
       colour = "Parameter",
       fill = "Parameter") +
  ylim(0,1) +
  geom_line(data = g_data, aes(x = Time, y = g), colour = "black") + # Use g_data here with black color
  theme_minimal()



save.image("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save.image/save_image.Rdata")

