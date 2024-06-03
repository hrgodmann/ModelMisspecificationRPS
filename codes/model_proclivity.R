

# generate data with increasing time varying proclivity for paper, 100, 250, 1000
# generate data with increasing guessing parameter
# all in same format

# close off if time left my approach



library(dplyr)
library(rstan)
library(ggplot2)
library(patchwork)

model <- "
data {
  int<lower=1> N;                         // Number of games
  int<lower=1, upper=3> choices[N];       // Player's choices: 1 = rock, 2 = paper, 3 = scissors
}
parameters {
  vector[3] choice_log_odds[N];           // Log odds of choosing rock, paper, scissors
  real<lower=0> sigma;                    // Standard deviation of random walk
}
transformed parameters {
  simplex[3] choice_probs[N];             // Probabilities of choosing rock, paper, scissors

  for (i in 1:N) {
    choice_probs[i] = softmax(choice_log_odds[i]);
  }
}
model {
  // Priors
  choice_log_odds[1] ~ normal(0, 1);      // Prior for the initial state
  sigma ~ cauchy(0, 1);                   // Prior for the standard deviation of the random walk

  // Random walk
  for (i in 2:N) {
    choice_log_odds[i] ~ normal(choice_log_odds[i-1], sigma);
  }

  // Likelihood of observed choices
  for (i in 1:N) {
    choices[i] ~ categorical(choice_probs[i]);
  }
}

"

stan_model <- stan_model(model_code = model)


dat <- read.csv("/Users/henrikgodmann/Desktop/rps_teaching/real_data/real_data.csv")


stan_data <- list(N = nrow(dat), 
                  choices = as.numeric(dat$Mona)
)


# Fit model
fit <- sampling(stan_model, data = stan_data, chains = 2, iter = 800, cores = 2, warmup = 200, control = list(adapt_delta = 0.99, max_treedepth = 17))






# Assuming you have already fit your model and have the 'fit' object

# Extract the choice probabilities directly
fit_data <- rstan::extract(fit)
choice_probs <- fit_data$choice_probs  # Assuming this is a 3D array: [iterations, N, choices]

# Calculate credible intervals
ci_level <- 0.8
ci_lower <- (1 - ci_level) / 2
ci_upper <- 1 - ci_lower
N <- dim(choice_probs)[2]
probs_ci_lower <- array(dim = c(N, 3))
probs_ci_upper <- array(dim = c(N, 3))

for (i in 1:N) {
  for (j in 1:3) {
    probs_ci_lower[i, j] <- quantile(choice_probs[, i, j], probs = ci_lower)
    probs_ci_upper[i, j] <- quantile(choice_probs[, i, j], probs = ci_upper)
  }
}

# Prepare data frame for ggplot
df <- data.frame(Time = rep(1:N, each = 3),
                 Parameter = rep(c("Rock", "Paper", "Scissors"), times = N),
                 Estimate = as.vector(t(apply(choice_probs, c(2, 3), mean))),
                 LowerCI = as.vector(t(probs_ci_lower)),
                 UpperCI = as.vector(t(probs_ci_upper)))

# Plot
ggplot(df, aes(x = Time, y = Estimate, color = Parameter)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Parameter), alpha = 0.2) +
  geom_line() +
  scale_color_manual(values = c("Rock" = "blue", "Paper" = "red", "Scissors" = "green")) +
  labs(title = "Choice Probabilities over Time with Credible Intervals",
       x = "Time", y = "Probability") +
  theme_minimal()

# ggsave("/Users/henrikgodmann/Desktop/rps_teaching/plots/proclivity_mona.pdf", colormodel = "cmyk")

############# check descriptively ################

library(tidyr)
library(ggplot2)
library(dplyr)

dat <- read.csv("/Users/henrikgodmann/Desktop/rps_teaching/real_data/real_data.csv")
choices_numeric <- as.numeric(dat$Mona)

# Divide the 250 trials into 10 phases
phases <- gl(10, 25, length = length(choices_numeric))

# Create a data frame with choices and phases
data <- data.frame(Choice = choices_numeric, Phase = phases)

# Calculate the counts of each choice within each phase
counts <- data %>%
  group_by(Phase, Choice) %>%
  summarise(Count = n(), .groups = 'drop')

# Calculate total counts per phase
totals <- counts %>%
  group_by(Phase) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Join totals back to counts and calculate proportion
proportions <- counts %>%
  left_join(totals, by = "Phase") %>%
  mutate(Proportion = Count / Total) %>%
  select(Phase, Choice, Proportion)

# Pivot wider for plotting
proportions_wide <- pivot_wider(proportions, names_from = Choice, values_from = Proportion, values_fill = list(Proportion = 0))


