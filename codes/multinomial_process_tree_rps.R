

library(rstan)
library(ggplot2)
library(patchwork)

model <- "

// Rock-Paper-Scissors Multinomial Processing Tree
data {
  int<lower=1> N;                          // Number of games
  int<lower=1, upper=3> choices[N];        // Player's choices: 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> prev_choices[N];   // Player's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> opp_choices[N];    // Opponent's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
}
parameters {
  real<lower=0, upper=1> g;        // Probability of guessing
  real<lower=0, upper=1> r;        // Probability of repeating own choice when not guessing
  real<lower=0, upper=1> c;        // Probability of copying opponent's choice when not repeating own
}
model {
  // Prior distributions for our parameters
  g ~ beta(1, 1);
  r ~ beta(1, 1);
  c ~ beta(1, 1);
  
  for (i in 1:N) {
    // Probabilities for each choice based on the MPT model
    real p_rock = g / 3 + (1 - g) * ((r * (prev_choices[i] == 1)) + ((1 - r) * (c * (opp_choices[i] == 1))));
    real p_paper = g / 3 + (1 - g) * ((r * (prev_choices[i] == 2)) + ((1 - r) * (c * (opp_choices[i] == 2))));
    real p_scissors = g / 3 + (1 - g) * ((r * (prev_choices[i] == 3)) + ((1 - r) * (c * (opp_choices[i] == 3))));
    
    // Normalizing the probabilities
    real sum_p = p_rock + p_paper + p_scissors;
    vector[3] choice_probs = to_vector([p_rock, p_paper, p_scissors]) / sum_p;

    // Likelihood of the observed choices
    choices[i] ~ categorical(choice_probs);
  }
}
"

stan_model <- stan_model(model_code = model)


dat <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_dice.csv")
# dat_real <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_chiara.csv")
# dat <- dat_real


# dat$PlayerPrevChoice <- c(0, dat$Player_A[1:nrow(dat)-1])
# dat$OpponentPrevChoice <- c(0, dat$Player_B[1:nrow(dat)-1])


stan_data <- list(N = nrow(dat), 
                  choices = dat$PlayerChoice,
                  prev_choices = dat$PlayerPrevChoice,
                  opp_choices = dat$OpponentPrevChoice
                  )



# Fit model
fit <- sampling(stan_model, data = stan_data, chains = 2, iter = 2000, cores = 2, warmup = 500, control = list(adapt_delta = 0.99, max_treedepth = 17))

# posterior samples
posterior_samples <- rstan::extract(fit)


posterior_df <- data.frame(
  g = posterior_samples$g,
  r = posterior_samples$r,
  c = posterior_samples$c
)

 # prior for plot
beta_prior <- function(x) dbeta(x, shape1 = 1, shape2 = 1)


# Plot for c with prior
plot_g <- ggplot(posterior_df, aes(x = g)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Prior and Posterior Distribution of g",
       x = "g",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 8)

# Plot for r with prior
plot_r <- ggplot(posterior_df, aes(x = r)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Prior and Posterior Distribution of r",
       x = "r",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 7.5)

# Plot for u with prior
plot_c <- ggplot(posterior_df, aes(x = c)) +
  geom_density(fill = "salmon", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Prior and Posterior Distribution of c",
       x = "c",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 7.5)



plot_combined <- plot_g + plot_r + plot_c + plot_layout(ncol = 3)
plot_combined
# save.image("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save.image/save_image.Rdata")





ggsave("/Users/henrikgodmann/Desktop/rps_teaching/plots/dice_mpt.pdf", colormodel = "cmyk")


