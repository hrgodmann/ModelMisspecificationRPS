
# if people were winning, did they repeat it both as a winner and as a looser

source("/Users/henrikgodmann/Desktop/GitHub/functions/Rcolors.R")

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


# dat <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/real/dat_dice.csv")
dat_real <- read.csv("/Users/henrikgodmann/Desktop/GitHub/ModelMisspecificationRPS/data/real_data/real_data.csv")
dat <- dat_real

# dat$PlayerPrevChoice <- c(0, dat$Player_A[1:nrow(dat)-1])
# dat$OpponentPrevChoice <- c(0, dat$Player_B[1:nrow(dat)-1])


dat$PlayerPrevChoice <- c(0, dat$Chiara[1:nrow(dat)-1])
dat$OpponentPrevChoice <- c(0, dat$Mona[1:nrow(dat)-1])


# > dat
# Round Chiara Mona PlayerPrevChoice OpponentPrevChoice
# 1       1      2    3                0                  0
# 2       2      3    1                2                  3
# 3       3      2    3                3                  1
# 4       4      3    2                2                  3
# 5       5      1    2                3                  2
# 6       6      3    1                1                  2
# 7       7      2    2                3                  1


# if people were winning, did they repeat it both as a winner and as a looser

# Determine winner (1 = Chiara wins, -1 = Mona wins, 0 = draw)
rps_winner <- function(p1, p2) {
  if (p1 == p2) return(0)  # Draw
  if ((p1 == 1 & p2 == 3) | (p1 == 2 & p2 == 1) | (p1 == 3 & p2 == 2)) return(1) else return(-1)
}

dat <- dat %>%
  mutate(
    Winner = mapply(rps_winner, Chiara, Mona),
    ChiaraRepeated = lag(Chiara) == Chiara,
    MonaRepeated = lag(Mona) == Mona
  )

# Convert data to long format for ggplot
dat_long <- dat %>%
  select(Round, Winner, ChiaraRepeated, MonaRepeated) %>%
  pivot_longer(cols = c(ChiaraRepeated, MonaRepeated),
               names_to = "Player", values_to = "Repeated") %>%
  mutate(Player = ifelse(Player == "ChiaraRepeated", "Chiara", "Mona"))


# Compute proportion of repetitions per (Winner, Player) group
dat_summary <- dat_long %>%
  group_by(Winner, Player) %>%
  summarise(PropRepeated = mean(Repeated, na.rm = TRUE), .groups = "drop")

setwd("/Users/henrikgodmann/Desktop/GitHub/ModelMisspecificationRPS/plots")
pdf("defect_rate_over_timeResultsSSMoptional.pdf", width = 7, height = 5)

# Plot
ggplot(dat_summary, aes(x = factor(Winner), y = PropRepeated, fill = Player)) +
  geom_col(position = "dodge") +  
  geom_hline(yintercept = 1/3, linetype = "dashed", color = my_red, linewidth = 1) +  # Add 1/3 reference line
  scale_x_discrete(labels = c("-1" = "Lost", "0" = "Draw", "1" = "Won")) +
  scale_fill_manual(values = c("Chiara" = my_grouping_col_1, "Mona" = my_grouping_col_2)) +
  labs(x = "Previous Round Outcome", y = "Proportion of Repeated Choices",
       title = "Did Players Repeat Their Choices After Winning or Losing?",
       fill = "Player") +
  theme_minimal()


dev.off()

library(dplyr)
library(ggplot2)

# Function to determine what move would have won/lost
winning_move <- function(move) {
  return(ifelse(move == 1, 2, ifelse(move == 2, 3, 1)))  # Rock -> Paper, Paper -> Scissors, Scissors -> Rock
}
losing_move <- function(move) {
  return(ifelse(move == 1, 3, ifelse(move == 2, 1, 2)))  # Rock -> Scissors, Paper -> Rock, Scissors -> Paper
}

# Add Previous Choices and Compute Move Switches
dat <- dat %>%
  mutate(
    Winner = mapply(rps_winner, Chiara, Mona),
    ChiaraChanged = lag(Chiara) != Chiara,
    MonaChanged = lag(Mona) != Mona,
    ChiaraSwitchType = case_when(
      Winner == 0 & ChiaraChanged & Chiara == winning_move(lag(Chiara)) ~ "To Winning Move",
      Winner == 0 & ChiaraChanged & Chiara == losing_move(lag(Chiara)) ~ "To Losing Move",
      Winner == 0 & ChiaraChanged ~ "Other",
      TRUE ~ NA_character_
    ),
    MonaSwitchType = case_when(
      Winner == 0 & MonaChanged & Mona == winning_move(lag(Mona)) ~ "To Winning Move",
      Winner == 0 & MonaChanged & Mona == losing_move(lag(Mona)) ~ "To Losing Move",
      Winner == 0 & MonaChanged ~ "Other",
      TRUE ~ NA_character_
    )
  )

# Convert to long format and filter only draw rounds
dat_long <- dat %>%
  filter(Winner == 0 & Round > 1) %>%
  select(Round, ChiaraSwitchType, MonaSwitchType) %>%
  pivot_longer(cols = c(ChiaraSwitchType, MonaSwitchType),
               names_to = "Player", values_to = "SwitchType") %>%
  mutate(Player = ifelse(Player == "ChiaraSwitchType", "Chiara", "Mona")) %>%
  filter(!is.na(SwitchType))  # Remove cases where no switch happened

# Compute proportions per Player
dat_summary <- dat_long %>%
  group_by(Player, SwitchType) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Player) %>%
  mutate(Prop = Count / sum(Count))  # Normalize by category

# Plot
ggplot(dat_summary, aes(x = Player, y = Prop, fill = SwitchType)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("To Winning Move" = my_green, "To Losing Move" = my_red, "Other" = "gray")) +
  labs(x = "Player", y = "Proportion of Switch Types",
       title = "How Players Change After a Draw",
       fill = "Switch Type") +
  theme_minimal()




library(dplyr)
library(ggplot2)

# Function to determine the winner of a round
rps_winner <- function(player1, player2) {
  if (player1 == player2) return(0)  # Draw
  if ((player1 == 1 & player2 == 3) | (player1 == 2 & player2 == 1) | (player1 == 3 & player2 == 2)) {
    return(1)  # Player 1 wins
  } else {
    return(2)  # Player 2 wins
  }
}

# Add winner/loser information
dat <- dat %>%
  mutate(
    Winner = mapply(rps_winner, Chiara, Mona),
    ChiaraPrev = lag(Chiara),  # Previous move of Chiara
    MonaPrev = lag(Mona),      # Previous move of Mona
    ChiaraWonLast = lag(Winner) == 1,  # Was Chiara the winner in the last round?
    MonaWonLast = lag(Winner) == 2,    # Was Mona the winner in the last round?
    ChiaraSwitched = ChiaraPrev != Chiara,  # Did Chiara switch moves?
    MonaSwitched = MonaPrev != Mona        # Did Mona switch moves?
  )

# Convert data to long format
dat_long <- dat %>%
  filter(Round > 1) %>%  # Remove first round since no previous winner
  select(Round, ChiaraWonLast, MonaWonLast, ChiaraSwitched, MonaSwitched) %>%
  pivot_longer(cols = c(ChiaraSwitched, MonaSwitched), names_to = "Player", values_to = "Switched") %>%
  mutate(WonLast = ifelse(Player == "ChiaraSwitched", ChiaraWonLast, MonaWonLast),
         Player = ifelse(Player == "ChiaraSwitched", "Chiara", "Mona"))

# Summarize switching behavior
switch_summary <- dat_long %>%
  group_by(WonLast, Switched) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(WonLast) %>%
  mutate(Prop = Count / sum(Count))

# Convert WonLast to "Losers" and "Winners"
switch_summary <- switch_summary %>%
  mutate(WonLast = ifelse(WonLast, "Winners", "Losers"),
         Switched = ifelse(Switched, "Switched", "Stayed"))

# Plot results
ggplot(switch_summary, aes(x = as.factor(WonLast), y = Prop, fill = as.factor(Switched))) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("0" = "Losers", "1" = "Winners")) +
  scale_fill_manual(values = c(my_red, my_green), labels = c("Stayed", "Switched")) +
  labs(x = "Previous Round Result", y = "Proportion", fill = "Action",
       title = "Do Winners Stick While Losers Switch?") +
  theme_minimal()
# Plot with corrected labels
ggplot(switch_summary, aes(x = WonLast, y = Prop, fill = Switched)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Stayed" = my_red, "Switched" = my_green)) +
  labs(x = "Previous Round Result", y = "Proportion", fill = "Action",
       title = "Do Winners Stick While Losers Switch?") +
  theme_minimal()











stan_data <- list(N = nrow(dat), 
                  choices = dat$Chiara,
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


