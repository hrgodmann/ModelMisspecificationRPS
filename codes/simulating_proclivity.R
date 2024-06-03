set.seed(42) # Set a seed for reproducibility

# Parameters
n_trials <- 1000 # Total number of trials
initial_prob_paper <- 1/3 # Initial probability of choosing paper

# Initialize vectors to store choices
choices_player_a <- integer(n_trials)
choices_player_b <- integer(n_trials)

# Simulate the trials
for (i in 1:n_trials) {
  # Increase the probability for paper linearly for Player A
  prob_paper_a <- initial_prob_paper + (i - 1) * ((2/3) - initial_prob_paper) / n_trials
  probs_player_a <- c(1/3 * (1 - prob_paper_a), prob_paper_a, 1/3 * (1 - prob_paper_a))
  
  # Player B's probabilities remain the same
  probs_player_b <- c(1/3, 1/3, 1/3)
  
  # Sample choices for each player
  choices_player_a[i] <- sample(c(1, 2, 3), size = 1, prob = probs_player_a)
  choices_player_b[i] <- sample(c(1, 2, 3), size = 1, prob = probs_player_b)
}



# Combine the choices into comma-separated strings
data_simulated <- data.frame(
  Player_A = choices_player_a,
  Player_B = choices_player_b
)

data_simulated$Round <- 1:n_trials
data_simulated <- data_simulated[c(3,1,2)]


write.csv(data_simulated, paste("/Users/henrikgodmann/Desktop/rps_teaching/data_proclivity/simulated_data_p_",n_trials,".csv", sep = ""), row.names = FALSE)













choices_numeric <- as.numeric(data_simulated$Player_A)

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
proportions_wide
