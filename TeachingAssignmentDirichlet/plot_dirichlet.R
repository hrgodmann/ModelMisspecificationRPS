# Install ggtern if not already installed
if (!requireNamespace("ggtern", quietly = TRUE)) {
  install.packages("ggtern")
}

library(ggtern)

# Function to generate points from a Dirichlet distribution
rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  x <- matrix(rgamma(n * k, alpha), ncol = k, byrow = TRUE)
  x <- x / rowSums(x)
  return(x)
}

# Parameters for the Dirichlet prior
alpha_prior <- c(2, 1, 3)

# Observations
observed_counts <- c(2, 1, 0)

# Updated (posterior) parameters
alpha_posterior <- alpha_prior + observed_counts

# Generate samples for the prior and posterior
set.seed(123)  # For reproducibility
samples_prior <- as.data.frame(rdirichlet(1000, alpha_prior))
samples_posterior <- as.data.frame(rdirichlet(1000, alpha_posterior))

# Plotting Dirichlet Prior
ggtern(data = samples_prior, aes(x = V1, y = V2, z = V3)) +
  geom_point(alpha = 0.5, color = "blue") +
  ggtitle("Dirichlet Prior (2, 1, 3)") +
  theme_minimal()

# Plotting Dirichlet Posterior
ggtern(data = samples_posterior, aes(x = V1, y = V2, z = V3)) +
  geom_point(alpha = 0.5, color = "red") +
  ggtitle("Dirichlet Posterior (4, 2, 3)") +
  theme_minimal()
