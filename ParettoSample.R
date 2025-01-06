# Load necessary library
library(ineq)
library(evd)  # For generalized Pareto distribution

# Number of households in the population
population_size <- 26000000

# Percentage of households with zero wealth
zero_wealth_percentage <- 0.80

# Number of households with zero wealth
zero_wealth_households <- population_size * zero_wealth_percentage

# Number of households with non-zero wealth
non_zero_wealth_households <- population_size - zero_wealth_households

# Sample size
sample_size <- 200000

# Adjust parameters of the generalized Pareto distribution to target Gini coefficient of 0.79
# We need to iterate to find appropriate values
set.seed(123)  # For reproducibility

# Initial parameters
loc <- 0
scale <- 1
shape <- 0.5

# Function to calculate Gini coefficient for a given sample
calculate_gini <- function(loc, scale, shape) {
  sample_wealth <- rpareto(sample_size, loc = loc, scale = scale, shape = shape)
  return(ineq(sample_wealth, type = "Gini"))
}

# Iterate to find better parameters
for (i in 1:1000) {
  gini_value <- calculate_gini(loc, scale, shape)
  if (abs(gini_value - 0.79) < 0.01) {
    break
  } else if (gini_value > 0.79) {
    scale <- scale - 0.01
  } else {
    scale <- scale + 0.01
  }
}

# Simulate wealth for the non-zero wealth households using the adjusted parameters
simulated_wealth <- rpareto(non_zero_wealth_households, loc = loc, scale = scale, shape = shape)

# Combine zero wealth and non-zero wealth households
population_wealth <- c(rep(0, zero_wealth_households), simulated_wealth)

# Calculate the Gini coefficient for the population
gini_coefficient <- ineq(population_wealth, type = "Gini")

# Print the Gini coefficient
print(gini_coefficient)