# Load necessary library
library(ineq)

# Number of households in the population
population_size <- 10000

# Percentage of households with zero wealth
zero_wealth_percentage <- 0.80

# Number of households with zero wealth
zero_wealth_households <- population_size * zero_wealth_percentage

# Number of households with non-zero wealth
non_zero_wealth_households <- population_size - zero_wealth_households

# Sample wealth distribution (assuming it follows a similar distribution as the sample)
sample_wealth <- rlnorm(100, meanlog = 10, sdlog = 2)  # Example distribution

# Simulate wealth for the non-zero wealth households
set.seed(123)  # For reproducibility
simulated_wealth <- sample(sample_wealth, non_zero_wealth_households, replace = TRUE)

# Combine zero wealth and non-zero wealth households
population_wealth <- c(rep(0, zero_wealth_households), simulated_wealth)

# Calculate the Gini coefficient for the population
gini_coefficient <- ineq(population_wealth, type = "Gini")

# Print the Gini coefficient
print(gini_coefficient)

g_sample <- ineq(sample_wealth, type = "Gini")
g_sample



# Updated

# Load necessary library
library(ineq)

# Number of households in the population
population_size <- 10000

# Percentage of households with zero wealth
zero_wealth_percentage <- 0.80

# Number of households with zero wealth
zero_wealth_households <- population_size * zero_wealth_percentage

# Number of households with non-zero wealth
non_zero_wealth_households <- population_size - zero_wealth_households

# Adjust parameters of the log-normal distribution to target Gini coefficient of 0.79
# We need to iterate to find appropriate values
set.seed(123)  # For reproducibility

# Initial parameters
meanlog <- 10
sdlog <- 1.8

# Function to calculate Gini coefficient for a given sample
calculate_gini <- function(meanlog, sdlog) {
  sample_wealth <- rlnorm(100, meanlog = meanlog, sdlog = sdlog)
  return(ineq(sample_wealth, type = "Gini"))
}

# Iterate to find better parameters
for (i in 1:1000) {
  gini_value <- calculate_gini(meanlog, sdlog)
  if (abs(gini_value - 0.79) < 0.01) {
    break
  } else if (gini_value > 0.79) {
    sdlog <- sdlog - 0.01
  } else {
    sdlog <- sdlog + 0.01
  }
}

# Simulate wealth for the non-zero wealth households using the adjusted parameters
simulated_wealth <- rlnorm(non_zero_wealth_households, meanlog = meanlog, sdlog = sdlog)

# Combine zero wealth and non-zero wealth households
population_wealth <- c(rep(0, zero_wealth_households), simulated_wealth)

# Calculate the Gini coefficient for the population
gini_coefficient <- ineq(population_wealth, type = "Gini")

# Print the Gini coefficient
print(gini_coefficient)



