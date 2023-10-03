# Set seed for reproducibility
set.seed(123)

# Number of months
n_months <- 60

# Generate the trend component
trend <- seq(100, 200, length.out = n_months)

# Generate the seasonal component (annual seasonality)
seasonality <- rep(c(10, -10, 5, -5), length.out = n_months)

# Generate random noise (positive)
noise <- runif(n_months, min = 0, max = 5)

# Combine trend, seasonality, and noise while ensuring all numbers are positive
data <- trend + seasonality + noise

# Ensure all numbers are above 100
data <- pmax(data, 100)

# Save the data as a text file with rows delimited by commas
write.table(data, "./data/sample.csv", sep = ",", row.names = FALSE, col.names = FALSE)
