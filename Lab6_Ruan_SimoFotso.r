
# Function implementing Euler's method
euler_method <- function(f, y0, t0, t_end, h) {
  t_values <- seq(t0, t_end, by = h)
  y_values <- numeric(length(t_values))
  y_values[1] <- y0
  
  for (i in 1:(length(t_values) - 1)) {
    y_values[i + 1] <- y_values[i] + h * f(t_values[i], y_values[i])
  }
  
  return(data.frame(t = t_values, y = y_values))
}

# Function representing the ODE y' = -y * cos(t)
ode_function <- function(t, y) {
  -y * cos(t)
}

# Initial conditions and parameters
y0 <- 1.241
t0 <- 0
t_end <- 6

# Calculate y(t) for different step sizes
h_values <- c(0.5, 0.25, 0.1)
solutions <- lapply(h_values, function(h) euler_method(ode_function, y0, t0, t_end, h))

# Plot the results
library(ggplot2)

plot_data <- do.call(rbind, lapply(1:length(h_values), function(i) {
  data <- solutions[[i]]
  data$h <- h_values[i]
  data
}))

ggplot(plot_data, aes(x = t, y = y, color = factor(h), group = h)) +
  geom_line() +
  labs(title = "Displacement y(t) using Euler's Method For Each Case",
       x = "t",
       y = "y(t)",
       color = "Step Size (h)") +
  theme_minimal()

# Load necessary library
library(ggplot2)

# Exact solution function based on the given formula
exact_solution <- function(t) {
  0.5 * exp(sin(2 * exp(-sin(t))))
}

# Function implementing Euler's method
euler_method <- function(f, y0, t0, t_end, h) {
  t_values <- seq(t0, t_end, by = h)
  y_values <- numeric(length(t_values))
  y_values[1] <- y0
  
  for (i in 1:(length(t_values) - 1)) {
    y_values[i + 1] <- y_values[i] + h * f(t_values[i], y_values[i])
  }
  
  return(data.frame(t = t_values, y = y_values))
}

# Initial conditions and parameters
y0 <- 1.241
t0 <- 0
t_end <- 6
h <- 0.5

# Define the ODE function y' = -y * cos(t)
ode_function <- function(t, y) {
  -y * cos(t)
}

# Calculate y(t) using Euler's Method for h = 0.5
solution_0.5 <- euler_method(ode_function, y0, t0, t_end, h)
t_0.5 <- solution_0.5$t
y_0.5 <- solution_0.5$y

# Calculate the exact solution values for comparison
y_exact_0.5 <- exact_solution(t_0.5)

# Calculate absolute and relative errors
absolute_error <- abs(y_exact_0.5 - y_0.5)
relative_error <- absolute_error / abs(y_exact_0.5)

# Create data frame for errors
error_data <- data.frame(t = t_0.5, absolute_error = absolute_error, relative_error = relative_error)

# Plot the errors
ggplot(error_data, aes(x = t)) +
  geom_line(aes(y = absolute_error, color = "Absolute Error")) +
  geom_line(aes(y = relative_error, color = "Relative Error")) +
  labs(title = "Errors for Euler's Method (h = 0.5)",
       x = "t",
       y = "Error",
       color = "Error Type") +
  theme_minimal()
