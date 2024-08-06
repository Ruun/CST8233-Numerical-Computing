
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
  0.5 * exp(sin(2) -sin(t))
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

# Function representing the ODE y' = -y * cos(t)
ode_function <- function(t, y) {
  -y * cos(t)
}

# Initial conditions and parameters
y0 <- 1.241
t0 <- 0
t_end <- 6
h <- 0.5

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
error_data <- data.frame(h = t_0.5, absolute_error = absolute_error, relative_error = relative_error)
print(error_data)

# Plot the displacement using Euler's method and the exact solution
ggplot() +
  geom_line(data = solution_0.5, aes(x = t, y = y, color = "Euler's Method")) +
  geom_point(data = solution_0.5, aes(x = t, y = y, color = "Euler's Method"), size = 2) +
  geom_line(aes(x = t_0.5, y = y_exact_0.5, color = "Exact Solution")) +
  geom_point(aes(x = t_0.5, y = y_exact_0.5, color = "Exact Solution"), size = 2) +
  labs(title = "Displacement y(t) using Euler's Method and Exact Solution",
       x = "t",
       y = "y(t)",
       color = "Method") +
  theme_minimal()
