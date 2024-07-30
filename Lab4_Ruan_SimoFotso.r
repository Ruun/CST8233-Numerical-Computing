
# Function to compute the Maclaurin series for ln(1 + x) and errors
maclaurin_ln <- function(x, terms = 10) {
  series <- 0
  cat("n\tSeries Value\n")
  cat("------------------------\n")
  
  for (n in 1:terms) {
    term <- ((-1)^(n + 1) * x^n) / n
    series <- series + term
    
    # Compute and print the value of the series for up to ten terms
    cat(n, "\t", series, "\n")
  }
  
  # Print a separator for clarity
  cat("\nErrors:\n")
  cat("n\tAbsolute Error\tRelative Error\n")
  cat("----------------------------------------\n")
  
  # Reinitialize series for error calculation
  series <- 0
  for (n in 1:terms) {
    term <- ((-1)^(n + 1) * x^n) / n
    series <- series + term
    
    # Calculate errors
    true_value <- log(1 + x)
    abs_error <- abs(true_value - series)
    rel_error <- abs_error / abs(true_value)
    
    # Print errors
    cat(n, "\t", abs_error, "\t", rel_error, "\n")
  }
}

# User input
x <- as.numeric(readline(prompt = "Enter the value of x: "))

# Compute series and errors
maclaurin_ln(x, terms = 10)

# Plotting
library(ggplot2)

# Original function and approximation
x_vals <- seq(-0.9, 2, by = 0.01)
y_vals <- log(1 + x_vals)
y_approx <- x_vals - x_vals^2/2 + x_vals^3/3 - x_vals^4/4 + x_vals^5/5 - x_vals^6/6

df <- data.frame(x = x_vals, Original = y_vals, Approximation = y_approx)

ggplot(df, aes(x)) +
  geom_line(aes(y = Original, color = "Original Function")) +
  geom_line(aes(y = Approximation, color = "Maclaurin Approximation")) +
  labs(title = "Maclaurin Series Approximation of ln(1 + x)",
       y = "f(x)",
       color = "Legend") +
  theme_minimal()


