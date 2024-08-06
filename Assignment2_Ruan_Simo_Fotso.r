
# Assignment 2: R Programming
bestFitFun <- function() {
  # Load necessary libraries
  library(readxl)
  library(ggplot2)
  
  # Function to prompt for file and load data
  load_data <- function() {
    repeat {
      cat("Please enter the name of the file to open: ")
      filename <- readline()
      if (file.exists(filename)) {
        return(read_excel(filename))
      } else {
        cat("File does not exist, please try again.\n")
      }
    }
  }
  
  # Function to calculate the sum of squares of residuals
  calculate_sr <- function(model, data) {
    fitted_values <- predict(model, newdata = data)
    return(sum((data$distance - fitted_values)^2))
  }
  
  # Main program
  rocket_data <- load_data()
  rocket_data <- as.data.frame(rocket_data)
  
  # Rename columns to match the expected names
  colnames(rocket_data) <- c("time", "distance")
  
  # Rescale the data to avoid large values
  rocket_data$time <- rocket_data$time / 100 #10
  rocket_data$distance <- rocket_data$distance / 1000000 #1000
  
  # Print the data frame for debugging
  #print(rocket_data)
  
  while (TRUE) {
    cat("MENU\n1. Best Fit\n2. Quit\n")
    choice <- as.integer(readline())
    
    if (choice == 2) {
      cat("Exiting...\n")
      break
    } else if (choice == 1) {
      # Fit power model: d = a * t^b
      cat("Fitting power model...\n")
      power_model <- tryCatch(
        nls(distance ~ a * time^b, data = rocket_data, start = list(a = 1, b = 1)),
        error = function(e) e
      )
      
      if (inherits(power_model, "error")) {
        cat("Error in fitting power model: ", power_model$message, "\n")
      } else {
        sr_power <- calculate_sr(power_model, rocket_data)
        
        # Print power model results
        cat("Power Model: d = a * t^b\n")
        print(summary(power_model)$coefficients)
        cat("Sr =", sr_power, "\n")
      }
      
      
      # Fit exponential model: D = a * exp(b * t)
      cat("Fitting exponential model...\n")
      exp_model <- tryCatch(
        nls(distance ~ a * exp(b * time), data = rocket_data, start = list(a = 1, b = 0.1)),
        error = function(e) e
      )
      
      if (inherits(exp_model, "error")) {
        cat("Error in fitting exponential model: ", exp_model$message, "\n")
        sr_exp <- Inf
      } else {
        sr_exp <- calculate_sr(exp_model, rocket_data)
        
        # Print exponential model results
        cat("Exponential Model: D = a * e^(b*t)\n")
        print(summary(exp_model)$coefficients)
        cat("Sr =", sr_exp, "\n")
      }
      
      # Determine the best fit model
      if (!inherits(power_model, "error") && sr_exp != Inf) {
        best_model <- ifelse(sr_power < sr_exp, "Power", "Exponential")
        cat("The best fit model is", best_model, "model.\n")
      } else if (!inherits(power_model, "error")) {
        best_model <- "Power"
        cat("The best fit model is", best_model, "model.\n")
      } else {
        best_model <- "None"
        cat("No model could be fitted properly.\n")
      }
      
      # Plot the data and best-fit model
      plot <- ggplot(rocket_data, aes(x = time, y = distance)) +
        geom_point() +
        labs(title = "Rocket Distance over Time", x = "Time-sec", y = "Distance-meter")
      
      if (!inherits(power_model, "error")) {
        plot <- plot + stat_function(fun = function(x) {
          predict(power_model, newdata = data.frame(time = x))
        }, color = "blue")
      }
      
      if (!inherits(exp_model, "error")) {
        plot <- plot + stat_function(fun = function(x) {
          predict(exp_model, newdata = data.frame(time = x))
        }, color = "red")
      }
      
      print(plot)
      ggsave("best_fit.pdf")
      
      # Extrapolation menu
      while (TRUE) {
        cat("MENU\n1. Extrapolation\n2. Main Menu\n")
        sub_choice <- as.integer(readline())
        
        if (sub_choice == 2) break
        
        cat("Please enter the time to extrapolate to: ")
        time_to_extrapolate <- as.numeric(readline())
        
        if (best_model == "Power" && !inherits(power_model, "error")) {
          extrapolated_value <- predict(power_model, newdata = data.frame(time = time_to_extrapolate / 10)) * 1000
        } else if (best_model == "Exponential" && !inherits(exp_model, "error")) {
          extrapolated_value <- predict(exp_model, newdata = data.frame(time = time_to_extrapolate / 10)) * 1000
        } else {
          extrapolated_value <- NA
        }
        
        if (!is.na(extrapolated_value)) {
          cat("Extrapolated distance at time", time_to_extrapolate, "is", extrapolated_value, "meters.\n")
        } else {
          cat("Extrapolation could not be performed due to model fitting errors.\n")
        }
      }
    } else {
      cat("Invalid choice, please try again.\n")
    }
  }
}




ODEsolver <- function() {
  # Define the ODE function
  f <- function(t, d) {
    cos(4 * t) - 2 * d
  }
  
  # Exact solution function
  exact_solution <- function(t) {
    0.1 * cos(4 * t) + 0.2 * sin(4 * t) + 2.9 * exp(-2 * t)
  }
  
  # Euler's Method
  euler_method <- function(h) {
    t <- seq(0, 2, by = h)
    d <- numeric(length(t))
    d[1] <- 3
    for (i in 1:(length(t) - 1)) {
      d[i + 1] <- d[i] + h * f(t[i], d[i])
    }
    return(data.frame(time = t, estimated = d))
  }
  
  # Runge-Kutta 4th Order Method
  runge_kutta_method <- function(h) {
    t <- seq(0, 2, by = h)
    d <- numeric(length(t))
    d[1] <- 3
    for (i in 1:(length(t) - 1)) {
      k1 <- h * f(t[i], d[i])
      k2 <- h * f(t[i] + h / 2, d[i] + k1 / 2)
      k3 <- h * f(t[i] + h / 2, d[i] + k2 / 2)
      k4 <- h * f(t[i] + h, d[i] + k3)
      d[i + 1] <- d[i] + (k1 + 2 * k2 + 2 * k3 + k4) / 6
    }
    return(data.frame(time = t, estimated = d))
  }
  
  # Main program
  while (TRUE) {
    cat("Choose the method for solving the ODE:\n1. Euler’s Method\n2. Runge-Kutta 4th Order Method\n")
    method_choice <- as.integer(readline())
    if (method_choice != 1 && method_choice != 2) {
      cat("Invalid choice, please try again.\n")
      next
    }
    
    cat("Choose step size “h” (0.8, 0.2, 0.05): ")
    h <- as.numeric(readline())
    
    if (!(h %in% c(0.8, 0.2, 0.05))) {
      cat("Invalid step size, please try again.\n")
      next
    }
    
    if (method_choice == 1) {
      results <- euler_method(h)
    } else {
      results <- runge_kutta_method(h)
    }
    
    exact_temps <- exact_solution(results$time)
    percentage_error <- abs((exact_temps - results$estimated) / exact_temps) * 100
    
    results$exact <- exact_temps
    results$error <- percentage_error
    
    # Print the results with rounding
    cat("Time(seconds)\tExact Temp(c)\tEstimated Temp(c)\tPercentage Error(%)\n")
    cat("----------------------------------------------------------------------------\n")
    for (i in 1:nrow(results)) {
      cat(sprintf("%.1f\t          %.3f\t             %.3f\t                %.2f\n", 
                  results$time[i], 
                  round(results$exact[i], 3), 
                  round(results$estimated[i], 3), 
                  round(results$error[i], 2)))
    }
    cat("----------------------------------------------------------------------------\n")
    
    cat("Choose another method and step size? (y/n): ")
    continue <- readline()
    if (tolower(continue) != 'y') break
  }
}

#/Users/thelion/CST8233-Numerical-Computing/rocket.xlsx

bestFitFun()
ODEsolver()
