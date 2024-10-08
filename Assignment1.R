# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(polynom)

# Read the CSV file with correct delimiter
CerealsDF <- read_delim("assignment1.csv", delim = ";", skip = 1)

# Rename columns to meaningful names based on assumed context
colnames(CerealsDF) <- c("name", "mfr", "type", "calories", "protein", "fat", "sodium",
                         "fiber", "carbo", "sugars", "potass", "vitamins", "shelf", 
                         "weight", "cups", "rating")

# Display the structure and first ten rows
str(CerealsDF)
print(head(CerealsDF, 10))

# Remove the second line (assuming it is the second row in the dataframe)
CerealsDF <- CerealsDF[-2, ]

# Print number of rows and columns
print(dim(CerealsDF))

# Add 'totalcarbo' column if 'carbo' and 'sugars' exist
if ("carbo" %in% colnames(CerealsDF) && "sugars" %in% colnames(CerealsDF)) {
  CerealsDF <- CerealsDF %>% mutate(totalcarbo = as.numeric(carbo) + as.numeric(sugars))
} else {
  print("Columns 'carbo' and/or 'sugars' not found.")
}

# Find number of hot cereals if 'type' exists
if ("type" %in% colnames(CerealsDF)) {
  hot_cereals <- subset(CerealsDF, type == "H")
  print(nrow(hot_cereals))
} else {
  print("Column 'type' not found.")
}

# Find number of unique manufacturers if 'mfr' exists
if ("mfr" %in% colnames(CerealsDF)) {
  unique_manufacturers <- unique(CerealsDF$mfr)
  print(length(unique_manufacturers))
} else {
  print("Column 'mfr' not found.")
}

# Extract cereals by Kellogg’s if 'mfr' exists
if ("mfr" %in% colnames(CerealsDF)) {
  cereals_K <- subset(CerealsDF, mfr == "K")
  print(cereals_K)
} else {
  print("Column 'mfr' not found.")
}

# Extract cereals with <= 90 calories and > 2 units of fat if 'calories' and 'fat' exist
if ("calories" %in% colnames(CerealsDF) && "fat" %in% colnames(CerealsDF)) {
  subset_cereals <- subset(CerealsDF, calories <= 90 & fat > 2)
  # Save this subset as a CSV file
  write_csv(subset_cereals, "subset_cereals.csv")
} else {
  print("Columns 'calories' and/or 'fat' not found.")
}

#TASK II: Given data
x_vals <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
f_x <- c(0, -1.921, 0, 3.584, 0, -6.718, 0)

# Implement the interpolation function MyIntCal
MyIntCal <- function(x_values, y_values, x_val) {
  n <- length(x_values)
  result <- 0
  for (i in 1:n) {
    term <- y_values[i]
    for (j in 1:n) {
      if (j != i) {
        term <- term * (x_val - x_values[j]) / (x_values[i] - x_values[j])
      }
    }
    result <- result + term
  }
  return(result)
}

# Degree of the interpolating function
degree <- length(x_vals) - 1
print(degree)

# Function to create Lagrange polynomials
create_lagrange_polynomial <- function(k, x_values) {
  n <- length(x_values)
  Lk <- function(x_val) {
    result <- 1
    for (j in 1:n) {
      if (j != k) {
        result <- result * (x_val - x_values[j]) / (x_values[k] - x_values[j])
      }
    }
    return(result)
  }
  return(Vectorize(Lk))
}

par(mfrow=c(4, 2), mar=c(2, 2, 1, 1))

for (i in 1:length(x_vals)) {
  Lk <- create_lagrange_polynomial(i, x_vals)
  curve(Lk, from = min(x_vals) - 1, to = max(x_vals) + 1, col = i, main = paste("L", i, sep=""), xlab = "x", ylab = "L(x)")
}

# Plot the final interpolating function
curve(MyIntCal(x_vals, f_x, x), from = min(x_vals) - 1, to = max(x_vals) + 1, col = "black", main = "Interpolating Function", xlab = "x", ylab = "f(x)")

# Save the plot as a PDF file
dev.copy(pdf, "MyIntFig.pdf")
dev.off()

# Using poly.calc to find interpolating function
pf_x <- poly.calc(x_vals, f_x)

# Find f(15) and f(24) using MyIntCal() and pf_x
f_15_MyIntCal <- MyIntCal(x_vals, f_x, 15)
f_15_pf_x <- predict(pf_x, 15)
f_24_MyIntCal <- MyIntCal(x_vals, f_x, 24)
f_24_pf_x <- predict(pf_x, 24)

print(f_15_MyIntCal)
print(f_15_pf_x)
print(f_24_MyIntCal)
print(f_24_pf_x)

