#Ruan Simo
#Load the package
require(PolynomF)

# Define the independent variable
x <- polynom(coef = c(0, 0, 0, 1))

# Define the function p(x) = x^3 - 3x^2 - 2x + 7
p <- polynom(c(7, -2, -3, 1))

# Check the class of p
class_p <- class(p)
print(class_p)

# Find the coefficients of p
coefficients_p <- coef(p)e
print(coefficients_p)

#-------------------------------------------------------

# Polynomial q = y^2 + 2y
y <- polynom(coef = c(0, 0, 1))
q <- polynom(c(0, 2, 1))

# Check the class of q and y
class_q <- class(q)
print(class_q)
class_y <- class(y)
print(class_y)

# Perform operations
p_plus_q <- p + q
p_minus_q <- p - q
p_times_q <- p * q

print(p_plus_q)
print(p_minus_q)
print(p_times_q)

#-------------------------------------------------------

# Find the derivatives
dpdx <- deriv(p)
dqdy <- deriv(q)

print(dpdx)
print(dqdy)

# Plot p and dpdx
curve(p(x), from = -2, to = 3, ylab = "p(x), dpdx", col = "blue", main = "p(x) and dpdx")
curve(dpdx(x), from = -2, to = 3, add = TRUE, col = "red")
abline(h = 0, col = "black")


#-------------------------------------------------------
#Statistics in R

# Load the dataset
data("airquality")
my_df <- airquality

# Display the structure
str(my_df)

# Print the first six lines
head(my_df)

# Find the column names
column_names <- names(my_df)
print(column_names)

#Create a new dataframe that shows only the temperature column and name it as my_df_temp. You can use select() function

# Load the dplyr package
require(dplyr)

# Select the temperature column
my_df_temp <- select(my_df, Temp)
print(my_df_temp)

#Find the mean, median, and standard deviation of the daily temperature in June, July, and August

summer_df <- filter(my_df, Month %in% c(6, 7, 8))

# Calculate mean, median, and standard deviation
mean_temp <- mean(summer_df$Temp, na.rm = TRUE)
median_temp <- median(summer_df$Temp, na.rm = TRUE)
sd_temp <- sd(summer_df$Temp, na.rm = TRUE)

print(mean_temp)
print(median_temp)
print(sd_temp)

# Probabilities using pnorm
prob_less_than_70 <- pnorm(70, mean = mean_temp, sd = sd_temp)
prob_greater_than_85 <- 1 - pnorm(85, mean = mean_temp, sd = sd_temp)
prob_between_75_and_90 <- pnorm(90, mean = mean_temp, sd = sd_temp) - pnorm(75, mean = mean_temp, sd = sd_temp)

print(prob_less_than_70)
print(prob_greater_than_85)
print(prob_between_75_and_90)



