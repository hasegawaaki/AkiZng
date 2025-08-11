# Load the tidyverse package for data manipulation and visualization
library(tidyverse)

install.packages("MASS")
library(MASS)

# Install the olsrr package
install.packages("olsrr")

# Load the olsrr package
library(olsrr)

# Install the GGally package if you haven't already
install.packages("GGally")

# Load the GGally package
library(GGally)

install.packages("car")
library(car)

# Assuming the file is saved as 'housing_data.csv' in your working directory
data <- read_csv("housing.csv")

# View the first few rows of the dataset
head(data)

# Check for missing values
sum(is.na(data))

# Convert 'parking' to a factor since it's categorical
data$parking <- as.factor(data$parking)

# Assuming your data frame is called 'data' and has the variables you're interested in
ggpairs(data, columns = c("price", "elevation", "dist_am1", "dist_am2", "dist_am3", "bath", "sqft", "precip"))

# Boxplot for 'parking' type against price
ggplot(data, aes(x = parking, y = price)) + geom_boxplot() + theme_minimal()

# Build initial linear regression model
model.lm <- lm(price ~ ., data = data)

# Summary of the model to check coefficients and statistics

outlierTest(model.lm, data = data)

# Remove the outlier by row number
data_clean <- data[-348, ]

# Refit the linear regression model without the outlier
model_refit <- lm(price ~ elevation + dist_am1 + dist_am2 + dist_am3 + bath + sqft + parking + precip, data = data_clean)

# Summary of the refitted model
summary(model_refit)

ols_step_forward_p(model_refit)

model.selection <- ols_step_all_possible(model_refit)

plot(model.selection)

model.selection$result$predictors[c(1, 9, 37, 163, 219, 247, 255)]
