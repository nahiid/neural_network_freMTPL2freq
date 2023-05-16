setwd("D:/research/neural_network_freMTPL2freq") 

# Read in the data
data <- read.csv("./data/freMTPL2freq.csv")

#Add average salary column to the dataset>>>

# Define a function to calculate the average salary based on the area
get_avg_salary <- function(area) {
  if (area == "A") {
    return(90)
  } else if (area == "B") {
    return(80)
  } else if (area == "C") {
    return(70)
  } else if (area == "D") {
    return(60)
  } else if (area == "E") {
    return(50)
  } else if (area == "F") {
    return(40)
  } else {
    return(10)
  }
}

# Apply the function to the 'Area' column to create the 'avgsalary' column
data$AvgSalary <- sapply(data$Area, get_avg_salary)

# save the updated dataset
write.csv(data, "./data/freMTPL2freq_updated.csv", row.names = FALSE)

# Load the updated dataset
data <- read.csv("./data/freMTPL2freq_updated.csv")
head(data)

# hot encoding for Area column>>

# Define the area values
area_values <- c("A", "B", "C", "D", "E", "F")

# Create a matrix of 0s with the same number of rows as the dataframe and
# one column for each possible area value
area_matrix <- matrix(0, nrow = nrow(data), ncol = length(area_values))

# Loop through the area values and set the corresponding column in the matrix to 1
for (i in 1:length(area_values)) {
  area_matrix[, i] <- as.integer(data$Area == area_values[i])
}

# Bind the matrix of one-hot encoded columns to the original dataframe
data <- cbind(data, area_matrix)

# Remove the original 'Area' column
data$Area <- NULL

library(dplyr)

# Rename columns
data <- data %>% rename(Area_A = '1', Area_B = '2', Area_C = '3', Area_D = '4', Area_E = '5', Area_F = '6')

# Check hot encoding
head(data)

# Remove the 'VehGas' column from the dataset
data$VehGas <- NULL

# Check dataset
head(data)

# hot encoding for 'VehBrand' column>>

# Define the Brand values
brand_values <- c("B12", "B6", "B3", "B2", "B5", "B10", "B14", "B13", "B4", "B1", "B11")

# Create a matrix of 0s with the same number of rows as the dataframe and
# one column for each possible brand value
brand_matrix <- matrix(0, nrow = nrow(data), ncol = length(brand_values))

# Loop through the brand values and set the corresponding column in the matrix to 1
for (j in 1:length(brand_values)) {
  brand_matrix[, j] <- as.integer(data$VehBrand == brand_values[j])
}

# Bind the matrix of one-hot encoded columns to the original dataframe
data <- cbind(data, brand_matrix)

# Remove the original 'VehBrand' column
data$VehBrand <- NULL

# Check hot encoding
head(data)

library(dplyr)

# Rename columns
data <- data %>% rename(B12 = '1', B6 = '2', B3 = '3', B2 = '4', B5 = '5', B10 = '6', B14 = '7', B13 = '8', B4 = '9', B1 = '10', B11 = '11')

# Check renaming
head(data)

# Get the unique values of the 'Region' column>>>
unique_regions <- unique(data$Region)

# Print the unique regions
print(unique_regions)

# hot encoding for 'Region' column>>

# Define the Brand values
region_values <- c("R82","R22","R72","R31","R91","R52","R93","R11","R24","R94",
                  "R83","R54","R26","R53","R73","R42","R25","R21","R41","R43","R74","R23")

# Create a matrix of 0s with the same number of rows as the dataframe and
# one column for each possible brand value
region_matrix <- matrix(0, nrow = nrow(data), ncol = length(region_values))

# Loop through the brand values and set the corresponding column in the matrix to 1
for (k in 1:length(region_values)) {
  region_matrix[, k] <- as.integer(data$Region == region_values[k])
}

# Bind the matrix of one-hot encoded columns to the original dataframe
data <- cbind(data, region_matrix)

# Remove the original 'Region' column
data$Region <- NULL

# Check hot encoding
head(data)

library(dplyr)

# Rename columns
data <- data %>% rename(R82 = '1', R22 = '2', R72 = '3', R31 = '4', R91 = '5', R52 = '6', R93 = '7', R11 = '8',
                      R24 = '9', R94 = '10', R83 = '11', R54 = '12', R26 = '13', R53 = '14', R73 ='15', 
                      R42 = '16', R25 = '17', R21 = '18', R41 = '19', R43= '20', R74= '21', R23 = '22')

# Check renaming
head(data)

library(caret)
library(neuralnet)

# Split the dataset into training and validation sets
set.seed(123)
trainIndex <- createDataPartition(data$AvgSalary, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
valid <- data[-trainIndex, ]

# Check the data types of each column
str(train)

# Check for missing or invalid values
summary(train)

# Load the required libraries
library(rstan)
library(brms)
library(rstanarm)

# Define the formula for the model
formula <- bf(AvgSalary ~ Area_A + Area_B + Area_C + Area_D + Area_E + Area_F +
                B12 + B6 + B3 + B2 + B5 + B10 + B14 + B13 + B4 + B1 + B11 +
                R82 + R22 + R72 + R31 + R91 + R52 + R93 + R11 + R24 + R94 +
                R83 + R54 + R26 + R53 + R73 + R42 + R25 + R21 + R41 + R43 + R74 + R23 + 
                BonusMalus + Density + DrivAge + VehAge + Exposure + VehPower + ClaimNb)

# Define the glm model
bnn_model <- stan_glm(formula, data = train, family = gaussian(),
                      prior_intercept = normal(0, 10),
                      prior = normal(0, 1), seed = 12345)


# Package loading:
require(tagi)
require(mvtnorm)

#set seed
set.seed(123)

# Specific Initialization
# Define the neural network properties, such as the number of epochs, activation function, etc.

nobs <- nrow(data)
ncvr <- 45
# Input features
x <- data[,1:ncvr]
# Output targets
y <- matrix(data[,46], ncol = 1)
nx <- ncol(x)
ny <- ncol(y)

NN <- list(
  "nx" = nx, # Number of input covariates
  "ny" = ny, # Number of output responses
  "batchSizeList" = c(1, 1, 1), # Batch size [train, val, test]
  "nodes" = c(nx, 100, ny), # Number of nodes for each layer
  "sx" = NULL, # Input standard deviation
  "sv" = 0.32 * matrix(1L, nrow = 1, ncol = ny), # Observations standard deviation
  "maxEpoch" = 40, # maximal number of learning epoch
  "hiddenLayerActivation" = "relu", # Activation function for hidden layer {'tanh','sigm','cdf','relu','softplus'}
  "outputActivation" = "linear", # Activation function for hidden layer {'linear', 'tanh','sigm','cdf','relu'}
  "ratio" = 0.8, # Ratio between training set and validation set
  "numSplits" = 20, # Number of splits
  "task" = "regression" # Task regression or classification
)
