setwd("D:/research/neural_network_freMTPL2freq") 
data <- read.csv("./data/freMTPL2freq.csv")

# Read in the data
data <- read.csv("./data/freMTPL2freq.csv")

# Define the area values
area_values <- c("A", "B", "C", "D", "E", "F", "G", "H")

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
data <- data %>% rename(Area_A = '1', Area_B = '2', Area_C = '3', Area_D = '4', Area_E = '5', Area_F = '6', Area_G = '7', Area_H = '8')
 
head(data)
#####me


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
  } else if (area == "G") {
    return(30)
  } else if (area == "H") {
    return(20)
  } else {
    return(10)
  }
}

# Apply the function to the 'Area' column to create the 'avgsalary' column
data$AvgSalary <- sapply(data$Area, get_avg_salary)

# save the updated dataset
write.csv(data, "./data/freMTPL2freq_updated.csv", row.names = FALSE)

library(caret)
library(neuralnet)

# Load the updated dataset
data <- read.csv("./data/freMTPL2freq_updated.csv")

# Split the dataset into training and validation sets
set.seed(123)
trainIndex <- createDataPartition(data$AvgSalary, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
valid <- data[-trainIndex, ]

# Scale the input variables to have zero mean and unit variance
preProcValues <- preProcess(train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","AvgSalary")], method = c("center", "scale"))

train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","AvgSalary")] <- predict(preProcValues, train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")])
valid[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","AvgSalary")] <- predict(preProcValues, valid[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")])

numeric <- function(area) {
  if (area == "A") {
    return(0)
  } else if (area == "B") {
    return(1)
  } else if (area == "C") {
    return(2)
  } else if (area == "D") {
    return(3)
  } else if (area == "E") {
    return(4)
  } else if (area == "F") {
    return(5)
  } else if (area == "G") {
    return(6)
  } else if (area == "H") {
    return(7)
  } else {
    return(8)
  }
}



# Apply the function to the 'Area' column to create the 'avgsalary' column
train$Area <- sapply(train$Area, numeric)

# Check the data types of each column
str(train)

# Check for missing or invalid values
summary(train)

# Remove any rows with missing or invalid values
train <- na.omit(train)

# Check the formula for correctness and completeness
formula <- as.formula("AvgSalary ~ IDpol + ClaimNb + Exposure + Area + VehPower + VehAge + DrivAge + BonusMalus ")
all.vars(formula)

# Create the neural network using the training set
set.seed(123)
nn <- neuralnet(formula, data = train, hidden = c(5,3), linear.output = FALSE)

# Check the structure and weights of the neural network
str(nn)
nn$weights

numeric <- function(area) {
  if (area == "A") {
    return(0)
  } else if (area == "B") {
    return(1)
  } else if (area == "C") {
    return(2)
  } else if (area == "D") {
    return(3)
  } else if (area == "E") {
    return(4)
  } else if (area == "F") {
    return(5)
  } else if (area == "G") {
    return(6)
  } else if (area == "H") {
    return(7)
  } else {
    return(8)
  }
}

# Apply the function to the 'Area' column to create the 'avgsalary' column
valid$Area <- sapply(valid$Area, numeric)

valid <- na.omit(valid)

# Make predictions on the validation set
predictions <- predict(nn, valid[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus")])

# Calculate the mean squared error between the predicted and actual values
mse <- mean((predictions - valid$AvgSalary)^2)

# Calculate the root mean squared error
rmse <- sqrt(mse)

# Print the root mean squared error
cat("Root Mean Squared Error:", rmse, "\n")


library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(data))
data$Variable <- as.factor(data$Variable)
newdata <- one_hot(as.data.table(data))

area_matrix <- matrix(0, nrow = nrow(df), ncol = length(valid$Area))

# Loop through the area values and set the corresponding column in the matrix to 1
for (i in 1:length(area_values)) {
  area_matrix[, i] <- as.integer(df$area == area_values[i])
}

# Bind the matrix of one-hot encoded columns to the original dataframe
df <- cbind(df, area_matrix)

# Remove the original 'area' column
df$area <- NULL

# //////////////////////////

# data <- data.frame(
#   Outcome = seq(1,100,by=1),
#   Variable = sample(c("Red","Green","Blue"), 100, replace = TRUE)
# )
# one_hot in mltools package
# library(mltools)
# library(data.table)

# newdata <- one_hot(as.data.table(data))
# data$Variable <- as.factor(data$Variable)
# newdata <- one_hot(as.data.table(data))