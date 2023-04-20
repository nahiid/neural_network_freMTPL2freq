setwd("D:/research/neural_network_freMTPL2freq") 
data <- read.csv("./data/freMTPL2freq.csv")

# delete the columns
data <- subset(data, select = -c(VehGas, Density, Region))

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
preProcValues <- preProcess(train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")], method = c("center", "scale"))

train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")] <- predict(preProcValues, train[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")])
valid[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")] <- predict(preProcValues, valid[,c("IDpol","ClaimNb","Exposure","Area","VehPower","VehAge","DrivAge","BonusMalus","VehBrand","AvgSalary")])

# Check the data types of each column
str(train)

# Check for missing or invalid values
summary(train)

# Convert any non-numeric columns to numeric
train$Area <- as.numeric(train$Area)
train$VehBrand <- as.numeric(train$VehBrand)

# Remove any rows with missing or invalid values
train <- na.omit(train)

# Check the formula for correctness and completeness
formula <- as.formula("AvgSalary ~ IDpol + ClaimNb + Exposure + Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand")
all.vars(formula)

# Create the neural network using the training set
set.seed(123)
nn <- neuralnet(formula, data = train, hidden = c(5,3), linear.output = FALSE)

# Check the structure and weights of the neural network
str(nn)
nn$weights


