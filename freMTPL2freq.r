# Load the updated dataset
data <- read.csv("./data/freMTPL2freq_updated.csv")
head(data)

library(tagi)
# Package loading:
require(mvtnorm)
require(randtoolbox)

#set seed
set.seed(123)

# Specific Initialization
# Define the neural network properties, such as the number of epochs, activation function, etc.
nobs <- nrow(data)
ratio <- 0.9
# Input features
x <- data[,!(names(data) %in% c("AvgSalary"))]
# Output targets
# Assuming 'data' is your dataset with the AvgSalary column
y <- as.matrix(data$AvgSalary)
nx <- ncol(x)
ny <- 1

x <- sapply(x, as.numeric)
library(dplyr)

NN <- list(
  "nx" = nx, # Number of input covariates
  "ny" = ny, # Number of output responses
  "batchSizeList" = c(1, 1, 1), # Batch size [train, val, test]
  "nodes" = c(nx, 30, ny), # Number of nodes for each layer
  "sx" = NULL, # Input standard deviation
  "sv" = 0.32, # Observations standard deviation
  "maxEpoch" = 10, # maximal number of learning epoch
  "hiddenLayerActivation" = "relu", # Activation function for hidden layer {'tanh','sigm','cdf','relu','softplus'}
  "outputActivation" = "linear", # Activation function for hidden layer {'linear', 'tanh','sigm','cdf','relu'}
  "ratio" = 0.8, # Ratio between training set and validation set
  "numSplits" = 20, # Number of splits
  "task" = "regression" # Task regression or classification
)

# initialize weights and biases and parameters for the split between training and testing set.
# Factor for initializing bias
NN$factor4Bp = 0.01 * matrix(1L, nrow = 1, ncol = length(NN$nodes) - 1) 
# Factor for initializing weights
NN$factor4Wp = 0.25 * matrix(1L, nrow = 1, ncol = length(NN$nodes) - 1)

trainIdx <- NULL
testIdx <- NULL

### Experiment

# Run the neural network model and collect metrics at each epoch.
out_regression <- tagi::regression(NN, x, y, trainIdx, testIdx)
mp = out_regression[[1]]
Sp = out_regression[[2]]
metric = out_regression[[3]]
time = out_regression[[4]]

