library(caret)
library(neuralnet)

# # Split the dataset into training and validation sets
# set.seed(123)
# trainIndex <- createDataPartition(data$AvgSalary, p = 0.8, list = FALSE)
# train <- data[trainIndex, ]
# valid <- data[-trainIndex, ]

# # Check the data types of each column
# str(train)

# # Check for missing or invalid values
# summary(train)

# # Load the required libraries
# library(rstan)
# library(brms)
# library(rstanarm)

# # Define the formula for the model
# formula <- bf(AvgSalary ~ Area_A + Area_B + Area_C + Area_D + Area_E + Area_F +
#                 B12 + B6 + B3 + B2 + B5 + B10 + B14 + B13 + B4 + B1 + B11 +
#                 R82 + R22 + R72 + R31 + R91 + R52 + R93 + R11 + R24 + R94 +
#                 R83 + R54 + R26 + R53 + R73 + R42 + R25 + R21 + R41 + R43 + R74 + R23 + 
#                 BonusMalus + Density + DrivAge + VehAge + Exposure + VehPower + ClaimNb)

# # Define the glm model
# bnn_model <- stan_glm(formula, data = train, family = gaussian(),
#                       prior_intercept = normal(0, 10),
#                       prior = normal(0, 1), seed = 12345)

library(tagi)
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

# initialize weights and biases and parameters for the split between training and testing set.
# Factor for initializing bias
NN$factor4Bp = 0.01 * matrix(1L, nrow = 1, ncol = length(NN$nodes) - 1) 
# Factor for initializing weights
NN$factor4Wp = 0.25 * matrix(c(1/NN$nodes[1],1/NN$nodes[2]), nrow = 1, ncol = 2) 

trainIdx <- NULL
testIdx <- NULL

### Experiment

# Run the neural network model and collect metrics at each epoch.
out_regression <- regression(NN, x, y, trainIdx, testIdx)
mp = out_regression[[1]]
Sp = out_regression[[2]]
metric = out_regression[[3]]
time = out_regression[[4]]


