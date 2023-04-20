setwd("D:/research/neural_network_freMTPL2freq") 
data <- read.csv("./data/freMTPL2freq.csv")

# delete the columns
data <- subset(data, select = -c(VehGas, Density, Region))
