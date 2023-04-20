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
write.csv(data, "freMTPL2freq_updated.csv", row.names = FALSE)

