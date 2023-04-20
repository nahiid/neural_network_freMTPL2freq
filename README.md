# neural_network_freMTPL2freq
This project uses a neural network to predict the average salary of car insurance policyholders in France. (freMTPL2freq in CASdatasets (manipulated!))

The dataset used is freMTPL2freq.csv, which contains information about the characteristics of the policies and the frequency of claims. 
The goal is to use this information to predict the average salary of the policyholders, which can be useful for pricing and risk management purposes.

Getting Started
Dependencies
This project requires the following dependencies:

R version 3.6.3 or higher
caret package version 6.0-85 or higher
neuralnet package version 1.44.2 or higher

Installing
To install the dependencies, run the following commands in R:

install.packages("caret")
install.packages("neuralnet")

Data
The FreMTPL2freq dataset can be found in the ./data directory. The dataset contains the following columns:

IDpol: policy identifier
ClaimNb: number of claims
Exposure: total exposure of the policy
Area: policy area (A-H)
VehPower: power of the insured vehicle
VehAge: age of the insured vehicle
DrivAge: age of the driver
BonusMalus: bonus-malus level
VehBrand: brand of the insured vehicle
AvgSalary: average salary of the policy area (we added this column to the original dataset)


To Be Continued...
