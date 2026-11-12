# Road Fatality ML Project

A machine learning project that predicts whether a road accident in Victoria, Australia is likely to be fatal.

## About the Project

Road crashes are a leading cause of injury and death, yet the vast majority of recorded accidents are not fatal. This project uses a dataset of 200,000 Victorian accident records, of which 3,387 resulted in a fatality, to understand what factors are linked to deadly crashes and to predict which people and situations carry the highest risk.

The dataset describes each accident in terms of the people involved (age, gender, helmet or seatbelt use, number of occupants), the vehicle, the time and day, and the road and weather conditions at the scene.

The project begins with exploratory analysis, comparing fatal accidents against all accidents to see how they differ. It then builds a logistic regression model to estimate the probability that an accident is fatal, and uses Ridge and Lasso regression to identify which factors matter most. Finally, the model is applied to a separate set of 10,000 records, which are ranked by predicted risk to pick out the 2,500 most likely to be involved in a fatal accident.

## What's in This Repository

| File | Description |
|---|---|
| `Road Fatality Project.R` | The full analysis, written in R |
| `VicRoadFatalData.csv` | Accident records used to train the model |
| `Drivers_Eval.csv` | New records used to generate predictions |

## Running the Project

The analysis is written in R and uses the `dplyr`, `ggplot2`, `ISLR2`, `boot`, `glmnet` and `caret` packages.

```r
install.packages(c("dplyr", "ggplot2", "ISLR2", "boot", "glmnet", "caret"))

library(caret)

VicRoadFatalData <- read.csv("VicRoadFatalData.csv")
Drivers_Eval     <- read.csv("Drivers_Eval.csv")

source("Road Fatality Project.R")
```
