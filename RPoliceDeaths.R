library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)

policeDeaths <- read.csv("PoliceDeaths.csv", header=TRUE, sep=",")