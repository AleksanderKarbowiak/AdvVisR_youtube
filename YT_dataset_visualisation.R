library(dplyr)
library(readxl)

GYT <- read.csv("Global_YouTube_Statistics.csv",header=TRUE, sep=',', dec='.')

str(GYT)

table(GYT$category)
