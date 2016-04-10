library(ggplot2)
library(kohonen)
library(dplyr)
source("somPlot/plotUMatrix.R")

readData <- function() {
  cf <- read.csv("2016_presidential_election/county_facts.csv")
  pr <- read.csv("2016_presidential_election/primary_results.csv")
  return (list(countyFacts=cf, primaryResults=pr))
}

runKohonen <- function(countyFacts, primaryResults) {
  cf.mat <- data.matrix(countyFacts)[c(1:nrow(countyFacts)),c(4:length(countyFacts))]
  #som() 
  som(cf.mat)
}

dat <- readData()
runKohonen(dat$countyFacts, dat$primaryResults)
