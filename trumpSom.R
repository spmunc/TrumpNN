library(ggplot2)
library(kohonen)
library(dplyr)

readData <- function() {
    cf <- read.csv("2016_presidential_election/county_facts.csv")
    pr <- read.csv("2016_presidential_election/primary_results.csv")
    return (list(countyFacts=cf, primaryResults=pr))
}