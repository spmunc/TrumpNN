###############################################################################
##
##  Supervised SOM to predict winning candidate
##             
##
###############################################################################

################################
# Load required package(s)
################################
require(kohonen)
require(dplyr)
require(caret)

################################
# Read In Data
################################
county_facts <- read.csv("2016_presidential_election/county_facts.csv")
primary_results <- read.csv("2016_presidential_election/primary_results.csv")


################################
# Get Data for SOM
################################
repub <- primary_results[primary_results$party == 'Republican',]
repub$trump_or_cruz <- ifelse(repub$candidate == 'Donald Trump' | repub$candidate == 'Ted Cruz', 0, 1)
repub2 <- repub %>% group_by(fips) %>% mutate(other_votes = sum(trump_or_cruz*fraction_votes))
repub2$candidate <- as.character(repub2$candidate)
repub2 <- repub2 %>% group_by(fips) %>% mutate(winner = candidate[which.max(fraction_votes)])
repub2$winner <- as.factor(repub2$winner)
som.dat <- repub2[,c('fips','winner')]
som.dat <- som.dat[!duplicated(som.dat$fips),]
som.dat <- merge(som.dat, county_facts, by = 'fips', all.x = TRUE, all.y = FALSE)
som.dat <- subset(som.dat, select = -c(area_name,state_abbreviation))

################################
# Pull out columns of interest
################################
# % over 65:"AGE775214", % black: "RHI225214", 
# % hispanic: "RHI725214", % white: "RHI825214", % foreign born: "POP645213"
# % bachelors: "EDU685213", % veteran: "VET605213", median hh income: "INC110213",
# pop per sqm: "POP060210",
columns_of_interest <- c("RHI825214","EDU685213", "INC110213")
som.input <- subset(som.dat, select = columns_of_interest)
som.input <- as.matrix(subset(som.dat, select = -c(fips,winner)))
som.input <- base::scale(som.input)
train_ind <- sample(1:nrow(som.input), 1500)

################################
# Fit supervised SOM
################################
somm <- xyf(som.input[train_ind, ], classvec2classmat((som.dat$winner)[train_ind]), grid=somgrid(16,16, 'hexagon'))

##############################################
# Predict winner of training and test set
##############################################
ssom1.predict <- predict(somm,newdata = som.input[-train_ind,],trainY = classvec2classmat((som.dat$winner)[train_ind]))
ssom2.predict <- predict(somm,newdata = som.input[train_ind,],trainY = classvec2classmat((som.dat$winner)[train_ind]))

##############################################
# Get confusion matrix of test and train set
##############################################
testConf <- confusionMatrix(ssom1.predict$prediction, som.dat$winner[-train_ind])
trainConf <- confusionMatrix(ssom2.predict$prediction, som.dat$winner[train_ind])

##############################################
# Plot the SOM
##############################################
xyfpredictions <- classmat2classvec(predict(somm)$unit.predictions)
plot(somm, main = 'SOM Input Grid with \n Corresponding Candidate')
plot(somm, type='quality', main='Mean Distance to Mapped Inputs')
plot(somm, type='changes')

