## robert's version

# git status - check robert
# git add .
# git commit -m "i did things"
# git push origin robert





###############################################################################
##
##  trumpNN.R: fits a neural network with one hidden layer to predict
##             the fraction of the votes in each county in NH, IA, SC, NV
##             that Donald Trump recieved based on demographics
## things
##
###############################################################################

################################
# Load required package(s) 
################################
require(neuralnet)

################################
# Read In Data
################################
county_facts <- read.csv("2016_presidential_election/county_facts.csv")
county_facts_dictionary <- read.csv("2016_presidential_election/county_facts_dictionary.csv")
primary_results <- read.csv("2016_presidential_election/primary_results.csv")
trump <- primary_results[primary_results$candidate == 'Donald Trump',]
trump2 <- as.data.frame(cbind(trump$fips,trump$fraction_votes))
names(trump2) <- c('fips','fraction_votes')
trump_county_facts <- merge(trump2, county_facts, by = 'fips', all.x = TRUE, all.y = FALSE)
trump_county_facts <- subset(trump_county_facts, select = -c(area_name,state_abbreviation))

################################
# Pull out columns of interest
################################
# % over 65:"AGE775214", % black: "RHI225214", 
# % hispanic: "RHI725214", % white: "RHI825214", % foreign born: "POP645213"
# % bachelors: "EDU685213", % veteran: "VET605213", median hh income: "INC110213",
# pop per sqm: "POP060210",
columns_of_interest <- c("AGE775214", "SEX255214", "EDU685213")# "RHI225214", "RHI725214",  "POP645213", "EDU685213", "VET605213", "INC110213", "RHI625214", "HSG445213", "PVY020213", "POP060210")
columns_of_interest <- c("HSG495213", "HSG096213", "EDU685213", "PVY020213", "BZA010213", "BZA110213", "NES010213", "LFE305213", "HSG445213", "SEX255214", "RHI325214", "RHI625214", "RHI125214", "RHI825214", "AGE295214", "EDU635213", "SBO415207", "SBO015207", "WTN220207", "POP815213", "VET605213")
columns_of_interest <- colnames(trump_county_facts[c(-1,-2)])
X <- trump_county_facts[,columns_of_interest]
Y <- as.data.frame(trump_county_facts[,2])
names(Y) <- c("fraction_votes")

################################
#  Fit Neural Net
################################
# Get indices for test set and split data
test_inds <- sample(1:nrow(X),round(0.25*nrow(X))) 
Xtrain <- X[-test_inds,]
Xtest <- X[test_inds,]
Ytrain <- Y[-test_inds,]
Ytest <- Y[test_inds,]

# Scale Xtrain, Xtest and Ytrain, Ytest by Xtrain and Ytrain ([0,1] scaling)
scale <- function(vector_to_scale, goal_min, goal_max){ 
  new_vector <- (vector_to_scale - min(vector_to_scale))*(goal_max - goal_min)/(max(vector_to_scale)-min(vector_to_scale)) + goal_min
  return(new_vector)
}
Xtrain <- apply(Xtrain, 2, function(x){scale(x, -0.85, 0.85)})
Xtest <- apply(Xtest, 2, function(x){scale(x, -0.85, 0.85)})
Ytrain <- scale(Ytrain, -0.85, 0.85)
Ytest <- scale(Ytest, -0.85, 0.85)

# Actually fit the neural net. Have to do weird thing with formula because it doesn accept "~."
dat <- as.data.frame(cbind(Ytrain,Xtrain))
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+07)


################################
#  Evaluate Results
################################
# predict the values using training and test set
Y_fit_train <- neuralnet::compute(mod, Xtrain)$net.result
Y_fit_test <- neuralnet::compute(mod, Xtest)$net.result

# MSE in scaled space
mse_train <- sqrt(mean((Y_fit_train - Ytrain)^2))
mse_test <- sqrt(mean((Y_fit_test - Ytest)^2))

# MSE of just choosing the mean (benchmark to beat)
benchmark_train <- sqrt(mean((Ytrain - mean(Ytrain))^2))
benchmark_test <- sqrt(mean((Ytest - mean(Ytest))^2))

# For Linear
Y_fit_train_lm <- predict(lm(f, data = dat), newdata=data.frame(Xtrain))
Y_fit_test_lm <- predict(lm(f, data = dat), newdata=data.frame(Xtest))

mse_lm_train <- sqrt(mean((Y_fit_train_lm - Ytrain)^2)) 
mse_lm_test <- sqrt(mean((Y_fit_test_lm - Ytest)^2)) 

sprintf("Training RMSE: %.4f", mse_train)
sprintf('Training Benchmark: %.4f', benchmark_train)
sprintf("Testing RMSE: %.4f", mse_test)
sprintf('Testing Benchmark: %.4f', benchmark_test)
sprintf('Testing LM Benchmark: %.4f', mse_lm_test)

# plot(Ytrain, Y_fit_train)
# plot(Ytest, Y_fit_test)

## Results
# With c("AGE775214", "SEX255214", "EDU685213", "RHI225214", "RHI725214",  "POP645213", "EDU685213", "VET605213", "INC110213") we get:
# "Training RMSE: 0.1555"
# "Training Benchmark: 0.2463"
# "Testing RMSE: 0.4992"
# "Testing Benchmark: 0.4733"

# With all
# "Training RMSE: 0.1177"
# "Training Benchmark: 0.2455"
# "Testing RMSE: 1.5733"
# "Testing Benchmark: 0.3393"

# With c("AGE775214", "SEX255214", "EDU685213", "RHI225214", "RHI725214",  "POP645213", "EDU685213", "VET605213", "INC110213", "RHI625214", "HSG445213", "PVY020213", "POP060210") and 1 hidden PE
# "Training RMSE: 0.1561"
# "Training Benchmark: 0.2461"
# "Testing RMSE: 0.3863"
# "Testing Benchmark: 0.4515"




################################
#  Looking at States in Order
################################
library(RColorBrewer)
library(reshape2)
library(plyr)
library(ggplot2)
february <- c('IA','NH','SC','NV')
feb_dates <- c(1,9,20,23)
march <- c('MN','AL','AR','GA','MA','OK','TN','TX','VT','VA','KA','KY','LA','ME','HI','MI','ID','MS','DC','WY','IL','NC','OH','FL','MO','AZ','UT')
mar_dates <- c(1,1,1,1,1,1,1,1,1,1,5,5,5,5,8,8,8,8,12,12,15,15,15,15,15,22,22)
order_of_states <- c(february, march)

# Republicans only
republicans <- primary_results[which(primary_results$party=='Republican'),]

# Winner by state
by_state_and_candidate <- aggregate(republicans$votes, by=list(State=republicans$state_abbreviation, Candidate=republicans$candidate), FUN = sum)
total_votes <- aggregate(by_state_and_candidate$x, by=list(by_state_and_candidate$State), FUN=sum)
by_state_and_candidate <- merge(by_state_and_candidate, total_votes, by.x='State', by.y='Group.1')
by_state_and_candidate$fraction <- by_state_and_candidate$x.x/by_state_and_candidate$x.y

order2 <- intersect(order_of_states, unique(republicans$state_abbreviation))

by2 <- by_state_and_candidate[with(by_state_and_candidate, order(Candidate, match(State, order2))), ]

by2$State <- with(by2, factor(State, levels=order2))

ggplot(data=by2, aes(x=State, y=fraction, group=Candidate)) + 
  geom_line(aes(color=Candidate)) +
  xlab('State (in order)') +
  ylab('Proportion') 









################################
#  Using Previous States to Predict Ahead
################################
february <- c('IA','NH','SC','NV')
feb_dates <- c(1,9,20,23)
# Break after 1, 9, 23
march <- c('MN','AL','AR','GA','MA','OK','TN','TX','VT','VA','KA','KY','LA','ME','HI','MI','ID','MS','DC','WY','IL','NC','OH','FL','MO','AZ','UT')
mar_dates <- c(1,1,1,1,1,1,1,1,1,1,5,5,5,5,8,8,8,8,12,12,15,15,15,15,15,22,22)
# Break after 1, 8, 15, 22


# Subset the data based on the above
trump_county_facts_all <- merge(trump2, county_facts, by = 'fips', all.x = TRUE, all.y = FALSE)

feb_1 <- subset(trump_county_facts_all, state_abbreviation=='IA')
feb_9 <- subset(trump_county_facts_all, state_abbreviation %in% c('IA', 'NH'))
feb_23 <- subset(trump_county_facts_all, state_abbreviation %in% february)
mar_1 <- subset(trump_county_facts_all, state_abbreviation %in% c(february, march[mar_dates<=1]))
mar_8 <- subset(trump_county_facts_all, state_abbreviation %in% c(february, march[mar_dates<=8]))
mar_15 <- subset(trump_county_facts_all, state_abbreviation %in% c(february, march[mar_dates<=15]))
mar_22 <- subset(trump_county_facts_all, state_abbreviation %in% c(february, march[mar_dates<=22]))


## ---- Feb 1 to predict Feb 9 ----
dat <- subset(feb_1, select=-c(fips,state_abbreviation,area_name,SBO115207))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- feb_9[(nrow(feb_1)+1):nrow(feb_9), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
feb_9_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(feb_9_predict, max(feb_1$fraction_votes), min(feb_1$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)


## ---- Feb 9 to predict Feb 20,23 ----
dat <- cbind(subset(feb_9, select=-c(fips,state_abbreviation,area_name,SBO115207)))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- feb_23[(nrow(feb_9)+1):nrow(feb_23), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
feb_23_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(feb_23_predict, max(feb_9$fraction_votes), min(feb_9$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)


## ---- Feb 23 to predict Mar 1 ----
dat <- cbind(subset(feb_23, select=-c(fips,state_abbreviation,area_name,SBO115207)))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- mar_1[(nrow(feb_23)+1):nrow(mar_1), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
mar_1_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(mar_1_predict, max(feb_23$fraction_votes), min(feb_23$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)


## ---- Mar 1 to predict Mar 8 ----
dat <- cbind(subset(mar_1, select=-c(fips,state_abbreviation,area_name,SBO115207)))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- mar_8[(nrow(mar_1)+1):nrow(mar_8), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
mar_8_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(mar_8_predict, max(mar_1$fraction_votes), min(mar_1$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)


## ---- Mar 8 to predict Mar 15 ----
dat <- cbind(subset(mar_8, select=-c(fips,state_abbreviation,area_name,SBO115207)))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- mar_15[(nrow(mar_8)+1):nrow(mar_15), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
mar_15_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(mar_15_predict, max(mar_1$fraction_votes), min(mar_1$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)


## ---- Mar 15 to predict Mar 22 ----
dat <- cbind(subset(mar_15, select=-c(fips,state_abbreviation,area_name,SBO115207)))
dat <- apply(dat, 2, function(x){scale(x, -0.85, 0.85)})
dat <- as.data.frame(dat)
names(dat)[1] <- c('D')
n <- names(dat)
f <- as.formula(paste("D ~", paste(n[!n %in% "D"], collapse = " + ")))
mod <- neuralnet(f, hidden = 10, data = dat, act.fct = "tanh", stepmax = 1e+06)

what_to_predict_on <- mar_22[(nrow(mar_15)+1):nrow(mar_22), ]
what_to_predict_on2 <- subset(what_to_predict_on, select=-c(fraction_votes, fips, state_abbreviation, area_name,SBO115207))
what_to_predict_on2 <- apply(what_to_predict_on2, 2, function(x){scale(x, -0.85, 0.85)})
what_to_predict_on2 <- as.data.frame(what_to_predict_on2)
mar_22_predict <- neuralnet::compute(mod, what_to_predict_on2)$net.result
scaled_predict <- scale(mar_22_predict, max(mar_1$fraction_votes), min(mar_1$fraction_votes))
plot(what_to_predict_on$fraction_votes, scaled_predict)







