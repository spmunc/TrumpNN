## robert's version





###############################################################################
##
##  trumpNN.R: fits a neural network with one hidden layer to predict
##             the fraction of the votes in each county in NH, IA, SC, NV
##             that Donald Trump recieved based on demographics
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
columns_of_interest <- c("AGE775214", "SEX255214", "EDU685213", "RHI225214", "RHI725214",  "POP645213", "EDU685213", "VET605213", "INC110213")
X <- trump_county_facts[,columns_of_interest]
Y <- as.data.frame(trump_county_facts[,2])
names(Y) <- c("fraction_votes")

################################
#  Fit Neural Net
################################
# Get indices for test set and split data
test_inds <- sample(1:nrow(X),30) 
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
mod <- neuralnet(f, data = dat, hidden = 10, act.fct = "tanh", stepmax = 1e+07)


################################
#  Evaluate Results
################################
# predict the values using training and test set
Y_fit_train <- compute(mod, Xtrain)$net.result
Y_fit_test <- compute(mod, Xtest)$net.result

# MSE in scaled space
mse_train <- sqrt(mean((Y_fit_train - Ytrain)^2))
mse_test <- sqrt(mean((Y_fit_test - Ytest)^2))

# MSE of just choosing the mean (benchmark to beat)
benchmark_train <- sqrt(mean((Ytrain - mean(Ytrain))^2))
benchmark_test <- sqrt(mean((Ytest - mean(Ytest))^2))

sprintf("Training RMSE: %.4f", mse_train)
sprintf('... Training Benchmark: %.4f', benchmark_train)
sprintf("Testing RMSE: %.4f", mse_test)
sprintf('... Testing Benchmark: %.4f', benchmark_test)

plot(Ytrain, Y_fit_train)
plot(Ytest, Y_fit_test)

################################
#  Looking at States in Order
################################
library(RColorBrewer)
library(reshape2)
library(plyr)
library(ggplot2)
february <- c('IA','NH','SC','NV')
march <- c('MN','AL','AR','GA','MA','OK','TN','TX','VT','VA','KA','KY','LA','ME','HI','MI','ID','MS','DC','WY','IL','NC','OH','FL','MO','AZ','UT')
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


