require(neuralnet)
require(rms)

county_facts <- read.csv("2016_presidential_election/county_facts.csv")
county_facts_dictionary <- read.csv("2016_presidential_election/county_facts_dictionary.csv")
primary_results <- read.csv("2016_presidential_election/primary_results.csv")
trump <- primary_results[primary_results$candidate == 'Donald Trump',]
trump2 <- as.data.frame(cbind(trump$fips,trump$fraction_votes))
names(trump2) <- c('fips','fraction_votes')
trump_county_facts <- merge(trump2, county_facts, by = 'fips', all.x = TRUE, all.y = FALSE)
trump_county_facts <- subset(trump_county_facts, select = -c(area_name,state_abbreviation))


# all columns used 
all_col <- county_facts_dictionary$column_name
remove_fips_fracvotes <- trump_county_facts[,-c(1,2)] # get rid of fips and fraction_votes
X <- remove_fips_fracvotes[,all_col]
Y <- as.data.frame(trump_county_facts[,2])
names(Y) <- c("fraction_votes")


# Get indices for test set and split data
test_inds <- sample(1:1881,550) # orig = 30
Xtrain <- X[-test_inds,]
Xtest <- X[test_inds,]
Ytrain <- Y[-test_inds,]
Ytest <- Y[test_inds,]

N <- 4

# Scale Xtrain, Xtest and Ytrain, Ytest by Xtrain and Ytrain ([0,1] scaling)
maxs <- apply(Xtrain, 2, function(x) max(x, na.rm = TRUE))
mins <- apply(Xtrain, 2, function(x) min(x, na.rm = TRUE))
Xtrain <- apply(Xtrain,2,function(x){(x-min(x))/(max(x)-min(x))})
Xtest <- apply(Xtest,2,function(x){(x-min(x))/(max(x)-min(x))})
maxy <- max(Ytrain)
miny <- min(Ytrain)
Ytrain <- ((Ytrain - miny)/(maxy-miny))
Ytest <- ((Ytest - miny)/(maxy-miny))

# Actually fit the neural net. Have to do weird thing with formula because it doesn accept "~."
dat_all <- as.data.frame(cbind(Ytrain,Xtrain))
names(dat_all)[1] <- c('D')
n_all <- names(dat_all)
f_all <- as.formula(paste("D ~", paste(n_all[!n_all %in% "D"], collapse = " + ")))
rm(mod_all)
mod_all <- neuralnet(f_all, data = dat_all, hidden = N, act.fct = "tanh", stepmax = 1e+06)


#plot(mod)

################################
#  Evaluate Results
################################
# predict the values using training and test set
Y_fit_train_all <- neuralnet::compute(mod_all, Xtrain)$net.result
Y_fit_test_all <- neuralnet::compute(mod_all, Xtest)$net.result

# MSE in scaled space
mse_train_all <- sqrt(mean((Y_fit_train_all - Ytrain)^2))
mse_test_all <- sqrt(mean((Y_fit_test_all - Ytest)^2))

# MSE of just choosing the mean (benchmark to beat)
benchmark_train_all <- sqrt(mean((Ytrain - mean(Ytrain))^2))
benchmark_test_all <- sqrt(mean((Ytest - mean(Ytest))^2))

sprintf("Training RMSE: %.4f", mse_train_all)
sprintf('... Training Benchmark: %.4f', benchmark_train_all)
sprintf("Testing RMSE: %.4f", mse_test_all)
sprintf('... Testing Benchmark: %.4f', benchmark_test_all)

## linear regression using ALL variables 

z <- ols(f_all, data=dat_all)
lr_train_all <- cbind(matrix(1, nrow=dim(Xtrain)[1],ncol=1), Xtrain) %*% z$coefficients 
lr_test_all <- cbind(matrix(1, nrow=dim(Xtest)[1],ncol=1), Xtest) %*% z$coefficients

rmse_lr_train_all <- sqrt(mean((lr_train_all - Ytrain)^2))
rmse_lr_test_all <- sqrt(mean((lr_test_all - Ytest)^2))

sprintf("Training RMSE: %.4f", mse_train_all)
sprintf('... Training Benchmark: %.4f', benchmark_train_all)
sprintf('... Linear Regression Training RMSE: %.4f', rmse_lr_train_all)
sprintf("Testing RMSE: %.4f", mse_test_all)
sprintf('... Testing Benchmark: %.4f', benchmark_test_all)
sprintf('... Linear Regression Testing RMSE: %.4f', rmse_lr_test_all)

# find most significant variables 

q <- fastbw(z, rule="p")
names_kept <- q$names.kept
print(names_kept)
columns_of_interest <- as.vector(q$names.kept)

# neural network with only those variables

Xtrain2 <- Xtrain[,columns_of_interest]
Ytrain2 <- Ytrain
Xtest2 <- Xtest[,columns_of_interest]
Ytest2 <- Ytest


# Actually fit the neural net. Have to do weird thing with formula because it doesn accept "~."
dat2 <- as.data.frame(cbind(Ytrain2,Xtrain2))
names(dat2)[1] <- c('D')
n2 <- names(dat2)
f2 <- as.formula(paste("D ~", paste(n2[!n2 %in% "D"], collapse = " + ")))
rm(mod2)
mod2 <- neuralnet(f2, data = dat2, hidden = N, act.fct = "tanh", stepmax = 1e+06)


#  Evaluate Results

# predict the values using training and test set
Y_fit_train2 <- neuralnet::compute(mod2, Xtrain2)$net.result
Y_fit_test2 <- neuralnet::compute(mod2, Xtest2)$net.result

# MSE in scaled space
mse_train2 <- sqrt(mean((Y_fit_train2 - Ytrain)^2))
mse_test2 <- sqrt(mean((Y_fit_test2 - Ytest)^2))

# MSE of just choosing the mean (benchmark to beat)
benchmark_train2 <- sqrt(mean((Ytrain - mean(Ytrain))^2))
benchmark_test2 <- sqrt(mean((Ytest - mean(Ytest))^2))

z2 <- ols(f2, data=dat2)
lr_train2 <- cbind(matrix(1, nrow=dim(Xtrain2)[1],ncol=1), Xtrain2) %*% z2$coefficients 
lr_test2 <- cbind(matrix(1, nrow=dim(Xtest2)[1],ncol=1), Xtest2) %*% z2$coefficients

rmse_lr_train2 <- sqrt(mean((lr_train2 - Ytrain)^2))
rmse_lr_test2 <- sqrt(mean((lr_test2 - Ytest)^2))

sprintf("Training RMSE: %.4f", mse_train2)
sprintf('... Training Benchmark: %.4f', benchmark_train2)
sprintf('... Linear Regression Training RMSE: %.4f', rmse_lr_train2)
sprintf("Testing RMSE: %.4f", mse_test2)
sprintf('... Testing Benchmark: %.4f', benchmark_test2)
sprintf('... Linear Regression Testing RMSE: %.4f', rmse_lr_test2)


# Print ALL RMSE: 
print("ALL")
sprintf("Training RMSE: %.4f", mse_train_all)
sprintf('... Training Benchmark: %.4f', benchmark_train_all)
sprintf('... Linear Regression Training RMSE: %.4f', rmse_lr_train_all)
sprintf("Testing RMSE: %.4f", mse_test_all)
sprintf('... Testing Benchmark: %.4f', benchmark_test_all)
sprintf('... Linear Regression Testing RMSE: %.4f', rmse_lr_test_all)

print("SOME")
sprintf("Training RMSE: %.4f", mse_train2)
sprintf('... Training Benchmark: %.4f', benchmark_train2)
sprintf('... Linear Regression Training RMSE: %.4f', rmse_lr_train2)
sprintf("Testing RMSE: %.4f", mse_test2)
sprintf('... Testing Benchmark: %.4f', benchmark_test2)
sprintf('... Linear Regression Testing RMSE: %.4f', rmse_lr_test2)

plot(rbind((Y_fit_test2 - Ytest2),(Y_fit_train2 - Ytrain)), 
     ylim=c(-.1,.1), ylab="Error", main="Error: Target - Predicted Values", col="green")
points(rbind((lr_test2 - Ytest2),(lr_train2 - Ytrain)), col="blue")
abline(h=mean(rbind((Y_fit_test2 - Ytest2),(Y_fit_train2 - Ytrain))), col="red", lwd=5)
abline(h=mean(rbind((lr_test2 - Ytest2),(lr_train2 - Ytrain))), col="red", lty=2, lwd=5)
legend("bottom", c("Linear Regression Fitted Data Error", "Backpropagation Fitted Data Error", 
                   "Linear Regression Mean Error", "Backpropagation Mean Error"), 
       pch=c(1,1, NA, NA), 
       lty=c(0,0,2,1),col=c("blue", "green", "red", "red"), cex=0.8, ncol=2)
# 
# data_that_works <- as.list(c(test_inds, dat_all, dat2))
# mod_that_works <- as.list(c(mod_all, mod2))
# save(data_that_works, mod_that_works, file="yay.RData")
