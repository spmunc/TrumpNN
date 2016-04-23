# Notes: 

# when ALL columns are used, training MSE goes to 0.0048 but testing MSE to 0.296 - OVERFIT 
# How to select only the best? 
# could look for redundant variables using regression
# significant varibles (excluding fips): 
reg_dat <- cbind(X, Y)
summary(lm(fraction_votes~ . - fips, data=reg_dat))
c("HSG495213",
  "HSG096213",
  "EDU685213",
  "PVY020213",
  "BZA110213" ,
  "NES010213",
  "LFE305213",
  "HSG445213" ,
  "SEX255214" ,
  "RHI825214",
  "AGE295214",
  "EDU635213",
  "SBO415207",
  "SBO015207",
  "POP815213",
  "VET605213")
# this was even worse - train-MSE = 0.088, test-MSE = 0.4742

library(rms)
reg_dat <- cbind(X, Y)
#names(X)[1] <- c('E')
n <- names(X)
n <- n[!(n=="fraction_votes" | n =="fips")]
r_f <- as.formula(paste("fraction_votes ~", paste(n[!n %in% "E"], collapse = " + ")))
z <- ols(r_f, data=reg_dat)
q <- fastbw(z, rule="p")
names_kept <- q$names.kept
columns_of_interest <- as.vector(q$names.kept)
> columns_of_interest
[1] "HSG495213" "HSG096213" "EDU685213" "PVY020213" "BZA010213" "BZA110213"
[7] "NES010213" "LFE305213" "HSG445213" "SEX255214" "RHI325214" "RHI625214"
[13] "RHI125214" "RHI825214" "AGE295214" "EDU635213" "SBO415207" "SBO015207"
[19] "WTN220207" "POP815213" "VET605213"
# with five hidden PE
# test rmse = 0.1327
# train rmse = 0.0839

# with 3 hidden PE: test rmse = 0.0985, train rmse = 0.0899
# with 10 hidden PE: baaaad 
# system: rmse/benchmark
# c(2,2) hidden PE: test: 0.144/0.1224, train: 0.0877/0.1449
# c(3,3) hidden PE: test: 0.1444/0.1224, train=0.0877, 0.1449
# c(4,4) hidden PE: test: 0.6729/0.1224, train=0.0812/0.1449 
# 4 hidden PE: test: 0.1435/0.1224, train=0.0878/0.1449 
# 5 hidden PE: bad bad bad 


### CHANGED NUMBER OF TESTING DATA POINTS TO 550 

#samples/(alpha*(inputs*outputs)),  2 < alpha < 10
(1881 - 550)/(2*21) # 31
(1881 - 550)/(10*21) # 6

# 7 hidden: test: .2129/.1672, train: .1981/0.1637
# 30 hidden: test: 0.2129/0.1672, train=0.1981/0.1637 
# 15 hidden: test = 0.2129/0.1672, train=0.1981/0.1637 



### RESET 


# 7 PE: train 0.1523/0.1468, test 0.1754/1390

linreg_pred <- z$fitted.values
Y_fit_train 

Xtest1 <- cbind(matrix(1, nrow=dim(Xtest)[1], ncol=1), Xtest)
test_ols_results <- Xtest1 %*% q$coefficients

Xtrain1 <- cbind(matrix(1, nrow=dim(Xtrain)[1], ncol=1), Xtrain)
train_ols_results <- Xtrain1 %*% q$coefficients

plot(Ytest, test_ols_results, ylim=c(.5,1))
points(Ytest, Y_fit_test, col="blue")


plot(Ytest) 
points(test_ols_results, col="blue")
points(Y_fit_test,col="green")
legend("bottom", c("Target", "Linear Regression Fitted Data", "Backpropagation Fitted Data"), 
       pch=c(1,1,1),col=c("black", "blue", "green"), cex=0.4,horiz=TRUE)


error_lr <- Ytest - test_ols_results
sqrt(mean(error_lr^2))
sqrt(mean((Ytrain - train_ols_results)^2))
error_bp <- Ytest - Y_fit_test

plot(error_lr, col="blue",ylab="Error from Target Values", main="Error: Target - Predicted Values")
points(error_bp, col="green")
abline(h=mean(error_lr), col="red", lwd=5)
abline(h=mean(error_bp), col="red", lty=2, lwd=5)
legend("bottom", c("Linear Regression Fitted Data", "Backpropagation Fitted Data", 
                   "Linear Regression Mean Error", "Backpropagation Mean Error"), 
       pch=c(1,1, NA, NA), 
       lty=c(0,0,1,2),col=c("blue", "green", "red", "red"), cex=0.8, ncol=2)