require(plyr)
require(dplyr)
require(tidyr)
require(stringr)
require(compositions)
require(neuralnet)

## Wrangle data to the correct format
county.facts <- as.tbl(
  read.csv("2016_presidential_election/county_facts.csv"))
county.facts.dictionary <- as.tbl(
  read.csv("2016_presidential_election/county_facts_dictionary.csv"))
primary.results <- as.tbl(
  read.csv("2016_presidential_election/primary_results.csv"))

# Narrow down to candidates of interest
candidates.interest <- 
  factor(c("Donald Trump", "Ted Cruz", "Marco Rubio", "John Kasich", "Jeb Bush"))
candidate.data.results <- primary.results %>%
  mutate(candidate=as.character(candidate)) %>% #it's read in as a factor
  filter(candidate %in% candidates.interest) %>% 
  mutate(candidate.num=sapply(candidate, function(x) which(x==candidates.interest))) %>%
  mutate(candidate.num=sapply(candidate.num-1, function(x) {
    p <- binary(x) %>% str_pad(3, pad="0")
    sprintf("%s,%s,%s", substr(p, 1, 1), substr(p, 2, 2), substr(p, 3, 3))
  })) %>%
  left_join(primary.results %>% 
              select(candidate, fraction_votes, fips) %>% 
              filter(candidate=="Donald Trump"), by=c("fips")) %>%
  select(fips, candidate.num, fraction_votes.y) %>%
  rename(candidate=candidate.num, trump.percent=fraction_votes.y) %>%
  separate(candidate, c("y1", "y2", "y3"), sep=",") %>%
  mutate(y1=strtoi(y1), y2=strtoi(y2), y3=strtoi(y3))

candidate.county.results <- county.facts %>% select(-area_name, -state_abbreviation)
combined.df <- candidate.data.results %>% 
  left_join(candidate.county.results, by=c("fips")) %>%
    select(-fips) %>% select(trump.percent,AGE775214, SEX255214, EDU685213, RHI225214, RHI725214,  POP645213, EDU685213, VET605213, INC110213, RHI625214, HSG445213, PVY020213, POP060210)


scale <- function(v, goal.min, goal.max) { 
  return((v - min(v))*(goal.max - goal.min)/(max(v)-min(v)) + goal.min)
}
scaled.df <- data.frame(apply(combined.df, 2, function(x) scale(x, 0, 1)))
X.mat <- scaled.df %>% select(-trump.percent)
Y.mat <- scaled.df %>% select(trump.percent)
## Create a neural network for the data.
set.seed(58)
prop <- .8
train.indices <- sample(1:nrow(X.mat), size = floor(nrow(X.mat)*prop))
X.train <- X.mat[train.indices,]
X.test <- X.mat[-train.indices,]
Y.train <- Y.mat[train.indices,]
Y.test <- Y.mat[-train.indices,]
scaled.train <- scaled.df[train.indices,]
scaled.test <- scaled.df[-train.indices,]
#all.form <- as.formula(paste("trump.percent + y1 + y2 + y3 ~ ", paste(colnames(X.mat), collapse="+")))
percent.form <- as.formula(paste("trump.percent ~ ", paste(colnames(X.mat), collapse="+")))

model <- neuralnet(percent.form, scaled.train, hidden=4, rep=1, act.fct = "tanh", lifesign = "full", stepmax = 1e+08)

Y.fit.train <- neuralnet::compute(model, X.train)$net.result
Y.fit.test <- neuralnet::compute(model, X.test)$net.result

Y.fit.train.scaled <- apply(Y.fit.train, 2, function(x) scale(x, 0, 1))
Y.fit.train.scaled[,c(1,2,3)] <- apply(Y.fit.train.scaled[,c(1,2,3)], 2, function(x) round(x))

# MSE in scaled space
mse.train <- sqrt(mean((Y.fit.train - Y.train)^2))
mse.test <- sqrt(mean((Y.fit.test - Y.test)^2))

benchmark.train <- sqrt(mean((Y.train - mean(Y.train))^2))
benchmark.test <- sqrt(mean((Y.test - mean(Y.test))^2))

train.lm <- lm(percent.form, scaled.train)
Y.fit.train.lm <- predict(train.lm, X.train)
Y.fit.test.lm <- predict(train.lm, X.test)
mse.train.lm <- sqrt(mean((Y.fit.train.lm - Y.train)^2))
mse.test.lm <- sqrt(mean((Y.fit.test.lm - Y.test)^2))

paste("mse train: ", mse.train)
paste("mse test: ", mse.test)
paste("benchmark train: ", benchmark.train)
paste("benchmark test: ", benchmark.test)
paste("lm train: ", mse.train.lm)
paste("lm test: ", mse.test.lm)

all.models <- lapply(c("4-neurons.RData", "8-neurons.RData", "16-neurons.RData"),
                     function(x) { 
                       load(x)
                       model
                      })
all.train.y <- lapply(all.models, function(m) neuralnet::compute(m, X.train)$net.result) %>%
  data.frame() %>% apply(1, mean)
all.test.y <- lapply(all.models, function(m) neuralnet::compute(m, X.test)$net.result) %>%
  data.frame() %>% apply(1, mean)
mse.train.avg <- sqrt(mean((all.train.y - Y.train)^2))
mse.test.avg <- sqrt(mean((all.test.y - Y.test)^2))

# Explore candidate effects
candidates.interest <- 
  factor(c("Donald Trump", "Ted Cruz", "Marco Rubio", "Ben Carson", "John Kasich", "Jeb Bush"))
candidate.result.indicator <- primary.results %>%
  mutate(candidate=as.character(candidate)) %>% #it's read in as a factor
  filter(candidate %in% candidates.interest) %>% 
  mutate(candidate.num=sapply(candidate, function(x) which(x==candidates.interest))) %>%
  mutate(candidate.code=sapply(candidate.num,
                               function(x) {
                                  m <- rep("0", length(candidates.interest)) 
                                  m[x] <- "1"
                                  paste(m, collapse=",")
                               })) %>%
  separate(candidate.code, c("x1", "x2", "x3", "x4", "x5", "x6"), sep=",", convert=TRUE) %>%

candidate.code.by.county <- candidate.result.indicator %>%
  ddply(.(fips), summarize, x1=sum(x1), x2=sum(x2), x3=sum(x3), x4=sum(x4), x5=sum(x5), x6=sum(x6))

candidate.results.trump <- candidate.result.indicator %>%
  left_join(candidate.code.by.county, by=c("fips")) %>%
  select(-x1.x, -x2.x, -x3.x, -x4.x, -x5.x, -x6.x) %>%
  left_join(primary.results %>% 
              select(candidate, fraction_votes, fips) %>% 
              filter(candidate=="Donald Trump"), by=c("fips")) %>%
  dplyr::rename(trump.percent=fraction_votes.y, x1=x1.y, x2=x2.y, x3=x3.y, x4=x4.y, x5=x5.y, x6=x6.y)


combined.df.indicator <- candidate.results.trump %>% 
  left_join(candidate.county.results, by=c("fips")) %>%
    select(-fips) %>% select(trump.percent, x1, x2, x3, x4, x5, x6, AGE775214, SEX255214, EDU685213, RHI225214, RHI725214,  POP645213, EDU685213, VET605213, INC110213, RHI625214, HSG445213, PVY020213, POP060210)

scaled.df.indicator.temp <- data.frame(apply(combined.df.indicator %>% select(-x1,-x2,-x3, -x4, -x5, -x6), 2, function(x) scale(x, 0, 1)))
scaled.df.indicator <- cbind(scaled.df.indicator.temp, combined.df.indicator %>% select(x1,x2,x3, x4, x5, x6))
X.mat <- scaled.df.indicator %>% select(-trump.percent)
Y.mat <- scaled.df.indicator %>% select(trump.percent)
## Create a neural network for the data.
set.seed(58)
prop <- .8
train.indices <- sample(1:nrow(X.mat), size = floor(nrow(X.mat)*prop))
X.train <- X.mat[train.indices,]
X.test <- X.mat[-train.indices,]
Y.train <- Y.mat[train.indices,]
Y.test <- Y.mat[-train.indices,]
scaled.train.indicator <- scaled.df.indicator[train.indices,]
scaled.test.indicator <- scaled.df.indicator[-train.indices,]
#all.form <- as.formula(paste("trump.percent + y1 + y2 + y3 ~ ", paste(colnames(X.mat), collapse="+")))
percent.form.indicator <- as.formula(paste("trump.percent ~", paste(colnames(X.mat), collapse="+")))

model <- neuralnet(percent.form.indicator, scaled.train.indicator, hidden=16, rep=1, act.fct = "tanh", lifesign = "full", stepmax = 1e+08)

Y.fit.train <- neuralnet::compute(model, X.train)$net.result
Y.fit.test <- neuralnet::compute(model, X.test)$net.result

# MSE in scaled space
mse.train <- sqrt(mean((Y.fit.train - Y.train)^2))
mse.test <- sqrt(mean((Y.fit.test - Y.test)^2))

benchmark.train <- sqrt(mean((Y.train - mean(Y.train))^2))
benchmark.test <- sqrt(mean((Y.test - mean(Y.test))^2))

paste("mse train: ", mse.train)
paste("mse test: ", mse.test)
paste("benchmark train: ", benchmark.train)
paste("benchmark test: ", benchmark.test)
