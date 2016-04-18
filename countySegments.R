require(kohonen)
require(neuralnet)
require(dplyr)
require(caret)
require(compositions)
require(tidyr)
require(stringr)
require(nnet)

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

model <- neuralnet(percent.form, scaled.train, hidden=16, rep=1, act.fct = "tanh", lifesign = "full", stepmax = 1e+08)

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

all.models <- lapply(c("8-neurons.RData", "16-neurons.RData"),
                     function(x) { 
                       load(x)
                       model
                      })
all.train.y <- lapply(all.models, function(m) neuralnet::compute(m, X.train)$net.result)
all.test.y <- lapply(all.models, function(m) neuralnet::compute(m, X.test)$net.result)

