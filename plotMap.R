require(maps)
require(mapproj)
require(dplyr)
setwd("~/Documents/personal/rice/2015-16/nml502/project/TrumpNN/")

load("indicator-16-neurons.RData")
model.16 <- model
load("indicator-8-neurons.RData")
model.8 <- model
load("indicator-4-neurons.RData")
model.4 <- model

Y.fit.train.4 <- neuralnet::compute(model.4, X.train)$net.result
Y.fit.test.4 <- neuralnet::compute(model.4, X.test)$net.result
mse.train.4 <- sqrt(mean((Y.fit.train.4 - Y.train)^2))
mse.test.4 <- sqrt(mean((Y.fit.test.4 - Y.test)^2)) 

Y.fit.train.8 <- neuralnet::compute(model.8, X.train)$net.result
Y.fit.test.8 <- neuralnet::compute(model.8, X.test)$net.result
mse.train.8 <- sqrt(mean((Y.fit.train.8 - Y.train)^2))
mse.test.8 <- sqrt(mean((Y.fit.test.8 - Y.test)^2)) 

Y.fit.train.16 <- neuralnet::compute(model.16, X.train)$net.result
Y.fit.test.16 <- neuralnet::compute(model.16, X.test)$net.result
mse.train.16 <- sqrt(mean((Y.fit.train.16 - Y.train)^2))
mse.test.16 <- sqrt(mean((Y.fit.test.16 - Y.test)^2)) 

Y.train.all <- data.frame(Y4=Y.fit.train.4, Y8=Y.fit.train.8, Y16=Y.fit.train.16, YActual=Y.train)
ensemble.lm <- lm(YActual ~ Y4 + Y8 + Y16, data=Y.train.all)

Y.test.all <- data.frame(Y4=Y.fit.test.4, Y8=Y.fit.test.8, Y16=Y.fit.test.16)
Y.combined.train <- predict(ensemble.lm, Y.train.all)
Y.combined.test <- predict(ensemble.lm, Y.test.all)

mse.ensemble.train <- sqrt(mean((Y.combined.train - Y.train)^2))
mse.ensemble.test <- sqrt(mean((Y.combined.test - Y.test)^2))
#c("Donald Trump", "Ted Cruz", "Marco Rubio", "Ben Carson", "John Kasich", "Jeb Bush")
NY.map <- map("county", regions = "New York")
NY.counties.candidates <- county.facts %>% filter(state_abbreviation == "NY") %>%
  select(fips,AGE775214, SEX255214, EDU685213, RHI225214, RHI725214,  POP645213, EDU685213, VET605213, INC110213, RHI625214, HSG445213, PVY020213, POP060210)

NY.counties.candidates.scale <- data.frame(apply(NY.counties.candidates %>% select(-fips), 2, function(x) scale(x, 0, 1))) %>%
  mutate(x1=1, x2=1, x3=0, x4=0, x5=1, x6=0)

#Y.fit.train <- neuralnet::compute(, NY.counties.candidates.scale)$net.result
#Y.fit.train.unscaled <- data.frame(trump.percent=
#                                     apply(Y.fit.train, 2,
#                                           function(x) scale(x, .15, max(.70))),
#                                   fips=NY.counties.candidates$fips)
NY.predict.tanh <- data.frame(trump.percent.tanh=tanh(
                                predict(ensemble.lm,
                                        data.frame(Y4=neuralnet::compute(model.4, NY.counties.candidates.scale)$net.result, 
                                                   Y8=neuralnet::compute(model.8, NY.counties.candidates.scale)$net.result, 
                                                   Y16=neuralnet::compute(model.16, NY.counties.candidates.scale)$net.result)
                              )),
                              fips=NY.counties.candidates$fips) %>% left_join(county.facts %>% select(fips, area_name), by="fips")
# define color buckets
colorBuckets <- as.numeric(cut(NY.predict.tanh$trump.percent, breaks = 50))
colorGenerate <- colorRampPalette(c("white", "#ef6c00"))(50)
colorsmatched <- colorGenerate[colorBuckets]

# draw map
map("county", region="New York", col = colorsmatched, fill = TRUE, resolution = 0,
    lty = 0)
  
kasich.counties <- NY.counties.candidates.scale %>% mutate(x5=0)
NY.predict.tanh.kasich <- data.frame(trump.percent.tanh=tanh(
                                predict(ensemble.lm,
                                        data.frame(Y4=neuralnet::compute(model.4, kasich.counties)$net.result, 
                                                   Y8=neuralnet::compute(model.8, kasich.counties)$net.result, 
                                                   Y16=neuralnet::compute(model.16, kasich.counties)$net.result)
                              )),
                              fips=NY.counties.candidates$fips) %>% left_join(county.facts %>% select(fips, area_name), by="fips")

# define color buckets
colorBuckets <- as.numeric(cut(NY.predict.tanh.kasich$trump.percent, breaks = 50))
colorGenerate <- colorRampPalette(c("white", "#ef6c00"))(50)
colorsmatched <- colorGenerate[colorBuckets]

# draw map
map("county", region="New York", col = colorsmatched, fill = TRUE, resolution = 0,
    lty = 0)
  
errors.kasich.trump <- data.frame(trump.percent.diff=(NY.predict.tanh$trump.percent.tanh -
                                                           NY.predict.tanh.kasich$trump.percent.tanh), 
                                  fips=NY.counties.candidates$fips)
# define color buckets
colorBuckets <- as.numeric(cut(errors.kasich.trump$trump.percent.diff, breaks = 50))
colorGenerate <- colorRampPalette(c("black", "white"))(50)
colorsmatched <- colorGenerate[colorBuckets]

# draw map
map("county", region="New York", col = colorsmatched, fill = TRUE, resolution = 0,
    lty = 0)

actual.results <- read.csv(file = "Trump_NY.csv") %>% select(votes, fips)

colorBuckets <- as.numeric(cut(actual.results$votes, breaks = 50))
colorGenerate <- colorRampPalette(c("white", "#ef6c00"))(50)
colorsmatched <- colorGenerate[colorBuckets]

# draw map
map("county", region="New York", col = colorsmatched, fill = TRUE, resolution = 0,
    lty = 0)


