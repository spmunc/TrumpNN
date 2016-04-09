###############################################################################
##
##  repubSOM.R: fits a an som to try to cluster counties to candidates
##             
##
###############################################################################

################################
# Load required package(s)
################################
require(kohonen)
require(dplyr)

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
columns_of_interest <- c("AGE775214", "SEX255214", "EDU685213", "RHI225214", "RHI725214",  "POP645213", "EDU685213", "VET605213", "INC110213")
som.input <- subset(som.dat, select = columns_of_interest)
#som.input <- as.matrix(subset(som.dat, select = -c(fips,winner)))

################################
# Fit SOM
################################
kohmap <- xyf(scale(som.input), classvec2classmat(som.dat$winner),
              grid = somgrid(10, 10, topo = "hexagon"), rlen = 100,
              alpha = c(0.10, 0.001), toroidal = TRUE)
xyfpredictions <- classmat2classvec(predict(kohmap)$unit.predictions)
colorss <- xyfpredictions
colorss <- as.integer(factor(colorss))
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap)
plot(kohmap, type="mapping",
     main = "another mapping plot",bgcol = as.integer(colorss))



################################
# Predict based on SOM
################################
non_trained_fips <- setdiff(county_facts$fips, som.dat$fips)
som.testing <- subset(county_facts, fips %in% non_trained_fips)
som.testing2 <- som.testing[,columns_of_interest]
Xtest <- scale(som.testing2,
               center = attr(scale(som.input), "scaled:center"),
               scale = attr(scale(som.input), "scaled:scale"))

som.prediction <- predict(kohmap, newdata = Xtest,
                          trainX = som.input,
                          trainY = classvec2classmat(som.dat$winner))

View(cbind(som.prediction$prediction, som.testing))








################################
# SuperSOM
################################
county_facts <- read.csv("2016_presidential_election/county_facts.csv")
primary_results <- read.csv("2016_presidential_election/primary_results.csv")
som.dat <- som.dat

# Remove area name and state abbreviation and choose the fips
new_county_facts <- subset(county_facts, select = -c(area_name, state_abbreviation))
new_county_facts <- subset(new_county_facts, fips %in% intersect(som.dat$fips, new_county_facts$fips))

# Reorder them
new_county_facts <- new_county_facts[,c('fips',colnames(new_county_facts)[order(setdiff(colnames(new_county_facts), 'fips'))+1])]

# Get the name codes
new_county_facts_codes <- unique(substr(setdiff(colnames(new_county_facts), 'fips'), 1,3))


# Create a new data object - a list
ll <- list()
for(i in 1:length(new_county_facts_codes)){
  #create the new matrix
  x <- as.matrix(new_county_facts[,c(grep(pattern = new_county_facts_codes[i], x = colnames(new_county_facts)))])
  
  colnames(x) <- colnames(new_county_facts)[grep(pattern = new_county_facts_codes[i], x = colnames(new_county_facts))]
  rownames(x) <- new_county_facts[,1]
  
  ll[[length(ll)+1]] <- x
}
names(ll) <- new_county_facts_codes
# Create the classes for each row - the winners of the primary
ll$winner <- factor(som.dat$winner)

# Supermap it
kohmapss <- supersom(ll, grid = somgrid(10, 10, topo = "hexagon"), rlen = 100,
                     alpha = c(0.10, 0.001), toroidal = TRUE)

classes <- levels(ll$winner)
colors <- c("yellow", "green", "blue", "red", "orange")
par(mfrow = c(3, 2))
plot(kohmapss, type = "mapping",
         pch = 1, main = "All", keepMargins = TRUE)
for(i in seq(along = classes)){
  X.class <- lapply(ll, function(x) subset(x, ll$winner == classes[i]))
  X.map <- kohonen::map(kohmapss, newdata=X.class)
  plot(kohmapss, type = "mapping", classif = X.map,
          col = colors[i], pch = 1, main = classes[i], keepMargins = TRUE,
          bgcol = gray(0.55))
}





