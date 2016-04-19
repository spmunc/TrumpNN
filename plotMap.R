require(maps)
require(mapproj)
require(dplyr)

load("indicator-8-neurons.RData")
#c("Donald Trump", "Ted Cruz", "Marco Rubio", "Ben Carson", "John Kasich", "Jeb Bush")
NY.map <- map("county", regions = "New York")
NY.counties.candidates <- county.facts %>% filter(state_abbreviation == "NY") %>%
  select(fips,AGE775214, SEX255214, EDU685213, RHI225214, RHI725214,  POP645213, EDU685213, VET605213, INC110213, RHI625214, HSG445213, PVY020213, POP060210) %>%
  #mutate(x1=1, x2=1, x3=0, x4=0, x5=1, x6=0)
NY.counties.candidates.scale <- data.frame(apply(NY.counties.candidates %>% select(-fips), 2, function(x) scale(x, 0, 1))) %>%
  mutate(x1=1, x2=1, x3=0, x4=0, x5=1, x6=0)


Y.fit.train <- neuralnet::compute(model, NY.counties.candidates.scale)$net.result
Y.fit.train.unscaled <- data.frame(apply(Y.fit.train, 2, function(x) scale(x, min(combined.df$trump.percent), max(combined.df$trump.percent)))
# define color buckets
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    county.fips$polyname)]
colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]

# draw map
map("county", region="New York", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
title("unemployment by county, 2009")
legend("topright", leg.txt, horiz = TRUE, fill = colors)
  