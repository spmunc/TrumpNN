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
Y.fit.train.unscaled <- data.frame(trump.percent=
                                     apply(Y.fit.train, 2,
                                           function(x) scale(x, .15, max(70))),
                                   fips=NY.counties.candidates$fips)
# define color buckets

colorBuckets <- as.numeric(cut(Y.fit.train.unscaled$trump.percent, breaks = 90))
colorGenerate <- colorRampPalette(c("red", "green"))(90)

# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties
cnty.fips <- county.fips$fips[match(map("county", region="New York", plot=FALSE)$names,
                                    county.fips$polyname)]
colorsmatched <- colorGenerate[match(cnty.fips, Y.fit.train.unscaled$fips)]

# draw map
map("county", region="New York", col = colorsmatched, fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic", resolution=0)
legend("topright", leg.txt, horiz = TRUE, fill = colorsmatched)
  