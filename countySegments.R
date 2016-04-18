require(kohonen)
require(neuralnet)
require(dplyr)
require(caret)
require(compositions)
require(tidyr)
require(stringr)

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
combined.df <- candidate.data.results %>% left_join(candidate.county.results, by=c("fips"))

## Create a neural network for the data.