library(tidyverse)

cereal  <- read.csv(file="Datasets/cereals.CSV",stringsAsFactors=TRUE,header=TRUE)

ggplot(cereal, aes(Sodium, Rating )) + geom_point() + geom_smooth(method = "lm")


summary(lm(Rating ~ Sodium, cereal))

cereal %>% 
  subset(Rating < 80) %>% 
  {.} -> cereal_subset

  summary(lm(Rating ~ Sodium, cereal_subset))

  