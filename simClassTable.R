### Script produces a table to summarise results of sim study
library(tidyverse)

load("wassersteinData.RData")

dat <- wassersteinData %>%
 group_by(sampProp, rate)

ggplot(dat) +
geom_bar(aes(x = as.character(sampProp), fill = class)) +
scale_fill_manual(name = "", values = alpha(viridis(2), 0.8)) +
facet_wrap(~rate)

# also table of clas vs rate
dat <- wassersteinData %>%
 group_by(sampProp) %>%
 count(class)

ggplot(subset(dat, class == "Date-Driven")) +
geom_point(aes(x = sampProp, y = n / 200))
