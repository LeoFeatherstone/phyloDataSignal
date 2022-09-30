### Script calculates wasserstein distance and classifies
library(transport)
library(tidyverse)

# clasifying by wasserstein statistic
load("postR0.RData")

# function for distance from posterior with full data
wassersteinCompare <- function(df) {
 op <- c(wasserstein1d(na.omit(df$fullData), na.omit(df$dateData)),
 wasserstein1d(na.omit(df$fullData), na.omit(df$seqData)),
 wasserstein1d(na.omit(df$fullData), na.omit(df$noData)))

 names(op) <- c("wd", "ws", "wn")
 return(op)
}

# Wrangle Wasserstein distances into data frame
tmp <- lapply(chains, function(x) wassersteinCompare(x))
wassersteinData <- as.data.frame(do.call("rbind", tmp))

# classify
wassersteinData <- wassersteinData %>%
mutate(
 class = case_when(
  wd < ws ~ "Date-Driven",
  ws < wd ~ "Seq-Driven"
 )
) %>%
rownames_to_column("id")

wassersteinData <- cbind(wassersteinData, tree, rate, sampProp)
# all(paste0('t', wassersteinData$tree, 'p', wassersteinData$sampProp, 'r', wassersteinData$rate) == rownames(wassersteinData)) # SANITY CHECK # nolint
save(wassersteinData, file = "wassersteinData.RData")
