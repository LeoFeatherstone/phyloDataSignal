### Figure to explain the difference in date and sequence information 
library(ape)
library(combinat)

# generating all base topologies for a tree with 3 tips
set.seed(1234)
t1 <- rtopology(3, rooted = TRUE, tip.label = NULL, br = 1)
t1$edge.length <- c(1, 3, 2, 1) # giving order to tips
t1$tip.labels <- LETTERS[1:3]

t2 <- t1
t2$edge[, 2] <- c(5, 3, 2, 1)
t3 <- t
t3$edge[, 2] <- c(5, 1, 3, 2)

# plot all base topologies
par(mfrow = c(1, 3))
plot(t1, direction = "downwards")
tiplabels(cex=3)
plot(t2, direction = "downwards")
tiplabels(cex=3)
plot(t3, direction = "downwards")
tiplabels(cex=3)

# all possible tip name orders
permn(LETTERS[1:3])

# generate all possibilities with just sequence information
seq <- list()
