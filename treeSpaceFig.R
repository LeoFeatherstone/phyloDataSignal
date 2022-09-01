### Figure to explain the difference in date and sequence information 
library(ape)
library(combinat)

# generating all base topologies for a tree with 3 tips
set.seed(1234)
t <- rtopology(3, rooted = TRUE, tip.label = NULL, br = 1)
t$edge.length <- c(1, 3, 2, 1) # giving order to tips
t$tip.label <- LETTERS[1:3]

trees <- list()
class(trees) <- "multiPhylo"
for (i in 1:3){
    trees[[i]] <- t
    }

# list of base topologies
tops <- list()
for (i in seq_along(combn(1:3, 2)[1, ])){
    tops[[i]] <- union(combn(1:3, 2)[, i], 1:3)
}

for (i in seq_along(trees)){
    trees[[i]]$edge[2:4, 2] <- tops[[i]]
}

all_tree <- list()
class(all_tree) <- "multiPhylo"
col <- vector()
for (i in seq_along(trees)){
    tmp <- trees[[i]]
    for (j in seq_along(permn(c(1, 2, 4)))){
        tmp$edge.length[-1] <- permn(c(1, 2.5, 4))[[j]]
        all_tree[[(i - 1) * max(seq_along(permn(c(1, 2, 4)))) + j]] <- tmp
        if (((i - 1) * max(seq_along(permn(c(1, 2, 4)))) + j) %in% c(1, 8, 17)) {
            col <- c(col, 'blue')
            } else {
                col <- c(col, 'black')
            }
    }
}

#plot
tiff("./figures/egTreeSpace.tiff", width = 4, height = 6, units = 'in', res = 300)
    par(mfrow = c(6, 3), mar = rep(1, 4))
    for (i in seq_along(all_tree)){
        plot.phylo(all_tree[[i]], edge.color = col[i], edge.width = 3, cex = 0.8)
    }
dev.off()
