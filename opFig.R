library(TreeSim)
library(ggtree)
library(ggplot2)
library(cowplot)

set.seed(4321)
t <- sim.bdsky.stt(n=3, lambdasky=2.5, deathsky=1, sampprobsky=0.5, timesky=0, timestop=0)[[1]]
# len
xSpan <- max(diag(vcv.phylo(t))) 
plot(t)

trees <- list()
order <- list(c(1:4), c(4:1), c(4,3,1,2))
scale <- c(1,0.6, 0.9)
for (i in 1:3) {
    trees[[i]] <- t
    trees[[i]]$edge.length <- trees[[i]]$edge.length[order[[i]]]*scale[i]
}

p1 <- ggtree(trees[[1]]) +  geom_tippoint(shape=16, col=c(1:3), size=4) + xlim(0, xSpan) + annotate('segment', x=xSpan, xend=xSpan, y=0, yend=3) + annotate('segment', x=max(diag(vcv.phylo(trees[[1]]))+trees[[1]]$root.edge), xend=xSpan, y=3, yend=3) + ggtitle(expression(1^{st}~Step)) + annotate('segment', x=0, y=0, xend=xSpan, yend=0) + annotate('label', x=(0.5*(xSpan)), y=0, label=expression(origin~x_0))
p2 <- ggtree(trees[[2]]) +  geom_tippoint(shape=16, col=c(1:3), size=4) + xlim(0, xSpan) + annotate('segment', x=xSpan, xend=xSpan, y=0, yend=3) + annotate('segment', x=max(diag(vcv.phylo(trees[[2]])))+0.01, xend=xSpan, y=2, yend=2, lty=2, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm"))) + annotate('label', x=(0.5*(max(diag(vcv.phylo(trees[[2]])))+0.01+xSpan)), y=2, label='FSO') + ggtitle(expression(i^{th}~Step))
p3 <- ggtree(trees[[3]]) +  geom_tippoint(shape=16, col=c(1:3), size=4) + xlim(0, xSpan) + annotate('segment', x=xSpan, xend=xSpan, y=0, yend=3) + annotate('segment', x=max(diag(vcv.phylo(trees[[3]])))+0.01, xend=xSpan, y=3, yend=3, lty=2, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm"))) + annotate('label', x=(0.5*(max(diag(vcv.phylo(trees[[3]])))+0.01+xSpan)), y=3, label='FSO') + ggtitle(expression(j^{th}~Step))

pdf('tipDateOp.pdf', useDingbats = F)
    cowplot::plot_grid(p1,p2,p3, nrow=3)
dev.off()
