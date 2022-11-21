# script to look at wass trajectories, and potentially plot them or give an example plot
library(ggplot2)
library(viridis)
library(reshape)
library(cowplot)
library(tidyverse)
library(latex2exp)
library(ggpmisc)
library(ggExtra)
library(GGally)
library(ggtext)
library(gganimate)

path <- getwd()
figPath <- paste0(path, "/figures/")

load("wassersteinData.RData")


trsup <- data.frame(x = c(0, 0, 0.75), y = c(0, 0.75, 0.75))
trinf <- data.frame(x = c(0, 0.75, 0.75), y = c(0, 0, 0.75))


# group date to look at W. change as p increases
wassersteinData <- wassersteinData %>%
 group_by(tree, rate)


# animate point trajs
p <- ggplot(wassersteinData) +
 geom_point(aes(x = wd, y = ws, fill = as.factor(sampProp)), pch = 21, size = 2.5) +
 scale_fill_manual(values = viridis(3), name = "Sampling\nProportion") +
 #facet_wrap(~rate) +
 xlim(0, 0.75) + ylim(0, 0.75) +
 # shading
 geom_polygon(aes(x = x, y = y), data = trsup, fill = alpha("grey", 0.5)) +
 geom_polygon(aes(x = x, y = y), data = trinf, fill = alpha("white", 0.5)) +
 geom_quadrant_lines(linetype = "solid") +
 # square plot
 coord_fixed(ratio = 1) +
 xlab(TeX("W_{D}")) + ylab(TeX("W_{S}")) +
 annotate("text", x = c(0.25, 0.55), y = c(0.55, 0.25),
  label = c("Date-Driven", "Seq-Driven"), size = 5) +
 # animation steps
 transition_states(sampProp, transition_length = 50, state_length = 1) +
 shadow_wake(wake_length = 0.1, alpha = FALSE) +
 theme_minimal() +
 theme(legend.position = "right",
 axis.title = element_text(size = 14),
 axis.text = element_text(size = 12))

anim <- animate(p, duration = 6)

anim_save(animation = anim, filename = paste0(figPath, "wassTraj.gif"), res = 300)

# Looking at changes over p
# NB data is still group as above - by tree and rate.

# sanity check for ordering
all(wassersteinData[wassersteinData$sampProp == 1, c("tree", "rate")] ==
 wassersteinData[wassersteinData$sampProp == 0.5, c("tree", "rate")])

all(wassersteinData[wassersteinData$sampProp == 0.5, c("tree", "rate")] ==
 wassersteinData[wassersteinData$sampProp == 0.05, c("tree", "rate")])

# checks out, so will just subtract columns to get vectors
u <- wassersteinData[wassersteinData$sampProp == 0.5, c("wd", "ws")] -
 wassersteinData[wassersteinData$sampProp == 0.05, c("wd", "ws")]

v <- wassersteinData[wassersteinData$sampProp == 1, c("wd", "ws")] -
 wassersteinData[wassersteinData$sampProp == 0.5, c("wd", "ws")]

# getting mean vecs
uhat <- c(mean(u$wd), mean(u$ws))
vhat <- c(mean(v$wd), mean(v$ws))

# vec plot
vecs <- cbind(u, v)
colnames(vecs) <- c("wd1", "ws1", "wd2", "wd2")

ggplot(vecs) +
 geom_segment(aes(x = 0, y = 0, xend = wd1, yend = ws1))
