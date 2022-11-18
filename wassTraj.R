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

plotTheme <- function() {
 theme_minimal() +
 theme(legend.position = "bottom",
 plot.margin = unit(c(0, 0, 0, 0), "cm")
 )
}

trsup <- data.frame(x = c(0, 0, 0.75), y = c(0, 0.75, 0.75))
trinf <- data.frame(x = c(0, 0.75, 0.75), y = c(0, 0, 0.75))


# group date to look at W. change as p increases
#wassersteinData <- 
wassersteinData %>%
 group_by(tree, rate) %>%
 group_vars()


# test animation
p <- ggplot(wassersteinData) +
 geom_point(aes(x = wd, y = ws), pch = 21, size = 2.5, fill = alpha("dodgerblue", 0.6)) +
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
  label = c("Date-Driven", "Seq-Driven"), size = 2.5) +
 geom_text(aes(x = 0.6, y = 1, label = sampProp)) +
 transition_time(sampProp) +
 shadow_wake(wake_length = 0.1, alpha = FALSE) +
 labs(title = "Sampling Proportion (w/ interpolation): {round(frame_time, 2)}") +
 plotTheme()

anim <- animate(p, fps = 4)

anim_save(animation = anim, filename = paste0(figPath, "wassTraj.gif"))

#TODO: get path between points