# script for plane figures of rate, sampling, and dates patterns against W statistics
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

path <- getwd()
figPath <- paste0(path, "/figures/")

load("wassersteinData.RData")

# traingles to shade regions
trsup <- data.frame(x = c(0, 0, 0.75), y = c(0, 0.75, 0.75))
trinf <- data.frame(x = c(0, 0.75, 0.75), y = c(0, 0, 0.75))

# baseline theme for plots
plotTheme <- function() {
 theme_minimal() +
 theme(legend.position = "bottom",
 plot.margin = unit(c(0, 0, 0, 0), "cm")
 )
}

# bar plot of classifications
barDat <- wassersteinData %>%
 group_by(sampProp, rate)

rate.labs <- c((" Rate: 10^(-5) (subs/site/time)"), (" Rate: 10^(-3) (subs/site/time)"))
names(rate.labs) <- unique(wassersteinData$rate)

pBar <- ggplot(barDat) +
 geom_bar(aes(x = as.character(sampProp), fill = class)) +
 scale_fill_manual(name = "", values = alpha(viridis(2), 0.8)) +
 xlab("Sampling Proportion") +
 facet_wrap(~rate, labeller = labeller(rate = rate.labs)) +
 coord_fixed(ratio = 0.027) +
 plotTheme() +
 theme(strip.text = element_markdown())

wassDist <- ggplot(wassersteinData, aes(x = wd, y = ws)) +
 xlim(0, 0.75) + ylim(0, 0.75) +
 # shading
 geom_polygon(aes(x = x, y = y), data = trsup, fill = alpha("grey", 0.5)) +
 geom_polygon(aes(x = x, y = y), data = trinf, fill = alpha("white", 0.5)) +
 geom_quadrant_lines(linetype = "solid") +
 # square plot
 coord_fixed(ratio = 1) +
 geom_point(pch = 21, size = 1.5, fill = alpha("steelblue", 0.6)) +
 #scale_fill_manual(name = "Rate\n(subs/site/time)",
  #values = alpha(rev(viridis(2)), 0.8),
  #labels = c("10^-3", "10^-5")) +
 geom_quadrant_lines(linetype = "solid") +
 xlab(TeX("W_{D}")) + ylab(TeX("W_{S}")) +
 annotate("text", x = c(0.25, 0.55), y = c(0.55, 0.25),
  label = c("Date-Driven", "Seq-Driven"), size = 2.5) +
 plotTheme()

wassDist <- ggMarginal(wassDist, size = 7, type = "histogram", fill = alpha("steelblue", 0.6))

# First row of panel plot
p1 <- plot_grid(wassDist, pBar, rel_widths = c(1, 1.8), labels = "AUTO")

tiff(file = paste0(figPath, 'wassDistandBar.tiff'),
 units = "in", height = 3, width = 6, res = 300, compression = "lzw")
 p1
dev.off()

# site pattern plot (second panel)
plotTheme <- function() {
 theme_minimal() +
 theme(legend.position = "bottom",
  plot.margin = unit(c(0, 0, 0, -1), "cm"),
  legend.title = element_text(size = 10),
  #strip.text = element_text(size = 12),
  #axis.title = element_text(size = 12)
  )
 }


sampProp.labs <- c("Sampling prop. = 0.05",
 "Sampling prop. = 0.5",
 "Sampling prop. = 1.0")
names(sampProp.labs) <- unique(wassersteinData$sampProp)

p2 <- ggplot(wassersteinData, aes(x = wd, y = ws, fill = log10(sitePatterns))) +
xlim(0, 0.75) + ylim(0, 0.75) +
# shading
geom_polygon(aes(x = x, y = y), data = trsup, fill = alpha("grey", 0.5)) +
geom_polygon(aes(x = x, y = y), data = trinf, fill = alpha("white", 0.5)) +
geom_quadrant_lines(linetype = "solid") +
geom_point(pch = 21, size = 2.5) +
coord_fixed(ratio = 1) +
scale_fill_continuous(name = TeX("log_{10}(Site Patterns)"), type = "viridis", breaks = 1:4) +
facet_wrap(~sampProp, labeller = labeller(sampProp = sampProp.labs)) +
facet_wrap(~sampProp, labeller = labeller(sampProp = sampProp.labs)) +
xlab(TeX("W_{D}")) + ylab(TeX("W_{S}")) +
annotate("text", x = c(0.25, 0.55), y = c(0.55, 0.25),
  label = c("Date-Driven", "Seq-Driven"), size = 2.5) +
plotTheme()

# Date span plot (third panel)
p3 <- ggplot(wassersteinData, aes(x = wd, y = ws, fill = datePatterns)) +
xlim(0, 0.75) + ylim(0, 0.75) +
# shading
geom_polygon(aes(x = x, y = y), data = trsup, fill = alpha("grey", 0.5)) +
geom_polygon(aes(x = x, y = y), data = trinf, fill = alpha("white", 0.5)) +
geom_quadrant_lines(linetype = "solid") +
geom_point(pch = 21, size = 2.5) +
coord_fixed(ratio = 1) +
#scale_fill_continuous(name = "Site Patterns", type = "viridis", breaks = seq(0, 4000, by = 500)) +
scale_fill_continuous(name = "Date Span             ", type = "viridis") +
facet_wrap(~sampProp, labeller = labeller(sampProp = sampProp.labs)) +
xlab(TeX("W_{D}")) + ylab(TeX("W_{S}")) +
annotate("text", x = c(0.25, 0.55), y = c(0.55, 0.25),
  label = c("Date-Driven", "Seq-Driven"), size = 2.5) +
plotTheme()


# combined plot
tiff(file = paste0(figPath, 'wassPanel.tiff'),
 units = "in", height = 8, width = 6, res = 300, compression = "lzw")

ggpubr::ggarrange(p1, p2, p3, nrow = 3, labels = c("", "C", "D"), align = "v", heights = c(1, 1.1, 1.1))

dev.off()


# Date and seq pattern plots
tiff(file = paste0(figPath, 'wassSeqPat.tiff'),
 units = "in", height = 3, width = 6, res = 300, compression = "lzw")
 p2
dev.off()

tiff(file = paste0(figPath, 'wassDatePat.tiff'),
 units = "in", height = 3, width = 6, res = 300, compression = "lzw")
 p3
dev.off()



# Violin distribution of wass. Maybe move to supps
source('ggSplitViolin.R')
newDat <- wassersteinData %>%
pivot_longer(starts_with("w"), values_to='wasserstein', names_to='type', values_drop_na=T)

tiff(file = paste0(figPath, "wassViolin.tiff"))
 ggplot(subset(newDat, type != "wn"),
  aes(x = as.character(sampProp), y = wasserstein, fill = type)) +
 geom_split_violin() +
 scale_fill_manual(values = alpha(viridis(2), 0.8),
   labels = c(expression(W[D]), expression(W[S]))) +
 xlab("Sampling Proportion") + ylab(expression(W[D]~or~W[S])) +
 theme_minimal() +
 plotTheme() +
 theme(
  legend.title = element_blank()
  )
 dev.off()

## Plot of patterns vs class for supps
tiff(file = paste0(figPath, "patternsClass.tiff"))
 ggplot(wassersteinData) +
 geom_point(aes(x = sitePatterns, y = datePatterns, col = class)) +
 facet_wrap(~rate + sampProp, scales = "free") +
 scale_color_manual(name = "Classification", values = viridis(2))
dev.off()