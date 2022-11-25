# figs for h1n1 data
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
library(transport)
source("ggSplitViolin.R")
library(lubridate)
library(ggforce)

path <- getwd()
figPath <- paste0(path, "/figures/")

logFiles <- dir(path = paste0(path, "/empiricalAnalyses/h1n1/strictClock"),
 pattern = ".+.log", full.names = TRUE)

logs <- lapply(logFiles, function(x) read.table(x, header = TRUE))

names(logs) <- gsub(
 gsub(logFiles, pattern = ".+/", replacement = ""),
 pattern = ".log", replacement = "")

backup <- logs

# applying burnin. Adapted for chain above 10^9 in length
burnin <- function(df) {
 if (dim(df)[1] > (10^9 / 10^4)) {
  df <- df[-c(1:(0.5 * 10^9 / 10^4)), ]
 }
 df <- df[-c(1:500), ]
}

logs <- lapply(logs, function(x) burnin(x))

# now slecting columns
logs <- lapply(logs,
 function(x) x[, grep(names(x), pattern = "reproductiveNumber|samplingProportion")])

names(logs)

posts <- bind_rows(logs, .id = "id") %>%
 pivot_longer(starts_with("reproductiveNumber"), values_to = "R0",
  names_to = "interval", values_drop_na = TRUE) %>%
 mutate(treatment = gsub(id, pattern = "H1N1.*", replacement = "")) %>%
 mutate(clockPrior = case_when(
  grepl(id, pattern = "FixedClock") ~ "fixed",
  TRUE ~ "prior")) %>%
 mutate(interval = gsub(interval, pattern = "_BDSKY_Serial.", replacement = "")) %>%
 mutate(pos = case_when(interval == "reproductiveNumber1" ~ as.Date("2009-05-07"),
 						interval == "reproductiveNumber2" ~ as.Date("2009-09-07"))) %>%
 mutate(interval = gsub(interval, pattern = "reproductiveNumber", replacement = ""))

# getting wasserstein values
# for prior
empWass <- data.frame(treat = NA, clock = NA, interval = NA, wR0 = NA, wP = NA)

empWass <- data.frame()
for (treat in unique(posts$treatment)){
 for (clock in unique(posts$clockPrior)){
	v1 <- unlist(na.omit(posts[which(posts$treatment == "fullData" & posts$clockPrior == clock), "samplingProportion_BDSKY_Serial"]))
	v2 <- unlist(na.omit(posts[which(posts$treatment == treat & posts$clockPrior == clock), "samplingProportion_BDSKY_Serial"]))
	wass <- wasserstein1d(v1, v2)

   empWass <- rbind(empWass, c(treat, clock, NA, NA, wass))

  for (int in unique(posts$interval)){
	v1 <- unlist(na.omit(posts[which(posts$treatment == "fullData" & posts$clockPrior == clock & posts$interval == int), "R0"]))
	v2 <- unlist(na.omit(posts[which(posts$treatment == treat & posts$clockPrior == clock & posts$interval == int), "R0"]))
	wass <- wasserstein1d(v1, v2)

   empWass <- rbind(empWass, c(treat, clock, int, wass, NA))
  }
 }
}
colnames(empWass) <- c("treat", "clock", "interval", "wR0", "wP")

# Get sampling times for sampling rug 
aln <- ape::read.dna(paste0(getwd(), "/empiricalAnalyses/h1n1/strictClock/NorthAm.Nov.fasta"), format = "fasta")
sampDates <- as.numeric(gsub(rownames(aln), pattern = ".+\\|.+\\|", replacement = ""))
sampDates <- as.Date(lubridate::date_decimal(sampDates))

fill <- alpha(viridis(4)[-3], 0.5)

# sanity - checking order of clock for split violin
#ggplot(data = subset(posts, treatment == "dateData" & interval == "reproductiveNumber1")) +
#geom_split_violin(aes(x = interval, y = R0, fill = clockPrior))


rPlot <- ggplot() +
 geom_split_violin(data = subset(posts, treatment == "fullData" & interval == 1),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[2], scale = "width", width = 75) +
  geom_split_violin(data = subset(posts, treatment == "dateData" & interval == 1),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[1], scale = "width", width = 75) +
  geom_split_violin(data = subset(posts, treatment == "seqData" & interval == 1),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[3], scale = "width", width = 75) +
  geom_split_violin(data = subset(posts, treatment == "dateData" & interval == 2),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[1], scale = "width", width = 75) +
  geom_split_violin(data = subset(posts, treatment == "fullData" & interval == 2),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[2], scale = "width", width = 75) +
  geom_split_violin(data = subset(posts, treatment == "seqData" & interval == 2),
  aes(x = pos, y = R0, group = clockPrior), fill = fill[3], scale = "width", width = 75) +
  geom_rug(data = as.data.frame(sampDates), aes(x = sampDates), sides = "b") +
  # adding tmrca
  #geom_histogram(data = backup[[3]],
   #aes(x = as.Date(date_decimal(decimal_date(max(sampDates)) - TreeHeight)), y = ..count.. / 10000)) +
  geom_histogram(data = backup[[4]],
   aes(x = as.Date(date_decimal(decimal_date(max(sampDates)) - TreeHeight)), y = ..count.. / 100000),
   fill= fill[2], bins = 50) +
  geom_vline(xintercept = as.Date("2009-07-07"), lty = 2) +
  annotate("label", x = unique(posts$pos)[1], y = 1.75,
   label = "Fixed Clock <-> Clock Prior ") +
  annotate("label", x = mean(as.Date(date_decimal(decimal_date(max(sampDates)) - backup[[4]]$TreeHeight))),
   y = 0.5, label = "MRCA\n(Fixed Clock,\nFull Data)") +
  scale_x_date(date_labels = "%b '%y", label = "",
   limits = as.Date(c("2008-11-01", max(sampDates)))) +
  xlab("") + ylab(expression(Posterior ~ italic(R[0]))) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11))

leg <- cowplot::get_legend(
 ggplot(subset(posts, treatment != "noData")) +
 geom_density(aes(x = R0, fill = treatment)) +
 scale_fill_manual(values = alpha(viridis(4)[-3], 0.5), 
  labels = c("Date Data", "Full Data", "Sequence Data"), name = "") +
 theme(legend.text = element_text(size = 11))
)

# splitting sampling plots
pFullSeqData <-
ggplot() +
 geom_split_violin(data = subset(posts, treatment == "seqData"),
   aes(y = samplingProportion_BDSKY_Serial, x = 1, group = clockPrior),
    fill = fill[3], scale = "width") +
 geom_split_violin(data = subset(posts, treatment == "fullData"),
   aes(y = samplingProportion_BDSKY_Serial, x = 1, group = clockPrior),
    fill = fill[2], scale = "width") +
 ylab("Posterior Sampling Porportion") +
 #theme_minimal() +
 theme(axis.text.x = element_blank(),
  axis.text.y = element_text(size = 11),
  axis.title = element_blank(),
  plot.background = element_rect(color = "black"))


pDateData <-
ggplot() +
 geom_split_violin(data = subset(posts, treatment == "dateData"),
   aes(y = samplingProportion_BDSKY_Serial, x = 0, group = clockPrior),
    fill = fill[1], scale = "width") +
 ylab("Posterior Sampling Porportion") +
 annotate("label", x = 0, y = 1,
   label = "Fixed Clock <-> Clock Prior ") +
 annotate("segment", x = 0, y = 0, xend = 1, yend = 0) +
 annotate("segment", x = 0, y = 0.009, xend = 0.5, yend = 0.05) +
 annotate("segment", x = 0.5, y = 0.05, xend = 0.75, yend = 0.8) +
 # insert seq and full data plot
 annotation_custom(ggplotGrob(pFullSeqData), xmin = 0.75, xmax = 2, ymin = 0, ymax = 0.8) +
 xlim(-1, 2) +
 theme_minimal() +
 theme(axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_text(size = 11),
  axis.title.y = element_text(size = 11))

sampPlot <- cowplot::plot_grid(pDateData, leg, nrow = 1, rel_widths = c(3, 1))

pdf(file = paste0(figPath, "h1n1Posts.pdf"), useDingbats = FALSE)
 cowplot::plot_grid(rPlot, sampPlot, nrow = 2)
dev.off()

