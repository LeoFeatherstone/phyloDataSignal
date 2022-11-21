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
  grepl(id, pattern = "FixedClock") ~ "fixed clock",
  TRUE ~ "prior")) %>%
 mutate(interval = gsub(interval, pattern = "_BDSKY_Serial.", replacement = ""))

# testing plot
pdf(file = paste0(figPath, "h1n1Posts.pdf"), useDingbats = FALSE)
ggplot(subset(posts, treatment != "noData")) +
 geom_density(aes(x = R0, fill = treatment, y = ..density..), alpha = 0.5, stat = "density") +
 facet_wrap(~ clockPrior * interval, scales = "free") +
 #xlim(0.5, 1.5) +
 theme_minimal() +
 theme(text = element_text(size = 16))
dev.off()

# TODO: Wasserstein metrics and markup figure (after posterior finished)
