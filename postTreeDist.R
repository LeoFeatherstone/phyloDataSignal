### Script to look at tree distance in each posterior
library(tidyverse)
library(treeio) # maybe do densitree plots?
library(ggplot2)
library(viridis)

path <- getwd()
figPath <- paste0(path, "/figures/")

# dist mat files
files <- dir(paste0(path, "/log/postTreeDist"), pattern  = ".+RData")

# read mats
mciDist <- list()
rfDist <- list()
infoRFDist <- list()
for (i in seq_along(files)){
 load(paste0(path, "/log/postTreeDist/", files[[i]]))
 mciDist[[i]] <- distMat1
 infoRFDist[[i]] <- distMat2
 rfDist[[i]] <- distMat3
}

names(mciDist) <- files
names(rfDist) <- files
names(infoRFDist) <- files
#save(mciDist, rfDist, infoRFDist, file = "micPostTreeDist.RData")

# summary stats for each
# mci
mciSummary <- lapply(mciDist, function(x) c(mean(x[upper.tri(x)])))
names(mciSummary) <- gsub(names(mciSummary), pattern = "TreeDist.RData", replacement = "")

# rf
rfSummary <- lapply(rfDist, function(x) c(mean(x[upper.tri(x)])))
names(rfSummary) <- gsub(names(rfSummary), pattern = "TreeDist.RData", replacement = "")

# info rf
infoRFSummary <- lapply(infoRFDist, function(x) c(mean(x[upper.tri(x)])))
names(infoRFSummary) <- gsub(names(infoRFSummary), pattern = "TreeDist.RData", replacement = "")

# make dfs
mciSummary <- as.data.frame(do.call(rbind, mciSummary))
colnames(mciSummary) <- c("mci")

rfSummary <- as.data.frame(do.call(rbind, rfSummary))
colnames(rfSummary) <- c("rf")

infoRFSummary <- as.data.frame(do.call(rbind, infoRFSummary))
colnames(infoRFSummary) <- c("infoRF")

distSummary <- cbind(mciSummary, rfSummary, infoRFSummary)

# sanity check: all(rownames(infoRFSummary) == rownames(mciSummary)) && all(rownames(infoRFSummary) == rownames(rfSummary)) #nolint

# wrangle data
names(distSummary) <- gsub(names(distSummary), pattern = "[./]", replacement = "")

backup <- distSummary

# format df
distSummary <- distSummary %>%
rownames_to_column() %>%
mutate(treat = gsub(rowname, pattern = "Data.+", replacement = "Data")) %>%
mutate(rate = gsub(rowname, pattern = ".+r", replacement = "")) %>%
mutate(sampProp = gsub(
 gsub(rowname, pattern = ".+p", replacement = ""),
 pattern = "r.+", replacement = "")) %>%
mutate(t = gsub(
    gsub(rowname, pattern = ".+Datat", replacement = ""),
    pattern = "p.+", replacement = "")) %>%
mutate(t = gsub(t, pattern = "p0.+", replacement = ""))

# plot
plotTheme <- function() {
 theme_minimal() +
 theme(legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 8),
  strip.text = element_text(size = 8),
  axis.title = element_text(size = 8),
  axis.text.x = element_blank())
 }

rate.labs <- c(("Rate: 10^-5 (subs/site/year)"),
 ("Rate: 10^-3 (subs/site/year)"))
names(rate.labs) <- unique(distSummary$rate)
sampProp.labs <- c("Sampling proportion: = 0.05",
 "Sampling proportion: = 0.5", "Sampling proportion: = 1.0")
names(sampProp.labs) <- unique(distSummary$sampProp)

tiff(file = paste0(figPath, "postMCITreeDist.tiff"),
 compression = "lzw", units = "in", height = 4, width = 6, res = 300)
 ggplot(distSummary) +
  geom_violin(aes(x = fct_reorder(treat, mci, .fun = mean), y = mci, fill = treat), scale = "width") +
  facet_wrap(~rate + sampProp, scales = "free", ncol = 3,
  labeller = labeller(rate = rate.labs, sampProp = sampProp.labs)) +
  scale_fill_manual(values = alpha(viridis(4), 0.6), 
  labels = c("Date Data", "Full Data", "Marginal Prior", "Sequence Data")) +
  ylab("Mean pairwise mutual clustering information (bits)") +
  xlab("") +
  plotTheme()
dev.off()

tiff(file = paste0(figPath, "postInfoRFrfTreeDist.tiff"),
 compression = "lzw", units = "in", height = 4, width = 6, res = 300)
 ggplot(distSummary) +
  geom_violin(aes(x = fct_reorder(treat, infoRF, .fun = mean), y = infoRF, fill = treat), scale = "width") +
  facet_wrap(~rate + sampProp, scales = "free", ncol = 3,
  labeller = labeller(rate = rate.labs, sampProp = sampProp.labs)) +
  scale_fill_manual(values = alpha(viridis(4), 0.6), 
  labels = c("Date Data", "Full Data", "Marginal Prior", "Sequence Data")) +
  ylab("Mean pairwise information-corrected RF Distance") +
  xlab("") +
  plotTheme()
dev.off()

tiff(file = paste0(figPath, "postRFrfTreeDist.tiff"),
 compression = "lzw", units = "in", height = 4, width = 6, res = 300)
 ggplot(distSummary) +
  geom_violin(aes(x = fct_reorder(treat, rf, .fun = mean), y = rf, fill = treat), scale = "width") +
  facet_wrap(~rate + sampProp, ncol = 3, scales = "free",
  labeller = labeller(rate = rate.labs, sampProp = sampProp.labs)) +
  scale_fill_manual(values = alpha(viridis(4), 0.6), 
  labels = c("Date Data", "Full Data", "Marginal Prior", "Sequence Data")) +
  ylab("Mean pairwise normalised RF Distance") +
  xlab("") +
  plotTheme()
dev.off()
