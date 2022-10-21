# Script to look at error bars on wasserstein stat via subsampling.
# Are they larger than diff between posteriors 
library(tidyverse)
library(ggplot2)

path <- getwd()
figPath <- paste0(path, '/figures/')

load('postR0.RData')

# function for distance from posterior with full data
wassersteinCompare <- function(df) {
 op <- data.frame()
 for (i in 1:100) {	
  fullData <- sample(na.omit(df$fullData), ceiling(0.5 * length(na.omit(df$fullData))), replace = FALSE)
  dateData <- sample(na.omit(df$dateData), ceiling(0.5 * length(na.omit(df$dateData))), replace = FALSE)
  seqData <- sample(na.omit(df$seqData), ceiling(0.5 * length(na.omit(df$seqData))), replace = FALSE)
  noData <- sample(na.omit(df$noData), ceiling(0.5 * length(na.omit(df$noData))), replace = FALSE)

  op <- rbind(op, c(transport::wasserstein1d(fullData, dateData),
   transport::wasserstein1d(fullData, seqData),
   transport::wasserstein1d(fullData, noData),
   coda::effectiveSize(coda::as.mcmc(fullData)),
   coda::effectiveSize(coda::as.mcmc(dateData)),
   coda::effectiveSize(coda::as.mcmc(seqData)),
   coda::effectiveSize(coda::as.mcmc(noData))
   ))
 }

# note burnin is already removed in getPosteriors.R, producting chains object
names(op) <- c('fullData||dateData', 'fullData||seqData', 'fullData||noData',
 'fullDataESS', 'dateDataESS', 'seqDataESS', 'noDataESS')
 return(op)
}

set.seed(1234)
tmp <- lapply(chains, function(x) wassersteinCompare(x))


tmp <- list()
for (i in seq_along(chains)){
  tmp[[i]] <- wassersteinCompare(chains[[i]])
  print(paste0("### DONE", i, " ###"))
}

# add names
for (i in seq_along(tmp)){
  id <- rep(names(chains)[i], dim(tmp[[i]])[1])
  tmp[[i]] <- cbind(tmp[[i]], id)
}

save(tmp, file = "subsampChains.RData")

# classify subsamples
classify <- function(df) {
 subsampClass <- vector()
 for (i in 1:dim(df)[1]){
  if(df[i, 1] < df[i, 2]) {
   subsampClass <- c(subsampClass, 'Date-Driven')
   } else {
   subsampClass <- c(subsampClass, 'Seq-Driven')
   }
 }
 return(cbind(df, subsampClass))
}

subsampWass <- do.call(rbind, tmp)
subsampWass <- classify(subsampWass)


save(subsampWass, file = "errorWasserstein.RData")

################################################################
### Can start from here once errorWasserstein.RData is saved ###
################################################################

# Now compare to full classification
load('wassersteinData.RData')
load('errorWasserstein.RData')

# bind full sim data to subsampled data
errorClass <- subsampWass %>%
 left_join(wassersteinData, by = "id")

colnames(errorClass)[1:3] <- c("sub_wd", "sub_ws", "sub_wn")

# now add dSD
errorClass <- errorClass %>%
 mutate(dSD = abs(wd - ws)) %>%
 mutate(mismatch = case_when(
   subsampClass == class ~ 0,
   subsampClass != class ~ 1,
 )) %>%
 select(id, dSD, mismatch, sampProp, rate)

errorClass <- errorClass %>%
 group_by(id, dSD) %>%
 mutate(num = sum(mismatch)) %>%
 #select(id, dSD, num, sampProp, rate) %>%
 distinct()

# plotting
viol <- ggplot(errorClass, aes(y = dSD, x = num > 0, fill = num > 0)) +
 geom_violin(draw_quantiles = c(0.5)) + 
 geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
 xlab("") + ylab(latex2exp::TeX("$\\log_{10}(d_{SD})$")) +
 scale_x_discrete(labels = c("No Mismatch", "Some Mismatch")) +
 scale_fill_manual(values = alpha(c("dodgerblue", "red"), 0.6),
  labels = c("No Mismatch", "Some Mismatch")) +
 scale_y_continuous(trans = scales::log10_trans(),
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
 theme_minimal() +
 theme(legend.position = "none",
  legend.title = element_blank(),
  legend.text = element_text(size = 14),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_blank())

line <- ggplot(errorClass, aes(y = num, x = dSD, fill = num > 0)) + 
 geom_point(shape = 21, size = 2, color = "black") +
 geom_smooth(se = F, col = "black", fill = NA, lwd = 0.5) +
 ylab("Number of Misclassifications") + xlab(latex2exp::TeX("$\\log_{10}(d_{SD})$")) +
 scale_x_continuous(trans = scales::log10_trans(),
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
 scale_fill_manual(values=alpha(c("dodgerblue", "red"), 0.8),
 labels = c("No Mismatch", "Some Mismatch")) +
 theme_minimal() +
 theme(legend.position = "none",
  legend.title = element_blank(),
  legend.text = element_text(size = 16),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 14))
leg <- cowplot::get_legend(line + theme(legend.position = "bottom"))


p1 <- cowplot::plot_grid(line, viol, labels = "AUTO", ncol = 2)
p1 <- cowplot::plot_grid(p1, leg, ncol = 1, rel_heights = c(5, 1))

tiff(file = paste0(figPath, "errorWasserstein.tiff"), compression = "lzw",
 units = "in", width = 6, height = 5, res = 300)
 p1
dev.off()


# later
errorClass <- errorClass %>%
 ungroup()
# num mismatches out of 60000
print(sum(errorClass$mismatch))
# =329
# how many of 400 had a mismatch
length(unique(errorClass[which(errorClass$mismatch == 1), ]$id)) # = 99
