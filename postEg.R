### Script produces figure for example posteriors.
### In future add app to visualise all

library(ggplot2)
library(viridis)
library(reshape)
library(cowplot)
library(tidyverse)
library(ggtext)

path <- getwd()
figPath <- paste0(path, "/figures/")

# get data
load("wassersteinData.RData")

load("postR0.RData")

# selecting plot data
post <- bind_rows(chains[7:12], .id = "id") %>%
left_join(wassersteinData) %>%
pivot_longer(ends_with("Data"), values_to = "R0", names_to = "type", values_drop_na = TRUE)

# labels for facets
rate.labs <- c(("Simulation rate: 10^(-5) (subs/site/year)"), ("Simulation rate: 10^(-3) (subs/site/year)"))
names(rate.labs) <- unique(post$rate)
sampProp.labs <- c("Simulation sampling proportion = 0.5", "Simulation sampling proportion = 1.0")
names(sampProp.labs) <- unique(post$sampProp)[-1]

pdf(file = paste0(figPath, "postEg.pdf"), useDingbats = FALSE)
 ggplot(subset(post, sampProp != 0.05), aes(x = R0, fill = type, label = class)) + geom_density() +
 labs(x = expression("Posterior R"[0]), y = "Density") +
 scale_fill_manual(values = alpha(viridis(4), 0.5), 
 labels = c("Date Data", "Full Data", "Marginal Prior", "Sequence Data")) +
 geom_text(x = 1.5, y = 2, check_overlap = TRUE, size = 5) +
 xlim(1, 4) +
 facet_wrap(~rate + sampProp,
  labeller = labeller(rate = rate.labs, sampProp = sampProp.labs),
  nrow = 2, scales = "free_y") +
 theme_bw() +
 theme(legend.position="bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  strip.text = element_markdown(size = 12),
  axis.title = element_text(size = 12))
dev.off()
