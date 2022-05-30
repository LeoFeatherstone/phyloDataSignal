library(ggplot2)
library(viridis)
library(cowplot)
library(tidyverse)

path <- getwd()
figPath <- paste0('../figures/')

# get data
load('empWassersteinData.RData')
load('empPostR0.RData')

# making plot data
metaData <- as.data.frame(cbind(empWassersteinClass, names(empWassersteinClass)))
colnames(metaData) <- c('wassersteinClass', 'id')

dat <- bind_rows(empChains, .id='id') %>% left_join(metaData) %>% 
pivot_longer(ends_with("Data"), values_to='R0', names_to='post', values_drop_na=T)

id.labs <- c('Cluster 1', 'Cluster 2')
names(id.labs) <- names(empChains)

# plotting posts
pdf(paste0(figPath, 'empPosteriorR0.pdf'), useDingbats=F, height=10, width=13)
	ggplot(dat, aes(x=R0, fill=post, label=wassersteinClass)) + geom_density() +
			labs(x=latex2exp::TeX('Posterior $R_{0}$')) +
			scale_fill_manual(values=alpha(viridis(4),0.5)) +
			geom_text(x=2, y=2.05, check_overlap=T, size=7) +
			xlim(0.5,2.5) +
			facet_wrap(~id, ncol=2, scales='free_y', labeller = labeller(id = id.labs)) +
			theme_minimal() +
		theme(legend.position='bottom', 
			legend.title=element_blank(),
			legend.text=element_text(size=14), 
			axis.title=element_text(size=14),
			axis.text=element_text(size=12),
			strip.text.x = element_text(size = 14, face='bold'))
dev.off() 