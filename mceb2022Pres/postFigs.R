library(ggplot2)
library(viridis)
library(reshape)
library(cowplot)
library(tidyverse)

path <- getwd()
figPath <- paste0(path, '/figs/')

# get data
load('../wassersteinData.RData')
load('../postR0.RData')

# making plot data
metaData <- as.data.frame(cbind(rate, sampProp, wassersteinClass, names(wassersteinClass)))
colnames(metaData) <- c('rate', 'sampProp', 'wassersteinClass', 'id')


post <- bind_rows(chains[c(6)], .id='id') %>% left_join(metaData) %>% pivot_longer(ends_with("Data"), values_to='R0', names_to='post', values_drop_na=T)

pdf(paste0(figPath, 'postEg1.pdf'), useDingbats=F, height=10, width=13)
	ggplot(post, aes(x=R0, fill=(post), label=wassersteinClass)) + geom_density() +
			labs(x=expression('Posterior R'[0]), y='Density') +
			scale_fill_manual(values=alpha(viridis(4),0.5), 
				labels=c('Date Data', 'Full Data', 'Marginal Prior', 'Sequence Data')) +
			geom_text(x=4.5, y=1, check_overlap=T) +
			theme_bw() +
			theme(legend.position='bottom',
				legend.title=element_blank()) 
dev.off()

post <- bind_rows(chains[c(5:7,32)], .id='id') %>% left_join(metaData) %>% pivot_longer(ends_with("Data"), values_to='R0', names_to='post', values_drop_na=T)

id.labs <- c(rep('Tree 2',3), 'Tree 8')
names(id.labs) <- names(chains[c(5:7,32)])
rate.labs <- c(('0.00001 (subs/site/year)'), ('0.001 (subs/site/year)'))
#rate.labs <- c(expression('10'^{-3}~'(subs/site/year)'), expression('10'^{-5}~'(subs/site/year)'))
names(rate.labs) <- unique(post$rate)
sampProp.labs <- c('p=0.5', 'p=1.0')
names(sampProp.labs) <- unique(post$sampProp)

pdf(paste0(figPath, 'postEg4.pdf'), useDingbats=F, height=10, width=13)
	ggplot(post, aes(x=R0, fill=(post), label=wassersteinClass)) + geom_density() +
			labs(x=expression('Posterior R'[0]), y='Density') +
			scale_fill_manual(values=alpha(viridis(4),0.5), 
				labels=c('Date Data', 'Full Data', 'Marginal Prior', 'Sequence Data')) +
			geom_text(x=4.5, y=1, check_overlap=T) +
			facet_wrap(id~rate~sampProp,
				labeller = labeller(id = id.labs, rate = rate.labs, sampProp=sampProp.labs),
				nrow=2) +
			theme_bw() +
			theme(legend.position='bottom',
				legend.title=element_blank()) 
dev.off()






