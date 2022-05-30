library(tidyverse)
library(viridis)
load('combinedData.Rdata')

path <- getwd()
figPath <- paste0(path, '/figures/')

dat$sitePatterns <- as.numeric(dat$sitePatterns)
dat$datePatterns <- as.numeric(dat$datePatterns)

p<- ggplot(dat) +
	geom_point(aes(x=as.numeric(sitePatterns), y=as.numeric(datePatterns), fill=wassersteinClass), shape=21, size=2) +
	xlab('Site Patterns') + ylab(latex2exp::TeX("Date patterns ($\\frac{Sample\\~Span}{Height}$)")) +
	facet_wrap(~paste('Sampling Proportion =', sampProp)*paste('Rate =', as.numeric(gsub(id, pattern='.+r', replacement='')), 'subs/site/time'), scales='free_x') +
	scale_fill_manual(values=alpha(viridis(2), 0.5)) +
	theme_minimal() +
	theme(legend.position='bottom', 
		legend.title=element_blank(),
		legend.text=element_text(size=14), 
		axis.title=element_text(size=14),
		axis.text=element_text(size=12),
		axis.text.x=element_text(angle=0))

pdf(file=paste0(figPath, 'seqDatePatterns.pdf'), useDingbats=F, width=10)
	p 
dev.off()