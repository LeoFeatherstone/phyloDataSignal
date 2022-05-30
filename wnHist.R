library(tidyverse)
library(viridis)

path <- getwd()
figPath <- paste0(path, '/figures/')

range(dat$noDataW) # = 0.104805 1.210893
dat <- dat %>% mutate(rate = as.numeric(gsub(id, pattern='.+r', replacement='')))
rate.labs <- c(('0.00001 (subs/site/year)'), ('0.001 (subs/site/year)'))
#rate.labs <- c(expression('10'^{-3}~'(subs/site/year)'), expression('10'^{-5}~'(subs/site/year)'))
names(rate.labs) <- unique(dat$rate)
sampProp.labs <- c('p=0.5', 'p=1.0')
names(sampProp.labs) <- unique(dat$sampProp)

p<-	ggplot(dat) +
	geom_histogram(aes(x=abs(noDataW)), fill='dodgerblue')+
	facet_wrap(~rate*sampProp,
		labeller = labeller(rate = rate.labs, sampProp=sampProp.labs),
				nrow=2) +
	xlim(0,1.25) +
	theme_minimal() +
	xlab(latex2exp::TeX('$W_{N}$')) +
	theme(legend.position='none', 
		legend.title=element_text(size=14),
		legend.text=element_text(size=14), 
		axis.title=element_text(size=14),
		axis.text=element_text(size=12))


pdf(file=paste0(figPath, 'wnHist.pdf'), useDingbats=F)
	p
dev.off()


