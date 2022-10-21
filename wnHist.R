library(tidyverse)
library(viridis)

path <- getwd()
figPath <- paste0(path, '/figures/')

load("wassersteinData.RData")

range(wassersteinData$wn) # 0.06231813 1.01906838

#dat <- dat %>% mutate(rate = as.numeric(gsub(id, pattern='.+r', replacement='')))
#rate.labs <- c(('0.00001 (subs/site/year)'), ('0.001 (subs/site/year)'))
rate.labs <- c("10^-3 (subs/site/year)", "10^-5 (subs/site/year)")
names(rate.labs) <- unique(wassersteinData$rate)
sampProp.labs <- c("p = 0.05", 'p=0.5', 'p=1.0')
names(sampProp.labs) <- unique(wassersteinData$sampProp)

p<-	ggplot(wassersteinData) +
	geom_histogram(aes(x=abs(wn)), fill='dodgerblue')+
	facet_wrap(~rate*sampProp,
		labeller = labeller(rate = rate.labs, sampProp=sampProp.labs)) +
	xlim(0,1.25) +
	theme_minimal() +
	xlab(latex2exp::TeX('$W_{N}$')) +
	theme(legend.position='none', 
		legend.title=element_text(size=14),
		legend.text=element_text(size=14), 
		axis.title=element_text(size=14),
		axis.text=element_text(size=12))


tiff(paste0(figPath, 'wnHist.tiff'), compression = "lzw", 
 units = "in", height = 4, width = 6, res = 300)
	p
dev.off()


