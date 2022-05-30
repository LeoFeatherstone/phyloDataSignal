# script for plane figures of rate, sampling, and dates patterns against W statistics
library(ggplot2)
library(viridis)
library(reshape)
library(cowplot)
library(tidyverse)
library(latex2exp)
library(ggpmisc)
library(ggExtra)

path <- getwd()
figPath <- paste0(path, '/figures/')


load('postR0.RData')
load('polarWassersteinConversion.RData')
load('datePatterns.RData')
load('sitePatterns.RData')
load('combinedData.Rdata')

trsup <- data.frame(x=c(0,0,1),y=c(0,1,1))
trinf <- data.frame(x=c(0,1,1),y=c(0,0,1))

p1 <-	ggplot(dat, aes(x=abs(dateDataW), y=abs(seqDataW))) +
		xlim(0,1) + ylim(0,1) +
		# shade quad 1
		geom_polygon(aes(x=x, y=y), data=trsup, fill=alpha("grey", 0.5)) +
		geom_polygon(aes(x=x, y=y), data=trinf, fill=alpha("white", 0.5)) +
		geom_quadrant_lines(linetype = "solid") +
		coord_fixed(ratio = 1) +
		#geom_point(pch=16, size=2.5, aes(col=wassersteinClass)) +
		geom_point(pch=21, size=2.5, fill=alpha('steelblue', 0.7)) +
			geom_quadrant_lines(linetype = "solid") +
	  		#scale_x_continuous(limits = symmetric_limits) +
	  		#scale_y_continuous(limits = symmetric_limits) 
	  		#scale_x_discrete(expand=c(0,0)) +
  			#scale_y_discrete(expand=c(0,0)) +
			#scale_fill_manual(name='Classification', values=alpha(viridis(2), 0.5)) +
			#scale_fill_gradient(name='Dissonance (r)', low=alpha(c('white'), 1), high=alpha(c('steelblue'), 1), breaks=c(0.25,1.25), labels=c('lower', 'higher')) +
			#scale_color_manual(name='', values=alpha(viridis(2), 0.8)) +
			xlab(TeX('W_{D}')) + ylab(TeX('W_{S}')) +	
			#annotate("segment", x = -1, xend = 1, y = -1, yend = 1, colour = "black", lty=2) +
			#annotate("segment", x = -1, xend = 1, y = 1, yend = -1, colour = "black", lty=2) +
			annotate("text", 	x = c(0.5, 0.85), 
							y = c(0.85,0.5), 
			label = c("Date-Driven", "Seq-Driven"), size=5) +
			theme_minimal() +
			guides(fill = guide_colourbar(ticks = F)) +
			theme(legend.position='bottom', 
				legend.title=element_text(size=14),
				legend.text=element_text(size=14),
				axis.title=element_text(size=14),
				axis.text=element_text(size=12))
p1 <-  ggMarginal(p1, size=7, type='histogram', fill=alpha('steelblue', 0.6))

# site pattern plot
p2 <- ggplot(dat, aes(x=abs(dateDataW), y=abs(seqDataW), fill=as.numeric(sitePatterns))) +
		xlim(0,1) + ylim(0,1) +
		# shade quad 1
		geom_polygon(aes(x=x, y=y), data=trsup, fill=alpha("grey", 0.5)) +
		geom_polygon(aes(x=x, y=y), data=trinf, fill=alpha("white", 0.5)) +
		geom_quadrant_lines(linetype = "solid") +
		geom_point(pch=21, size=2.5) +
		coord_fixed(ratio = 1) +
		scale_fill_continuous(name='Site Patterns', type='viridis', breaks = seq(0,800, by=200)) +
		xlab(TeX('W_{D}')) + ylab(TeX('W_{S}')) +
		#annotate("segment", x = -1, xend = 1, y = -1, yend = 1, colour = "black", lty=2) +
		#annotate("segment", x = -1, xend = 1, y = 1, yend = -1, colour = "black", lty=2) +
		annotate("text", 	x = c(0.5, 0.85), 
							y = c(0.85,0.5), 
			label = c("Date-Driven", "Seq-Driven"), size=5) +
		theme_minimal() +
		theme(legend.position='bottom', 
			legend.title=element_text(size=14),
			axis.title=element_text(size=14),
			axis.text=element_text(size=12))

# date pattern plot
trsup <- data.frame(x=c(0,0,1),y=c(0,1,1))
trinf <- data.frame(x=c(0,1,1),y=c(0,0,1))

p3 <- ggplot(dat, aes(x=abs(dateDataW), y=abs(seqDataW), fill=as.numeric(datePatterns))) +
		xlim(0,1) + ylim(0,1) +
		# shade quad 1
		geom_polygon(aes(x=x, y=y), data=trsup, fill=alpha("grey", 0.5)) +
		geom_polygon(aes(x=x, y=y), data=trinf, fill=alpha("white", 0.5)) +
		geom_quadrant_lines(linetype = "solid") +
		geom_point(pch=21, size=2.5) +
		coord_fixed(ratio = 1) +
		scale_fill_continuous(name='Relative Date Span', type='viridis') +
		xlab(TeX('W_{D}')) + ylab(TeX('W_{S}')) +
		#annotate("segment", x = -1, xend = 1, y = -1, yend = 1, colour = "black", lty=2) +
		#annotate("segment", x = -1, xend = 1, y = 1, yend = -1, colour = "black", lty=2) +
		annotate("text", 	x = c(0.5, 0.85), 
							y = c(0.85,0.5), 
			label = c("Date-Driven", "Seq-Driven"), size=5) +
		theme_minimal() +
		theme(legend.position='bottom', 
			legend.title=element_text(size=14),
			axis.title=element_text(size=14),
			axis.text=element_text(size=12))

# W vs sampProp
source('ggSplitViolin.R')
newDat <- dat %>% pivot_longer(ends_with("DataW"), values_to='wasserstein', names_to='type', values_drop_na=T)

p4 <- ggplot(subset(newDat, type!='noDataW'), aes(x=sampProp, y=wasserstein, fill=(type))) +
		geom_split_violin() +
		#scale_y_continuous(breaks=seq(0,1,by=0.25), limits=c(0,1)) +
		scale_fill_manual(values=alpha(viridis(2), 0.8), labels=c(expression(W[D]), expression(W[S]))) +
		xlab('Sampling Proportion') + ylab(expression(W[D]~or~W[S])) +
		theme_minimal() +
		theme(legend.position='bottom', 
			legend.title=element_blank(),
			legend.text=element_text(size=14), 
			axis.title=element_text(size=14),
			axis.text=element_text(size=12)) +
		coord_fixed(ratio = 1) 


pdf(file=paste0(figPath, 'wassersteinDistro.pdf'), useDingbats=F, width=10, height=10)
	plot_grid(p1, p2, p3, p4, labels='AUTO', ncol=2)
dev.off()





