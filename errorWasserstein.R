# Script to look at error bars on wasserstein stat via subsampling.
# Are they larger than diff between posteriors 
library(tidyverse)

path <- getwd()
figPath <- paste0(path, '/figures/')

load('postR0.RData')

# function for distance from posterior with full data
wassersteinCompare <- function(df) {
	OP <- data.frame()
	for (i in 1:100) {
	
		fullData <- sample(na.omit(df$fullData), ceiling(0.5*length(na.omit(df$fullData))), replace=F)
		dateData <- sample(na.omit(df$dateData), ceiling(0.5*length(na.omit(df$dateData))), replace=F)
		seqData <- sample(na.omit(df$seqData), ceiling(0.5*length(na.omit(df$seqData))), replace=F)
		noData <- sample(na.omit(df$noData), ceiling(0.5*length(na.omit(df$noData))), replace=F)

	
		OP <- rbind(OP, c(transport::wasserstein1d(fullData, dateData),
					transport::wasserstein1d(fullData, seqData),
					transport::wasserstein1d(fullData, noData),
					coda::effectiveSize(coda::as.mcmc(fullData)),
					coda::effectiveSize(coda::as.mcmc(dateData)),
					coda::effectiveSize(coda::as.mcmc(seqData)),
					coda::effectiveSize(coda::as.mcmc(noData))
					))
		}

		# note burnin is already removed in getPosteriors.R, producting chains object
	names(OP) <- c('fullData||dateData', 'fullData||seqData', 'fullData||noData', 
		'fullDataESS', 'dateDataESS', 'seqDataESS', 'noDataESS')
	return(OP)
		
}

set.seed(1234)
tmp <- lapply(chains, function(x) wassersteinCompare(x))

# classify subsamples
classify <- function(df){
	wassersteinClass <- vector()
	for (i in 1:dim(df)[1]){
		if(df[i,1]<df[i,2]){
			wassersteinClass <- c(wassersteinClass, 'Date-Driven')
		} else {
			wassersteinClass <- c(wassersteinClass, 'Seq-Driven')
		}
	}
	return(cbind(df, wassersteinClass))
}

errorClass <- lapply(tmp, function(x) classify(x))
save(errorClass, file='errorWasserstein.RData')

################################################################
### Can start from here once errorWasserstein.RData is saved ###
################################################################

# Now compare to full classification
load('wassersteinData.RData')
load('errorWasserstein.RData')

# convert errorClass to a tibble to check match between subsample and full-data classification 
completeAnalysisClass <- as.data.frame(cbind(names(wassersteinClass), unname(wassersteinClass)))
colnames(completeAnalysisClass) <- c('id', 'completeAnalysisClass')

errorClass <- bind_rows(errorClass, .id='id') %>% left_join(completeAnalysisClass, by='id')


# finding mismatch, add difference in Wdate and Wseq to compare this with
## need to rename columns with pipe
wassersteinDist <- cbind(rownames(wassersteinDist), wassersteinDist)
colnames(wassersteinDist)[1:4] <- c('id', 'dateDataW', 'seqDataW', 'noDataW')

diff <- wassersteinDist %>% mutate(dSD = abs(dateDataW-seqDataW)) %>% select(dateDataW, seqDataW, noDataW, dSD,id)

errorClass <- errorClass %>% mutate(mismatch = as.numeric(wassersteinClass != completeAnalysisClass)) %>% 
				left_join(diff, by='id')

# num mismatches by id
tmp <- errorClass %>% select(id, mismatch) %>% group_by(id) %>% summarise(num=sum(mismatch))
# linking to diff
diff <- diff %>% left_join(tmp, by='id') 

# plotting
viol <- ggplot(diff, aes(y=dSD, x=num>0, fill=num>0)) + 
		geom_violin(draw_quantiles = c(0.5)) + 
		geom_point(shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1) +
		xlab('') + ylab(latex2exp::TeX('$\\log_{10}(d_{SD})$')) +
		scale_x_discrete(labels=c('No Mismatch', 'Some Mismatch')) +
		#scale_fill_manual(values=alpha(viridis::viridis(2), 0.8), labels=c('No Mismatch', 'Some Mismatch')) +
		scale_fill_manual(values=alpha(c('dodgerblue', 'red'), 0.6), labels=c('No Mismatch', 'Some Mismatch')) +
		scale_y_continuous(trans = scales::log10_trans(),
   			breaks = scales::trans_breaks("log10", function(x) 10^x),
    		labels = scales::trans_format("log10", scales::math_format(10^.x))) +
		theme_minimal() +
		theme(legend.position='none', 
				legend.title=element_blank(),
				legend.text=element_text(size=14),
				axis.title=element_text(size=16),
				axis.text=element_text(size=14))

line <- ggplot(diff, aes(y=num, x=dSD, fill=num>0)) + 
		geom_point(shape = 21,size=2,color="black") +
		geom_smooth(se=F, col='black', fill=NA, lwd=0.5) +
		ylab('Number of Misclassifications') + xlab(latex2exp::TeX('$\\log_{10}(d_{SD})$')) +
		scale_x_continuous(trans = scales::log10_trans(),
   			breaks = scales::trans_breaks("log10", function(x) 10^x),
    		labels = scales::trans_format("log10", scales::math_format(10^.x))) +
		#scale_fill_manual(values=alpha(viridis::viridis(2), 0.8), labels=c('No Mismatch', 'Some Mismatch')) +
		scale_fill_manual(values=alpha(c('dodgerblue', 'red'), 0.8), labels=c('No Mismatch', 'Some Mismatch')) +
		theme_minimal() +
		theme(legend.position='none', 
				legend.title=element_blank(),
				legend.text=element_text(size=16),
				axis.title=element_text(size=14),
				axis.text=element_text(size=14))

leg <- cowplot::get_legend(ggplot(diff, aes(y=num, x=dSD, fill=num>0)) + 
		geom_point(shape = 21,size=2,color="black") +
		geom_smooth(se=F, col='steelblue', fill=NA) +
		ylab('Number of Misclassifications') + xlab(latex2exp::TeX('$\\log_{10}(|W_{S_{R_{0}}} - W_{D_{R_{0}}}|)$')) +
		scale_x_continuous(trans = scales::log10_trans(),
   			breaks = scales::trans_breaks("log10", function(x) 10^x),
    		labels = scales::trans_format("log10", scales::math_format(10^.x))) +
		#scale_fill_manual(values=alpha(viridis::viridis(2), 0.8), labels=c('No Mismatch', 'Some Mismatch')) +
		scale_fill_manual(values=alpha(c('dodgerblue', 'red'), 0.8), labels=c('No Mismatch', 'Some Mismatch')) +
		theme_minimal() +
		theme(legend.position='bottom', 
				legend.title=element_blank(),
				legend.text=element_text(size=14),
				axis.title=element_text(size=14),
				axis.text=element_text(size=14)))

p1 <- cowplot::plot_grid(line, viol, labels='AUTO', ncol=2)

pdf(paste0(figPath, 'errorWasserstein.pdf'), useDingbats=F, height=5, width=8)
	cowplot::plot_grid(p1, leg, ncol=1, rel_heights=c(5,1))
dev.off()

# num mismatches out of 40000
print(sum(errorClass$mismatch))
# =329
# how many of 400 had a mismatch
length(unique(errorClass[which(errorClass$mismatch == 1),]$id)) # = 17

