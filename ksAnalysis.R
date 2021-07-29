library(ggplot2)
library(entropy)
library(viridis)
library(reshape)

# reading in data
load('posteriorRNaught.RData')

# getting path
path <- getwd()

# function for distance from posterior with full data with komolgrov-smirnov statistic
distanceCompare <- function(df) {
	OP <- c(ks.test(df$fullData, df$occData)$statistic,
			ks.test(df$fullData, df$dateEstOcc)$statistic,
				ks.test(df$fullData, df$truePrior)$statistic)
	names(OP) <- c('fullData||occData',
					'fullData||dateEstOcc',
					'fullData||truePrior')
	return(OP)
}

# creating TV data
ksDat <- data.frame()

for (i in 1:length(comps)){
	ksDat <- rbind(ksDat, distanceCompare(comps[[i]]))
}

ksDat <- as.data.frame(cbind(ksDat, occProp, chain, rep, subSamp))
colnames(ksDat) <- c('occData',
					'dateEstOcc',
					'truePrior', 
					'occProp', 'chain', 'rep', 'subSamp')

write.csv(ksDat, 'ksDat.csv', row.names=F)

backup <- ksDat

# modiofying ksDat now
ksDat <- melt(ksDat, id = c("occProp", "chain", "rep", 'subSamp'))

#exploratory figures
pdf(paste0(path, '/figures/', 'ksCounts.pdf'), height=10, width=10, useDingbats=F)
	ggplot(subset(ksDat, occProp %in% c(0, 1) & rep %in% c(1,3,7)), aes(x=value, fill=variable)) + 
	geom_histogram(position='identity', bins=2000) +
	scale_fill_manual(values = alpha(viridis(3), 0.5)) +
	facet_wrap(~rep, ncol=1)
dev.off()

pdf(paste0(path, '/figures/', 'ksDensity.pdf'), height=10, width=10, useDingbats=F)
	ggplot(subset(ksDat, occProp %in% c(0, 1) & rep %in% c(1,3,7)), aes(x=value, fill=variable)) + 
	geom_histogram(position='identity', bins=2000, stat='density') +
	scale_fill_manual(values = alpha(viridis(3), 0.5)) +
	facet_wrap(~rep, ncol=1)
dev.off()



