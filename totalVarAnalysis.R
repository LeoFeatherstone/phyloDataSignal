library(ggplot2)
library(entropy)
library(viridis)
library(reshape)

# reading in data
load('posteriorRNaugh.RData')

# getting path
path <- getwd()

# function for distance from posterior with full data
distanceCompare <- function(df) {
	OP <- c(totVar(df$fullData, df$occData),
			totVar(df$fullData, df$dateEstOcc),
				totVar(df$fullData, df$truePrior))
	names(OP) <- c('fullData||occData',
					'fullData||dateEstOcc',
					'fullData||truePrior')
	return(OP)
}

# defining distance total variance distance function
u <- vector()
v <- vector()

totVar <- function(u, v) {
	v <- v[!(is.na(v))]
	u <- u[!(is.na(u))]
	combined <- c(u, v)

	len <- length(c(u, v))
	ran <- abs(max(combined)-min(combined))
	bins <- seq(from=min(combined), to=max(combined), by=ran/len)

	uTab <- table(cut(u, breaks=bins))/length(u)
	vTab <- table(cut(v, breaks=bins))/length(v)

	#plot.new()
	#hist(v, breaks=bins, col='red')
	#hist(u, breaks=bins, add=T, col='lightblue') 

	return(sum(abs(uTab-vTab)))
}

# creating TV data
tvDat <- data.frame()

for (i in 1:length(comps)){
	tvDat <- rbind(tvDat, distanceCompare(comps[[i]]))
}

tvDat <- as.data.frame(cbind(tvDat, occProp, chain, rep, subSamp))
colnames(tvDat) <- c('occData',
					'dateEstOcc',
					'truePrior', 
					'occProp', 'chain', 'rep', 'subSamp')

write.csv(tvDat, 'tvDat.csv', row.names=F)

backup <- tvDat

# modiofying tvDat now
tvDat <- melt(tvDat, id = c("occProp", "chain", "rep", 'subSamp'))

#exploratory figures
pdf(paste0(path, '/figures/', 'totVarCounts.pdf'), height=10, width=10, useDingbats=F)
	ggplot(subset(tvDat, occProp %in% c(0, 1) & rep %in% c(1,3,7)), aes(x=value, fill=variable)) + 
	geom_histogram(position='identity', bins=2000) +
	scale_fill_manual(values = alpha(viridis(3), 0.5)) +
	facet_wrap(~rep, ncol=1)
dev.off()

pdf(paste0(path, '/figures/', 'totVarDensity.pdf'), height=10, width=10, useDingbats=F)
	ggplot(subset(tvDat, occProp %in% c(0, 1) & rep %in% c(1,3,7)), aes(x=value, fill=variable)) + 
	geom_histogram(position='identity', bins=2000, stat='density') +
	scale_fill_manual(values = alpha(viridis(3), 0.5)) +
	facet_wrap(~rep, ncol=1)
dev.off()



