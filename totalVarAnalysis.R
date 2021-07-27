library(ggplot2)
library(entropy)
library(viridis)

# function to apply burning
burnin <- function(df) {
	df <- df[-c(1:floor(0.1*dim(df)[1])),]
}

files <- dir(pattern='.+log')
log <- lapply(files, function(x) read.table(head=T, x))

for (i in 1:length(log)){
	log[[i]] <- burnin(log[[i]])
}
names(log) <- files

# constructing a list with logs grouped in 4 for each condition
comps <- list()
chain <- vector()
rep <- vector()
occProp <- vector()
subSamp <- vector()
nSubSamp <- 100

# looping through reps
for (r in 1:10){
	# loopng through occurrence proportions
	for (p in c(0,0.25,0.5,0.75,1)){
		# looping though chain replicates
		for (c in 1:3){
			for (j in 1:nSubSamp){
				if (j==1) {


				# seq data p= (equiv. full data or amount reduced)
				name <- paste0('seqInfREP', r, '_p', p, '_chain', c, '.log')
				fullData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# seq data p=1 (equiv. occurrences)
				name <- paste0('seqInfREP', r, '_p', 1, '_chain', c, '.log')
				occData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# date date p=0 (equiv. date estiamtion or amount reduced)
				name <- paste0('dateInfREP', r, '_p', p, '_chain', c, '.log')
				dateEstOcc <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# date date p=1 (equiv. true prior)
				name <- paste0('dateInfREP', r, '_p', 1, '_chain', c, '.log')
				truePrior <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]
				
				maxLen <- max(length(fullData), length(occData), length(dateEstOcc), length(truePrior))
				length(fullData) <- maxLen
				length(occData) <- maxLen
				length(dateEstOcc) <- maxLen
				length(truePrior) <- maxLen

				elementName <- paste0('rep', r, '_p', p, '_chain', c, '_samp', j)
				comps[[elementName]] <- as.data.frame(cbind(fullData, occData, dateEstOcc, truePrior))

				chain <- c(chain, c)
				occProp <- c(occProp, p)
				rep <- c(rep, r)
				subSamp <- c(subSamp, j)

				} else {
									# seq data p= (equiv. full data or amount reduced)
				name <- paste0('seqInfREP', r, '_p', p, '_chain', c, '.log')
				fullData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# seq data p=1 (equiv. occurrences)
				name <- paste0('seqInfREP', r, '_p', 1, '_chain', c, '.log')
				occData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# date date p=0 (equiv. date estiamtion or amount reduced)
				name <- paste0('dateInfREP', r, '_p', p, '_chain', c, '.log')
				dateEstOcc <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

				# date date p=1 (equiv. true prior)
				name <- paste0('dateInfREP', r, '_p', 1, '_chain', c, '.log')
				truePrior <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]
				
				maxLen <- max(length(fullData), length(occData), length(dateEstOcc), length(truePrior))
				length(fullData) <- maxLen
				length(occData) <- maxLen
				length(dateEstOcc) <- maxLen
				length(truePrior) <- maxLen

				elementName <- paste0('rep', r, '_p', p, '_chain', c, '_samp', j)
				comps[[elementName]] <- as.data.frame(cbind(fullData, occData, dateEstOcc, truePrior))
				comps[[elementName]] <- comps[[elementName]][sample(1:dim(comps[[elementName]])[1], 1000),]
				
				chain <- c(chain, c)
				occProp <- c(occProp, p)
				rep <- c(rep, r)
				subSamp <- c(subSamp, j)
				}

			}

		}
	}
}

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
# testing
#distanceCompare(comps[[49]])
#distanceCompare(rnorm(0,1,n=100), rnorm(1,1,n=100)))
# I think it's very sensitive to bins argument 


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

ggplot(subset(tvDat, occProp %in% c(0, 1) & rep %in% c(1,3,7)), aes(x=value, fill=variable)) + 
geom_histogram(position='identity', bins=2000) +
scale_fill_manual(values = alpha(viridis(3), 0.5)) +
facet_wrap(~rep, ncol=1)

ggplot(tvDat, aes(x=value, fill=variable)) + 
geom_histogram(position='identity', bins=20000) +
scale_fill_manual(values = alpha(viridis(3), 0.5)) +
facet_wrap(~rep*chain, ncol=5, scales='free_x') +
theme(legend.position='none')

