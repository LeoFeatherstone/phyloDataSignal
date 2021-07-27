# function to apply burnin
burnin <- function(df) {
	df <- df[-c(1:floor(0.1*dim(df)[1])),]
}

logPath <- '~/Desktop/PhD/phyloDataSignal/logFiles/'

files <- dir(path=logPath, pattern='.+.log')
log <- lapply(paste0(logPath, files), function(x) read.table(head=T, x))

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
save(comps, chain, occProp, rep, subSamp, file='posteriorRNaught.RData')