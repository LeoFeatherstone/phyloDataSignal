# function to apply burnin 
burnin <- function(df) {
	df <- df[-c(1:floor(0.1*dim(df)[1])),]
}

path <- getwd()
logPath <- paste0(path, '/log/')

files <- dir(path=logPath, pattern='.+.log')

log <- lapply(paste0(logPath, files[1:length(files)]), function(x) read.table(head=T, x))

log <- list()
for (i in 1:length(files)){
	log[[i]] <- read.table(head=T, file=paste0(logPath, files[i]))
	print(paste('===', i, '==='))
}
backup <- log

names(log) <- gsub(files, pattern='\\.log', replacement='')

# keeping burnin for thinned analyses
for (i in 1:length(log)){
		log[[i]] <- burnin(log[[i]])
		print(i)
}

# for ess check
ess <- vector()
for (i in 1:length(log)){
	ess <- c(ess, coda::effectiveSize(coda::as.mcmc(log[[i]])[, which(grepl('reproductiveNumber', colnames(coda::as.mcmc(log[[i]]))))]))
}
names(ess) <- names(log)

# listing those that need extension
writeLines(names(ess[which(ess>200)]), con=paste0(logPath, 'sufficientESS.txt'))

# constructing a list with logs grouped in 4 for each condition
chains <- list()
#chain <- vector()
tree <- vector()
rate <- vector()
sampProp <- vector()

# looping through reps
for (t in 1:100){
	# loopng through occurrence proportions
	for (p in c(0.5,1)){
		# looping through rates
		for (r in c('1e-05', '1e-03')){
			# seq data p= (equiv. full data or amount reduced)
			name <- paste0('fullData', 't', t,'p', p, 'r', r)
			fullData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

			# seq data p=1 (equiv. occurrences)
			name <- paste0('dateData', 't', t,'p', p, 'r', r)
			dateData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

			# date date p=0 (equiv. date estiamtion or amount reduced)
			name <- paste0('seqData', 't', t,'p', p, 'r', r)
			seqData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]

			# date date p=1 (equiv. true prior)
			name <- paste0('noData', 't', t, 'p', p, 'r', r)
			noData <- log[[name]][, which(grepl('reproductiveNumber', names(log[[name]])))]
					
			maxLen <- max(length(fullData), length(dateData), length(seqData), length(noData))
			length(fullData) <- maxLen
			length(dateData) <- maxLen
			length(seqData) <- maxLen
			length(noData) <- maxLen

			elementName <- paste0('t', t, 'p', p, 'r', r)
			chains[[elementName]] <- as.data.frame(cbind(fullData, dateData, seqData, noData))

			sampProp <- c(sampProp, p)
			tree <- c(tree, t)
			rate <- c(rate, r)
		}
	}
}

save(chains, sampProp, rate, tree, ess, file='postR0.RData')
