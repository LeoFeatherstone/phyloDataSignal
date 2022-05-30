library(ape)

# starting with a dataset of 10 trees
path <- getwd()
treePath <- paste0(path, '/trees/')

# get data
tFiles <- dir(path=treePath, pattern='t.+.tree')
trees <- lapply(paste0(treePath, tFiles), function(x) read.tree(x))

names <- vector()
getSampTimes <- function(names) {
	return(as.numeric(gsub(names, pattern='t.+_', replacement='')))
}

sampTimes <- lapply(trees, function(x) getSampTimes(names=x$tip.label))

# testing out sampling entropy
times <- vector
# date span statistic
# If sampling times are Ti, = (Tmax-Tmin)/Tmax

normDateSpan <- function(times){
	Tmax = max(times)
	Tmin = min(times)
	return((Tmax-Tmin)/Tmax)
}

datePatterns <- unlist(lapply(sampTimes, function(x) normDateSpan(times=x)))
names(datePatterns) <- gsub(tFiles, pattern='.tree', replacement='')

save(datePatterns, file='datePatterns.RData')