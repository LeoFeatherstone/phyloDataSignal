# clasifying by wasserstein statistic
load('postR0.RData')

# function for distance from posterior with full data
wassersteinCompare <- function(df) {
	OP <- c(transport::wasserstein1d(na.omit(df$fullData), na.omit(df$dateData)),
			transport::wasserstein1d(na.omit(df$fullData), na.omit(df$seqData)),
				transport::wasserstein1d(na.omit(df$fullData), na.omit(df$noData)))
	names(OP) <- c('fullData||dateData',
					'fullData||seqData',
					'fullData||noData')
	return(OP)
}

tmp <- lapply(chains, function(x) wassersteinCompare(x))
wassersteinDist <- data.frame()

for (i in 1:length(tmp)){
	wassersteinDist <- rbind(wassersteinDist, tmp[[i]])
}

rownames(wassersteinDist) <- names(tmp)
colnames(wassersteinDist) <- names(tmp[[1]])

# classify
wassersteinClass <- vector()
for (i in 1:dim(wassersteinDist)[1]){
	if(wassersteinDist[i,1]<wassersteinDist[i,2]){
		wassersteinClass <- c(wassersteinClass, 'Date-Driven')
	} else {
		wassersteinClass <- c(wassersteinClass, 'Seq-Driven')
	}
}
names(wassersteinClass) <- names(chains)

save(wassersteinDist, wassersteinClass, file='wassersteinData.RData')


