library(ape)
library(lubridate)

# exploring alignments for num site patters, date dist, potentially root height
path <- getwd()
fastaPath <- paste0(path, '/clustrersForLeo/')

aFiles <- dir(path=fastaPath, pattern='.+fasta')

aln <- lapply(paste0(fastaPath, aFiles), function(x) read.dna(x, format='fasta', as.character=T))

dims <- lapply(aln, function(x) dim(x)) # pretty sure they're aligned b/c torsten's magic num sites

# getting dates
getDates <- function(alignment){
	return(as.numeric(gsub(rownames(alignment), pattern='.+\\@', replacement='')))
}

dates <- lapply(aln, function(x) getDates(x))

# getting sit epatterns
segSites <- function(mat){
	sites <- 0
	for(i in 1:dim(mat)[2]){
		if (length(unique(mat[,i])) > 1){
			sites <- sites+1
		}
	}
	return(sites)
}

sites <- lapply(aln, function(x) segSites(x))

# selecting those without NA dates
noNA <- vector()
for (i in 1:length(dates)){
	if(!any(is.na(dates[[i]]))){
		noNA <- c(noNA, i)
	} 
}
# looks like 1 and 9 are the go

sites[noNA]
dates[noNA]
aln[noNA]

#seeing date dist
par(mfrow=c(1,2))
hist(as.Date(date_decimal(dates[[noNA[1]]])), breaks=60)
hist(as.Date(date_decimal(dates[[noNA[2]]])), breaks=60)

## considering root and origin heights
# set 1
max(dates[[noNA[1]]])-min(dates[[noNA[1]]])
# set 2
max(dates[[noNA[2]]])-min(dates[[noNA[2]]])

# both 0.142 convenitnetly == > root > 0.142 ==> origin in (root)
