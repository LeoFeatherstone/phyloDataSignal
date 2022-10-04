# getting site patterns
library(ape)

# starting with a dataset of 10 trees
path <- getwd()
fastaPath <- paste0(path, '/fasta/')

# loading sequence data and calculating num site patterns and mult. Likelihood
aFiles <- dir(pattern='.fasta', path=fastaPath)
stem <- gsub(aFiles, pattern='.fasta', replacement='')

## NB reading in as DNAbin object for easy access of site patterns
aln <- lapply(paste0(fastaPath, aFiles), function(x) read.dna(x, format='fasta'))

sitePatterns <- unlist(lapply(aln, function(x) attr(phangorn::phyDat(x, return.index=T, type='DNA'), 'nr')))
names(sitePatterns) <- stem

save(sitePatterns, file='sitePatterns.RData')
