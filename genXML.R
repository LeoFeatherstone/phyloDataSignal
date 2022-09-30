# Script to write xml teplates to individual analyses, 
# providing rate and origin parameters to fix as it goes.

# paths to imnput and output
path <- getwd()
fastaPath <- paste0(path, '/fasta/')
tempXMLPath <- paste0(path, '/xmlTemplates/')
xmlPath <- paste0(path, '/xml/')
load(paste0(path, '/trees/originData.RData'))

sampProp <- gsub(xmlData$tree, pattern='.+p', replacement='')
xmlData <- cbind(xmlData, sampProp)

# xml templates
xmlTempsNames <- dir(pattern='.+Template.xml', path=tempXMLPath)
xmlTemps <- lapply(xmlTempsNames, function(x) readLines(con=paste0(tempXMLPath, x)))
names(xmlTemps) <- gsub(xmlTempsNames, pattern='Template.xml', replacement='')

# fix rate, origin, fasta-stem, and then write
for(i in 1:dim(xmlData)[1]){
	tmp <- xmlTemps
	tmp <- lapply(tmp, function(x) gsub(x, pattern='ORIGIN', replacement=xmlData[i,2]))
	tmp <- lapply(tmp, function(x) gsub(x, pattern='RATE', replacement=format(as.numeric(xmlData[i,3]), scientific=F)))
	tmp <- lapply(tmp, function(x) gsub(x, pattern='SAMPPROP', replacement=format(as.numeric(xmlData[i,4]), scientific=F)))
	fastaStem <- paste0(xmlData[i,1], 'r', xmlData[i,3])
	tmp <- lapply(tmp, function(x) gsub(x, pattern='STEM', replacement=fastaStem))

	for (j in 1:length(tmp)){
		writeLines(tmp[[j]], con=paste0(xmlPath, names(tmp[j]), fastaStem, '.xml'))
	}
}

