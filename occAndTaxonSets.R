library(ape)

path <- getwd()
fastaPath <- paste0(path, '/fasta.nosync/')

aFiles <- dir(pattern='.fasta', path=fastaPath)
stem <- gsub(aFiles, pattern='.fasta', replacement='')

aln <- lapply(paste0(fastaPath, aFiles), function(x) read.dna(x, as.character=T, format='fasta'))


# sampling with replacement
add_occ <- function(prop, aln){
	prop <- prop[order(prop, decreasing=T)]
	names <- rownames(aln)
	subs <- list()
	occ_aln <- list()

	names <- sample(names, size=prop[1]*length(names), replace=FALSE)
	subs[[1]] <- names

	if (length(prop) > 1){
		for (i in 2:length(prop)){
			names <- sample(names, size=(prop[i]/prop[i-1])*length(names), replace=FALSE)
			subs[[i]] <- names

		}
	}

	for (i in 1:length(subs)){
		occ_aln[[i]] <- aln
		occ_aln[[i]][which(rownames(occ_aln[[i]]) %in% subs[[i]]),] <- rep('n', times=dim(occ_aln[[i]])[2])
	}
	names(occ_aln) <- paste0('p', prop)
	return(list(occ_aln, subs))
}

for (i in 1:length(aln)){

	occs <- add_occ(prop=c(0, 0.25, 0.5, 0.75, 1), aln=aln[[i]])

	for (j in 1:length(occs[[1]])){
		# writing fasta file
		write.dna(occs[[1]][[j]], file=paste0(fastaPath, stem[i], names(occs[[1]])[j], '.fasta'), format='fasta')
		# corresponding taxon set
		writeLines(noquote(gsub(paste(occs[[2]][[j]], collapse=" "), pattern=' ', replacement=',')), con=paste0(fastaPath, stem[i], names(occs[[1]])[j], 'taxonSet.csv'))
	}

}



