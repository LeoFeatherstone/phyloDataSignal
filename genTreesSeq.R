library(ape)
library(TreeSim)

#setting seed
set.seed(1234)

# starting with a dataset of 10 trees
path <- getwd()

# making directories for trees and sequence alignments 
system('mkdir trees')
treePath <- paste0(path, '/trees/')
system('mkdir fasta')
fastaPath <- paste0(path, '/fasta.nosync/')

# making trees
trees <- lapply(1:100, function(x) sim.bdsky.stt(n=100, lambdasky=2.5, deathsky=1, sampprobsky=0.5, timesky=0, timestop=0))

#formatting tip labels
for (i in 1:length(trees)){
	trees[[i]][[1]]$tip.label <- paste0(trees[[i]][[1]]$tip.label, '_', diag(vcv.phylo(trees[[i]][[1]])))
}

# saving trees and recording names
tFiles <- vector()
for (i in 1:length(trees)){
	write.tree(trees[[i]][[1]], file=paste0(treePath, 'REP', i, '.tree'))
	tFiles <- c(tFiles, paste0('REP', i, '.tree'))
}

# generating sequences 
for (i in 1:length(tFiles)){
	system(paste0('/Applications/PHYLO_APPS/Seq-Gen-1.3.4/source/seq-gen -l 30000 -m HKY -i 0.33 -s 0.001 -of ', treePath, tFiles[i], ' >>', fastaPath, 'REP', i,'.fasta'))
}

