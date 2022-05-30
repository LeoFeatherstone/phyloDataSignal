library(ape)
library(TreeSim)
library(NELSI)



# starting with a dataset of 10 trees
path <- getwd()

# making directories for trees and sequence alignments 
#system('mkdir trees')
treePath <- paste0(path, '/trees/')
#system('mkdir fasta.nosync')
fastaPath <- paste0(path, '/fasta.nosync/')

# making trees
#setting seed
set.seed(1234)
trees1 <- lapply(1:100, function(x) sim.bdsky.stt(n=50, lambdasky=2.5, deathsky=1, sampprobsky=0.5, timesky=0, timestop=0))
set.seed(1234)
trees2 <- lapply(1:100, function(x) sim.bdsky.stt(n=100, lambdasky=2.5, deathsky=1, sampprobsky=1, timesky=0, timestop=0))

# data for naming
sampProp <- c(rep(0.5, times=100), rep(1, times=100))
treeNum <- rep(1:100, times=2)

#sanity check of subtree via sampling
#which(diag(vcv.phylo(trees1[[1]][[1]])) %in% diag(vcv.phylo(trees2[[1]][[1]])))

# combining tree lists
trees <- c(trees1, trees2)

# getting info on height for origins 
origin <- vector()
for (i in 1:length(trees)){
	origin <- c(origin, max(allnode.times(trees[[i]][[1]], tipsonly=T)+trees[[i]][[1]]$root.edge))
}

#formatting tip labels
for (i in 1:length(trees)){
	trees[[i]][[1]]$tip.label <- paste0(trees[[i]][[1]]$tip.label, '_', diag(vcv.phylo(trees[[i]][[1]])))
}

# saving trees and recording names
# format tNUMpPROP.treerRATE.tree
tFiles <- vector()
for (i in 1:length(trees)){
	write.tree(trees[[i]][[1]], file=paste0(treePath, 't', treeNum[i], 'p', sampProp[i], '.tree'))
	tFiles <- c(tFiles, paste0('t', treeNum[i], 'p', sampProp[i]))
}


# generating sequences under JC
# NB, omitting the -f and 0t flags under the HKY results in the JC as base frequencies are assumed equal and TS/TV = 0.5
for (i in 1:length(tFiles)){
	# r = 10^-3
	system(paste0('/Applications/PHYLO_APPS/Seq-Gen-1.3.4/source/seq-gen -z 4321 -l 20000 -m HKY -s 0.001 -of ', treePath, tFiles[i], '.tree', ' >>', fastaPath, tFiles[i], 'r1e-03','.fasta'))
	# r= 10^-5
	system(paste0('/Applications/PHYLO_APPS/Seq-Gen-1.3.4/source/seq-gen -z 4321 -l 20000 -m HKY -s 0.00001 -of ', treePath, tFiles[i], '.tree', ' >>', fastaPath, tFiles[i], 'r1e-05','.fasta'))
}

# saving xml data
xmlData <- as.data.frame(cbind(rep(tFiles, each=2), rep(origin, each=2), rep((c('1e-03', '1e-05')), times=100)))
colnames(xmlData) <- c('tree', 'origin', 'rate')
save(xmlData, file=paste0(treePath, 'originData.RData'))

# generating sequences under HKY
#for (i in 1:length(tFiles)){
#	# r = 10^-3
#	system(paste0('/Applications/PHYLO_APPS/Seq-Gen-1.3.4/source/seq-gen -z 4321 -l 30000 -m HKY -i 0.33 -s 0.001 -of ', treePath, tFiles[i], ' >>', fastaPath, 't', i, 'n', 50, 'r1e-03','.fasta'))
	# r= 10^-5
#	system(paste0('/Applications/PHYLO_APPS/Seq-Gen-1.3.4/source/seq-gen -z 4321 -l 30000 -m HKY -i 0.33 -s 0.00001 -of ', treePath, tFiles[i], ' >>', fastaPath, 't', i, 'n', 50, 'r1e-05','.fasta'))
#}
