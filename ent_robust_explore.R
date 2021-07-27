library(ggplot2)
library(entropy)
library(viridis)

# function to apply burning
burnin <- function(df) {
	df <- df[-c(1:floor(0.1*dim(df)[1])),]
}

files <- dir(pattern='.+log')
log <- lapply(files, function(x) read.table(head=T, x))

for (i in 1:length(log)){
	log[[i]] <- burnin(log[[i]])
}
names(log) <- files

files <- dir(pattern='.+log')
rep <- as.numeric(gsub(gsub(files, pattern='.+REP', replacement=''), pattern='_p.+', replacement=''))
occ_prop <- as.numeric(gsub(gsub(files, pattern='.+_p', replacement=''), pattern='_chain.+', replacement=''))
chain <- as.numeric(gsub(gsub(files, pattern='.+_chain', replacement=''), pattern='.log', replacement=''))

postREnt <- vector()

for (i in 1:length(log)){
	postREnt <- c(postREnt, entropy(discretize(log[[i]][, which(grepl('reproductiveNumber', names(log[[i]])))], numBins=100)))
}

analysis <- rep('NA', times=length(files))
analysis[which(grepl(files, pattern='seqInf'))] <- 'seqInf'
analysis[which(grepl(files, pattern='dateInf'))] <- 'dateInf'

df <- as.data.frame(cbind(analysis, rep, occ_prop, postREnt, files, chain))

# looking at ent change across chains withour subsampling
#pdf('notifEntRedChains.pdf', useDingbats=F, width=15, height=10)
#	ggplot(subset(df, analysis=='seqInf'), aes(x=as.numeric(occ_prop), y=as.numeric(postREnt),  col=as.factor(chain))) + geom_point() + geom_line(aes(col=as.factor(chain))) + 
#			scale_colour_manual(values=viridis(3)) + facet_wrap(~rep, ncol=5) + 
#			xlab('Proportion of Occurrences') + ylab(expression('Posterior R'[0]*' Entropy (nats)')) + 
#			theme_bw()
#dev.off()



original <- rep(0, times=dim(df)[1])

df <- as.data.frame(cbind(df, original))

# adding subsampling for ent
tmp <- data.frame()
step <- vector()

BACKUP <- df

nSubSamp <- 100
for (i in 1:dim(df)[1]){
	for (j in 1:nSubSamp) {

		#analysis <- c(analysis, df[i,1])
		print(paste0('row_', i, 'iteration_', j))
		step <- c(step, j)
		rep <- c(rep, df[i,2])
		occ_prop <- c(occ_prop, df[i,3])
		postREnt <- c(postREnt, entropy(discretize(sample(log[[df[i, 'files']]][, which(grepl('reproductiveNumber', names(log[[df[i, 'files']]])))], 1000), numBins=100)))
		files <- c(files, df[i,5])
		original <- c(original, j)
		chain <- c(chain, df[i,6])
	}
}

# re-establishing df
df <- as.data.frame(cbind(analysis, as.numeric(rep), as.numeric(occ_prop), as.numeric(postREnt), files, as.numeric(original), as.numeric(chain)))
colnames(df) <- c('analysis', 'rep', 'occ_prop', 'postREnt', 'files', 'original', 'chain')

write.csv(df, 'chain_subsamp_data.csv', row.names=F)

# looking at ent change across chains
pdf('notifEntRedC.pdf', useDingbats=F, width=15, height=10)
	ggplot(subset(df, analysis=='seqInf'), aes(x=as.numeric(occ_prop), y=as.numeric(postREnt),  col=as.factor(chain))) + geom_point() + geom_line(aes(col=as.factor(chain))) + 
			scale_colour_manual(values=viridis(3)) + facet_wrap(~rep, ncol=5) + 
			xlab('Proportion of Occurrences') + ylab(expression('Posterior R'[0]*' Entropy (nats)')) #+ 
			theme_bw()
dev.off()

# violin across chain, rep, and occprop
ggplot(subset(df, occ_prop %in% c(0, 1) & rep %in% c(1:5)), aes(y=as.numeric(postREnt), x=interaction(as.factor(occ_prop),analysis), fill=interaction(as.factor(occ_prop),analysis,chain))) +
geom_violin(draw_quantiles = c(0.5)) + scale_fill_manual(values=alpha(viridis(12),0.2)) +
facet_wrap(~rep, ncol=1) +
	theme(legend.position='bottom')

# hists across treatment and chain layers
ggplot(subset(df, occ_prop %in% c(0, 1) & rep %in% c(1)), aes(x=as.numeric(postREnt), fill=interaction(as.factor(occ_prop),analysis))) +
geom_histogram(position='identity', bins=10) + scale_fill_manual(values=alpha(viridis(4),0.2)) +
facet_wrap(~chain, ncol=1) +
	theme(legend.position='bottom')

# plotting by occprop and by rep hists
ggplot(subset(df, analysis=='seqInf'), aes(x=as.numeric(postREnt), fill=as.factor(occ_prop))) + geom_histogram(bins=20, position='identity') +
geom_vline(data=aggregate(as.numeric(df[,'postREnt']), list(df[,'occ_prop']), median), 
      mapping=aes(xintercept=x, col=Group.1)) +
scale_fill_manual(values=alpha(viridis(5),0.2)) + scale_colour_manual(values=alpha(viridis(5),1)) +
facet_wrap(~rep)

# plotting by occprop and by rep violin
pdf(file='entByOccpropRepViolin.pdf', useDingbats=F, height=25, width=10)
	ggplot(subset(df, analysis=='seqInf'), aes(x=as.factor(occ_prop), y=as.numeric(postREnt), fill=as.factor(occ_prop))) + 
	geom_violin(draw_quantiles = c(0.5)) +
	scale_fill_manual(values=alpha(viridis(5),0.2)) + scale_colour_manual(values=alpha(viridis(5),1)) +
	facet_wrap(~rep, ncol=1) +
	theme(legend.position='bottom')
dev.off()

# looking at CoV
aggregate(as.numeric(df[,'postREnt']), list(df$occ_prop), sd)
aggregate(as.numeric(df[,'postREnt']), list(df$occ_prop), mean)

aggregate(as.numeric(df[,'postREnt']), list(df$occ_prop), sd)$x/aggregate(as.numeric(df[,'postREnt']), list(df$occ_prop), mean)$x

# e.g. comparison between occ_prop = 0 and 1
t.test(subset(df, analysis=='seqInf' & occ_prop==0.0 & rep==1)$postREnt,
		subset(df, analysis=='seqInf' & occ_prop==1 & rep==1)$postREnt)

p <- vector()
for (i in unique(df$rep)) {
	p <- c(p,
			t.test(subset(df, analysis=='seqInf' & occ_prop==0.0 & rep==i)$postREnt,
		subset(df, analysis=='seqInf' & occ_prop==1 & rep==i)$postREnt)$p.value)
}
# all 'significant'


# comparing ent subsamples across, occs, dates, and originaol data

ggplot(subset(df, occ_prop %in% c(0, 1) & rep %in% c(1)), aes(x=as.numeric(postREnt), fill=interaction(as.factor(occ_prop),analysis))) +
geom_histogram(bins=25) + scale_fill_manual(values=alpha(viridis(4),0.2))

#with violin plots
ggplot(subset(df, occ_prop %in% c(0, 1) & rep %in% c(1:5)), aes(y=as.numeric(postREnt), x=interaction(as.factor(occ_prop),analysis), fill=interaction(as.factor(occ_prop),analysis))) +
geom_violin(draw_quantiles = c(0.5)) + scale_fill_manual(values=alpha(viridis(4),0.2)) +
facet_wrap(~rep, ncol=1) +
	theme(legend.position='bottom')

# doing old ant plane plot 

# Looking at sequence or data driven signals
toolData <- subset(df, occ_prop %in% c(0,1))

toolDataParms <- unique(df[,-c(1,4,5)])


totalEnt <- vector()
dateEnt <- vector()
seqEnt <- vector()

for (i in 1:dim(toolDataParms)[1]){
	print(toolDataParms[i,])
	dat <- df[which(df[,-c(1,4,5)] %in% toolDataParms[i,]),]
	print(dat)

}



















