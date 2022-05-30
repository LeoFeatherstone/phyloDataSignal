# working out origin stats
library(ape)

aFiles <- dir(pattern='T.+.fasta')
fasta <- lapply(aFiles, function(x) read.dna(x, format='fasta'))

dates <- lapply(fasta, function(x) as.numeric(gsub(rownames(x), pattern='.+_', replacement='')))

ranges <- lapply(dates, function(x) range(x))
names(ranges) <- aFiles

# 1 week = 7/365 = 0.019 decimal years
# therefore set origin max and min value to [minSampDate+0.019+minSampDate]

unifRanges <- lapply(ranges, function(x) return(c(min(x)-0.019, min(x))))

#$TN3_GC.7.117.101.140.fasta
#[1] 2020.555 2020.574

#$TN3_GC.7.117.502.114.fasta
#[1] 2020.456 2020.475


# want height of oldest samples here for origin exponential prior offset
oldestHeight <- lapply(dates, function(x) max(x)-min(x))
names(oldestHeight) <- aFiles

#$TN3_GC.7.117.101.140.fasta
#[1] 0.142

#$TN3_GC.7.117.502.114.fasta
#[1] 0.142


