## adding date and site pattern data for plot as the 'dat' data frame
# used by scripts: 
load('postR0.RData')
load('wassersteinData.RData')
load('datePatterns.RData')
load('sitePatterns.RData')

## Site patterns plot: load and date data
# integrating data
#wassersteinData <- as_tibble(cbind(wassersteinDist, wassersteinClass, names(wassersteinClass)))
wassersteinData <- as_tibble(cbind(wassersteinDist, wassersteinClass, names(wassersteinClass)))
colnames(wassersteinData)[5] <- 'id'

# combining patern and sampling data
sitePatterns <- as_tibble(cbind(names(sitePatterns), sitePatterns))
colnames(sitePatterns) <- c('id', 'sitePatterns')

sampProp <- as_tibble(cbind(names(chains), sampProp))
colnames(sampProp) <- c('id', 'sampProp')

dat <- left_join(left_join(wassersteinData, sitePatterns, .id='id'), sampProp, .id='id')
dat <- dat %>% mutate(id2 = gsub(id, pattern='r.+', replacement=''))

datePatterns <- as_tibble(cbind(names(datePatterns), datePatterns))
colnames(datePatterns) <- c('id2', 'datePatterns')

# linking in datePatterns
dat <- left_join(dat, datePatterns, .id='id2')

colnames(dat)[1:3] <- c('dateDataW', 'seqDataW', 'noDataW')

save(dat, file='combinedData.Rdata')