### Script collates mcmc chains for simulation study with burnin

# function to apply burnin
burnin <- function(df) {
 df <- df[-c(1:500), ]
}

path <- getwd()
logPath <- paste0(path, "/log/")

files <- dir(path = logPath, pattern = ".+.log")

log <- lapply(paste0(logPath, files), function(x) read.table(head = TRUE, x))
backup <- log

names(log) <- gsub(files, pattern = "[.]log", replacement = "")

# apply burnin
for (i in 1:seq_along(log)){
 log[[i]] <- burnin(log[[i]])
 print(i)
}

# sanity check final ESS values
ess <- vector()
library(coda)
for (i in 1:length(log)){
 colNames <- colnames(as.mcmc(log[[i]]))
 ess <- c(ess, effectiveSize(as.mcmc(log[[i]])[, which(grepl("reproductiveNumber", colNames))]))
}
names(ess) <- names(log)
# all(ess > 200) == TRUE # nolint

# constructing a list with logs grouped in 4 for each condition
chains <- list()

tree <- vector()
rate <- vector()
sampProp <- vector()

# looping through reps
for (t in 1:100){
 # loopng through occurrence proportions
 for (p in c(0.05, 0.5, 1)){
  # looping through rates
  for (r in c("1e-05", "1e-03")){
   # seq data p= (equiv. full data or amount reduced)
   name <- paste0("fullData", "t", t, "p", p, "r", r)
   fullData <- log[[name]][, which(grepl("reproductiveNumber", names(log[[name]])))]

   # seq data p=1 (equiv. occurrences)
   name <- paste0("dateData", "t", t,"p", p, "r", r)
   dateData <- log[[name]][, which(grepl("reproductiveNumber", names(log[[name]])))]

   # date date p=0 (equiv. date estiamtion or amount reduced)
   name <- paste0("seqData", "t", t,"p", p, "r", r)
   seqData <- log[[name]][, which(grepl("reproductiveNumber", names(log[[name]])))]

   # date date p=1 (equiv. true prior)
   name <- paste0("noData", "t", t, "p", p, "r", r)
   noData <- log[[name]][, which(grepl("reproductiveNumber", names(log[[name]])))]

   # equalise lengths and group posteriors				
   maxLen <- max(length(fullData), length(dateData), length(seqData), length(noData))
   length(fullData) <- maxLen
   length(dateData) <- maxLen
   length(seqData) <- maxLen
   length(noData) <- maxLen
   
   elementName <- paste0("t", t, "p", p, "r", r)
   chains[[elementName]] <- as.data.frame(cbind(fullData, dateData, seqData, noData))

   # recording simulation conditions
   sampProp <- c(sampProp, p)
   tree <- c(tree, t)
   rate <- c(rate, r)
   }
  }
}

save(chains, sampProp, rate, tree, ess, file = "postR0.RData")
