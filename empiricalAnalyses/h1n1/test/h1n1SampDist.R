# looking at distribution of sampling times to gauge
# R0 time breakpoint

aln <- ape::read.dna(paste0(getwd(), "/empiricalAnalyses/h1n1/NorthAm.Nov.fasta"), format = "fasta")
#sampDates <- as.Date(lubridate::date_decimal(
# as.numeric(
 # gsub(rownames(aln), pattern = ".+\\|.+\\|", replacement = ""))))

sampDates <- as.numeric(gsub(rownames(aln), pattern = ".+\\|.+\\|", replacement = ""))
hist(sampDates, breaks = 50)
# looks to be a split at 2009.6
lubridate::date_decimal(2009.6) # "2009-08-07 23:59:59 UTC"

# Now calculating height value for change time
max(sampDates) - 2009.6 # = 0.315

# shallowest origin
max(sampDates) - min(sampDates) # = 0.666