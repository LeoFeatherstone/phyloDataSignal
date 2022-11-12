#!/usr/bin/env Rscript --vanilla

# A script to take a log file and returns T if chain needs continuing for ESS threshold
suppressPackageStartupMessages(require(coda))

args <- commandArgs(trailingOnly = TRUE) # expect burnin, ESS thresh, log file

main <- function() {
 # For now, default parm is $R_\{0}$
 burnin <- args[1] # in rows, not actual states
 ess_thresh <- args[2]
 log_file <- args[3]
 parms <- args[4]

 log <- as.mcmc(read.table(head = TRUE, file = log_file)[-c(1:burnin), ])
 cols <- which(grepl(parms, colnames(log)))
 current_ess <- effectiveSize(log[, cols])
	
 cat(as.numeric(current_ess),
  file = paste0(gsub(log_file, pattern = ".log", replacement=""), "_ESS.txt"),
  sep = "\n", append = TRUE)

 if (any(as.numeric(current_ess) < as.numeric(ess_thresh))) {
  return("TRUE")
 }# returns empty otherwise
}
main()
