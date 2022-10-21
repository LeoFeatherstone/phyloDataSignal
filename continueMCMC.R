#!/usr/bin/env Rscript

# A script to take a log file and returns T/F is ESS is high enough
suppressPackageStartupMessages(require(coda))

args <- commandArgs(trailingOnly = TRUE) # expect burnin, ESS thresh, log file

main <- function() {
    # For now, default parm is $R_\{0}$
    burnin <- args[1] # in rows, not actual states
    ess_thresh <- args[2]
    log_file <- args[3]

    log <- as.mcmc(read.table(head = TRUE, file = log_file)[-c(1:burnin), ])
    cols <- which(grepl("reproductiveNumber", colnames(log)))
    current_ess <- effectiveSize(log[, cols])

    if (current_ess < ess_thresh) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
main()