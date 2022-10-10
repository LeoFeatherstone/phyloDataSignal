#!/usr/bin/env Rscript

# script loops though all .trees files in dir and get number of unique
# be sure to run the following bash commany to make the nexus files readable/
# sed -i '' 's/.*tree STATE/tree STATE/g' *.trees

args <- commandArgs(trailingOnly = TRUE) # expect File, and burnin. Maybe later subsamp etc

suppressPackageStartupMessages(require(codetools))
suppressPackageStartupMessages(require(ape))
suppressPackageStartupMessages(require(TreeDist))

main <- function() {
 FILE_PAT <- as.character(args[1])
 BURNIN <- as.numeric(args[2])
 OUTFILE_STEM <- args[3]

 t_files <- dir(pattern = FILE_PAT, full.names = TRUE)
 distMats <- list()

 for (i in seq_along(t_files)){
  # parse trees and apply burnin
  start <- Sys.time()
  tree <- read.nexus(t_files[i])[-c(1:BURNIN)]
  end <- Sys.time()
  cat(paste0("### Read in ", t_files[i], ": ", i, " of 2400 ###"),
   file = paste0(OUTFILE_STEM, "Progress", ".txt"), sep = "\n", append = TRUE)
  cat(paste0("### Took: ", round(end - start, 2), "s"),
   file = paste0(OUTFILE_STEM, "Progress", ".txt"), sep = "\n", append = TRUE)

  # calculate mutual clustering information
  start <- Sys.time()
  distMats[[i]] <- ClusteringInfoDistance(tree, tree)
  end <- Sys.time()
  cat(paste0("### Dist mat for ", t_files[i], " calculated in ", round(end - start, 2), "s ###\n"),
   file = paste0(OUTFILE_STEM, "Progress", ".txt"), sep = "\n", append = TRUE)
 }

 # finally, name and save data
 names(distMats) <- gsub(t_files, pattern = "[.]trees", replacement = "")
 start <- Sys.time()
 save(distMats, file = paste0(OUTFILE_STEM, ".RData"))
 end <- Sys.time()
 cat(paste0("### Took: ", round(end - start, 2), "s to compress."),
  file = paste0(OUTFILE_STEM, "Progress", ".txt"), sep = "\n", append = TRUE)
}
main()