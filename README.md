# phyloDataSignal
All scripts needed to reproduce simulation and empirical analyses in [Paper to Come](https://www.INSERTLATER.com)

## Breakdown
- `\*.R`: R scripts to simulate data, wrangle data, and produce figures  
- `figures/`: Figures from the paper produced by some of the above scripts  
- `xmlTemplates/`: Template xml files for simulation study  
- `empiricalAnalyses/`: Contains all xml templates and R scripts to analyse empirical data  
- `ms`: Manuscript files
## Note for reproducing the study
+ If you want to reproduce the simulation study, clone the repo and add the following directories:
	- `trees/` (stores backbaone trees)  
	- `fasta/` (store simulated alignments)    
	- `log/` (stores log files for individual analyses)  
	- `xml/` (stores .xml files for individual analyses)  
+ Be sure to change `#SEQ-GEN-PATH` to the correct path for [Seq-Gen](https://github.com/rambaut/Seq-Gen) in genTreesSeq.R
## Order to Run Scripts
+ Simulate Data
	1. genTreesSeq.R (simulateed alignments take ~50Gb of memory!)
	2. genXML.R
	3. BEAST v2 for each .xml in `xml/`. Ensure [feast](https://github.com/tgvaughan/feast) and [BDMM-Prime](https://github.com/tgvaughan/BDMM-Prime) are installed.
+ Collect Data - The above all generate .RData files used in the following
	1. getPosteriors.R
	2. wassersteinClassification.R
	3. sitePatterns.R
	4. datePatterns.R
	5. combinePatternsAndWasserstein.R
+ Other R scripts named after each figure in `/figures` should now produce figures

