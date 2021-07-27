#!/bin/bash

path=$(pwd)
tempPath="${path}/xmlTemplates/"
opPath="${path}/xml/"

# setting up chain 1 for p>0 analyses
for stem in $(echo REP{1..10}p{0.25,0.5,0.75,1});
	do
	# dateInf
	taxa=$(cat "${path}/fasta.nosync/${stem}taxonSet.csv")
	cat "${tempPath}dateInfRedTemplate.xml" | sed "s/STEM/${stem}/g" | sed "s/TAXONSET/${taxa}/1" > "${opPath}dateInf${stem}c1.xml"
	# seqInf
	cat "${tempPath}seqInfRedTemplate.xml" | sed "s/STEM/${stem}/g" > "${opPath}seqInf${stem}c1.xml"
done

# setting up analyses for p=0
for stem in $(echo REP{1..10}p0);
	do
	# dateInf
	cat "${tempPath}dateInfRedFullDataTemplate.xml" | sed "s/STEM/${stem}/g" > "${opPath}dateInf${stem}c1.xml"
	# seqInf
	cat "${tempPath}seqInfRedTemplate.xml" | sed "s/STEM/${stem}/g" > "${opPath}seqInf${stem}c1.xml"
done

# creating additional chains
for file in $(ls "${opPath}")
do
	for i in $(echo {2..3})
	do
		stem=$(sed 's/c1\.xml//g' <<< ${file})
		cat "${opPath}${file}" | sed "s/c1\.log/c${i}\.log/g"| sed "s/c1\.trees/c${i}\.trees/g" > "${opPath}${stem}c${i}.xml"
	done
done
