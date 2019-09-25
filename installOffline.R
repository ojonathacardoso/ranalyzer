setwd("RPackages")

pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]

install.packages(pkgFilenames, repos = NULL, type = "win.binary")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("BiocGenerics")
BiocManager::install("graph")
BiocManager::install("Rgraphviz")