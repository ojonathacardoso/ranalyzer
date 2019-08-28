install.packages('shiny')
install.packages('shinyjs')
install.packages('shinyalert')

install.packages('ggplot2')
install.packages('ggExtra')

install.packages('wordcloud2')

install.packages('tm')
install.packages('stringr')
install.packages('DT')
install.packages('lexiconPT')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('magrittr')

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("BiocGenerics")
BiocManager::install("graph")
BiocManager::install("Rgraphviz")