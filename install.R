r = getOption("repos") 
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

install.packages('shiny')
install.packages('shinyjs')
install.packages('shinyalert')

install.packages('tm')
install.packages('stringr')
install.packages('DT')
install.packages('lexiconPT')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('magrittr')
install.packages('quanteda')

install.packages('ggplot2')
install.packages('ggExtra')
install.packages('wordcloud2')
install.packages('visNetwork')