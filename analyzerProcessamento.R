processarTexto <- function(textLoaded){

    # Carrega texto
    textLoaded <- iconv(textLoaded, from='UTF-8', to='ASCII//TRANSLIT')
    
    # Transforma em Corpus
    docs <- Corpus(VectorSource(textLoaded))
    
    # ???
    toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
    
    # Padrões de remoção de caracteres
    urlPat <- function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
    emlPat <- function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
    tun <- function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
    tt <- function(x) gsub("RT |via", "", x)
    
    # Remove caracteres especiais
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    
    # Remove caracteres de URLs, e-mails e Twitter.
    docs <- tm_map(docs, urlPat)
    docs <- tm_map(docs, emlPat)
    docs <- tm_map(docs, tt)
    docs <- tm_map(docs, tun)
    
    # Remove as stopwords
    stopWords <- stopwords("portuguese")
    stopWords <- iconv(stopWords, from="UTF-8", to="ASCII//TRANSLIT")
    
    docs <- tm_map(docs, removeWords, stopWords)
    
    # Transforma em matriz
    dtm <<- TermDocumentMatrix(docs)
    m <<- as.matrix(dtm)
    
    print(m)
    
    
    
    # Organiza em ordem alfabética
    v <<- sort(rowSums(m),decreasing=TRUE)
    
    # Cria um dataframe
    dataFrame <<- data.frame(word = names(v),freq=v)
    
    #print(dataFrame)
    
    op30 <<- oplexicon_v3.0
    sent <<- sentiLex_lem_PT02
    
    m <<- as.matrix(dtm)
    
    textPrepared <<- textLoaded
  
}