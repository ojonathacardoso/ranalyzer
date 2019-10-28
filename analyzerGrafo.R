carregarGrafo <- function(input, output, dataFrame){
  
  observeEvent(
    {
      input$grafosCorrSlider
      input$grafosQtdeSlider
    },
    {
      withProgress({
        setProgress(message = "Gerando grafo...")
        
        #
        # DataFame de palavras
        #
        
        palavrasFrequentes <- data.frame(words=head(dataFrame, input$grafosQtdeSlider)$word,
                                         freqs=head(dataFrame, input$grafosQtdeSlider)$freq)
        
        maiorFreq <- head(dataFrame, 1)$freq
        
        # Pego as palavras e crio um array delas.
        palavrasLista <- strsplit(toString(palavrasFrequentes$words), ", ")    
        
        palavras <- data.frame(id=1:length(palavrasLista[[1]]),
                               palavra_base=palavrasLista,
                               frequencia=head(dataFrame, input$grafosQtdeSlider)$freq)
        
        names(palavras)[2] <- "palavra_base"
        names(palavras)[3] <- "frequencia"
        
        palavras$label <- palavras$palavra_base
        palavras$title <- paste0(palavras$frequencia, " ocorrências")
        
        palavras$shadow <- TRUE
        palavras$fixed <- TRUE
        palavras$labelHighlightBold <- TRUE
        palavras$font <- "40px arial #FF4500"
        palavras$borderWidth <- 1
        
        cores <- c("#8B0000",
                   "#B22222",
                   "#A52A2A",
                   "#FF0000",
                   "#FF6347",
                   "#FF7F50",
                   "#FFA07A",
                   "#E9967A",
                   "#FA8072")
        
        palavras$color <- cores[ceiling(palavras$id/10)]
        
        # Cálculos para posicionar as palavras em uma grade de 5 colunas por X linhas
        xyPosition = palavras$id/5
        palavras$y <- (ceiling(xyPosition)) * 350
        palavras$x <- (palavras$id - ((floor(xyPosition))*5)) * 350
        
        # Tamanho proporcional à frequência.
        # Tamanho 50 indica palavra mais frequente.
        palavras$size <- 10 + ((palavras$frequencia/maiorFreq) * 50)
        
        #
        # DataFame de correlações
        #
        
        base <- c(-1)
        correlacionadas <- c(-1)
        
        for(iBase in 1:length(palavrasLista[[1]]))
        {
          associacoes <- data.frame(word=(findAssocs(dtm,
                                                     palavrasLista[[1]][iBase],
                                                     corlimit=input$grafosCorrSlider/100)[1]))
          names(associacoes)[1] <- "palavra_base"
          
          for(iAssoc in 1:nrow(associacoes))
          {
            iCorr <- which(palavras$palavra_base==rownames(associacoes)[iAssoc])
            
            if(! identical(iCorr, integer(0)))
            {
              # Testa apenas as correlações que não foram anteriormente adicionadas.
              if(iCorr > iBase)
              {
                base <- c(base, iBase)
                correlacionadas <- c(correlacionadas, iCorr)
              }
            }
          }
        }
        
        correlacoes <- data.frame(from=base, 
                                  to=correlacionadas)
        
        correlacoes$arrows <- "to"
        correlacoes$width <- 1
        correlacoes$selectionWidth <- 10
        
        output$grafo <- renderVisNetwork({
          
          grafo <- visNetwork(palavras,
                              correlacoes, 
                              width = "100%",
                              margin=c(0, 0, 0, 0)) %>%
            visInteraction(dragNodes = TRUE, navigationButtons = TRUE) %>%
            visEdges(color = list(highlight = "#8B0000", hover = "#A52A2A")) %>% 
            visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                               labelOnly = FALSE, hover = TRUE),) 
        })
      })
    })
  
  
  
}