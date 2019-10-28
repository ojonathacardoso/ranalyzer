carregarCorrelacao <- function(input, output, dtm){
  
  # Remove a tag de associações de palavras, exibida na aba de correlações
  removeUI(
    selector = "div:has(> #correlacoesPalavras)"
  )
  
  # Insere a lista de palavras na aba de correlações
  insertUI(selector = "#correlacoesEspaco",
           where = "afterEnd",
           ui = selectInput("correlacoesPalavras",
                            NULL,
                            choices = sort(dataFrame$word, decreasing = FALSE)
           )
  )
  
  observeEvent(
    {
      input$correlacoesCorrSlider
      input$correlacoesQtdeSlider
      input$correlacoesBuscar
    },
    {
      withProgress({
        setProgress(message = "Gerando gráfico...")
        
        termo = input$correlacoesPalavras
        correlacao <- input$correlacoesCorrSlider / 100
        correlacoes <- as.data.frame(findAssocs(dtm, terms = termo, corlimit = correlacao))
        colnames(correlacoes)[1] <- "relacao"
        correlacoes$relacao <- correlacoes$relacao * 100
        
        correlacoes <- head(correlacoes,input$correlacoesQtdeSlider)
        #correlacoes <- sort(correlacoes, decreasing = FALSE)
        
        output$correlacoes <- renderPlot(
          ggplot(correlacoes,
                 aes(x=rownames(correlacoes),
                     y=relacao,
                     label=relacao,
                     fill=relacao)
          ) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = relacao), hjust = -0.3) +
            ggtitle(paste("Correlação com a palavra '",termo,"'")) +
            guides(fill=guide_legend(title="Correlação em %")) +
            theme(
              plot.title = element_text(size=14, face="bold", hjust=0.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=11)
            ) +
            coord_flip()
        )
      })
    })
  
}