carregarFrequencia <- function(input, output, dtm){
  
  observeEvent({
    input$frequenciasFreqSlider
    input$frequenciasQtdeSlider
  },
  {
    withProgress({
      setProgress(message = "Gerando gráfico...")
      
      term.freq <- sort(rowSums(as.matrix(dtm)), decreasing=TRUE)
      term.freq <- subset(term.freq, term.freq >= input$frequenciasFreqSlider)
      palavras <- data.frame(term = names(term.freq), freq = term.freq)
      
      palavras <- head(palavras,input$frequenciasQtdeSlider)
      
      output$frequencias <- renderPlot(
        ggplot(palavras, aes(x=term,
                             y=freq,
                             label=freq,
                             fill=freq)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = freq), hjust = -0.3) +
          theme(
            plot.title = element_text(size=14, face="bold", hjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=11)
          ) +
          ggtitle("Palavras mais frequentes") +
          guides(fill=guide_legend(title="Frequência")) +
          coord_flip()
      )
    })
  })
  
}