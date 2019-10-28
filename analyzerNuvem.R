carregarNuvem <- function(input, output, dataFrame){
  
  output$nuvem <- renderWordcloud2(
    {
      withProgress({
        setProgress(message = "Gerando nuvem...")
        
        wordcloud2(data = dataFrame,
                   size=input$nuvemSlider,
                   backgroundColor='white',
                   shape = 'diamond')
      })
    })
  
}