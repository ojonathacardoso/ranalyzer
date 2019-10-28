carregarTabela <- function(input, output, dataFrame){
  
  output$palavras <- DT::renderDataTable(
    DT::datatable(
      {
        subset(dataFrame, dataFrame$freq >= input$palavrasSlider)
      },
      class = 'cell-border stripe',
      rownames = FALSE,
      colnames = c('Palavra', 'Frequência'),
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
        pageLength = 20
      )
    )
  )
  
}