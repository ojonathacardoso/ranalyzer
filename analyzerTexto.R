carregarTexto <- function(input, output, texto, limiteLinhas){
  
  textoPorParagrafo = ""
  textoPorFrase = ""
  
  totalCaracteres <- 0
  numLinha <- 0
  for(paragrafo in texto)
  {
    numLinha <- numLinha + 1
    totalCaracteres = totalCaracteres + str_length(paragrafo)
    
    textoPorParagrafo <- paste(textoPorParagrafo,
                               "<p>",
                               "<b>",numLinha,"</b>:",
                               paragrafo)
    
  }
  
  textoDividido <- paste(texto,collapse=" ")
  frases <- tokens(textoDividido,"sentence")
  
  numFrase <- 0
  for(frase in frases[[1]])
  {
    numFrase <- numFrase + 1
    textoPorFrase <- paste(textoPorFrase,
                           "<p>",
                           "<b>",numFrase,"</b>:",
                           frase)
  }
  
  output$arquivoInf <- renderUI({
    
    limiteTexto <- ""
    if(limiteLinhas > 0 && totalLinhas > limiteLinhas)
    {
      limiteTexto <- paste("<div style='color: red;'>
                            ATENÇÃO! Foram selecionadas apenas as primeiras ",
                           limiteLinhas,
                           " linhas do arquivo selecionado
                        </div>")
    }
    
    HTML(paste(
      "<b>Nome: </b>", input$arquivo$name, "<br>",
      "<b>Total de caracteres: </b>", format(round(as.numeric(totalCaracteres), 1), nsmall=0, big.mark="."), "<br>",
      "<b>Total de parágrafos/linhas: </b>", format(round(as.numeric(totalLinhas), 1), nsmall=0, big.mark="."), "<br>",
      "<b>Total de frases: </b>", format(round(as.numeric(numFrase), 1), nsmall=0, big.mark="."), "<br>",
      limiteTexto
    ))
  })
  
  output$arquivoPar <- renderUI({
    HTML(paste(textoPorParagrafo))
  })
  
  output$arquivoFra <- renderUI({
    HTML(paste(textoPorFrase))
  })
  
}