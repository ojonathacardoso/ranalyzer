carregarArquivo <- function(input){
  
  withProgress({
    setProgress(message = "Carregando arquivo...")
    
    runif(input$arquivo)
    textoArquivo <- readLines(input$arquivo$datapath, encoding="UTF-8")
    textoLimitado <- NULL
    
    # Verifica total de linhas e analisa limite informado pelo usuário
    totalLinhas <<- length(textoArquivo)
    limiteLinhas <<- 0
    if(!is.na(input$arquivoLinhas) && input$arquivoLinhas > 0)
      limiteLinhas <- input$arquivoLinhas
    
    textoLimitado <- textoArquivo
    
    # Se houver limite de linhas, obtém apenas a quantidade de linhas limitada
    if(limiteLinhas > 0)
    {
      textoLimitado <- textoArquivo[-((limiteLinhas+1):totalLinhas)]
      totalLinhas <<- limiteLinhas
    }
    
    return(textoLimitado)
    
  })
  
}