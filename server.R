function(input, output, session) {
  
  output$arquivo <- eventReactive(input$arquivo, 
  {
    
    #######
    # Carga do arquivo
    #######
    
    source("analyzerArquivo.R")
    
    textoLimitado <- carregarArquivo(input)
    
    #############
    # Preparação do texto carregado
    #############
    
    source("analyzerProcessamento.R")
    
    processarTexto(textoLimitado)
    
    #######
    # Informações do arquivo e texto
    #######
    
    source("analyzerTexto.R")
    
    carregarTexto(input, output, textPrepared, limiteLinhas)
    
    #######
    # Nuvem de palavras
    #######
    
    source("analyzerNuvem.R")
    
    carregarNuvem(input, output, dataFrame)
    
    #######
    # Histograma de frequência de palavras
    #######
    
    source("analyzerFrequencia.R")
    
    carregarFrequencia(input, output, dtm)
    
    #######
    # Histograma de correlação de palavras
    #######
    
    source("analyzerCorrelacao.R")
    
    carregarCorrelacao(input, output, dtm)
    
    #######
    # Tabela de palavras
    #######
    
    source("analyzerTabela.R")
    
    carregarTabela(input, output, dataFrame)
    
    #######
    # Análise de sentimento
    #######
    
    source("analyzerSentimentos.R")
    
    carregarSentimentos(input, output, textPrepared)
    
    #######
    # Grafo de palavras
    #######
    
    source("analyzerGrafo.R")
    
    carregarGrafo(input, output, dataFrame)
    
    output$downloadGrafo <- downloadHandler()
    
    #######
    # Sobre o software
    #######
    
    source("analyzerSobre.R")
    
    carregarSobre(output)
    
  })
  
  #######
  # Ajuda
  #######
  
  source("analyzerAjuda.R")
  
  carregarAjuda(input)

}