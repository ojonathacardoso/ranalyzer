carregarAjuda <- function(input){
  
  observeEvent(input$ajudaArquivo, {
    shinyalert(
      title = "Arquivo",
      text = "Neste menu, o arquivo a ser carregado é selecionado, sendo que o mesmo deve estar no formato TXT.
        É possível limitar o número de linhas (registros) a serem carregados.
        Após a carga, são exibidas as informações de nome, quantidade de linhas, caracteres, parágrafos e frases.
        Também é exibida a íntegra do mesmo, tanto dividido por parágrafos/linhas, quanto separado por frases.",
      type = "info")
  })
  
  observeEvent(input$ajudaPalavras, {
    shinyalert(
      title = "Lista de palavras",
      text = "Neste menu, é exibida uma tabela com todas as palavras que estão presentes no texto - excluídas as stopwords e caracteres especiais.
        É possível ordená-la pela ordem alfabética ou de frequência (quantidade de aparições).
        A tabela pode ser filtrada não só pesquisando um termo, como também pela barra de frequência mínima.",
      type = "info")
  })
  
  observeEvent(input$ajudaNuvem, {
    shinyalert(
      title = "Nuvem de palavras",
      text = "Neste menu, é exibida uma nuvem de palavras, sendo que o tamanho destas varia conforme a frequência de aparição no texto.
        Ao passar o mouse em cima de uma, é exibida essa frequência.
        É possível alterar o nível da frequência, em um fator que varia de 0.5 a 10. Quanto maior o valor, menos palavras aparecerão e menor será a frequência máxima de aparição.",
      type = "info")
  })
  
  observeEvent(input$ajudaGrafo, {
    shinyalert(
      title = "Grafo de palavras",
      text = "Neste menu, é exibido um grafo de palavras, apresentando a relação entre elas.
        Nas barras laterais, é possível alterar a quantidade de palavras exibida no grafo - de 10 a 100 palavras mais frequentes.
        Também é possível ajustar a correlação (5 a 100%), sendo que quanto maior o valor, mais forte deve ser a correlação entre as palavras para que a ligação seja exibida.
        Quanto maior e mais vermelha é a palavra, maior a quantidade de ocorrências. ",
      type = "info")
  })
  
  observeEvent(input$ajudaFrequencias, {
    shinyalert(
      title = "Frequências",
      text = "Neste menu, é exibido um histograma que apresenta quais são as palavras mais frequentes no texto enviado.
        É possivel exibir entre 5 e 30 palavras, desde que atendam à frequência de ocorrência mínima escolhida na barra à esquerda.
        Quanto mais claro o tom de azul, maior é seu grau de ocorrência.",
      type = "info")
  })
  
  observeEvent(input$ajudaCorrelacoes, {
    shinyalert(
      title = "Correlações",
      text = "Neste menu, é exibido um histograma que apresenta quais são as palavras que tem maior correlação com a palavra selecionada à direita.
        É possivel exibir entre 5 e 30 palavras, desde que atendam à correlação mínima escolhida na barra à esquerda - quanto maior este valor, mais forte deve ser a ligação entre as duas palavras para aparecer no gráfico.
        Quanto mais claro o tom de azul, maior é sua relação com a palavra selecionada.",
      type = "info")
  })
  
  observeEvent(input$ajudaSentimentosAmbos, {
    shinyalert(
      title = "Análise de sentimentos",
      text = "Neste menu, é exibida uma análise dos sentimentos capturados a partir do texto enviado. Para isto, faz-se uso de dois dicionários léxicos - o SentiLex e o OpLexicon. 
        Nesta tela, é exibido um gráfico de dispersão.
        Os pontos que estão dentro do retângulo com borda verde/azul representam as frases ou parágrafos que foram considerados como positivos pelos dois dicionários.
        Os pontos que estão dentro do retângulo com borda vermelha representam as frases ou parágrafos que foram considerados como negativos pelos dois dicionários.
        Quanto maior o tamanho do ponto, mais frases ou parágrafos ele representa.
        Quanto mais à direita um ponto está, mais pontos ele recebeu como positivo pelo OpLexicon, e quanto mais à esquerda, mais pontos recebeu como negativo.
        Quanto mais acima um ponto está, mais pontos ele recebeu como positivo pelo SentiLex, e quanto mais à direita, mais pontos recebeu como negativo.",
      type = "info")
  })
  
  observeEvent(input$ajudaSentimentosOpLexicon, {
    shinyalert(
      title = "Análise de sentimentos",
      text = "Neste menu, é exibida uma análise dos sentimentos capturados a partir do texto enviado.
        Para isto, faz-se uso de dois dicionários léxicos - o SentiLex e o OpLexicon. Cada um é exibido numa tela.
        Ao clicar em 'OpLexicon' ou 'SentiLex', você pode ver a análise de sentimento para cada frase/parágrafo (conforme a opção escolhida na tela 'Ambos').
        É possível ajustar a marcação das linhas exibidas no gráfico conforme é movida a barra 'Detalhamento das linhas'.
        Abaixo dessa opção, aparece a frase/parágrafo mais positiva e a mais negativa - ambos acompanhados da sua pontuação.
        Nas três telas, você pode ver quantas frases/parágrafos o texto possui e quantos foram analisados.",
      type = "info")
  })
  
}