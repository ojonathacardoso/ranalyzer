shinyServer(

function(input, output, session) {

  shinyjs::disable("downloadGrafo")
  
  inputArquivo <- function() {
    
    withProgress({
      setProgress(message = "Carregando arquivo...")
      
      shinyjs::disable("downloadGrafo")
      
      runif(input$arquivo)
      textoArquivo <- readLines(input$arquivo$datapath, encoding="UTF-8")
      textoLimitado <- NULL
      
      # Verifica total de linhas e analisa limite informado pelo usuário
      totalLinhas <- length(textoArquivo)
      limiteLinhas <- 0
      if(!is.na(input$arquivoLinhas) && input$arquivoLinhas > 0)
        limiteLinhas <- input$arquivoLinhas
  
      textoLimitado <- textoArquivo
  
      # Se houver limite de linhas, obtém apenas a quantidade de linhas limitada
      if(limiteLinhas > 0)
        textoLimitado <- textoArquivo[-((limiteLinhas+1):totalLinhas)]
  
      textoCompleto = ""
  
      # Verifica total de caracteres
      totalCaracteres <- 0
  
      for(x in textoArquivo)
      {
        totalCaracteres = totalCaracteres + str_length(x)
        textoCompleto <- paste(textoCompleto, "<p>", x)
      }
  
      # Carrega arquivo
      prepararDados(textoLimitado)
    })
    
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

    #######
    # Informações do arquivo
    #######

    output$arquivoInfo <- renderText({
      limiteTexto <- ""
      if(limiteLinhas > 0 && totalLinhas > limiteLinhas)
      {
        limiteTexto <- paste("<div style='color: red;'>
                            ATENÇÃO! Foram selecionadas apenas as primeiras ",
                            limiteLinhas,
                            " linhas do arquivo selecionado
                        </div>")
      }

      paste(
            "<b>Nome: </b>", input$arquivo$name, "<br>",
            "<b>Caracteres: </b>", format(round(as.numeric(totalCaracteres), 1), nsmall=0, big.mark="."), "<br>",
            "<b>Linhas: </b>", format(round(as.numeric(totalLinhas), 1), nsmall=0, big.mark="."), "<br>",
            limiteTexto
            )
    })


    #######
    # Texto completo
    #######

    output$arquivoTexto <- renderText(
    {
      textoCompleto
    })

    #######
    # Nuvem de palavras
    #######

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

    #######
    # Grafo de palavras
    #######

    observeEvent(
    {
      input$grafosCorrSlider
      input$grafosTamanhoSlider
      input$grafosFreqSlider
      input$grafosPesoLinhas
    },
    {
      output$grafos <- renderImage(
      {
        outfile <- tempfile(fileext='.png')

        ajusteTamanho <- 1
        if(input$grafosCorrSlider < 15)
          ajusteTamanho <- 3

        correlacao = input$grafosCorrSlider

        # O tamanho da imagem a ser exibida é influenciado pela correlação
        # Não fazer isto, possibilita gerar imagens com os grafos sobrepostos entre si.
        tamanhoReal = (0.4-correlacao)*10000*ajusteTamanho
        tamanhoEscala = tamanhoReal * (input$grafosTamanhoSlider/100)

        withProgress({
          setProgress(message = "Gerando grafo...")
        
          # A frequência mínima usada no findFreqTerms baseia-se na quantidade 
          # de palavras mais frequentes escolhida pelo usuário
          freqMinima <- head(dataFrame, input$grafosQtdeSlider)[input$grafosQtdeSlider,2]
        
          png(outfile, width=tamanhoReal, height=tamanhoReal)
          plot(dtm,
               term = findFreqTerms(dtm, lowfreq=freqMinima),
               corThreshold = correlacao,
               weighting = input$grafosPesoLinhas,
               attrs=list(node=list(width=5,fontsize=15,fontcolor="blue",color="red"))
          )
          dev.off()
        })
        
        # Retorna o grafo
        list(src = outfile,
             alt = "Grafo",
             width = tamanhoEscala,
             height = tamanhoEscala)
      },
      deleteFile = TRUE)
      
      shinyjs::enable("downloadGrafo")
    })

    #######
    # Histograma de frequência de palavras
    #######

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

    #######
    # Histograma de correlação de palavras
    #######

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

    #######
    # Tabela de palavras
    #######

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
    
    #######
    # Análise de sentimento
    #######
    
    data("oplexicon_v3.0")
    data("sentiLex_lem_PT02")

    dataFrameSent <- data.frame(text = textPrepared)
    dataFrameSent %<>% mutate(linha_id = row_number(), text = textPrepared)
    dataFrameSentPalavras <- dataFrameSent %>% unnest_tokens(term, text)

    dataFrameSentPalavras <- dataFrameSentPalavras %>%
      inner_join(op30, by = "term") %>%
      inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>%
      group_by(linha_id) %>%
      summarise(
        comment_sentiment_op = sum(polarity),
        comment_sentiment_lex = sum(lex_polarity),
        n_words = n()
      ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(
        most_neg = min(comment_sentiment_lex, comment_sentiment_op),
        most_pos = max(comment_sentiment_lex, comment_sentiment_op)
      )

    # Remove os outliers
    dataFrameSentPalavras %<>% filter(between(comment_sentiment_op, -10, 10))
    dataFrameSentPalavras %<>% filter(between(comment_sentiment_lex, -10, 10))

    output$sentimentos <- renderPlot(dataFrameSentPalavras %>%
      ggplot(aes(x = comment_sentiment_op, y = comment_sentiment_lex)) +
      geom_point(aes(color = n_words), shape = 18, size = 6) +
      scale_color_continuous(low = "green", high = "red") +
      labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
      scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
      ggtitle("Análise de sentimento") +
      labs(color="Palavras") +
      theme(
        plot.title = element_text(size=14, face="bold", hjust=0.5),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      coord_flip()
    )

    most_pos <- which.max(dataFrameSentPalavras$most_pos)
    most_neg <- which.min(dataFrameSentPalavras$most_neg)
    
    output$sentimentosFrases <- renderText({
      paste(
        "<b>Mais positiva: </b><br>",
        dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentPalavras$linha_id[most_pos]],
        "<br><br><br>",
        "<b>Mais negativa: </b><br>",
        dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentPalavras$linha_id[most_neg]],
        "<br><br>"
      )
    })
    
    output$downloadGrafo <- downloadHandler(
      
      filename = function() {
        paste("grafo", "png", sep=".")
      },
      
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        
        correlacao = input$grafosCorrSlider
        
        # O tamanho da imagem a ser exibida é influenciado pela correlação
        # Não fazer isto, possibilita gerar imagens com os grafos sobrepostos entre si.
        tamanhoReal = 5000

        png(file, width=tamanhoReal, height=tamanhoReal)

        # A frequência mínima usada no findFreqTerms baseia-se na quantidade
        # de palavras mais frequentes escolhida pelo usuário
        freqMinima <- head(dataFrame, input$grafosQtdeSlider)[input$grafosQtdeSlider,2]

        plot(dtm,
             term = findFreqTerms(dtm, lowfreq=freqMinima),
             corThreshold = correlacao,
             weighting = input$grafosPesoLinhas,
             attrs=list(node=list(width=5,fontsize=15,fontcolor="blue",color="red"))
        )
        
        dev.off()
      } 
    )
    
  }
  
  output$arquivo <- eventReactive(input$arquivo, 
  {
    output$arquivoInfo <- renderText({""})
    output$arquivoTexto <- renderText({""})

    inputArquivo()
  })
  
  observeEvent(input$ajudaArquivo, {
    shinyalert(
      title = "Arquivo",
      text = "Neste menu, o arquivo a ser carregado é selecionado, sendo que o mesmo deve estar no formato TXT. É possível limitar o número de linhas (registros) a serem carregados. Após a carga, são exibidas as informações de nome, quantidade de linhas e caracteres, bem como a íntegra do mesmo.",
      type = "info")
  })
  
  observeEvent(input$ajudaPalavras, {
    shinyalert(
      title = "Lista de palavras",
      text = "Neste menu, é exibida uma tabela com todas as palavras que estão presentes no texto - excluídas as stopwords e caracteres especiais. É possível ordená-la pela ordem alfabética ou de frequência (quantidade de aparições). A tabela pode ser filtrada não só pesquisando um termo, como também pela barra de frequência mínima.",
      type = "info")
  })
  
  observeEvent(input$ajudaNuvem, {
    shinyalert(
      title = "Nuvem de palavras",
      text = "Neste menu, é exibida uma nuvem de palavras, sendo que o tamanho destas varia conforme a frequência de aparição no texto. Ao passar o mouse em cima de uma, é exibida essa frequência. É possível alterar o nível da frequência, em um fator que varia de 0.5 a 10. Quanto maior o valor, menos palavras aparecerão e menor será a frequência máxima de aparição.",
      type = "info")
  })
  
  observeEvent(input$ajudaGrafo, {
    shinyalert(
      title = "Grafo de palavras",
      text = "Neste menu, é exibido um grafo de palavras, apresentando a relação entre elas, e quão forte ela é. Nas barras laterais, é possível alterar a quantidade de palavras exibida no grafo - de 10 a 100 palavras mais frequentes. Também é possível ajustar a correlação, sendo que quanto maior o valor, mais forte deve ser a correlação entre as palavras pra que seja exibido um grafo. Também é ajustado o tamanho da imagem exibida na tela, bem como se as linhas que ligam as palavras ficam mais finas ou espessas conforme o grau de correlação.",
      type = "info")
  })
  
  observeEvent(input$ajudaFrequencias, {
    shinyalert(
      title = "Frequências",
      text = "Neste menu, é exibido um histograma que apresenta quais são as palavras mais frequentes no texto enviado. É possivel exibir entre 5 e 30 palavras, desde que atendam à frequência de ocorrência mínima escolhida na barra à esquerda. Quanto mais claro o tom de azul, maior é seu grau de ocorrência.",
      type = "info")
  })
  
  observeEvent(input$ajudaCorrelacoes, {
    shinyalert(
      title = "Correlações",
      text = "Neste menu, é exibido um histograma que apresenta quais são as palavras que tem maior correlação com a palavra selecionada à direita. É possivel exibir entre 5 e 30 palavras, desde que atendam à correlação mínima escolhida na barra à esquerda - quanto maior este valor, mais forte deve ser a ligação entre as duas palavras para aparecer no gráfico. Quanto mais claro o tom de azul, maior é sua relação com a palavra selecionada.",
      type = "info")
  })
  
  observeEvent(input$ajudaSentimentos, {
    shinyalert(
      title = "Análise de sentimentos",
      text = "Neste menu, é exibida uma análise dos sentimentos capturados a partir do texto enviado. Para isto, faz-se uso de dois dicionários léxicos - o SentiLex e o OpLexicon. Quanto mais a direita os pontos estão, mais positivo ele é para o SentiLex, e quanto mais para cima os pontos estão, mais positivo ele é para o OpLexicon - lembrando que não são exibidos pontos para todas as linhas, mas é feita apenas uma análise geral. Além disto, à esquerda são exibidas as frases consideradas como mais positiva e mais negativa.",
      type = "info")
  })
  
}
)