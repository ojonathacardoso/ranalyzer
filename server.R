library(reshape2)

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
      totalLinhas <<- length(textoArquivo)
      limiteLinhas <- 0
      if(!is.na(input$arquivoLinhas) && input$arquivoLinhas > 0)
        limiteLinhas <- input$arquivoLinhas
  
      textoLimitado <- textoArquivo
  
      # Se houver limite de linhas, obtém apenas a quantidade de linhas limitada
      if(limiteLinhas > 0)
      {
        textoLimitado <- textoArquivo[-((limiteLinhas+1):totalLinhas)]
        totalLinhas <<- limiteLinhas
      }
  
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
    
    observeEvent(
    {
      input$sentimentosPorFrase
      input$precisaoSentimentosOpLexicon
      input$precisaoSentimentosSentiLex
    },
    {
      withProgress({
        setProgress(message = "Gerando gráfico...")
      
        textAnalyzed <- textPrepared
        
        if(input$sentimentosPorFrase == TRUE)
        {
          textSentenced <- paste(textAnalyzed,collapse=" ")
          sentences <- tokens(textSentenced,"sentence")
          textAnalyzed <- sentences[[1]]
        }
        
        dataFrameSent <- data.frame(text = textAnalyzed)
        dataFrameSent %<>% mutate(linha_id = row_number(), text = textAnalyzed)
        dataFrameSentPalavras <- dataFrameSent %>% unnest_tokens(term, text)
        
        #
        # Ambos os dicionários
        #
        
        dataFrameSentAmbos <- dataFrameSentPalavras %>%
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
            most_neg = min(comment_sentiment_op, comment_sentiment_lex),
            most_pos = max(comment_sentiment_op, comment_sentiment_lex)
          )
          
          # Remove os outliers
          dataFrameSentAmbos %<>% filter(between(comment_sentiment_op, -10, 10))
          dataFrameSentAmbos %<>% filter(between(comment_sentiment_lex, -10, 10))
          
          output$sentimentosAmbos <- renderPlot(dataFrameSentAmbos %>%
            ggplot(aes(x = comment_sentiment_op, y = comment_sentiment_lex)) +
            scale_color_continuous(low = "red", high = "blue") +
            #geom_point() +
            geom_count() +
            geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 0, ymax = 5.5), colour='blue', alpha = 0.002) +
            geom_rect(aes(xmin = -5.5, xmax = 0, ymin = -5.5, ymax = 0), colour='red', alpha = 0.002) +
            scale_size_area() +
            labs(x = "SentiLex", y = "OpLexicon") +
            scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
            scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
            ggtitle("Análise geral com ambos os dicionários") +
            labs(size="Quantidade") +
            theme(
              plot.title = element_text(size=14, face="bold", hjust=0.5),
              axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=11),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()
            ) +
            coord_flip()
          )
          
          output$sentimentosTextosAmbos <- renderText({
            paste(
              "<b>Total de registros: </b>", length(textAnalyzed), "<br>",
              "<b>Total analisado: </b>", nrow(dataFrameSentAmbos), "<br><br>"
            )
          })
          
          #
          # OpLexicon
          #
          
          dataFrameSentOp <- dataFrameSentPalavras %>%
            inner_join(op30, by = "term") %>%
            group_by(linha_id) %>%
            summarise(
              comment_sentiment_op = sum(polarity),
              n_words = n()
            ) %>%
            ungroup() %>%
            rowwise() %>%
            mutate(
              most_neg = min(comment_sentiment_op),
              most_pos = max(comment_sentiment_op)
            ) 
          
          # Remove os outliers
          dataFrameSentOp %<>% filter(between(comment_sentiment_op, -10, 10))
          
          mid <- mean(dataFrameSentOp$comment_sentiment_op)
          
          output$sentimentosOpLexicon <- renderPlot(
            ggplot(dataFrameSentOp,
                   aes(x=linha_id,
                       y=comment_sentiment_op)
            ) +
              geom_bar(stat = "identity", aes(fill=comment_sentiment_op)) +
              #scale_color_gradient(low = "red", high = "blue") +
              scale_fill_gradient2(midpoint = mid,
                                   low = "red",
                                   mid = "grey",
                                   high = "blue",
                                   guide = "colourbar",
                                   guide_legend(title="Pontos"),
                                   limits = c(-10, 10),
                                   breaks = seq(-10, 10, 4)#,
                                   #guides(fill=guide_legend(title="Pontos"))
              )+
              geom_rect(aes(xmin = 0, xmax = 0, ymin = -10, ymax = 10)) +     # Truque pra exibir de -10 a 10
              scale_y_continuous(breaks = seq(from = -10, to = 10, by = 1)) +
              scale_x_continuous(breaks = seq(0,length(textAnalyzed), 2*input$precisaoSentimentosOpLexicon)) +
              ggtitle(paste("Análise por linha com o OpLexicon")) +
              labs(y = "Pontuação", x = "Linha") +
              theme(
                plot.title = element_text(size=14, face="bold", hjust=0.5),
                axis.text.x = element_text(size=11),
                axis.text.y = element_text(size=11),
                panel.grid.major = element_blank()
              ) +
              coord_flip()
          )
          
          most_pos <- which.max(dataFrameSentOp$most_pos)
          most_neg <- which.min(dataFrameSentOp$most_neg)
          
          output$sentimentosTextosOpLexicon <- renderText({
            paste(
              "<b>Total de registros: </b>", length(textAnalyzed), "<br>",
              "<b>Total analisado: </b>", nrow(dataFrameSentOp), "<br><br>",
              "<b>Mais positivo(a): </b><br><i>",
              dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentOp$linha_id[most_pos]],
              "</i><br><b>Pontos:</b> ", dataFrameSentOp$most_pos[most_pos],
              "<br><br>",
              "<b>Mais negativo(a): </b><br><i>",
              dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentOp$linha_id[most_neg]],
              "</i><br><b>Pontos:</b> ", dataFrameSentOp$most_neg[most_neg],
              "<br><br>"
            )
          })
          
          #
          # SentiLex
          #
          
          dataFrameSentLex <- dataFrameSentPalavras %>%
            inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>%
            group_by(linha_id) %>%
            summarise(
              comment_sentiment_lex = sum(lex_polarity),
              n_words = n()
            ) %>%
            ungroup() %>%
            rowwise() %>%
            mutate(
              most_neg = min(comment_sentiment_lex),
              most_pos = max(comment_sentiment_lex)
            ) 
          
          # Remove os outliers
          dataFrameSentLex %<>% filter(between(comment_sentiment_lex, -10, 10))
          
          mid <- mean(dataFrameSentLex$comment_sentiment_lex)
          
          output$sentimentosSentiLex <- renderPlot(
            ggplot(dataFrameSentLex,
                   aes(x=linha_id,
                       y=comment_sentiment_lex)
            ) +
              geom_bar(stat = "identity", aes(fill=comment_sentiment_lex)) +
              #scale_color_gradient(low = "red", high = "blue") +
              scale_fill_gradient2(midpoint = mid,
                                   low = "red",
                                   mid = "grey",
                                   high = "blue",
                                   guide = "colourbar",
                                   guide_legend(title="Pontos"),
                                   limits = c(-10, 10),
                                   breaks = seq(-10, 10, 4)#,
                                   #guides(fill=guide_legend(title="Pontos"))
              )+
              geom_rect(aes(xmin = 0, xmax = 0, ymin = -10, ymax = 10)) +     # Truque pra exibir de -10 a 10
              scale_y_continuous(breaks = seq(from = -10, to = 10, by = 1)) +
              scale_x_continuous(breaks = seq(0,length(textAnalyzed), 2*input$precisaoSentimentosSentiLex)) +
              ggtitle(paste("Análise por linha com o SentiLex")) +
              labs(y = "Pontuação", x = "Linha") +
              theme(
                plot.title = element_text(size=14, face="bold", hjust=0.5),
                axis.text.x = element_text(size=11),
                axis.text.y = element_text(size=11),
                panel.grid.major = element_blank()
              ) +
              coord_flip()
          )
          
          most_pos <- which.max(dataFrameSentLex$most_pos)
          most_neg <- which.min(dataFrameSentLex$most_neg)
          
          output$sentimentosTextosSentiLex <- renderText({
            paste(
              "<b>Total de registros: </b>", length(textAnalyzed), "<br>",
              "<b>Total analisado: </b>", nrow(dataFrameSentLex), "<br><br>",
              "<b>Mais positivo(a): </b><br><i>",
              dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentLex$linha_id[most_pos]],
              "</i><br><b>Pontos:</b> ", dataFrameSentLex$most_pos[most_pos],
              "<br><br>",
              "<b>Mais negativo(a): </b><br><i>",
              dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentLex$linha_id[most_neg]],
              "</i><br><b>Pontos:</b> ", dataFrameSentLex$most_neg[most_neg],
              "<br><br>"
            )
          })
          
        })
    })
    
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

    output$downloadGrafo <- downloadHandler(
      
      
    )
    
  }
  
  output$sobre <- renderText({
    paste(
      "<b>Desenvolvido por:</b>", "<br>",
      "Jonatha Martins Cardoso <ojonathacardoso@gmail.com>",
      "<br>","<br>",
      "<b>Código-fonte e instruções de instalação:</b>","<br>",
      "<a href='https://github.com/ojonathacardoso/ranalyzer'>https://github.com/ojonathacardoso/ranalyzer</a>",
      "<br>","<br>",
      "<b>Referência para citação bibliográfica:</b>","<br>",
      "CARDOSO, Jonatha Martins. <b>Mineração de texto em fontes da Internet, com enfoque na Administração Pública e Atividade Política.</b> 2019. Trabalho de Conclusão de Curso (Monografia) – Curso de Sistemas de Informação, Universidade Feevale, Novo Hamburgo, RS, 2019."
    )
  })

  cores<- c("#cbb3fd","#fdb3b6", "#b3f4fd", "#e5fdb3", "#fdb3f9", 
            "#f8fdb3", "#fde1b3", "#b11a48", "#4964fc", "#1ab185")
  
  # Recebe os dois data frames de entrada e gera o grafo (visNetwork) das disciplinas 
  # e seus pré-requisitos
  # data frame disciplinas: matriz curricular com informações de código, nome, sigla, 
  # semestre, sequência no semestre e observações
  # data frame prerequisitos: informações com pré-requisitos entre as 
  # disciplinas - origem -> destino
  # O parâmetro tipo serve para indicar se F: grafo fixo; ou M: grafo móvel
  curriculo.interativo<-function() {
    
    tipo = "M"
    
    df.disciplinas <- read.csv2('cico.csv', header=TRUE, sep=";",
                                quote="", encoding="UTF-8")
    df.prerequisitos <- read.csv2('cico_prerequisitos.csv', header=TRUE, sep=";",
                                  quote="", encoding="UTF-8")
    
    # colocando as informações do data frame de disciplinas na variável disciplinas
    disciplinas <- data.frame(id=df.disciplinas[,"codigo"],
                              nome=df.disciplinas[,"nome"],
                              sigla=df.disciplinas[,"sigla"],
                              semestre=df.disciplinas[,"semestre"],
                              num=df.disciplinas[,"num"],
                              obs=df.disciplinas[,"obs"])
    
    # Descobrindo o número de semestres do curso
    numero.semestres <- max(df.disciplinas[,"semestre"])
    
    # Colocando as informações do data frame de pré-requisitos na variável prerequisitos
    prerequisitos <- data.frame(from=df.prerequisitos[,"from"], 
                                to=df.prerequisitos[,"to"])
    
    # Definindo todas as características dos nodos (disciplinas)
    disciplinas$shape <- "square"
    disciplinas$shadow <- TRUE
    # Texto que aparece quando o mouse é colocado sobre o nodo
    disciplinas$title <- paste0("(",disciplinas$id,") ",disciplinas$nome,"<br>",
                                disciplinas$semestre,"o. semestre","<br>",
                                disciplinas$obs)
    # Texto que aparece abaixo do nodo
    disciplinas$label <- disciplinas$sigla
    #disciplinas$color <- cores[disciplinas$semestre]
    disciplinas$labelHighlightBold <- TRUE
    
    if (tipo == "F") {
      disciplinas$fixed <- TRUE
      disciplinas$x <- disciplinas$num * 350
      disciplinas$y <- disciplinas$semestre * 250
      disciplinas$font <- "32px arial black"
      disciplinas$size <- 40
      disciplinas$borderWidth <- 3
    }
    else if (tipo == "M") {
      disciplinas$font <- "18px arial black"
      disciplinas$size <- 20
      disciplinas$borderWidth <- 3
    }
    
    # Configurando arestas: pré-requisitos
    prerequisitos$arrows <- "to"
    prerequisitos$width <- 4
    prerequisitos$selectionWidth <- 10

    
    #dtm,
    #term = findFreqTerms(dtm, lowfreq=freqMinima),
    #corThreshold = correlacao
    
    
    
    # Gera um grafo de disciplinas como nodos e pré-requisitos como arestas
    grafo <- visNetwork(dtm, correlacao, 
                        margin=c(0, 0, 0, 0)) %>%
      #visInteraction(dragNodes = FALSE) %>%
      #visLayout(hierarchical = TRUE) %>%
      visOptions(highlightNearest = TRUE)
    
    return (grafo)
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
      text = "Neste menu, o arquivo a ser carregado é selecionado, sendo que o mesmo deve estar no formato TXT.
      É possível limitar o número de linhas (registros) a serem carregados.
      Após a carga, são exibidas as informações de nome, quantidade de linhas e caracteres, bem como a íntegra do mesmo.",
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
      text = "Neste menu, é exibido um grafo de palavras, apresentando a relação entre elas, e quão forte ela é.
      Nas barras laterais, é possível alterar a quantidade de palavras exibida no grafo - de 10 a 100 palavras mais frequentes.
      Também é possível ajustar a correlação, sendo que quanto maior o valor, mais forte deve ser a correlação entre as palavras pra que seja exibido um grafo.
      Também é ajustado o tamanho da imagem exibida na tela, bem como se as linhas que ligam as palavras ficam mais finas ou espessas conforme o grau de correlação.",
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
      Os pontos que estão dentro do retângulo com borda verde representam as frases ou parágrafos que foram considerados como positivos pelos dois dicionários.
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
)