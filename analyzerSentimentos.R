carregarSentimentos <- function(input, output, texto){
  
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
        
        textAnalyzed <- texto
        
        legendaGrafico <- "Linha/Parágrafo"
        
        if(input$sentimentosPorFrase == TRUE)
        {
          textSentenced <- paste(textAnalyzed,collapse=" ")
          sentences <- tokens(textSentenced,"sentence")
          textAnalyzed <- sentences[[1]]
          legendaGrafico <- "Frase"
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
            scale_fill_gradient2(midpoint = mid,
                                 low = "red",
                                 mid = "grey",
                                 high = "blue",
                                 guide = "colourbar",
                                 guide_legend(title="Pontos"),
                                 limits = c(-10, 10),
                                 breaks = seq(-10, 10, 4)#,
            )+
            geom_rect(aes(xmin = 0, xmax = 0, ymin = -10, ymax = 10)) +   # Truque pra exibir de -10 a 10
            scale_y_continuous(breaks = seq(from = -10, to = 10, by = 1)) +
            scale_x_continuous(breaks = seq(0,length(textAnalyzed), input$precisaoSentimentosOpLexicon)) +
            ggtitle(paste("Análise por",legendaGrafico,"com o OpLexicon")) +
            labs(y = "Pontuação", x = legendaGrafico) +
            theme(
              plot.title = element_text(size=14, face="bold", hjust=0.5),
              axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=11),
              panel.grid.major = element_blank()
            ) +
            coord_flip()
        )
        
        most_pos_op <- which.max(dataFrameSentOp$most_pos)
        most_neg_op <- which.min(dataFrameSentOp$most_neg)
        
        pontos_most_pos_op <- dataFrameSentOp$most_pos[most_pos_op]
        pontos_most_neg_op <- dataFrameSentOp$most_neg[most_neg_op]
        
        output$sentimentosTextosOpLexicon <- renderUI({
          HTML(paste(
            "<b>Total de registros: </b>", length(textAnalyzed), "<br>",
            "<b>Total analisado: </b>", nrow(dataFrameSentOp), "<br><br>",
            "<b>Mais positivo(a): </b><br><i>",
            dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentOp$linha_id[most_pos_op]],
            "</i><br><b>Pontos:</b> ", pontos_most_pos_op,
            "<br><br>",
            "<b>Mais negativo(a): </b><br><i>",
            dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentOp$linha_id[most_neg_op]],
            "</i><br><b>Pontos:</b> ", pontos_most_neg_op,
            "<br><br>"
          ))
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
            scale_fill_gradient2(midpoint = mid,
                                 low = "red",
                                 mid = "grey",
                                 high = "blue",
                                 guide = "colourbar",
                                 guide_legend(title="Pontos"),
                                 limits = c(-10, 10),
                                 breaks = seq(-10, 10, 4)#,
            )+
            geom_rect(aes(xmin = 0, xmax = 0, ymin = -10, ymax = 10)) +     # Truque pra exibir de -10 a 10
            scale_y_continuous(breaks = seq(from = -10, to = 10, by = 1)) +
            scale_x_continuous(breaks = seq(0,length(textAnalyzed), input$precisaoSentimentosSentiLex)) +
            ggtitle(paste("Análise por",legendaGrafico,"com o SentiLex")) +
            labs(y = "Pontuação", x = legendaGrafico) +
            theme(
              plot.title = element_text(size=14, face="bold", hjust=0.5),
              axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=11),
              panel.grid.major = element_blank()
            ) +
            coord_flip()
        )
        
        most_pos_lex <- which.max(dataFrameSentLex$most_pos)
        most_neg_lex <- which.min(dataFrameSentLex$most_neg)
        
        pontos_most_pos_lex <- dataFrameSentLex$most_pos[most_pos_lex]
        pontos_most_neg_lex <- dataFrameSentLex$most_neg[most_neg_lex]
        
        output$sentimentosTextosSentiLex <- renderText({
          paste(
            "<b>Total de registros: </b>", length(textAnalyzed), "<br>",
            "<b>Total analisado: </b>", nrow(dataFrameSentLex), "<br><br>",
            "<b>Mais positivo(a): </b><br><i>",
            dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentLex$linha_id[most_pos_lex]],
            "</i><br><b>Pontos:</b> ", pontos_most_pos_lex,
            "<br><br>",
            "<b>Mais negativo(a): </b><br><i>",
            dataFrameSent$text[dataFrameSent$linha_id == dataFrameSentLex$linha_id[most_neg_lex]],
            "</i><br><b>Pontos:</b> ", pontos_most_neg_lex,
            "<br><br>"
          )
        })
        
      })
    })
  
}