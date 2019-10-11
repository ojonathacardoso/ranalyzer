fluidPage(
  
  shinyjs::useShinyjs(), 
  
  title="RAnalyzer",
  
  useShinyalert(),
  
  fluidRow(
    img(height = 70, src = "logo.png")
  ),

  fluidRow(
      # Cabeçalho 
      tags$head(
        tags$style(HTML("
                      .sidebar { height: 500px; overflow-y: auto; }
                      .tabbable { margin-left: 20px; margin-right: 20px; }
                      .sweet-alert { text-align: justify; }
                      " )
        ),
        tags$link(rel="shortcut icon", href="favicon.PNG")
      ),
      
      tabsetPanel(id = "tabsetPanel",
                  type = "pills",
                  
                  #######
                  # Informações do arquivo
                  #######
                  
                  tabPanel("Arquivo",
                           fluidRow(
                             column(3,
                                br(),
                                fileInput("arquivo",
                                          "Selecione o arquivo:",
                                          buttonLabel = "Abrir...",
                                          placeholder = "Nenhum arquivo",
                                          accept = c("text/plain")
                                ),
                                numericInput("arquivoLinhas",
                                             "Número máximo de linhas a serem carregadas:",
                                             value = 0,
                                             min = 0
                                )
                             ),
                             column(9,
                                br(),
                                actionButton("ajudaArquivo", "Leia mais"),
                                br(),br(),
                                htmlOutput("arquivoInfo"),
                                hr(),
                                uiOutput("arquivoTexto"),
                                plotOutput("arquivo")
                             )

                          )
                  ),
                  
                  #######
                  # Tabela de palavras
                  #######
                  
                  tabPanel("Lista de palavras",
                           fluidRow(
                             column(3,
                                    br(),
                                    helpText("Selecione a frequência mínima de aparição
                                         de uma palavra no texto. "),
                                    sliderInput("palavrasSlider",
                                                "Frequência de ocorrência",
                                                min = 1,
                                                max = 200,
                                                value = 10,
                                                step = 1
                                    )
                             ),
                             column(9,
                                    br(),
                                    actionButton("ajudaPalavras", "Leia mais"),
                                    br(),br(),
                                    DT::dataTableOutput("palavras")
                             )
                           )
                  ),

                  #######
                  # Nuvem de palavras
                  #######

                  tabPanel("Nuvem",
                           fluidRow(
                             column(3,
                                br(),
                                helpText("Selecione o nível da frequência para alterar a nuvem.
                                         Quanto menor o nível, mais palavras com maior frequência aparecerão."),
                                sliderInput("nuvemSlider",
                                             "Nível de Frequência",
                                             min = 0.5,
                                             max = 10,
                                             value = 1.5,
                                             step = 0.1
                                            )
                             ),
                             column(9,
                                br(),
                                actionButton("ajudaNuvem", "Leia mais"),
                                br(),br(),
                                wordcloud2Output("nuvem",
                                                height = "500px",
                                                width = "100%"
                                )
                             )
                          )
                  ),

                  ##############
                  # Grafos de palavras
                  ##############

                  tabPanel("Grafo",
                           fluidRow(
                             column(3,
                               br(),
                               helpText("Selecione a quantidade de palavras mais frequentes
                                        exibida no grafo."),
                               sliderInput("grafosQtdeSlider",
                                           "Quantidade de palavras",
                                           min = 10,
                                           max = 100,
                                           value = 50,
                                           step = 1
                               ),
                               helpText("Selecione a correlação mínima entre estas palavras."),
                               sliderInput("grafosCorrSlider",
                                           "Correlação",
                                           min = 5,
                                           max = 100,
                                           value = 50,
                                           step = 1
                               )
                            ),
                            column(9,
                              br(),
                              actionButton("ajudaGrafo", "Leia mais"),
                              visNetworkOutput('grafo', width = "100%", height = "500px"),
                              br()
                            )
                          )
                  ),

                  #######
                  # Histograma de frequência de palavras
                  #######

                  tabPanel("Frequências",
                           fluidRow(
                             column(3,
                               br(),
                               helpText("Selecione a frequência mínima de aparição
                                 de uma palavra no texto"),
                               sliderInput("frequenciasFreqSlider",
                                           "Frequência de ocorrência",
                                           min = 5,
                                           max = 200,
                                           value = 10,
                                           step = 1
                               ),
                               helpText("Selecione a quantidade máxima de palavras
                                 frequentes que serão exibidas"),
                               sliderInput("frequenciasQtdeSlider",
                                           "Quantidade de palavras",
                                           min = 5,
                                           max = 30,
                                           value = 20,
                                           step = 1
                               )
                             ),
                             column(9,
                               br(),
                               actionButton("ajudaFrequencias", "Leia mais"),
                               br(),br(),
                               plotOutput("frequencias",
                                          height = "500px",
                                          width = "100%"
                               )
                             )
                           )
                  ),

                  #######
                  # Histograma de correlação de palavras
                  #######

                  tabPanel("Correlações",
                           fluidRow(
                             column(3,
                               br(),
                               helpText("Selecione a correlação mínima (em %) entre
                                a palavra selecionada e os resultados obtidos."),
                               sliderInput("correlacoesCorrSlider",
                                           "Correlação mínima",
                                           min = 5,
                                           max = 100,
                                           value = 30,
                                           step = 5
                               ),
                               helpText("Selecione a quantidade máxima de palavras
                                 correlacionadas que serão exibidas no gráfico."),
                               sliderInput("correlacoesQtdeSlider",
                                           "Quantidade de palavras",
                                           min = 5,
                                           max = 30,
                                           value = 20,
                                           step = 1
                               ),
                               actionButton("correlacoesBuscar",
                                             "Buscar por palavra",
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               br(),
                               br(),
                               htmlOutput("correlacoesEspaco"),
                               selectInput("correlacoesPalavras",
                                            NULL,
                                           choices = NULL
                               ),
                               helpText("Pesquise e selecione acima a palavra,
                                 para que sejam buscadas as correlações.")
                             ),
                             column(9,
                               br(),
                               actionButton("ajudaCorrelacoes", "Leia mais"),
                               br(),br(),
                               plotOutput("correlacoes",
                                         height = "500px",
                                         width = "100%"
                               )
                             )
                          )
                  ),

                  #######
                  # Análise de sentimento
                  #######

                  tabPanel("Análise de sentimentos",
                           br(),
                           tabsetPanel(id = "tabsetSentimentos",
                                       type = "pills",
                                       tabPanel("Ambos",
                                                fluidRow(
                                                  column(4,
                                                         br(),
                                                         actionButton("ajudaSentimentosAmbos", "Leia mais"),
                                                         br(),br(),
                                                         htmlOutput("sentimentosTextosAmbos"),
                                                         br(),
                                                         radioButtons("sentimentosPorFrase",
                                                                      "Analisar por:",
                                                                      choiceNames = list("Parágrafo","Frase"),
                                                                      choiceValues = list(F, T)
                                                         ),
                                                         helpText("O retângulo verde representa as frases ou parágrafos positivos.
                                                           O retângulo vermelho representa os negativos.
                                                           Quanto maior o ponto, mais frases ou parágrafos ele representa.")
                                                  ),
                                                  column(8,
                                                         br(),
                                                         plotOutput("sentimentosAmbos",
                                                                    height = "500px",
                                                                    width = "100%"
                                                         )
                                                  )
                                                )
                                       ),
                                       tabPanel("OpLexicon",
                                         fluidRow(
                                           column(4,
                                                  br(),
                                                  actionButton("ajudaSentimentosOpLexicon", "Leia mais"),
                                                  br(),br(),
                                                  sliderInput("precisaoSentimentosOpLexicon",
                                                              "Detalhamento das linhas",
                                                              min = 1,
                                                              max = 25,
                                                              value = 5,
                                                              step = 1
                                                  ),
                                                  htmlOutput("sentimentosTextosOpLexicon")
                                           ),
                                           column(8,
                                                  br(),
                                                  plotOutput("sentimentosOpLexicon",
                                                             height = "1000px",
                                                             width = "100%"
                                                  )
                                           )
                                         )
                                       ),
                                       tabPanel("SentiLex",
                                          fluidRow(
                                            column(4,
                                                   br(),br(),br(),br(),
                                                   sliderInput("precisaoSentimentosSentiLex",
                                                               "Detalhamento das linhas",
                                                               min = 1,
                                                               max = 25,
                                                               value = 5,
                                                               step = 1
                                                   ),
                                                   htmlOutput("sentimentosTextosSentiLex")
                                            ),
                                            column(8,
                                                   br(),
                                                   plotOutput("sentimentosSentiLex",
                                                              height = "1000px",
                                                              width = "100%"
                                                   )
                                            )
                                          )
                                       )
                           )
                  ),
                  tabPanel("Sobre",
                           fluidRow(
                             column(12,
                                    br(),
                                    htmlOutput("sobre")
                             )
                             
                           )
                  )
                  
            )
                  
        )
    
)