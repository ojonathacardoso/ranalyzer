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
                                         de uma palavra no texto."),
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
                                helpText("Selecione o nível da frequência para alterar a nuvem."),
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
                                           min = 0.20,
                                           max = 0.30,
                                           value = 0.25,
                                           step = 0.01
                               ),
                               helpText("Ajuste o tamanho da imagem que será exibida ao lado."),
                               sliderInput("grafosTamanhoSlider",
                                           "Tamanho",
                                           min = 10,
                                           max = 100,
                                           value = 50,
                                           step = 5
                               ),
                               radioButtons("grafosPesoLinhas",
                                            "Exibir linhas com peso",
                                            choiceNames = list("Sim","Não"),
                                            choiceValues = list(T, F)
                               )
                            ),
                            column(9,
                              br(),
                              actionButton("ajudaGrafo", "Leia mais"),
                              downloadButton("downloadGrafo", "Baixar a imagem"),
                              br(),br(),
                              style = "overflow-y:scroll; max-height: 700px",
                              br(),
                              imageOutput("grafos",
                                         height = "500px",
                                         width = "100%"
                              )
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
                               helpText("Selecione a correlação mínima entre
                                a palavra selecionada e os resultados."),
                               sliderInput("correlacoesCorrSlider",
                                           "Correlação mínima",
                                           min = 5,
                                           max = 100,
                                           value = 30,
                                           step = 5
                               ),
                               helpText("Selecione a quantidade máxima de palavras
                                 correlacionadas que serão exibidas."),
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
                           fluidRow(
                             column(4,
                                    br(),
                                    actionButton("ajudaSentimentos", "Leia mais"),
                                    br(),br(),
                                    htmlOutput("sentimentosFrases")
                             ),
                             column(8,
                                    br(),
                                    plotOutput("sentimentos",
                                               height = "500px",
                                               width = "100%"
                                    )
                             )
                           )
                  )
                  
            )
                  
        )
    
)