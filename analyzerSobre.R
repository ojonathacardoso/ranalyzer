carregarSobre <- function(output){
  
  output$sobre <- renderText({
    paste(
      "<b>Desenvolvido por:</b>", "<br>",
      "Jonatha Martins Cardoso - ojonathacardoso@gmail.com",
      "<br>","<br>",
      "<b>Código-fonte e instruções de instalação:</b>","<br>",
      "<a href='https://github.com/ojonathacardoso/ranalyzer'>https://github.com/ojonathacardoso/ranalyzer</a>",
      "<br>","<br>",
      "<b>Referência para citação bibliográfica:</b>","<br>",
      "CARDOSO, Jonatha Martins. <b>Mineração de texto em fontes da Internet, com enfoque na Administração Pública e Atividade Política.</b> 2019. Trabalho de Conclusão de Curso (Monografia) – Curso de Sistemas de Informação, Universidade Feevale, Novo Hamburgo, RS, 2019."
    )
  })
  
}