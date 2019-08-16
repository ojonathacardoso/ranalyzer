require(shiny)
options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)
folder_address <- 'E://Documentos//GitHub//ranalyzer'
runApp(folder_address, launch.browser=TRUE)