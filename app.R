library(shiny)
library(shinyBS)
ui =fluidPage(
  textOutput("curName"),
  br(),
  textInput("newName", "Name of variable:", "myname"),
  br(),
  actionButton("BUTnew", "Change"),
  bsModal("modalnew", "Change name", "BUTnew", size = "small",
          HTML("Do you want to change the name?"),
          actionButton("BUTyes", "Yes"),
          actionButton("BUTno", "No")
  )
)
server = function(input, output, session) {
  values <- reactiveValues()
  values$name <- "myname";
  
  output$curName <- renderText({
    paste0("Current name: ", values$name)
  })
  
  observeEvent(input$BUTyes, {
    toggleModal(session, "modalnew", toggle = "close")
    values$name <- input$newName
  })
  
  observeEvent(input$BUTno, {
    toggleModal(session, "modalnew", toggle = "close")
    updateTextInput(session, "newName", value=values$name)
  })
}
runApp(list(ui = ui, server = server))