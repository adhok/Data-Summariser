library(shiny)
jsfile <- "getdata.js"
cssfile <- "style.css"
ui <- shinyUI(
  fluidPage(
    tags$head(tags$link(rel="stylesheet", href=cssfile, type="text/css"),
              tags$script(src=jsfile)),
    sidebarLayout(
      
      sidebarPanel(
        fluidRow(
          h5("Drop Datasets"),
          div(class="col-xs-12", id="drop-area", ondragover="dragOver(event)", 
              ondrop="dropData(event)")),width=2
        
      ),
      mainPanel(
      
        #uiOutput("table_summary"),
        #uiOutput('tables'),
        fluidRow(column(width=4,uiOutput('num_buttons_1'),uiOutput('num_buttons_2')),column(width=4,uiOutput('factor_button'),uiOutput('numeric_input'))),
        uiOutput("plots_first_row"),
        uiOutput('plots_second_row'),
        width=12
        
      )
    )
    
  )
)