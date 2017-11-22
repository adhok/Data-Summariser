library(shiny)
library(shinyjs)

appCSS <- "
#loading {
  position: fixed;
left: 50%;
top: 50%;
z-index: 1;
width: 150px;
height: 150px;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 120px;
height: 120px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}
@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
"


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
        useShinyjs(),
        inlineCSS(appCSS),
        
       
        hidden(div(id='loading')),
      
          hidden(div(
            id='app-content',
            
        #uiOutput("table_summary"),
        #uiOutput('tables'),
        fluidRow(column(width=4,uiOutput('num_buttons_1'),uiOutput('num_buttons_2'),uiOutput('grouped_ungrouped')),column(width=4,uiOutput('factor_button'),
                                                                                            uiOutput('numeric_input'),
                                                                                            uiOutput('output_type')
                                                                                            ),column(width=4,uiOutput('factor_button_for_ring_chart'))),
        uiOutput("plots_first_row"),
        uiOutput('plots_second_row'),
        width=12
        
      )
          )
      
      )
    )
    
  )
)