library(shiny)

ui <- fluidPage(
  tags$style(type="text/css", "plot img{float:left;}"),
  
  h3("User interface for evaluating digits"),
  h6("In this interface you are requested to evaluate the label of various handwritten digits"),
  
  fluidRow(
   
    column(6, offset = 0,
      h4("Image Display"),
      plotOutput("view", height=300, click = "plot_click")
      ,
	fluidRow(  
	  column(4, offset = 0,
	  h6("Press button to forward"),
	  actionButton("counter", label= "Next Image", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
	  h6("Index"),
	  verbatimTextOutput("value")
	  ),
	  column(4, offset=0,
	  h6("Pixel coordinates and pixel intensity"),
	  verbatimTextOutput("info")
	  )
        
      )
      ),
    
    column(6, offset = 0,
      h4("Digit labels"),
      radioButtons("radio", label=NULL, choices = list("Digit 0" = 0, "Digit 1" = 1, "Digit 2" = 2, "Digit 3" = 3, "Digit 4" = 4, "Digit 5" = 5, "Digit 6" = 6, "Digit 7" = 7, "Digit 8" = 8, "Digit 9" = 9, "No Idea" = 99), selected = 0)
      ,
      
	fluidRow(  
	  column(3, offset = 0,
	  h6("Your selection"),
	  verbatimTextOutput("value2")
	  ),
	  column(3, offset=0,
	  h6("Select data"),
	  selectInput("dataset", NA, choices = c("MNIST", "Own Digits"))
	  )
      )    
      )
  ),

  fluidRow(
    
  )
)



server <- function(input, output) {

 output$value <- renderText({
    input$counter
    })

 output$value2 <- renderText({
    input$radio
    })
 
 datasetInput <- reactive({
    switch(input$dataset,
	    "MNIST" = train1,
	    "Own Digits" = train2)
    })
 
 output$view <- renderPlot({
    dataset <- datasetInput()
    m = t(apply(matrix(unlist(dataset[input$counter+1,-1]), nrow=28, byrow=TRUE), 2, rev))
    par(mfrow=c(1,1),
        oma = c(0,0,0,0) + 0.1, # bottom, left, top, right
        mar = c(2,0,2,4) + 0.1)
    image(m, col=grey.colors(255), axes=FALSE, asp=1)
    })

 output$info <- renderText({
    dataset <- datasetInput()
    x=as.numeric(input$plot_click$x)
    y=as.numeric(input$plot_click$y)
    nx=ifelse(floor((x+0.03)*28)>0, floor((x+0.03)*28), 1)
    ny=ifelse(floor((y+0.03)*28)>0, floor((y+0.03)*28), 1)
    #nx=ifelse(floor(x*(24-1)+1)>0, floor(x*(24-1)+1), 1)
    #ny=ifelse(floor(y*(24-1)+1)>0, floor(y*(24-1)+1), 1)
    m = t(apply(matrix(unlist(dataset[input$counter+1,-1]), nrow=28, byrow=TRUE), 2, rev))
    paste0("X coord.= ", round(nx,2), "\nY coord.= ", round(ny,2), "\nPix int.= ", round(m[nx,ny],2))
    # paste0("x=", round(nx,2), "(", round(x,2), ")", "\ny=", round(ny,2), "(", round(y,2), ")", "\nintensity=", round(m[nx,ny],2))
    })
}

shinyApp(ui, server)






