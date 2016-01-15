# ui.R  This is the ui.R file for the Shiny App. The app can be seen at https://sanjeevv1.shinyapps.io/predictv3

shinyUI(fluidPage(
  titlePanel("Next Word Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter a word, or a few, and then click submit, to see our 
				prediction of what the next word may be: (App is ready when
				you see the message 'Please enter input'. Initial data
				loading takes 10-12 secs)"),
    
      textInput("text", label = h3("Text input")),
        #value = "Enter text...")
		actionButton("goButton","Submit!")
		
		
	  
    ),
  
    mainPanel(
              tabsetPanel(
              tabPanel("Our Prediction",h5(textOutput("predict")),h4("_________________"),h4(textOutput("predict2")),h5(textOutput("predict3"))),
			  #tabPanel("predict1",textOutput("predict1"))
              #tabPanel("State Data",h1(textOutput("text1")),tableOutput("text2")),
              tabPanel("Help",verbatimTextOutput("helptext"))
  )
))))
