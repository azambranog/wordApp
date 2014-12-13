library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("A simple Word Predictor"),
    
    sidebarLayout(
        sidebarPanel(
            "Documentation",
            p("This app was is a simple word predictor. The model was
                           build using a small ammount of text from twitter, 
                           blog inputs and news articles. The code for the app 
                           can be found at",
              a("github", href="https://github.com/azambranog/wordApp")),
            p("The model is very simple and it is base on the ocurence of 
                           4-grams,3-grams and 2grams in the training text"),
            p("The app is very simple to use, jus input some phrase 
                           or word in the texbox and then click predict, a list of
                           the 5 most probable endings for your phrase will be shown.
                           Additionally, a small word cloud containing some more possible
                           endings is shown. The bigger the word in the cloud the most 
                           probable it is the ending of your prhase"),
            p("Enjoy")
        )
        ,
        
        
        # Show a plot of the generated distribution
        mainPanel(
            textInput("input", label = h5("Insert your phrase here"), value = ""),
            actionButton("predict", label = "PREDICT"),
            h5("The most probable endings are:"),
            verbatimTextOutput("solution"),
            plotOutput("cloud")
        )
)
))