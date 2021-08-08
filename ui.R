library(shiny)

# Define UI for application that predicts the next word based on the words entered
shinyUI(fluidPage(

    # Application title
    titlePanel("Predictive model for capstone project"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            textInput("Input_for_prediction", "Please enter some word, the model will predict the next words", value="Happy Mothers"),
            br(),
            br(),
            h5("Example: Happy Mothers, Martin luther..."),
            h5("Note: If the model fail to find any match, it will return the word 'the', which is the most widely found word.")
        ),

        # Main panel
        mainPanel(
            h3("Predicted next words based on the words entered"),
            textOutput('pred_word'),
            br(),
            br(),
            h5("Please wait for a while for the result to be shown, Thanks!")
        )
    )
))
