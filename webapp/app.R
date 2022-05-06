#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages and dependencies ----------------------------------------------------
library(shiny)
source(here::here("src/wordle_solver.R"))

# Import word list -------------------------------------------------------------
word_list <- readLines(here::here("res/words.txt"))


# Shiny ------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wordle Solver"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Input"),
            fixedRow(
              column(2,
                     actionButton("first_letter", "W")),
              column(2,
                     actionButton("second_letter", "W")),
              column(2,
                     actionButton("third_letter", "W")),
              column(2,
                     actionButton("fourth_letter", "W")),
              column(2,
                     actionButton("fifth_letter", "W")),
            ),
            textInput("flag", h3("Enter flag"), value = "00000"),
            textInput("guess", h3("Enter Custom Guess"), value = ""),
            actionButton("go_button", "Next Iteration"),
            actionButton("reset_button", "Reset")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Flag:"),
          textOutput("flag"),
          h3("Guess:"),
          textOutput("guess"),
          h3("Best Words:"),
          dataTableOutput("words_by_score")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    word_list_score <- score_words(word_list) |> 
      dplyr::arrange(dplyr::desc(`V1`))
    
    guess <- word_list_score |> 
      dplyr::slice_max(`V1`, n = 1, with_ties = F) |>
      magrittr::use_series("word_list")
    
    rv <- reactiveValues(new_word_list = word_list,
                         guess = guess,
                         score = word_list_score)
    
    observe({updateTextInput(session, "guess", value = rv$guess)})
    
    observeEvent(input$go_button,
                 {
                   flags_for_guess <- input$flag |>
                     strsplit('') |>
                     magrittr::extract2(1) |>
                     as.numeric()
                   
                   rv$new_word_list <-
                     word_list_filter(rv$new_word_list,
                                      input$guess,
                                      flags_for_guess) |>
                     na.omit()
                   
                   if (length(rv$new_word_list) == 1) {
                     rv$guess <- rv$new_word_list[[1]]
                   } else {
                     rv$score <- score_words(rv$new_word_list) |>
                       dplyr::arrange(dplyr::desc(`V1`))
                     
                     rv$guess <- rv$score |> 
                       dplyr::slice_max(`V1`, n = 1, with_ties = F) |>
                       magrittr::use_series("word_list")
                   }
                   
                 })
    
    observeEvent(input$reset_button,
                 {
                   word_list_score <- score_words(word_list) |> 
                     dplyr::arrange(dplyr::desc(`V1`))
                   
                   guess <- word_list_score |> 
                     dplyr::slice_max(`V1`, n = 1, with_ties = F) |>
                     magrittr::use_series("word_list")
                   
                   rv$new_word_list <- word_list
                   rv$guess <- guess
                   rv$score <- word_list_score
                   
                   updateTextInput(session, "flag", value = "00000")
                   updateTextInput(session, "guess", value = rv$guess)
                 })
    
    output$guess <- renderText({
      input$guess
    })
    
    output$flag <- renderText({
      input$flag
    })
    
    output$words_by_score <- renderDataTable({
      rv$score
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
