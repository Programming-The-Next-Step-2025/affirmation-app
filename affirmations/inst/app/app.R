library(shiny)

# Define UI
ui <- fluidPage(

  # Custom CSS to style the app
  tags$style(HTML("
    body {
      background-color: pink;
      font-family: Arial, sans-serif;
      height: 100vh;
      display: flex;
      justify-content: center;
      align-items: center;
      margin: 0;
      overflow: hidden;
    }
    .content {
      text-align: center;
      padding: 20px;
    }
    h2, h3 {
      font-size: 40px;
      color: black;
      text-transform: uppercase;
      letter-spacing: 2px;
    }
    .btn {
  background-color: #ffb3cc;
  font-size: 18px; /* smaller font */
  padding: 10px 20px; /* smaller padding */
  border: none;
  border-radius: 10px;
  cursor: pointer;
  box-shadow: 0 4px 10px rgba(255, 105, 180, 0.6);
  transition: all 0.8s ease;
    }
.btn-lower {
  margin-top: 40px;
}
    .btn:hover {
      background-color: #ff80bf;
      transform: scale(1.1);
    }
    .results {
      font-size: 20px;
      margin-top: 20px;
      color: #ff66b2;
    }
    .affirmation {
      font-style: italic;
      font-size: 28px;
      margin-top: 20px;
      font-weight: bold;
    }
    .affirmation-emoji {
      font-size: 30px;
      color: #ff00cc;
    }
    .question-container {
      display: flex;
      justify-content: center;
      align-items: flex-start;
      gap: 60px;
      margin-top: 20px;
    }
    .question-column {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      height: 400px;
    }
    .question-box {
      width: 300px;
      margin: 0 auto;
    }
    .fade-enter {
      opacity: 0;
      transform: translateY(20px);
      transition: opacity 0.8s ease, transform 0.8s ease;
    }
    .fade-enter.fade-enter-active {
      opacity: 1;
      transform: translateY(0);
    }
  ")),

  uiOutput("page_ui")
)

# Define Server Logic
server <- function(input, output, session) {

  current_page <- reactiveVal(1)
  selected_questions <- reactiveVal(NULL)

  observe({
    question_list <- unlist(
      lapply(personality_data, function(x) sample(x$questions, 2)),
      use.names = FALSE
    )
    selected_questions(question_list)
  })

  output$page_ui <- renderUI({
    page_num <- current_page()

    if (page_num == 1) {
      return(
        div(class = "fade-enter fade-enter-active content",
            div(class = "container",
                h2("✨Your Personalized Affirmation - because you deserve it✨"),
                actionButton("next_btn", "Next", class = "btn")
            )
        )
      )
    }

    if (page_num == 2) {
      questions <- selected_questions()
      left_column <- questions[1:5]
      right_column <- questions[6:10]

      return(
        div(class = "fade-enter fade-enter-active content",
            div(class = "container",
                h3("Answer some questions for us to get to know you better!🩷"),
                div(class = "question-container",
                    div(class = "question-column",
                        lapply(left_column, function(q) {
                          q_id <- gsub(" ", "_", q)
                          div(class = "question-box",
                              radioButtons(inputId = q_id, label = q, choices = c("Yes", "No"))
                          )
                        })
                    ),
                    div(class = "question-column",
                        lapply(right_column, function(q) {
                          q_id <- gsub(" ", "_", q)
                          div(class = "question-box",
                              radioButtons(inputId = q_id, label = q, choices = c("Yes", "No"))
                          )
                        })
                    )
                ),
                div(class = "btn-lower", actionButton("next_btn", "Next", class = "btn"))
            )
        )
      )
    }

    if (page_num == 3) {
      scores <- calculate_scores()
      max_score <- max(scores)
      top_personalities <- names(scores)[scores == max_score]
      selected_personality <- sample(top_personalities, 1)
      affirmation <- sample(personality_data[[selected_personality]]$affirmations, 1)
      paragraph <- sample(personality_data[[selected_personality]]$paragraphs, 1)

      return(
        div(class = "fade-enter fade-enter-active content",
            div(class = "container",
                h3(paste("Your personality today is:", gsub("_", " ", selected_personality))),
                div(class = "results", p(paragraph)),
                div(class = "affirmation",
                    span(class = "affirmation-emoji", "✨"),
                    p(affirmation),
                    span(class = "affirmation-emoji", "✨")
                )
            )
        )
      )
    }
  })

  observeEvent(input$next_btn, {
    current_page(current_page() + 1)
  })

  calculate_scores <- function() {
    scores <- setNames(rep(0, length(personality_data)), names(personality_data))

    for (q in selected_questions()) {
      q_id <- gsub(" ", "_", q)
      response <- input[[q_id]]

      if (response == "Yes") {
        for (personality in names(personality_data)) {
          if (q %in% personality_data[[personality]]$questions) {
            scores[personality] <- scores[personality] + 1
          }
        }
      }
    }
    return(scores)
  }
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

