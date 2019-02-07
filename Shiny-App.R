#' This file contains the code for our Shiny application.
#' 
#' The working directory should be the project root folder `stat628-module1`.

library(shiny)

#' x: weight measurement
#' y: abdomen measurement
calculator <- function(x, y) {
  return(-40 - 0.14 * x + 0.91 * y)
  if (x < 80 | x > 400) {
    return("weight-out-of-bounds")
  } else if (y < 40 | y > 150) {
    return("abdomen-out-of-bounds")
  } else {
    return(-40.27 - 0.14 * x + 0.91 * y)
  }
}

evafunc <- function(c) {
  if (c > 2 & c <= 5) {
    return("Your body fat is essential fat.")
  } else if (c > 5 & c <= 13) {
    return("You are athletic.")
  } else if (c > 13 & c <= 17) {
    return("You are fit.")
  } else if (c > 17 & c <= 25) {
    return("You are average.")
  } else if (c > 25) {
    return("You are obese.")
  } else {
    return("Please check your input.")
  }
}

ui <- fluidPage(
  titlePanel("Compute your bodyfat!"),
  sidebarPanel(
    selectInput("weightunit", "Unit for Weight:",
                c(Pounds = "lbs", Kilogram = "kg")),
    numericInput("weight", label = "Weight", value = 0),
    selectInput("abdomenunit", "Unit for Abdomen:",
                c(Inches = "in", Centimeter = "cm")),
    numericInput("abdomen", label = "Abdomen Circumference", value = 0),
    submitButton("Update")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               h4(textOutput("intro"))
      ),
      tabPanel("Rule of Thumb",
               h4(textOutput("rot1")),
               h4(textOutput("rot2")), 
               h4(textOutput("rot3")),  
               h4(textOutput("rot4"))
      ),
      tabPanel("Calculator",
               h4(textOutput("bfp")),
               h4(textOutput("eva")),
               imageOutput("imageforshiny")
               ),
      tabPanel("The American Council on Exercise Body Fat Categorization",
               tableOutput("ref")
      )
    )
  )
)

server <- function(input, output) {
  
  # Introduction
  introText <- reactive({
    paste("This app is used to provide a simple, robust, accurate and precise ",
          "\"rule-of-thumb\" method to calculate men's bodyfat percentage. ",
          "Choose units, input your weight and abdomen values, and click ",
          "Update!")
  })
  output$intro <- renderText({
    introText()
  })
  
  # PNG image
  output$imageforshiny <- renderImage({
    list(src = "images/imageforshiny.png",
         contentType = 'image/png',
         width = 500,
         height = 287)
  }, deleteFile = FALSE)
  
  # Rule of thumb
  ruleofthumb1 <- reactive({
    paste("RULE OF THUMB: ")
  })
  output$rot1 <- renderText({
    ruleofthumb1()
  })
  ruleofthumb2 <- reactive({
    paste("90% abdomen minus 10% weight!")
  })
  output$rot2 <- renderText({
    ruleofthumb2()
  })
  ruleofthumb3 <- reactive({
    paste("Don't forget to minus 40 percent!")
  })
  output$rot3 <- renderText({
    ruleofthumb3()
  })
  ruleofthumb4 <- reactive({
    paste("Get your body fat!")
  })
  output$rot4 <- renderText({
    ruleofthumb4()
  })
  
  # Bodyfat percentage
  bodyfatpercentage <- reactive({
    if(input$weightunit == "lbs") {
      w <- input$weight
    } else {
      w <- input$weight * 2.20
    }
    if(input$abdomenunit == "cm") {
      ab <- input$abdomen
    } else {
      ab <- input$abdomen * 2.54
    }
    result <- round(calculator(w, ab), 2)
    if(result > 0 & result < 100) {
      paste("Your bodyfat percentage is: " , result, "%", sep = "")
    } else {
      paste("You have unusual input values leading to negative bodyfat.")
    }
  })
  output$bfp <- renderText({
    bodyfatpercentage()
  })
  
  # Evaluation
  evaluation <- reactive({
    if(input$weightunit == "lbs") {
      w <- input$weight
    } else {
      w <- input$weight * 2.20
    }
    if(input$abdomenunit == "cm") {
      ab <- input$abdomen
    } else {
      ab <- input$abdomen * 2.54
    }
    result <- round(calculator(w, ab), 2)
    evafunc(result)
  })
  output$eva <- renderText({
    evaluation()
  })
  
  # References
  output$ref <- renderTable({
    Description <- c("Essential fat", "Athletes", "Fitness", "Average", "Obese")
    Bodyfat <- c("2-5%", "6-13%", "14-17%", "18-25%", "25+%")
    data.frame(Description, Bodyfat)
  })
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
