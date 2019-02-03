##shiny app for final model
library(shiny)
calculator<-function(x,y){
  if(x<=400 & x>=80 & y>=40 & y<=150){
    return(-40-0.14*x+0.91*y)
  }
  else{
    return(-1)
  }
}
evafunc<-function(c){
  if(c<0){
    return("Please check your input")
  }
  else{
    if(c>2 &c<=5){
      return("You are essential fat !")
    }
    else{
      if(c>5 &c<=13){
        return("You are athletes")
      }
      else{
        if(c>13 &c<=17){
          return("You are fitness")
        }
        else{
          if(c>17& c<=25){
            return("You are average")
          }
          else{
            if(c>25){
              return("You are obese")
            }
          }
        }
      }
    }
  }
}
ui <- fluidPage(
  titlePanel("Compute your bodyfat!"),
  sidebarPanel(
    selectInput("weightunit","Unit for Weight:",c(Pounds="lbs",Kilogram="kg")),
    numericInput("weight",label="Weight",value=0),
    selectInput("abdomenunit","Unit for Abdomen:",c(Inches="in",Centimeter="cm")),
    numericInput("abdomen",label="Abdomen circumference",value=0),
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
               img(src="imageforshiny.png", height = 500, width = 800)      
               ),
      tabPanel("The American Council on Exercise Body Fat Categorization",
               tableOutput("ref")
      )
  )))
server <- function(input, output) {
  ## introduction
  introText<-reactive({
    paste("This app is used to provide a simple, robust, accurate and precise  “rule-of-thumb” method to calculate men's bodyfat percentage.Choose units, input your weight and abdomen values, click Update !")
  })
  output$intro<-renderText({
    introText()
  })
  ##rule of thumb
  ruleofthumb1<-reactive({
    paste("RULE OF THUMB: ")
  })
  output$rot1<-renderText({
    ruleofthumb1()
  })
  ruleofthumb2<-reactive({
    paste("90% abdomen minus 10% weight!")
  })
  output$rot2<-renderText({
    ruleofthumb2()
  })
  ruleofthumb3<-reactive({
    paste("Don't forget to minus 40 percent!")
  })
  output$rot3<-renderText({
    ruleofthumb3()
  })
  ruleofthumb4<-reactive({
    paste("Get your body fat!")
  })
  output$rot4<-renderText({
    ruleofthumb4()
  })
  ## bodyfat percentage
  bodyfatpercentage<-reactive({
    if(input$weightunit=="lbs"){
      w=input$weight
    }
    else{
      w=input$weight*2.2
    }
    if(input$abdomenunit=="cm"){
      ab=input$abdomen
    }
    else{
      ab=input$abdomen*2.54
    }
    result<-round(calculator(w,ab),2)
    if(result>0){
      paste("Your bodyfat percentage is : ",result,"%")
    }
    else{
      paste("You have unusual input values.")
    }
  })
  output$bfp<-renderText({
    bodyfatpercentage()
  })
  ## evaluation
  evaluation<-reactive({
    result<-calculator(input$weight,input$abdomen)
    paste(evafunc(result),"!")
  })
  output$eva<-renderText({
    evaluation()
  })
  ## references
  output$ref<-renderTable({
    Description<-c("Essential fat","Athletes","Fitness","Average","Obese")
    Bodyfat<-c("2-5%","6-13%","14-17%","18-25%","25+%")
    data.frame(Description,Bodyfat)
  })
  }

shinyApp(ui=ui,server=server)
