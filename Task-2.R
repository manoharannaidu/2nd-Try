library(shiny)
library(ggplot2)
library(DT)
library(shinydashboard)
library(shinyjs)

ui <- fluidPage(
  tagList(
  useShinyjs(),
  titlePanel(
    h1
             ("Payment Methods", style = "color:#F5071D;"
             )
    ),
  tabsetPanel(
    id = "panels",
    tabPanel(
      value = "Inputs",
      h6("Inputs", style = "color:#0719F5;
         font-size:20px;"),
      hr(),
      fluidRow(
        column(
          3,
          inputPanel(
            #style = "background-color:#A8F5F0;",
            h4("Enter the following Values"),
            br(),
            sliderInput("P","Payment Amount : ", 0,100,2),
            textOutput("error"),
            tags$style("#error{color:red;}"),
            br(),
            sliderInput("r","Rate Of Interest", 0,100,10),
            sliderInput("n", label = "Number of Payments (in Years)", 2,100,2)
            # actionLink(
            #   "nextI",
            #   actionButton("next1",h5("Choose Method",style = "color:#019FFF;"))
            #   #icon = icon("beer")
            # )
            )
          )
      )
      ),
    tabPanel(
      value = "Method",
      h6("Choose Method", style = "color:#0719F5;
         font-size:20px;"),
      hr(),
      wellPanel(
            # checkboxInput("foo", "Show Square Payment Method", T),
            actionButton("spmb",h5("Calculate using Square Payment Method", style = "color:#019FFF;")),
            br(),
            br(),
            # checkboxInput("foo1", "Show Double Payment Method", T),
            actionButton("dpmb",h5("Calculate using Double Payment Method", style = "color:#019FFF;"))
          )
    ),
    tabPanel(
      value = "mytab2",
      h6("Square Payment Method", style = "color:#0719F5;
         font-size:20px;"),
      fluidRow(
        column(
          5,
      br(),
      dataTableOutput("SquarePaymentTable"),
      br()
        ),
      column(
        5,
        offset = 2,
      plotOutput("SquarePaymentPlot"),
      br()
      ),
      fluidRow(
        actionLink(
          "ins",
          actionButton("insb",h5("Inputs", style = "color:#019FFF;"))
          #icon = icon("beer")
          )
      # actionLink(
      #   "ins",
      #   actionButton("insb","Inputs")
      #   #icon = icon("beer")
      # )
        )
    )
      ),
    tabPanel(
      value = "mytab1",
      h6("Double Payment Method", style = "color:#0719F5;
         font-size:20px;"),
      fluidRow(
      column(
        3,
      inputPanel(
        numericInput("Pin","Initial Principal Amount : ", 0, 0)
      )
      )
      ),
      br(),
      fluidRow(
        column(
          5,
      br(),
      dataTableOutput("DoublePaymentTable")
        ),
      column(
        5,
        offset = 2,
      br(),
      plotOutput("DoublePaymentPlot"),
      tags$style(type="text/css", "#df th, td {border: medium solid #010101;text-align:center}")
      )
      ),
      fluidRow(
      actionLink(
        "ind",
        actionButton("indb",h5("Inputs", style = "color:#019FFF;"))
        #icon = icon("beer")
      )
      )
      )
    )
  )
)

server <- function(input, output, session)
{
  observe(
    {
    hide(selector = "#panels li a[data-value=mytab2]")
  }
  )
  observe(
    {
      hide(selector = "#panels li a[data-value=mytab1]")
    }
  )
  observeEvent(
    input$spmb,
    {
      toggle(selector = "#panels li a[data-value=mytab2]")
    }
  )
  observeEvent(
    input$dpmb,
    {
      toggle(selector = "#panels li a[data-value=mytab1]")
    }
  )
  # observeEvent(
  #     eventExpr = input$next1,
  #     {
  #       newvalue <- "Method"
  #       updateTabItems(session, "panels", newvalue)
  #     }
  #   )
  observeEvent(
    input$insb,
    {
      newvalue <- "Inputs"
      updateTabItems(session, "panels", newvalue)
    }
  )
  observeEvent(
    input$indb,
    {
      newvalue <- "Inputs"
      updateTabItems(session, "panels", newvalue)
    }
  )
  observeEvent(
    input$dpmb,
    {
      newvalue <- "mytab1"
      updateTabItems(session, "panels", newvalue)
    }
  )
  observeEvent(
    input$spmb,
    {
      newvalue <- "mytab2"
      updateTabItems(session, "panels", newvalue)
    }
  )
  observeEvent(
    input$spmb,
    {
      output$SquarePaymentTable <- renderDataTable(
        {
        pay_amount <- 0
        pay_amount[1] <- input$P
        for (i in 1 : (input$n - 1))
          {
          pay_amount[i + 1] <- pay_amount[i] * pay_amount[i]
        }
        SquarePaymentTable <- data.frame(pay_amount)
        interest_row <- c(SquarePaymentTable$pay_amount * (input$r/100))
        SquarePaymentTable <- data.frame(pay_amount, interest_row)
        amount_row <- c(SquarePaymentTable$pay_amount + SquarePaymentTable$interest_row)
        if(input$P > 1)
        {
        SquarePaymentTable <- data.frame("Principal" = pay_amount, "Interest" = interest_row, "Amount" = amount_row)
        }
        }
      )
      output$SquarePaymentPlot <- renderPlot(
        {
          pay_amount <- 0
          pay_amount[1] <- input$P
          for (i in 1 : (input$n - 1))
          {
            pay_amount[i + 1] <- pay_amount[i] * pay_amount[i]
          }
          SquarePaymentTable <- data.frame(pay_amount)
          interest_row <- c(SquarePaymentTable$pay_amount * (input$r/100))
          SquarePaymentTable <- data.frame(pay_amount, interest_row)
          amount_row <- c(SquarePaymentTable$pay_amount + SquarePaymentTable$interest_row)
          if(input$P > 1)
          {
          return(
            # barplot(height = rbind(pay_amount, amount_row)),
            barplot(height = rbind(pay_amount, amount_row), names.arg = c(1 : input$n), beside = FALSE, main = "Summary", legend.text = c("Principal", "Amount"), col = c("red", "blue"), border = NA, xlab = "Year", ylab = "Rupees")
            )
          }
        }
      )
    }
  )
  # observeEvent(
  #   input$spmb,
  #   {
  #     
  #   }
  # )
  observeEvent(
    input$P,
    {
  if(input$P <= 1)
  {
    output$error <- renderText(
      {
        "(Principal should be greater than 1 for Square Payment Method)"
        }
      )
  }
    }
  )
  observeEvent(
    eventExpr = input$dpmb,
    {
      output$DoublePaymentTable <- renderDataTable(
        {
          if(input$Pin > 0)
          {
          pay_amount1 <- input$P
          pay_amount <- 0
          pay_amount[1] <- input$Pin
          for (i in 1 : (input$n - 1))
          {
            pay_amount[i + 1] <- pay_amount[i] + pay_amount1
            pay_amount1 <- (2 * pay_amount1)
          }
          DoublePaymentTable <- data.frame(pay_amount)
          interest_row <- c(DoublePaymentTable$pay_amount * (input$r/100))
          DoublePaymentTable <- data.frame(pay_amount, interest_row)
          amount_row <- c(DoublePaymentTable$pay_amount + DoublePaymentTable$interest_row)
          return(data.frame("Principal" = pay_amount, "Interest" = interest_row, "Amount" = amount_row))
          }
          else
          {
            return(data.frame())
          }
        }
      )
      output$DoublePaymentPlot <- renderPlot(
        {
          if(input$Pin > 0)
          {
            pay_amount1 <- input$P
            pay_amount <- 0
            pay_amount[1] <- input$Pin
            for (i in 1 : (input$n - 1))
            {
              pay_amount[i + 1] <- pay_amount[i] + pay_amount1
              pay_amount1 <- (2 * pay_amount1)
            }
            DoublePaymentTable <- data.frame(pay_amount)
            interest_row <- c(DoublePaymentTable$pay_amount * (input$r/100))
            DoublePaymentTable <- data.frame(pay_amount, interest_row)
            amount_row <- c(DoublePaymentTable$pay_amount + DoublePaymentTable$interest_row)
          return(
            # barplot(height = rbind(pay_amount, amount_row)),
            barplot(height = rbind(pay_amount, amount_row), names.arg = c(1 : input$n), beside = FALSE, main = "Summary", legend.text = c("Principal", "Amount"), col = c("red", "blue"), border = NA, xlab = "Year", ylab = "Rupees")
          )
          }
          else
          {
            return()
          }
        }
      )
    }
  )
}

shinyApp(ui = ui, server = server)