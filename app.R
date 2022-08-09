# Dashborad de análise gráfica do modelo de concorrência monopolística de Dixit-Stiglitz
# Bernardo C. G. da Silva
# Agosto de 2022

library(shiny)
library(shinydashboard)
library(tidyverse)

header <- dashboardHeader(title = "Modelo de Dixit-Stiglitz", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sliderInput("alpha", "Custo fixo", 1, 100, 1),
  sliderInput("beta", "Custo variável", 1, 100, 1),
  sliderInput("L", "População", 1, 10000, 1),
  sliderInput("epsilon", "Elasticidade de substituição", 1, 20, 1),
  sliderInput("delta", "Parte da renda gasta em manufaturados", 0, 1, 0.5),
  sliderInput("gamma", "Parte da população em manufaturados", 0, 1, 0.5)
)

body <- dashboardBody(
  fluidRow(
    box(
      width = 6,
      plotOutput("bem_estar_gamma"),
      title = "Bem-estar x Gamma"
    ),
    box(
      width = 6,
      plotOutput("bem_estar_delta"),
      title = "Bem-estar x Delta"
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")



###################################################################################
#                                     SERVER
###################################################################################

bem_estar <- function(beta, epsilon, delta, gamma, alpha, L) {
  (((beta * epsilon) / (epsilon - 1))**(-delta)) *
    (((delta / (1 - delta)) * ((1 - gamma) / gamma))**(1 - delta)) *
    (((gamma * L) / (alpha * epsilon))**(delta / (epsilon - 1)))
}

server <- function(input, output) {

  # gamma
  output$bem_estar_gamma <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(gamma = c(0, 1)), aes(gamma)) +
        stat_function(
          fun = bem_estar,
          args = list(
            alpha = a,
            beta = b,
            L = c,
            epsilon = d,
            delta = e
          )
        )+
        theme_classic()
    }
    gg(input$alpha, input$beta, input$L, input$epsilon, input$delta)
  })

  # delta
  output$bem_estar_delta <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(delta = c(0, 1)), aes(delta)) +
        stat_function(
          fun = bem_estar,
          args = list(
            alpha = a,
            beta = b,
            L = c,
            epsilon = d,
            gamma = e
          )
        ) +
        theme_classic()
    }
    gg(input$alpha, input$beta, input$L, input$epsilon, input$gamma)
  })

  }

shinyApp(ui, server)





# fx <- function(x,p){
#  x*p**2
# }
# ggplot()+stat_function(fun = fx, args = list(x=5))
