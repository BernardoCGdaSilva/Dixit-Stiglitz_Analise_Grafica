# Dashborad de análise gráfica do modelo de concorrência monopolística de Dixit-Stiglitz
# Bernardo C. G. da Silva
# Agosto de 2022

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rsconnect)

###################################################################################
#                                     UI
###################################################################################

header <- dashboardHeader(title = "Modelo de Dixit-Stiglitz", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  sliderInput("alpha", "Custo fixo", min = 1, max = 1000, value = 100, step = 1, animate = animationOptions(interval = 3000)),
  sliderInput("beta", "Custo variável", min = 1, max = 1000, value = 100, step = 1, animate = animationOptions(interval = 3000)),
  sliderInput("L", "População", min = 1, max = 10000, value = 1000, step = 1, animate = animationOptions(interval = 3000)),
  sliderInput("epsilon", "Elasticidade de substituição", min = 1, max = 20, value = 2, step = 1, animate = animationOptions(interval = 3000)),
  sliderInput("delta", "Parte da renda gasta em manufaturados", min = 0, max = 1, value = 0.5, step = 0.05, animate = animationOptions(interval = 3000)),
  sliderInput("gamma", "Parte da população em manufaturados", min = 0, max = 1, value = 0.5, step = 0.05, animate = animationOptions(interval = 3000))
)

body <- dashboardBody(
  fluidRow(
    box(
      width = 4,
      plotOutput("bem_estar_gamma"),
      title = "Bem-estar x Gamma"
    ),
    box(
      width = 4,
      plotOutput("bem_estar_delta"),
      title = "Bem-estar x Delta"
    ),
    box(
      width = 4,
      plotOutput("bem_estar_epsilon"),
      title = "Bem-estar x Epsilon"
    )
  ),
  fluidRow(
    box(
      width = 4,
      plotOutput("bem_estar_alpha"),
      title = "Bem-estar x Alpha"
    ),
    box(
      width = 4,
      plotOutput("bem_estar_beta"),
      title = "Bem-estar x Beta"
    ),
    box(
      width = 4,
      plotOutput("bem_estar_L"),
      title = "Bem-estar x L"
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
        ) +
        theme_classic() +
        labs(x = expression(gamma), y = "Bem-estar")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
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
        theme_classic() +
        labs(x = expression(delta), y = "Bem-estar")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }
    gg(input$alpha, input$beta, input$L, input$epsilon, input$gamma)
  })

  # epsilon
  output$bem_estar_epsilon <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(epsilon = c(1, 20)), aes(epsilon)) +
        stat_function(
          fun = bem_estar,
          args = list(
            alpha = a,
            beta = b,
            L = c,
            delta = d,
            gamma = e
          )
        ) +
        theme_classic() +
        labs(x = expression(epsilon), y = "Bem-estar")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }
    gg(input$alpha, input$beta, input$L, input$delta, input$gamma)
  })

  # alpha
  output$bem_estar_alpha <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(alpha = c(1, 1000)), aes(alpha)) +
        stat_function(
          fun = bem_estar,
          args = list(
            epsilon = a,
            beta = b,
            L = c,
            delta = d,
            gamma = e
          )
        ) +
        theme_classic() +
        labs(x = expression(alpha), y = "Bem-estar")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }
    gg(input$epsilon, input$beta, input$L, input$delta, input$gamma)
  })

  # beta
  output$bem_estar_beta <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(beta = c(1, 1000)), aes(beta)) +
        stat_function(
          fun = bem_estar,
          args = list(
            epsilon = a,
            alpha = b,
            L = c,
            delta = d,
            gamma = e
          )
        ) +
        theme_classic() +
        labs(x = expression(beta), y = "Bem-estar") +
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }
    gg(input$epsilon, input$alpha, input$L, input$delta, input$gamma)
  })

  # L
  output$bem_estar_L <- renderPlot({
    gg <- function(a, b, c, d, e) {
      ggplot(data.frame(L = c(1, 10000)), aes(L)) +
        stat_function(
          fun = bem_estar,
          args = list(
            epsilon = a,
            alpha = b,
            beta = c,
            delta = d,
            gamma = e
          )
        ) +
        theme_classic() +
        labs(x = "L", y = "Bem-estar")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    }
    gg(input$epsilon, input$alpha, input$beta, input$delta, input$gamma)
  })
}

shinyApp(ui, server)

###################################################################################
#                                     DEPLOY
###################################################################################

#rsconnect::deployApp(account = "bernardocgdasilva", appName = "Modelo_de_Dixit_Stiglitz")
