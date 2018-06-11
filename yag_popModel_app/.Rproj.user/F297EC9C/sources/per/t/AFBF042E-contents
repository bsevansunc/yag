library(shiny)
library(stringi)
library(tidyverse)
library(readr)
library(stringr)
library(shinyjs)
library(shinyBS)

source('setup.R')

shinyUI(
  fluidPage(
    fluidRow(
      column(
        5, offset = 1,
        HTML(
          '<h1>Population model app</h1>'
        )),
      column(5, '')
      ),
    hr(),
    fluidRow(
      column(
        5, offset = 1,
        selectInput(
          'system',
          'System:',
          choices = c('natural', 'anthropogenic', 'conservation'),
          selected = 'natural'
        )
      ),
      column(6, uiOutput('uiScenarios'))
    ),
    br(),
    fluidRow(
      column(
        5, offset = 1,
        actionButton('goGen', 'Calculate')
      ),
      column(6, '')
    ),
    hr(),
    fluidRow(
      column(
        8, offset = 1,
        DT::dataTableOutput('outGeneration')
      ),
      column(3, '')
      ),
    br(),
    fluidRow(
      column(
        4, offset = 1,
        actionButton('refresh', 'Refresh table')
      ),
      column(7, ''))
    # end -----------------------------------------------------------------
  )
)

