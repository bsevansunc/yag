library(shiny)
library(stringi)
library(stringr)
library(tidyverse)
library(readr)
library(DT)

filter <- dplyr::filter

source('setup.R')

server <- shinyServer(function(input, output) {
  
  # Reactive values -------------------------------------------------------
  
  values <- reactiveValues()
  
  scenarios <-
    reactive({
      outTable <- scenarioList[['natural']]
      if(!is.null(input$system)){
        outTable <-
          scenarioList[[input$system]]
      }
      outTable
    })
  
  # Reactive inputs -------------------------------------------------------
  
  output$uiScenarios <- 
    renderUI({
        selectInput(
          'scenario',
          'Scenario:',
          choices = sort(scenarios()$scenario)
        )
    })
  
  # Reactive output table -------------------------------------------------
  
  observeEvent(input$goGen, {
    if(is.null(values[['outTable']])){
      scenarioValues <- scenarios() %>%
        filter(scenario == input$scenario)
      values$outTable <-
        model_survival(
          nAdult = 10, 
          adultModifier = scenarioValues$adultSurv,
          nNestlings = 0, 
          nestlingModifier = scenarioValues$juvSurv,
          fecundity = scenarioValues$fecundity
        ) %>%
        mutate(generation = 1) %>%
        select(generation, newAdults:nNestlings)
    } else {
      scenarioValues <- scenarios() %>%
        filter(scenario == input$scenario)
      outTable <- values$outTable
      values$outTable <- bind_rows(
        outTable, 
        model_survival(
          nAdult = outTable[nrow(outTable),]$nAdults, 
          adultModifier = scenarioValues$adultSurv,
          nNestlings = outTable[nrow(outTable),]$nNestlings, 
          nestlingModifier = scenarioValues$juvSurv,
          fecundity = scenarioValues$fecundity
        ) %>%
          mutate(generation = nrow(outTable) + 1) %>%
          select(generation, newAdults:nNestlings)
      )
    }
  })
  
  observeEvent(input$refresh, {
    values$outTable <- NULL
  })
  
  # observeEvent(input$goGen, {
  #   outTable <- NULL
  #   if(input$generation == 1){
  #     scenarioValues <- scenarios() %>%
  #       filter(scenario == input$scenario)
  #     values$g1 <-
  #       model_survival(
  #         nAdult = 10, 
  #         adultModifier = scenarioValues$adultSurv,
  #         nNestlings = 0, 
  #         nestlingModifier = scenarioValues$juvSurv,
  #         fecundity = scenarioValues$fecundity
  #       ) %>%
  #       mutate(generation = 1) %>%
  #       select(generation, newAdults:nNestlings)
  #   }
  #   if(input$generation == 2){
  #     scenarioValues <- scenarios() %>%
  #       filter(scenario == input$scenario)
  #     values$g2 <-
  #       model_survival(
  #         nAdult = values[['g1']]$nAdults, 
  #         adultModifier = scenarioValues$adultSurv,
  #         nNestlings = values[['g1']]$nNestlings, 
  #         nestlingModifier = scenarioValues$juvSurv,
  #         fecundity = scenarioValues$fecundity
  #       ) %>%
  #       mutate(generation = 2) %>%
  #       select(generation, newAdults:nNestlings)
  #   }
  #   if(input$generation == 3){
  #     scenarioValues <- scenarios() %>%
  #       filter(scenario == input$scenario)
  #     values$g3 <-
  #       model_survival(
  #         nAdult = values[['g2']]$nAdults, 
  #         adultModifier = scenarioValues$adultSurv,
  #         nNestlings = values[['g2']]$nNestlings, 
  #         nestlingModifier = scenarioValues$juvSurv,
  #         fecundity = scenarioValues$fecundity
  #       ) %>%
  #       mutate(generation = 3) %>%
  #       select(generation, newAdults:nNestlings)
  #   }
  #   if(input$generation == 4){
  #     scenarioValues <- scenarios() %>%
  #       filter(scenario == input$scenario)
  #     values$g4 <-
  #       model_survival(
  #         nAdult = values[['g3']]$nAdults, 
  #         adultModifier = scenarioValues$adultSurv,
  #         nNestlings = values[['g3']]$nNestlings, 
  #         nestlingModifier = scenarioValues$juvSurv,
  #         fecundity = scenarioValues$fecundity
  #       ) %>%
  #       mutate(generation = 4) %>%
  #       select(generation, newAdults:nNestlings)
  #   }
  #   if(input$generation == 5){
  #     scenarioValues <- scenarios() %>%
  #       filter(scenario == input$scenario)
  #     values$g5 <-
  #       model_survival(
  #         nAdult = values[['g4']]$nAdults, 
  #         adultModifier = scenarioValues$adultSurv,
  #         nNestlings = values[['g4']]$nNestlings, 
  #         nestlingModifier = scenarioValues$juvSurv,
  #         fecundity = scenarioValues$fecundity
  #       ) %>%
  #       mutate(generation = 5) %>%
  #       select(generation, newAdults:nNestlings)
  #   }
  #   values$outTable <- bind_rows(
  #     values$g1, values$g2, values$g3, values$g4, values$g5
  #   )
  # })
  
  # Outputs for each generation -------------------------------------------
  
  output$outGeneration <- renderDataTable(
    datatable(
      convert_outTable_to_printTable(values[['outTable']]),
      options = list(dom = 't')
      )
  )
  
  # output$generation_output <- 
    # DT::renderDataTable(
    #   datatable(scenarioFrame(), filter = 'none', rownames = FALSE,
    #             options = list(pageLength = 3))
    # )

  # scenario <-
  #   scenarioFrame() %>%
  #   filter(scenario == input$generation_scenario)
  # if(input$generation_select == 1){
  #   values$g1 <- 
  #     model_survival(
  #       nAdult = 10, 
  #       adultModifier = scenario$adultSurv,
  #       nNestlings = 0, 
  #       nestlingModifier = scenario$juvSurv,
  #       fecundity = 2
  #     )
  # }
  #
  # output$generation1_output <-
  #   renderUI({
  #     if(!is.null(values[['g1']])){
  #       print_modelResults(values[['g1']])
  #     }
  #   })
  # 

  # fin -------------------------------------------------------------------
})