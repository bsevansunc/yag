# function to model survival ----------------------------------------------

model_survival <- 
  function(
    nAdult, 
    adultSurvival,
    nNestlings, 
    nestlingSurvival,
    fecundity
  ){
    adultsDied <-
      rbinom(1, nAdult, 1-adultSurvival)
      # (nAdult*(1-adultModifier)) %>% 
      # floor
    adultsNew <-
      rbinom(1, nNestlings, nestlingSurvival)
      # (nNestlings*nestlingModifier) %>%
      # floor
    adultsT <- nAdult - adultsDied + adultsNew
    if(adultsT > 1){
      nestlingsT <-
        adultsT*fecundity
    } else {
      nestlingsT <- 0
    }
    return(
      data.frame(
        newAdults = adultsNew,
        adultsDied = adultsDied,
        nAdults = adultsT,
        nNestlings = nestlingsT
      )
    )
  }

# scenario frame, natural system ------------------------------------------

scenarioFrame_natural <-
  data_frame(
    id = c(1:5),
    scenario = c(
      'fire',
      'storm',
      'lotsa-bugs',
      'plenty-o-shrubs',
      'natural-predators'),
    juvSurv = c(0.05,0.05,.3,.25,.15),
    adultSurv = c(.9,.9,.75,.8,.75),
    fecundity = rep(2, 5)
  ) %>%
  select(scenario:fecundity) %>%
  distinct

# scenario frame, anthropogenic system ------------------------------------------

scenarioFrame_anthropogenic <-
  data_frame(
    id = c(1:6),
    scenario = c(
      'bad-lawns',
      'tasteless-plants',
      'collisions',
      'cats', 
      'disease',
      'pollution'),
    juvSurv = c(0.1,0.1, 0.15, 0.05, 0.1, .1),
    adultSurv = c(.7,.65, .5, .3, .6, .7),
    fecundity = c(2,2,2,2,2,2)
  ) %>%
  select(scenario:fecundity) %>%
  distinct

# scenario frame, conservation --------------------------------------------

scenarioFrame_conservation <-
  data_frame(
    id = c(1:7),
    scenario = c(
      'mowing-less',
      'insect-solution',
      'local-plants',
      'window-stickers',
      'cats_inside', 
      'clean-feeders',
      'pollution-solution'),
    juvSurv = c(.2, .25, .25, .15, .15, .15, .15),
    adultSurv = c(.75, .75, .85, .75, .75, .85, .8),
    fecundity = c(2,2,2,2,2,2,2)
  ) %>%
  select(scenario:fecundity) %>%
  distinct

scenarioList <- 
  list(
    natural = scenarioFrame_natural,
    anthropogenic = scenarioFrame_anthropogenic,
    conservation = scenarioFrame_conservation
  )


# function to print model results -----------------------------------------

print_modelResults <- 
  function(modelOutput){
  if(!is.null(modelOutput$adultsDied)){
    HTML(
      paste0(
        '<p>Adults died = ',
        modelOutput$adultsDied,
        '</p>',
        '<p>Nestlings become adults = ',
        modelOutput$newAdults,
        '</p>',
        '<p>Number of adults in bin = ',
        modelOutput$nAdults,
        '</p>',
        '<p>Number of nestlings = ',
        modelOutput$nNestlings,
        '</p>')
    )
  }
}

get_modelResults <- 
  function(selected_scenario, scenarioFrame, adults, nestlings){
    if(!is.null(selected_scenario)){
      scenario <-
        scenarioFrame %>%
        filter(scenario == selected_scenario)
      g1 <- 
        model_survival(
          nAdult = adults, 
          adultModifier = scenario$adultSurv,
          nNestlings = nestings, 
          nestlingModifier = scenario$juvSurv,
          fecundity = 2
        )
      print_modelResults(g1)
    }
  }
    
convert_outTable_to_printTable <-
  function(outTable){
    if(!is.null(outTable)){
      outTable %>%
        select(
          Generation = generation,
          `Adults died` = adultsDied,
          `New adults` = newAdults,
          `Number of adults in bin` = nAdults,
          `Number of nestlings in bin` = nNestlings
        )
    } else {
      outTable <- NULL
    }
  }

