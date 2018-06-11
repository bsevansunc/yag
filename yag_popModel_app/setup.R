# function to model survival ----------------------------------------------

model_survival <- 
  function(
    nAdult, 
    adultModifier,
    nNestlings, 
    nestlingModifier,
    fecundity
  ){
    adultsDied <-
      (nAdult*(1-adultModifier)) %>% 
      floor
    adultsNew <-
      (nNestlings*nestlingModifier) %>%
      floor
    adultsT <- nAdult - adultsDied + adultsNew
    nestlingsT <-
      adultsT*fecundity %>%
      floor
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
    juvSurv = c(0.0,0.0,.3,.15,.1),
    adultSurv = c(.85,.85,.7,.75,.7),
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
    juvSurv = c(0.1,.1, .15, 0.05, 0.1, .1),
    adultSurv = c(.5,.75, .6, .3, .5, .6),
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
    juvSurv = c(.2, .2, .2, .15, .15, .15, .15),
    adultSurv = c(.75, .75, .75, .7, .75, .75, .75),
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

# one model run with scenario ---------------------------------------------

runModel <-
  function(
    scenarioFrame,
    generation,
    nAdults,
    nNestlings,
    anthropogenic
  ){
    scenario <- sample_n(scenarioFrame, 1)
    survivalOutput <-
      model_survival(
        nAdult = nAdults,
        adultModifier = scenario$adultSurv,
        nNestlings = nNestlings,
        nestlingModifier = scenario$juvSurv,
        fecundity = scenario$fecundity,
        anthropogenic
      )
    bind_cols(
      generation = generation,
      scenario = scenario$scenario,
      survivalOutput
    )
  }


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



