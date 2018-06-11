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

# model test --------------------------------------------------------------

model_survival(
  nAdult = 10, 
  adultModifier = .8,
  nNestlings = 0, 
  nestlingModifier = .05,
  fecundity = 2
)

model_survival(
  nAdult = 10, 
  adultModifier = .8,
  nNestlings = 10, 
  nestlingModifier = .2,
  fecundity = 2
)

# function to run simulations ---------------------------------------------

runModels <- 
  function(
    scenarioFrame,
    nGenerations,
    seedAdults = 10,
    seedNestlings = 0
  ){
    outList <-
      vector('list', length = nGenerations)
    
    outList[[1]] <-
      data_frame(
        generation = 0,
        scenario = 'colonization',
        nAdults = seedAdults,
        nNestlings = seedNestlings
      )
    
    for(i in 2:length(outList)){
      scenario <- 
        sample_n(scenarioFrame, 1)
      generation <- i-1
      surviveM <-
        model_survival(
          nAdult = outList[[i-1]]$nAdults,
          adultModifier = scenario$adultSurv,
          nNestlings = outList[[i-1]]$nNestlings,
          nestlingModifier = scenario$juvSurv,
          fecundity = scenario$fecundity
        )
      # }
      outList[[i]] <-
        bind_cols(
          generation = generation,
          scenario = scenario$scenario,
          surviveM
        )
    }
    bind_rows(outList)
  }


# scenario frame, natural system ------------------------------------------

scenarioFrame_natural <-
  data_frame(
    id = c(1:8),
    scenario = c(
      'fire',
      'storm',
      'lotsa-bugs',
      'lotsa-bugs',
      'plenty-o-shrubs',
      'plenty-o-shrubs',
      'natural-predators',
      'natural-predators'),
    juvSurv = c(0.0,0.0,.3,.3,.15,.15,.1,.1),
    adultSurv = c(.9,.9,.7,.7,.75,.75,.7,.7),
    fecundity = rep(2, 8)
  )

# scenario frame, neutral -------------------------------------------------

scenarioFrame_neutral <-
  data_frame(
    id = c(1),
    scenario = c(
      'neutral'),
    juvSurv = c(.1),
    adultSurv = c(.7),
    fecundity = c(2)
  )

# scenario frame, anthropogenic system ------------------------------------------

scenarioFrame_anthropogenic <-
  data_frame(
    id = c(1:8),
    scenario = c(
      'cats',
      'bad-lawns',
      'pesticide',
      'new-plants',
      'collisions',
      'cats', 
      'disease',
      'pollution'),
    juvSurv = c(0.05, 0.1, .1, .1, .1, 0.05, 0.1, .1),
    adultSurv = c(.3, .5, .6, .75, .6, .3, .5, .5),
    fecundity = c(2,2,2,2,2,2,2,2)
  )

# scenario frame, conservation --------------------------------------------

scenarioFrame_conservation <-
  data_frame(
    id = c(1:8),
    scenario = c(
      'cats_inside',
      'mowing-less',
      'insect-solution',
      'local-plants',
      'window-stickers',
      'cats_inside', 
      'clean-feeders',
      'pollution-solution'),
    juvSurv = c(.15, .2, .2, .2, .15, .15, .15, .15),
    adultSurv = c(.75, .75, .75, .75, .7, .75, .75, .75),
    fecundity = c(2,2,2,2,2,2,2,2)
  )

# test simulations, natural system ----------------------------------------

runModels(scenarioFrame_natural,nGenerations = 6,
          seedAdults = 10, seedNestlings = 0)


# function to run multiple simulations ------------------------------------

simMultiple <-
  function(
    scenarioFrame,
    nGen,
    nSim){
    simulationList <-
      vector('list', length = nSim)
    
    for(i in 1:length(simulationList)){
      simFrame <- 
        runModels(scenarioFrame,nGenerations = nGen,
                  seedAdults = 10, seedNestlings = 0)
      simulationList[[i]] <-
        bind_cols(sim = rep(i, nGen),
                  simFrame)
    }
    bind_rows(simulationList)
  }

# run and plot scenarios --------------------------------------------------

simMultiple(
  scenarioFrame_natural,
  nGen = 6,
  nSim = 10
) %>%
  ggplot(aes(
    x = generation,
    y = nAdults,
    color = factor(sim)
  )) +
  ylim(0, 25) +
  geom_point() + geom_line() +
  theme_bw()

simMultiple(
  scenarioFrame_anthropogenic,
  nGen = 6,
  nSim = 10
) %>%
  ggplot(aes(
    x = generation,
    y = nAdults,
    color = factor(sim)
  )) +
  ylim(0, 25) +
  geom_point() + geom_line() +
  theme_bw()

simMultiple(
  scenarioFrame_conservation,
  nGen = 6,
  nSim = 10
) %>%
  ggplot(aes(
    x = generation,
    y = nAdults,
    color = factor(sim)
  )) +
  ylim(0, 25) +
  geom_point() + geom_line() +
  theme_bw()


# simulating different scenarios ------------------------------------------

# Neutral:

simMultiple(
  scenarioFrame_neutral,
  nGen = 6,
  nSim = 10
)

# Natural:

simMultiple(
  scenarioFrame_natural,
  nGen = 6,
  nSim = 10,
  anthropogenic = FALSE
)

# Anthropogenic:

simMultiple(
  scenarioFrame_anthropogenic,
  nGen = 6,
  nSim = 10,
  anthropogenic = TRUE
)

# Conservation

simMultiple(
  scenarioFrame_conservation,
  nGen = 5,
  nSim = 10,
  anthropogenic = FALSE
)

# simulating across scenarios ------------------------------------------

bind_rows(
  simMultiple(
    scenarioFrame_natural,
    nGen = 6,
    nSim = 1
  ) %>%
    mutate(island = 'Natural'),
  simMultiple(
    scenarioFrame_anthropogenic,
    nGen = 6,
    nSim = 1
  ) %>%
    mutate(island = 'Humans'),
  simMultiple(
    scenarioFrame_conservation,
    nGen = 6,
    nSim = 1
  ) %>%
    mutate(island = 'Conservation')
) %>%
  ggplot(aes(x = generation, y = nAdults, color = factor(island))) +
  ylim(0, 25) +
  geom_point() + geom_line() +
  theme_bw()

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

# messaging on runs -------------------------------------------------------

runModel(scenarioFrame_natural, 1, 10, 10, FALSE)

'Due to natural predators in La-La Land it was a hard year for nestlings. Only 1 nestling survived to be an adult. Meanwhile 3 adults died.'




