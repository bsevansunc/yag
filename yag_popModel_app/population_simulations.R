# Simulations to explore output

library(tidyverse)

# functions ---------------------------------------------------------------

# Function to run a simulation:

model_run <-
  function(
    simFrame,
    initialPop = 10,
    nGenerations = 5,
    fecund = 2
  ){
    populationFrame <-
      data_frame(
        nAdult = c(initialPop, rep(0, nGenerations -1)),
        nNestlings = rep(0, nGenerations)
      )
    
    for(i in 2:5){
      scenario <-
        simFrame %>%
        sample_n(1)
      
      model_output <- 
        model_survival(
          nAdult = populationFrame[i-1,]$nAdult, 
          adultSurvival = scenario$adultSurv,
          nNestlings = populationFrame[i-1,]$nNestlings, 
          nestlingSurvival = scenario$juvSurv,
          fecundity = fecund
        )
      
      populationFrame[i,] <-
        model_output[,3:4]
      
    }
    populationFrame %>%
      mutate(gen = 1:nGenerations) %>%
      select(gen, nAdult, nNestlings)
  }

# Function to plot multiple simulations:

plot_simulations <-
  function(simFrame_scenario, nSim)
  {
    runList <- vector('list', length = nSim)
    
    for(i in 1:length(runList)){
      runList[[i]] <- 
        model_run(simFrame = simFrame_scenario) %>%
        mutate(sim = i)
    }
    
    bind_rows(runList) %>%
      ggplot(aes(x = gen, y = nAdult, group = sim, color = factor(sim))) +
      geom_point(show.legend = FALSE) +
      geom_line(show.legend = FALSE) +
      theme_bw() +
      ylim(c(0, 40))
  }

# scenario frame, natural system ------------------------------------------

# Scenario frame:

scenarioFrame_natural <-
  data_frame(
    id = c(1:5),
    scenario = c(
      'fire',
      'storm',
      'lotsa-bugs',
      'plenty-o-shrubs',
      'natural-predators'),
    juvSurv = c(0.0,0.0,.3,.25,.2),
    adultSurv = c(.9,.9,.75,.8,.75),
    fecundity = rep(2, 5)
  ) %>%
  select(scenario:fecundity) %>%
  distinct

# Simulation frame:

simFrame_natural <- 
  scenarioFrame_natural %>%
  bind_rows(scenarioFrame_natural %>%
              filter(scenario == 'lotsa-bugs')) %>%
  bind_rows(scenarioFrame_natural %>%
              filter(scenario == 'lotsa-bugs')) %>%
  bind_rows(scenarioFrame_natural %>%
              filter(scenario == 'plenty-o-shrubs')) %>%
  mutate(id = 1:8)

# anthropogenic system ----------------------------------------------------

# Scenario frame:

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

# Simulation frame:

simFrame_anthropogenic <- 
  scenarioFrame_anthropogenic %>%
  bind_rows(scenarioFrame_anthropogenic %>%
              filter(scenario == 'cats')) %>%
  bind_rows(scenarioFrame_anthropogenic %>%
              filter(scenario == 'cats')) %>%
  mutate(id = 1:8)


# conservation system -----------------------------------------------------

# Scenario frame:

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

# Simulation frame:

simFrame_conservation <- 
  scenarioFrame_conservation %>%
  bind_rows(scenarioFrame_conservation %>%
              filter(scenario == 'local-plants')) %>%
  mutate(id = 1:8)

# Run models --------------------------------------------------------------

plot_simulations(simFrame_natural, 10)

plot_simulations(simFrame_anthropogenic, 10)

plot_simulations(simFrame_conservation, 10)





