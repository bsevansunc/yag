library(tidyverse)





# teacher -----------------------------------------------------------------


teacher <-
  read_csv('evaluation_teacher.csv')


teacher %>% 
  select(question, qDetail) %>%
  distinct %>%
  View

teacher %>%
  filter(question == '8f') %>%
  mutate_all(.funs = tolower) %>%
  na.omit %>%
  group_by(answer) %>%
  summarize(n = n())



teacher %>%
  filter(question == '5') %>%
  na.omit %>%
  mutate(answer = as.numeric(answer)) %>%
  summarize(ms = mean(answer), sd = sd(answer))

teacher %>%
  mutate_all(.funs = tolower) %>%
  na.omit %>%
  group_by(question, qDetail, answer) %>%
  summarize(n = n()) %>%
  View
  

# student -----------------------------------------------------------------

pre <- 
  read_csv('evaluation_studentPre.csv') %>%
  mutate(answer = ifelse(answer == 'na', NA, answer))

post <-
  read_csv('evaluation_studentPost.csv') %>%
  mutate(answer = ifelse(answer == 'na', NA, answer)) 

post %>% select(question, qDetail) %>% distinct %>% View

preSummary <-
  pre %>%
  filter(qDetail %in% post$qDetail) %>%
  filter(!question %in% c('1a', '1b')) %>%
  mutate(answer = as.numeric(answer)) %>%
  filter(!is.na(answer)) %>%
  group_by(qDetail) %>%
  summarize(mnPre = mean(answer))

postSummary <-
  post %>%
  filter(qDetail %in% pre$qDetail) %>%
  filter(!question %in% c('1a', '1b')) %>%
  mutate(answer = as.numeric(answer)) %>%
  filter(!is.na(answer)) %>%
  group_by(qDetail) %>%
  summarize(mnPost = mean(answer))

prePost_summary <- 
  left_join(preSummary, postSummary, by = 'qDetail') %>%
  mutate(percentChange = (mnPost - mnPre)*10)

View(prePost_summary)

postScore <-
  post %>%
  filter(question %in% c('2', '5', '12a', '12b', '12c')) %>%
  mutate(answer = as.numeric(answer)) %>%
  na.omit %>%
  group_by(qDetail) %>%
  summarize(mnPost = mean(answer))
  

