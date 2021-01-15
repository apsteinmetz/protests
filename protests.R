# protests from acledata.com
# for methodology see
# https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2020/10/ACLED_USAMethodology_2020.pdf

library(tidyverse)
library(readxl)

# "https://acleddata.com/download/22846/"

protests_raw <- read_excel("data/USA_2020_2021_Jan08update.xlsx")

# make long data pivoted on actors
# clean up and organize the data
protests <- protests_raw %>%
   separate(ASSOC_ACTOR_1,into = c("A_ACTOR_1a","A_ACTOR_1b","A_ACTOR_1c","A_ACTOR_1d","A_ACTOR_1e","A_ACTOR_1f"),sep = ";",extra="merge") %>%
   separate(ASSOC_ACTOR_2,into = c("A_ACTOR_2a","A_ACTOR_2b","A_ACTOR_2c","A_ACTOR_2d","A_ACTOR_2e","A_ACTOR_2f"),sep = ";",extra="merge") %>%
   mutate(across(contains("ACTOR"),trimws)) %>%
   pivot_longer(contains("ACTOR"),names_to = NULL,values_to = "ACTOR") %>%
   filter(!is.na(ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Police Forces"),"Police Forces",ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Military Forces"),"Military Forces",ACTOR)) %>%
   mutate(ACTOR = ifelse(str_detect(ACTOR,"Government of the United"),"Government Forces",ACTOR)) %>%
   mutate(ACTOR = str_remove(ACTOR,"\\(United States\\)")) %>%
   mutate(across(contains("ACTOR"),trimws)) %>%
   mutate(across(ACTOR = as.factor(ACTOR))) %>%
   mutate(across(contains("EVENT_TYPE"),as.factor)) %>%
   mutate(alignment = "unspecified") %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Forces"),"government",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Patriot"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"GOP"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Republican"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Regiment"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Blue"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"BLM"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Black"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Communist"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"People"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"LGBT"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Workers"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Fasci"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Democrat"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Antifa"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Labor|Labour"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"NFAC"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Latin"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Afri"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"III"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Boys"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Bois"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"QAnon"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Christian"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"White"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Oath"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Free"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Watchmen"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Pro-Police"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Militia"),"right",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Social"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"ACLU"),"left",alignment)) %>%
   mutate(alignment = ifelse(str_detect(ACTOR,"Justice"),"left",alignment)) %>%
   mutate(alignment = factor(alignment,levels = c("left","unspecified","government","right"))) %>%
   unique()



# count(ACTOR,name="event_count",sort=TRUE) %>%

# remove generic (non-political group) actors
# where the labels apply to both left and right protests
generics <- c("Protesters","Students", "Rioters","Teachers","Women",
               "Civilians","Sole Perpetrator","Journalists")
protests <- protests %>%
   filter(!(ACTOR %in% generics))


#How many actor-events did we classify?
protests %>% ggplot(aes(alignment)) + geom_bar() +
   labs(y = "Event Count",
        x = "Actor Political Alighnment")

