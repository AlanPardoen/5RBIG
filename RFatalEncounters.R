library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)

fatalEncounters <- read.csv("FatalEncounters.csv", header=TRUE, sep=",")

Raceinfo <- fatalEncounters %>% group_by(Race) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%  
  mutate(Race = factor(Race, levels = rev(unique(Race))))

ggplot(data = Raceinfo, aes(x = n,y = Race)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")



Raceinfo <- fatalEncounters %>% group_by(Race.with.imputations) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%  
  mutate(Race.with.imputations = factor(Race.with.imputations, levels = rev(unique(Race.with.imputations))))

ggplot(data = Raceinfo, aes(x = n,y = Race.with.imputations)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")


Countyinfo <- fatalEncounters %>% group_by(Location.of.death..county.) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(Location.of.death..county. = factor(Location.of.death..county., levels = rev(unique(Location.of.death..county.))))

ggplot(data = Countyinfo, aes(x = n,y = Location.of.death..county.)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

Cityinfo <- fatalEncounters %>% group_by(Location.of.death..city.) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(Location.of.death..city. = factor(Location.of.death..city., levels = rev(unique(Location.of.death..city.))))

ggplot(data = Cityinfo, aes(x = n,y = Location.of.death..city.)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")
