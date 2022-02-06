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

Stateinfo <- fatalEncounters %>% group_by(State) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(State = factor(State, levels = rev(unique(State))))

ggplot(data = Stateinfo, aes(x = n,y = State)) + 
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


ageinfo <- fatalEncounters %>% group_by(Age) %>% summarise(n = n()) %>% 
  arrange(Age) %>% top_n(25) %>% 
  mutate(Age = factor(Age, levels = rev(unique(Age))))
  
ggplot(data = ageinfo, aes(x = n,y = Age)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

genreinfos <- fatalEncounters %>% group_by(Gender) %>% summarise(n = n()) %>% 
  arrange(desc(Gender)) %>% top_n(25) %>% 
  mutate(Gender = factor(Gender, levels = rev(unique(Gender))))

ggplot(data = genreinfos, aes(x = n,y = Gender)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")





