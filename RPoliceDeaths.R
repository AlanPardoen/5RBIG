library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)

policeDeaths <- read.csv("PoliceDeaths.csv", header=TRUE, sep=",")


yearinfo <- policeDeaths %>% group_by(year) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%  top_n(15) %>% 
  mutate(year = factor(year, levels = rev(unique(year))))

ggplot(data = yearinfo, aes(x = n,y = year)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

yearinfo <- policeDeaths %>% group_by(year) %>% summarise(n = n()) %>% 
  arrange(desc(year)) %>% 
  mutate(year = factor(year, levels = rev(unique(year))))

ggplot(data = yearinfo[0:20,], aes(x = n,y = year)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

causeinfo <- policeDeaths %>% group_by(cause_short) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%  
  mutate(cause_short = factor(cause_short, levels = rev(unique(cause_short))))

ggplot(data = causeinfo, aes(x = n,y = cause_short)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

canineinfo <- policeDeaths %>% group_by(canine) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(canine = factor(canine, levels = rev(unique(canine))))

ggplot(data = canineinfo, aes(x = n,y = canine)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

stateinfo <- policeDeaths %>% group_by(state) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(state = factor(state, levels = rev(unique(state))))

ggplot(data = stateinfo, aes(x = n,y = state)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")
