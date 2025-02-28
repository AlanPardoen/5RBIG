library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)
library(tidyverse)
library(ggpubr)
library(rstatix)

WahsingtonPost <- read.csv("WahsingtonPostDatabase.csv", header=TRUE, sep=",")

WahsingtonPost$date <- ymd(WahsingtonPost$date)

raceinfo <- WahsingtonPost %>% group_by(race) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%  
  mutate(race = factor(race, levels = rev(unique(race))))

ggplot(data = raceinfo, aes(x = n,y = race, fill=group)) + 
  geom_bar(stat="identity", 1)
ggplot(raceinfo, aes(x="", y=n, fill=race))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)

ggplot(data = WahsingtonPost, aes(y = gender)) + 
  geom_barh(aes(fill = ..count..)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = date)) + 
  geom_histogram(aes(fill = ..count..), bins = 25) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(x = NULL, y = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = month(date, label = TRUE))) + 
  geom_bar(aes(fill = ..count..)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(x = NULL, y = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = wday(date, label = TRUE))) + 
  geom_bar(aes(fill = ..count..)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(x = NULL, y = "Number of deaths")

stateinfo <- WahsingtonPost %>% group_by(state) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(state = factor(state, levels = rev(unique(state))))
ggplot(stateinfo, aes(x = n, y = state)) +
  geom_barh(stat="identity", aes(fill = n)) +
  geom_text(aes(x = 17, y = state, label=as.character(state)), color="white", size=4) +
  labs(y = NULL, x = "Number of deaths") +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  theme_minimal(base_size = 13) +
  theme(axis.text.y=element_blank()) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0))

mean = mean(stateinfo$n)
print(mean)


armedinfo <- WahsingtonPost %>% group_by(armed) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(10) %>% 
  mutate(armed = factor(armed, levels = rev(unique(armed))))

ggplot(data = armedinfo, aes(x = n, y = armed)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

fleeinfo <- WahsingtonPost %>% group_by(flee) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  mutate(flee = factor(flee, levels = rev(unique(flee))))

ggplot(data = fleeinfo, aes(x = n, y = flee)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")


ageinfo <- WahsingtonPost %>% group_by(Age) %>% summarise(n = n()) %>% 
  arrange(Age) %>% top_n(25) %>% 
  mutate(Age = factor(Age, levels = rev(unique(Age))))

ggplot(data = ageinfo, aes(x = n,y = Age)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

mean = mean(WahsingtonPost$age)
print(mean)


illessinfo <- WahsingtonPost %>% group_by(signs_of_mental_illness) %>% summarise(n = n()) %>% 
  arrange(signs_of_mental_illness) %>% top_n(25) %>% 
  mutate(signs_of_mental_illness = factor(signs_of_mental_illness, levels = rev(unique(signs_of_mental_illness))))

ggplot(data = illessinfo, aes(x = n,y = signs_of_mental_illness)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = NULL, x = "Number of deaths")

wash
ggboxplot(WahsingtonPost, x = "flee", y = "age")

ggboxplot(WahsingtonPost, x = "race", y = "age")






