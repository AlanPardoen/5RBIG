library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)

WahsingtonPost <- read.csv("WahsingtonPostDatabase.csv", header=TRUE, sep=",")
fatalEncounters <- read.csv("FatalEncounters.csv", header=TRUE, sep=",")
policeDeaths <- read.csv("PoliceDeaths.csv", header=TRUE, sep=",")
WahsingtonPost$date <- ymd(WahsingtonPost$date)

WahsingtonPost[,c(4:5,7:8,9:14)] <- lapply(WahsingtonPost[,c(4:5,7:8,9:14)], as.factor)
levels(WahsingtonPost$gender)
levels(WahsingtonPost$race) 
WahsingtonPost$race <- factor(WahsingtonPost$race, levels(WahsingtonPost$race)[c(5,2,1,4,3,6)])
levels(WahsingtonPost$flee)
WahsingtonPost$flee <- factor(WahsingtonPost$flee, levels(WahsingtonPost$flee)[c(1,5,3,2,4)])
levels(WahsingtonPost$manner_of_death)
WahsingtonPost$manner_of_death <- factor(WahsingtonPost$manner_of_death, levels(WahsingtonPost$manner_of_death)[c(1,3,2)])

ggplot(data = WahsingtonPost, aes(y = race)) + 
  geom_barh(aes(fill = ..count..)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(y = NULL, x = "Number of deaths")

ggplot(data = WahsingtonPost, aes(y = gender)) + 
  geom_barh(aes(fill = ..count..)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(y = NULL, x = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = date)) + 
  geom_histogram(aes(fill = ..count..), bins = 25) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(x = NULL, y = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = month(date, label = TRUE))) + 
  geom_bar(aes(fill = ..count..)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(x = NULL, y = "Number of deaths")

ggplot(data = WahsingtonPost, aes(x = wday(date, label = TRUE))) + 
  geom_bar(aes(fill = ..count..)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(x = NULL, y = "Number of deaths")

stateinfo <- WahsingtonPost %>% group_by(state) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(15) %>% 
  mutate(state = factor(state, levels = rev(unique(state))))
ggplot(stateinfo, aes(x = n, y = state)) +
  geom_barh(stat="identity", aes(fill = n)) +
  geom_text(aes(x = 17, y = state, label=as.character(state)), color="white", size=4) +
  labs(y = NULL, x = "Number of deaths") +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  theme_minimal(base_size = 13) +
  theme(axis.text.y=element_blank()) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0))


armedinfo <- WahsingtonPost %>% group_by(armed) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% top_n(10) %>% 
  mutate(armed = factor(armed, levels = rev(unique(armed))))

ggplot(data = armedinfo, aes(x = n, y = armed)) + 
  geom_barh(stat="identity", aes(fill = n)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(y = NULL, x = "Number of deaths")

ggplot(data = WahsingtonPost, aes(y = flee)) + 
  geom_barh(aes(fill = ..count..)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "royalblue3", high = "navyblue") +
  labs(y = NULL, x = "Number of deaths")


