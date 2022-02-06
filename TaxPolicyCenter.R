library(devtools)
library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)
library(readxl)

TaxPolicyCenter <- read_excel("TaxPolicyCenter.xls", col_names=TRUE )
TaxPolicyCenter <- na.omit(TaxPolicyCenter)
names(TaxPolicyCenter) <- c('regionsandstates', 'total','intergovernmental','total2', 'Elementaryandsecondaryeducation', 'Highereducation', 'Publicwelfare', 
                       'healthandhospital', 'highways', 'police', 'all other', 'ExhibitpersonalIncome[1](millions)')
options(digits=10)

TaxPolicyCenter$police <- as.numeric(as.character(TaxPolicyCenter$police))

ggplot(data = TaxPolicyCenter, aes(x = police ,y = regionsandstates)) + 
  geom_barh(stat="identity", aes(fill = police)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradient(low = "dodgerblue", high = "firebrick1") +
  labs(y = "regionsandstates", x = "Number of deaths")
