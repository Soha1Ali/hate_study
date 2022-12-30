#------------------Load packages--------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hablar")
install.packages("tidyr")
install.packages("reshape2")
install.packages("mapdata")
install.packages("psych")
install.packages("lavaan")
install.packages("rstatix")
install.packages("lmerTest")
install.packages("emmeans")
install.packages("effectsize")
install.packages("GPArotation")
install.packages("ggpubr")
install.packages("sjPlot")
install.packages("cowplot")
install.packages("magick")
install.packages("kableExtra")


library(ggplot2)
library(dplyr)
library(hablar)
library(magrittr)
library(tidyverse)
library(tidyr)
library(knitr)
library(reshape2)
library(mapdata)
library(haven)
library(tidyverse)
library(psych)
library(lavaan)
library(rstatix)
library(lmerTest)
library(emmeans)
library(effectsize)
library(GPArotation)
library(ggpubr)
library(sjPlot)
library(cowplot)
library(magick)
library(kableExtra)

#------------------READ IN DATA--------------

# read in 
hate.df <- read.csv("hate_study2.csv")
# convert to local data frame
hate.df <- tibble::as_tibble(hate.df)

#------------------FORMATTING--------------

# order columns 
hate.df = hate.df %>%
  select(1,112:122,3,2,104,108,7,8,9,13,18,12,10,14,19,16,11,15,20,21,
          32:41,42,43,74:103)

# re-code reverse coded items 
hate.df = hate.df %>%
  mutate(
    anger4 = recode(anger4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1),
    contempt4 = recode(contempt4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1),
    disgust4 = recode(disgust4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1)
  )

# convert to factor and name
hate.df = hate.df %>% # demographics 
  mutate(
    sex = factor(sex, levels=c(1,2,3),
                 labels=c("male", "female", "other")),
    condition = factor(condition), #scales 
    topic.realistic = factor(topic.realistic, levels=c(1, 2, 3),
                             labels=c("education", "technology", "lifestyle")),
    topic.symbolic = factor(topic.symbolic, levels=c(1, 2, 3),
                             labels=c("abortion", "drugs", "same-sex")),
    topic.control = factor(topic.control, levels=c(1, 2, 3),
                             labels=c("education", "technology", "lifestyle"))
  )

# check
is.factor(hate.df$topic.control)
levels(hate.df$topic.control) 

#------------------COMPUTATIONS--------------

hate.df = hate.df %>% 
  mutate(
    topic = coalesce(topic.control, topic.realistic, topic.symbolic)
  )
