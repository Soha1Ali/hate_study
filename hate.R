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
hate <- read_csv("hate_study2.csv")
# convert to local data frame
hate.df <- tibble::as_tibble(hate)

#------------------MUNGING--------------

# order columns 
hate.df <- hate.df %>%
  select(1,112:122,3,2,104,108,7,8,9,13,18,12,10,14,19,16,11,15,20,21,
          32:41,42,43,74:103)

# re-code reverse coded items 
hate.df <- hate.df %>%
  mutate(
    anger4 = recode(anger4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1),
    contempt4 = recode(contempt4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1),
    disgust4 = recode(disgust4, "1"=7, "2"=6, "3"=5, "4"=4, "5"=3, "6"=2, "7"=1)
  )

# fix wonky age responses
hate.df[136,3]=28
hate.df[204,3]=38
hate.df[328,3]=24

# rename occupation column
colnames(hate.df)[5]="occupation"

# convert to factor and name
hate.df <- hate.df %>% # demographics 
  mutate(
    sex = factor(sex, levels=c(1,2,3),
                 labels=c("male", "female", "other")),
    education = factor(education, levels=c(1, 2, 3, 4, 5),
                       labels=c("no formal", "primary", "secondary", "undergrad", "grad")),
    occupation = factor(occupation, levels=c(1, 2, 4, 7, 10, 11),
                        labels=c("employee", "self-employed", "no work", "other", "undergrad", "grad")),
    truthfulness = factor(truthfulness, levels=c(5, 6), # "did you answer the questions in the present survey truthfully"
                          labels=c("yes", "no")), 
    tangram.check = factor(tangram.check, levels = c(1,2), # "do you understand the tanagram task?"
                           labels = c("yes", "no")), 
    tangram.videocheck = factor(tangram.videocheck, levels = c(1,2,3,4), # "were you able to properly view the video?"
                                labels = c("yes", "no video", "no audio", 
                                           "novideo & audio")),
    condition = factor(condition), #scales 
    topic.realistic = factor(topic.realistic, levels=c(1, 2, 3),
                             labels=c("education", "technology", "lifestyle")),
    topic.symbolic = factor(topic.symbolic, levels=c(1, 2, 3),
                             labels=c("abortion", "drugs", "same-sex")),
    topic.control = factor(topic.control, levels=c(1, 2, 3),
                             labels=c("education", "technology", "lifestyle")),
  )

# rename 

hate.df <- hate.df %>%
  rename(
    voodoo1 = `voodoo1 - Fill with 0's and 1's`,         
    voodoo2 = `voodoo2 - Fill with 0's and 1's`,          
    voodoo3 = `voodoo3 - Fill with 0's and 1's`,          
    voodoo4 = `voodoo4 - Fill with 0's and 1's`,         
    voodoo5 = `voodoo5 - Fill with 0's and 1's`,       
    voodoo6 = `voodoo6 - Fill with 0's and 1's`,        
    voodoo7 = `voodoo7 - Fill with 0's and 1's`,        
    voodoo8 = `voodoo8 - Fill with 0's and 1's`,          
    voodoo9 = `voodoo9 - Fill with 0's and 1's`,          
    voodoo10 =`voodoo10 - Fill with 0's and 1's`,
    tangram.easy1 =`tangram.easy1 - Fill with 0's and 1's`,   
    tangram.easy2 =`tangram.easy2 - Fill with 0's and 1's`,    
    tangram.easy3 =`tangram.easy3 - Fill with 0's and 1's`,    
    tangram.easy4 =`tangram.easy4 - Fill with 0's and 1's`,    
    tangram.easy5 =`tangram.easy5 - Fill with 0's and 1's`,    
    tangram.easy6 =`tangram.easy6 - Fill with 0's and 1's`,    
    tangram.easy7 =`tangram.easy7 - Fill with 0's and 1's`,    
    tangram.easy8 =`tangram.easy8 - Fill with 0's and 1's`,   
    tangram.easy9 =`tangram.easy9 - Fill with 0's and 1's`,    
    tangram.easy10 =`tangram.easy10 - Fill with 0's and 1's`,   
    tangram.medium1 =`tangram.medium1 - Fill with 0's and 1's`,  
    tangram.medium2 =`tangram.medium2 - Fill with 0's and 1's`,  
    tangram.medium3 =`tangram.medium3 - Fill with 0's and 1's`,  
    tangram.medium4 =`tangram.medium4 - Fill with 0's and 1's`,  
    tangram.medium5 =`tangram.medium5 - Fill with 0's and 1's`,  
    tangram.medium6 =`tangram.medium6 - Fill with 0's and 1's`,  
    tangram.medium7 =`tangram.medium7 - Fill with 0's and 1's`,  
    tangram.medium8 =`tangram.medium8 - Fill with 0's and 1's`,  
    tangram.medium9 =`tangram.medium9 - Fill with 0's and 1's`,  
    tangram.medium10 =`tangram.medium10 - Fill with 0's and 1's`, 
    tangram.hard1=`tangram.hard1 - Fill with 0's and 1's`,   
    tangram.hard2=`tangram.hard2 - Fill with 0's and 1's`,   
    tangram.hard3=`tangram.hard3 - Fill with 0's and 1's`,    
    tangram.hard4 = `tangam.hard4 - Fill with 0's and 1's`,     
    tangram.hard5 = `tagram.hard5 - Fill with 0's and 1's`,     
    tangram.hard6 = `tangram.hard6 - Fill with 0's and 1's`,    
    tangram.hard7 = `tangram.hard7 - Fill with 0's and 1's`,    
    tangram.hard8 = `tangram.hard8 - Fill with 0's and 1's`,    
    tangram.hard9 = `tangram.hard9 - Fill with 0's and 1's`,
    tangram.hard10 = `tangram.hard10 - Fill with 0's and 1's`
  )

# check
is.factor(hate.df$topic.control)
levels(hate.df$topic.control) 

# add additional columns
hate.df <- hate.df %>% 
  mutate(
    topic = coalesce(topic.control, topic.realistic, topic.symbolic), # create condition column
    anger.total = (anger1 + anger2 + anger3 + anger4)/4, #sub-scale total
    contempt.total = (contempt1 + contempt2 + contempt3 + contempt4)/4,
    disgust.total = (disgust1 + disgust2 + disgust3 + disgust4)/4,
    hate.total = round( #anger scale total
      (anger1 + anger2 + anger3 + anger4 + 
         contempt1 + contempt2 + contempt3 + contempt4 + 
         disgust1 + disgust2 + disgust3 + disgust4)/12, digits = 2),
    political.total = (political.orientation + political.orientation.prog + 
                         political.orientation.econ + political.orientation.soc +
                         political.orientation.safe)/5,
    voodoo.total = (voodoo1 + voodoo2 + voodoo3 + voodoo4 + voodoo5 +
                      voodoo6 + voodoo7 + voodoo8 + voodoo9 + voodoo10),
    tangram.total.easy = (tangram.easy1 + tangram.easy2 + tangram.easy3 + 
                            tangram.easy4 + tangram.easy5 + tangram.easy6 +
                            tangram.easy7 + tangram.easy8 + tangram.easy9 + 
                            tangram.easy10),
    tangram.total.medium = (tangram.medium1 + tangram.medium2 + tangram.medium3 + 
                              tangram.medium4 + tangram.medium5 + tangram.medium6 +
                              tangram.medium7 + tangram.medium8 + tangram.medium9 + 
                              tangram.medium10),
    tangram.total.hard = (tangram.hard1 + tangram.hard2 + tangram.hard3 + 
                            tangram.hard4 + tangram.hard5 + tangram.hard6 +
                            tangram.hard7 + tangram.hard8 + tangram.hard9 + 
                            tangram.hard10)
  )

# re-code tangram questions

hate.df <- hate.df %>%
  mutate(
    tangram.total.easy = recode(tangram.total.easy, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4, "6"=5, "7"=6, "8"=7, "9"=8, "10"=9),
    tangram.total.medium = recode(tangram.total.medium, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4, "6"=5, "7"=6, "8"=7, "9"=8, "10"=9),
    tangram.total.hard = recode(tangram.total.hard, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4, "6"=5, "7"=6, "8"=7, "9"=8, "10"=9),
  )

# select and order columns 

hate.df <- hate.df %>% 
  select(1:6, 73, 12, 13, 74, 17, 18, 75:79, 82) 

# truthfulness check

hate.df %>%
  filter(truthfulness == "No")

# how many per condition

hate.df %>%
  count(condition, topic)

#------------------CALCULATIONS--------------

# descriptives

hate.df %>%
  summarise(n=n(),
            age_mean=mean(age),
            age_sd=sd(age)) 

# -------realistic threats-------

m.checks.descriptives <- hate.df %>% 
  group_by(condition) %>%
  get_summary_stats(mcheck1, mcheck2, type ="mean_sd") %>% 
  print()

violon.realistic <- hate.df %>% 
  select(mcheck1, mcheck2, condition) %>%
  gather(key="check", value="val", -condition) %>%
  filter(condition=="realistic") %>%
  ggplot(aes(x=check, y=val)) +
  geom_violin(fill="lightblue") +
  stat_summary(fun=mean, geom ="point", col="red")

violon.symbolilc <- hate.df %>% 
  select(mcheck1, mcheck2, condition) %>%
  gather(key="check", value="val", -condition) %>%
  filter(condition=="symbolic") %>%
  ggplot(aes(x=check, y=val)) +
  geom_violin(fill="lightblue") +
  stat_summary(fun=mean, geom ="point", col="red")

violon.control <- hate.df %>% 
  select(mcheck1, mcheck2, condition) %>%
  gather(key="check", value="val", -condition) %>%
  filter(condition=="control") %>%
  ggplot(aes(x=check, y=val)) +
  geom_violin(fill="lightblue") +
  stat_summary(fun=mean, geom ="point", col="red")

# compare mcheck1 to mcheck2 with paired t-test

m.checks.realistic <- hate.df %>% 
  filter(condition=="realistic") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  t_test(val ~ check, paired=TRUE, detailed=TRUE) %>%
  add_significance() %>%
  print()

m.checks.realistic.d <- hate.df %>% 
  filter(condition=="realistic") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  rstatix::cohens_d(val ~ check, paired=TRUE) %>%
  print()

# -------symbolic threats-------

m.checks.symbolic <- hate.df %>% 
  filter(condition=="symbolic") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  t_test(val ~ check, paired=TRUE, detailed=TRUE) %>%
  add_significance() %>%
  print()

m.checks.symbolic.d <- hate.df %>% 
  filter(condition=="symbolic") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  rstatix::cohens_d(val ~ check, paired=TRUE) %>%
  print()

# -------control group-------

m.checks.control <- hate.df %>% 
  filter(condition=="control") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  t_test(val ~ check, paired=TRUE, detailed=TRUE) %>%
  add_significance() %>%
  print()

m.checks.control.d <- hate.df %>% 
  filter(condition=="control") %>% 
  select(mcheck1, mcheck2) %>%
  gather(key="check", value="val", mcheck1, mcheck2) %>%
  rstatix::cohens_d(val ~ check, paired=TRUE) %>%
  print()



