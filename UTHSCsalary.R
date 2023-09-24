library(dplyr)
library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)

NAME = c("Karen Johnson","Michelle Martin","Fridtjof Thomas","Phyllis Richey", "Charisse Madlock-Brown", "Mathilda Coday",
         "Julia Graber","James Hood","Joseph Handwerker")

POSITION2021 <-  c("Professor and Chair","Professor","Professor","Professor","Associate Professor","Professor",
                 "Research Project Coordinator","Research Project Coordinator","Research Coordinator I")
SALARY2021 <-  c(431742,201367,191000,178602,113906,91140,56644,52194,44013)
DF2021 <- data.frame("Name"=NAME,
                     "Position" = POSITION2021,
                     "Year" = as.factor(rep(2021,9)),
                     "Salary" = SALARY2021)



POSITION2022 = c("Professor and Chair","Professor","Professor","Professor","Associate Professor","Professor",
             "Research Project Coordinator","Research Project Coordinator","Research Study Coordinator I-L2")
SALARY2022 = c(447012,208222,198640,185746,118462,94786,60320,58909,48402)

DF2022 <- data.frame("Name"=NAME,
                 "Position" = POSITION2022,
                 "Year" = as.factor(rep(2022,9)),
                 "Salary" = SALARY2022)

DF <- DF2022 %>% left_join(DF2021,by="Name") %>% 
  mutate("Salary Change" = DF2022$Salary - DF2021$Salary,
         "Salary2022" = DF2022$Salary,
         "Salary2021" = DF2021$Salary,
         "Position Changed" = DF2021$Position != DF2022$Position,
         "Group" = c(1,2,2,2,2,2,3,3,3)) %>% 
  select("Name","Group","Salary2021","Salary2022","Position Changed","Salary Change")

DF
# DF <- data.frame(rbind(DF2021,DF2022))
# 
# ggplot(DF, aes(x="", y=Salary, color=Year))+
#   geom_boxplot()

wilcox.test(SALARY2021,SALARY2022,paired=TRUE)

DF %>%
  group_by(Group) %>%
  get_summary_stats(Salary2022 - Salary2021, type = "mean_sd")

o <- aov(Salary2022~Group, data=DF)
summary(o)
