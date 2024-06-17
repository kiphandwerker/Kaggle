library(dplyr)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(data.table)
library(lme4)
o <- read.csv('UTHSCSalary.csv',header=T)



tbl <- reshape2::dcast(o, Name ~ Year, value.var = "Salary")
tbl <- tbl[(order(tbl$`2023`,decreasing = T)),]
row.names(tbl) <- NULL
kbl(tbl) %>% kable_classic()

