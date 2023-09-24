library(ggplot2)
library(dplyr)
library(tidyverse)


FP <- read.csv("Fruit Prices 2020.csv")

ggplot(FP) + 
  aes(x = Form, y = CupEquivalentPrice, color = Form) + 
  geom_boxplot() +
  geom_jitter()


group_by(FP, Form) %>%
  summarise(
    count = n(),
    mean = mean(CupEquivalentPrice, na.rm = TRUE),
    sd = sd(CupEquivalentPrice, na.rm = TRUE),
    median = median(CupEquivalentPrice, na.rm = TRUE),
    IQR = IQR(CupEquivalentPrice, na.rm = TRUE)
  )


res_aov <- aov(CupEquivalentPrice~Form, data = FP)
summary(res_aov)
par(mfrow=c(2,2))
plot(res_aov)

kruskal.test(CupEquivalentPrice~Form, data = FP)

pairwise.wilcox.test(FP$CupEquivalentPrice, FP$Form,
                     p.adjust.method = "bonf")
