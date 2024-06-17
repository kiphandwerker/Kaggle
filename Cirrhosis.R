library(mice)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
# Read data
C <- read.csv("cirrhosis.csv")

# Refactor and clean
#C$Status <- as.factor(C$Status)
str(C)
C$Drug <- as.factor(C$Drug)
C$Sex <- as.factor(C$Sex)
C$Ascites <- as.factor(C$Ascites)
C$Hepatomegaly <- as.factor(C$Hepatomegaly)
C$Spiders <- as.factor(C$Spiders)
C$Edema <- as.factor(C$Edema)
C$Stage <- as.factor(C$Stage)
C$Status <- as.factor(C$Status)
C <- C %>% mutate(AgeYrs = Age/365)

C_nodrug <- C %>% mutate(Drug = case_when(is.na(Drug)~"None",.default = Drug))


# Primary objective
#write.csv(C_nodrug, "cirrhosisNoDRUG.csv", na=".")

# Secondary objective
sapply(C, function(x)sum(is.na(x)))

set.seed(1)
# numerics 
NumericData <- C %>% select(where(is.numeric))

Cirr_nums <- data.frame(
  original = NumericData$Cholesterol,
  imputed_pmm = complete(mice(NumericData, method = "pmm"))$Cholesterol,
  imputed_cart = complete(mice(NumericData, method = "cart"))$Cholesterol,
  imputed_lasso = complete(mice(NumericData, method = "lasso.norm"))$Cholesterol,
  imputed_norm = complete(mice(NumericData, method = "norm"))$Cholesterol,
  imputed_normpred = complete(mice(NumericData, method = "norm.predict"))$Cholesterol
)


# Check which imputation fits the data the most visually
h1 <- ggplot(Cirr_nums, aes(x = original)) +
  geom_histogram(fill = "red", color = "#000000", position = "identity") +
  ggtitle("Original") +
  theme_classic()
h2 <- ggplot(Cirr_nums, aes(x = imputed_pmm)) +
  geom_histogram(fill = "green", color = "#000000", position = "identity") +
  ggtitle("PMM") +
  theme_classic()
h3 <- ggplot(Cirr_nums, aes(x = imputed_cart)) +
  geom_histogram(fill = "blue", color = "#000000", position = "identity") +
  ggtitle("CART") +
  theme_classic()
h4 <- ggplot(Cirr_nums, aes(x = imputed_lasso)) +
  geom_histogram(fill = "yellow", color = "#000000", position = "identity") +
  ggtitle("LASSO") +
  theme_classic()
h5 <- ggplot(Cirr_nums, aes(x = imputed_norm)) +
  geom_histogram(fill = "orange", color = "#000000", position = "identity") +
  ggtitle("NORM") +
  theme_classic()
h6 <- ggplot(Cirr_nums, aes(x = imputed_normpred)) +
  geom_histogram(fill = "purple", color = "#000000", position = "identity") +
  ggtitle("NORM.PRED") +
  theme_classic()

plot_grid(h1, h2, h3, h4, h5, h6, nrow = 3, ncol = 2)

set.seed(1)
# Factor data
FactorData <- C %>% select(where(is.factor)) %>% select(-Stage)
sapply(FactorData, function(x)sum(is.na(x)))
str(FactorData)


Cirr_facts <- data.frame(
  original = FactorData$Ascites,
  imputed_pmm = complete(mice(FactorData, method = "pmm"))$Ascites,
  imputed_cart = complete(mice(FactorData, method = "cart"))$Ascites,
  imputed_logreg = complete(mice(FactorData, method = "logreg"))$Ascites,
  imputed_lassologreg = complete(mice(FactorData, method = "lasso.logreg"))$Ascites,
  imputed_lassoselectlogreg = complete(mice(FactorData, method = "lasso.select.logreg"))$Ascites
)

# Check which imputation fits the data the most visually
h1 <- ggplot(Cirr_facts, aes(x = original)) +
  geom_bar(fill = "red", color = "#000000", position = "identity") +
  ggtitle("Original") +
  theme_classic()
h2 <- ggplot(Cirr_facts, aes(x = imputed_pmm)) +
  geom_bar(fill = "green", color = "#000000", position = "identity") +
  ggtitle("PMM") +
  theme_classic()
h3 <- ggplot(Cirr_facts, aes(x = imputed_cart)) +
  geom_bar(fill = "blue", color = "#000000", position = "identity") +
  ggtitle("CART") +
  theme_classic()
h4 <- ggplot(Cirr_facts, aes(x = imputed_logreg)) +
  geom_bar(fill = "yellow", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()
h5 <- ggplot(Cirr_facts, aes(x = imputed_lassologreg)) +
  geom_bar(fill = "orange", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()
h6 <- ggplot(Cirr_facts, aes(x = imputed_lassoselectlogreg)) +
  geom_bar(fill = "purple", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()

plot_grid(h1, h2, h3, h4, h5, h6 ,nrow = 3, ncol = 2)

set.seed(1)
FactorData <- C %>% select(where(is.factor))
Cirr_facts <- data.frame(
  original = FactorData$Stage,
  imputed_pmm = complete(mice(FactorData, method = "pmm"))$Stage,
  imputed_cart = complete(mice(FactorData, method = "cart"))$Stage,
  imputed_polr = complete(mice(FactorData, method = "polr"))$Stage,
  imputed_polyreg = complete(mice(FactorData, method = "polyreg"))$Stage,
  imputed_lda = complete(mice(FactorData, method = "lda"))$Stage
)

# Check which imputation fits the data the most visually
h1 <- ggplot(Cirr_facts, aes(x = original)) +
  geom_bar(fill = "red", color = "#000000", position = "identity") +
  ggtitle("Original") +
  theme_classic()
h2 <- ggplot(Cirr_facts, aes(x = imputed_pmm)) +
  geom_bar(fill = "green", color = "#000000", position = "identity") +
  ggtitle("PMM") +
  theme_classic()
h3 <- ggplot(Cirr_facts, aes(x = imputed_cart)) +
  geom_bar(fill = "blue", color = "#000000", position = "identity") +
  ggtitle("CART") +
  theme_classic()
h4 <- ggplot(Cirr_facts, aes(x = imputed_polr)) +
  geom_bar(fill = "yellow", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()
h5 <- ggplot(Cirr_facts, aes(x = imputed_polyreg)) +
  geom_bar(fill = "orange", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()
h6 <- ggplot(Cirr_facts, aes(x = imputed_lda)) +
  geom_bar(fill = "purple", color = "#000000", position = "identity") +
  ggtitle("logreg") +
  theme_classic()

plot_grid(h1, h2, h3, h4, h5, h6 ,nrow = 3, ncol = 2)

# Apply the imputed method to the numerics
#Cirr_nums_imputed <- complete(mice(NumericData, method = "pmm"))

# Apply imputation to factors
#Cirr_facts_imputed <- complete(mice(FactorData, method = "pmm"))

C_final <- complete(mice(C,method='pmm'))
# Combine data
C_final <- cbind(Cirr_facts_imputed,Cirr_nums_imputed)
sapply(C_final, function(x)sum(is.na(x)))



C$Status <- as.character(C$Status)
C_final <-  C_final %>% mutate(Status = case_match(Status, "C"~0, "CL"~0,"D"~1))

#write.csv(C_final, "cirrhosisCLEAN.csv")


CnoNA <- C %>% na.omit()
CnoNA$Status <- as.character(CnoNA$Status)
CnoNA <-  CnoNA %>% mutate(Status = case_match(Status, "C"~0, "CL"~0,"D"~1))

write.csv(CnoNA, "cirrhosisMESSY.csv")
str(CnoNA)
