library('countrycode')
library('ggplot2')

DF <- read.csv("customer_data.csv")

DF$region <- countrycode(sourcevar = DF$country, origin = "country.name",destination = "region")
#DF$region[is.na(DF$region)] <- "other"
DF <- na.omit(DF)
DF$region <- as.factor(DF$region)
DF$education <- as.factor(DF$education)
df <- as.data.frame(table(DF$region))

ggplot(data=df, aes(x = "", y= Freq, fill = Var1)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1")


model <- lm(income ~ gender + relevel(education,ref="High School"), data=DF)
model <- lm(spending ~ income+relevel(education,ref="High School"), data=DF)
summary(model)

