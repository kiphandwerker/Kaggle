library(ggplot2)
df <- read.csv('Student Mental health.csv')
colnames(df) <- c("TimeStamp","Gender","Age","Major","Year","CGPA","Married","Depression","Anxiety","PanicAttacks","Treatment")
df$Depression <-  as.factor(df$Depression)

table(df$Choose.your.gender)

ggplot(data=df,aes(x=Age,fill=Choose.your.gender)) + geom_histogram()

table(df$Age)

lm(Depression~Age ,data=df)

summary(glm(factor(Depression)~Age+Gender,data=df,family = binomial()))

ggplot(data=df,aes(x=Depression,fill=Gender)) + geom_bar()

chisq.test(df$Gender,df$Depression)
