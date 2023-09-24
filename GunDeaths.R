library(dplyr)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(reshape2)

FileList <- list.files("../Kaggle/Gun/")
DF <- lapply(FileList,function(x){read.csv(paste0("../Kaggle/Gun/",x))})
DF <- do.call('rbind',DF)

DF <- DF %>% mutate(year = as.factor(year))%>% mutate_if(is.character,as.factor)
DF <- DF %>% mutate(AGE = case_when(age < 10~"<10",
                                    age >=10 & age<20~"10-19",
                                    age >=20 & age<30~"20-29",
                                    age >=30 & age<40~"30-39",
                                    age >=40 & age<50~"40-49",
                                    age >=50 & age<60~"50-59",
                                    age >=60 & age<70~"60-69",
                                    age >=70 & age<80~"70-79",
                                    age >=80~">80"))
DF$police <- as.factor(DF$police)


p1 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=sex),position = "dodge") + ggtitle("Gun deaths per year by Gender")
p2 <- ggplot(DF, aes(x=sex)) + geom_bar(aes(fill=sex))+coord_flip()+ ggtitle("Total gun deaths by Gender")

p3 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=place),position = "dodge")+ ggtitle("Gun deaths per year by Place")
p4 <- ggplot(DF, aes(x=place)) + geom_bar(aes(fill=place))+coord_flip()+ ggtitle("Total gun deaths by place")

p5 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=race),position = "dodge")+ ggtitle("Gun deaths per year by Race")
p6 <- ggplot(DF, aes(x=race)) + geom_bar(aes(fill=race))+coord_flip()+ ggtitle("Total gun deaths by race")

p7 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=intent),position = "dodge")+ ggtitle("Gun deaths per year by Intent")
p8 <- ggplot(DF, aes(x=intent)) + geom_bar(aes(fill=intent))+coord_flip()+ ggtitle("Total gun deaths by Intent")

p9 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=education),position = "dodge")+ ggtitle("Gun deaths per year by Education")
p10 <- ggplot(DF, aes(x=education)) + geom_bar(aes(fill=education))+coord_flip()+ ggtitle("Total gun deaths by Education")

p11 <- ggplot(DF, aes(x=year)) + geom_bar(aes(fill=AGE),position = "dodge")+ ggtitle("Gun deaths per year by Age")
p12 <- ggplot(DF, aes(x=AGE)) + geom_bar(aes(fill=AGE))+coord_flip()+ ggtitle("Total gun deaths by Age")

grid.arrange(p1, p2, p3, p4, p5, p6 ,ncol=2)
grid.arrange(p7,p8,p9,p10,p11,p12 ,ncol=2)

DeathsByMonth <- DF %>% group_by(month) %>% tally() %>% mutate(MonthName=factor(month.abb,levels=month.abb))
ggplot(DeathsByMonth, aes(x=MonthName,y=n,group=1)) + geom_line() + geom_point()


MonthByRace <- as.data.frame.matrix(table(DF$month,DF$race))
MonthByRace$MonthName <- factor(month.abb,levels=month.abb)
MonthByRace <-melt(MonthByRace,id="MonthName")
ggplot(MonthByRace, aes(x=MonthName,y=value,group=variable,color=variable)) + 
  geom_line()+ 
  stat_summary(fun.y = sum, na.rm = TRUE, group = 1, color = 'black', geom ='line')


RacebyPolice <- as.data.frame.matrix(table(DF$race,DF$police))
RacebyPolice$Proportion <- round((RacebyPolice$"1"/RacebyPolice$"0")*100,2)
colnames(RacebyPolice) <- c("No Police Involvement","Police Involvement","Proportion (%)")
kbl(RacebyPolice)%>%
  kable_styling(bootstrap_options = "striped", font_size = 15)


RacebyIntent <- as.data.frame.matrix(table(DF$race,DF$intent))
kbl(RacebyIntent)%>%
  kable_styling(bootstrap_options = "striped", font_size = 15)
ggplot(DF, aes(x=intent)) + geom_bar(aes(fill=race))+ ggtitle("Total gun deaths by Race and Intent")


o <- glm(police~AGE+race,data=DF, family="binomial")
summary(o)
