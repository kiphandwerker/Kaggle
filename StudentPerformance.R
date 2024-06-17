library(dplyr)
library(fmsb)
library(MASS)
library(ordinal)
library(rms)
df <- read.csv("DATA (1).csv")
df <- df[-1]

df <- df %>% 
  mutate(across(everything(), factor))

df2 <- df %>% 
  
  rename('Age' = X1, 'Sex' = X2, 'HStype' = X3,'Scholarship' = X4, 'AdditionalWork' = X5, 'Extracurriculars' = X6,
         'Partner' = X7, 'Salary' = X8, 'Transportation' = X9, 'LivingLocation' = X10,
         'MotherEdu' = X11, 'FatherEdu' = X12, 'Siblings' = X13, 'ParentalStatus' = X14, 'MotherOcc' = X15,
         'FatherOcc' = X16, 'WeeklyStudy' = X17, 'NonSciReadingFreq' = X18, 'SciReadingFreq' = X19,
         'DeptEngagememt' = X20, 'ImpactofProjects' = X21, 'Attendance' = X22, 'MidtermPrepGroup' = X23,
         'MidtermPrepStudy' = X24, 'NoteTaking' = X25, 'Listening' = X26, 'DiscussionViews' = X27,
         'FlipClassroomViews' = X28, 'LastSemesterGPA' = X29, 'ExpectedGPA' = X30,
         'CourseID' = COURSE.ID, 'Grade' = GRADE) %>% 
  
  mutate(
  Age = factor(Age, levels = 1:3,
                     labels = c('18-21','22-25','26+')),
  Sex = factor(Sex, levels = 1:2,
                  labels = c('female','male')),
  
  HStype = factor(HStype, levels = 1:3,
                  labels = c('private','state','other')),
  
  Scholarship = factor(Scholarship, levels = 1:5,
                       labels = c('None','25%','50%','75%','Full')),
  
  AdditionalWork = factor(AdditionalWork, levels = 1:2,
                       labels = c('Yes','No')),
  
  Extracurriculars  = factor(Extracurriculars, levels = 1:2,
                           labels = c('Yes','No')),
  
  Partner = factor(Partner, levels = 1:2,
                          labels = c('Yes','No')),
  
  Salary = factor(Salary, levels = 1:5,
                  labels = c('135-200','201-270','271-340','341-410','+410')),
  
  Transportation = factor(Transportation, levels = 1:4,
                          labels = c('Bus','Private car or taxi','Bike','Other')),
  
  LivingLocation = factor(LivingLocation, levels = 1:4,
                          labels = c('Rental','Dorms','Family','Other')),
  
  MotherEdu = factor(MotherEdu, levels = 1:6,
                     labels = c('Primary','Secondary','HS','University','MSc.','Ph.D')),
  
  FatherEdu = factor(FatherEdu, levels = 1:6,
                     labels = c('Primary','Secondary','HS','University','MSc.','Ph.D')),
  
  Siblings = factor(Siblings, levels = 1:5,
                    labels = c('1','2','3','4','5+')),
  
  ParentalStatus = factor(ParentalStatus, levels = 1:3,
                          labels = c('Married','Divorced','Died')),
  
  MotherOcc = factor(MotherOcc, levels = 1:6,
                          labels = c('Retired','Housewife','Government','Private','Selfemployeed','Other')),
  
  FatherOcc = factor(FatherOcc, levels = 1:5,
                     labels = c('Retired','Government','Private','Selfemployeed','Other')),
  
  WeeklyStudy = factor(WeeklyStudy, levels = 1:5,
             labels = c('None','<5 hours','6-10 hours','11-20 hours','+20 hours')),

  NonSciReadingFreq = factor(NonSciReadingFreq, levels = 1:3,
             labels = c('None', 'Sometimes', 'Often')),

  SciReadingFreq = factor(SciReadingFreq, levels = 1:3,
             labels = c('None', 'Sometimes', 'Often')),


  DeptEngagememt = factor(DeptEngagememt, levels = 1:2,
             labels = c('Yes','No')),
  
  ImpactofProjects = factor(ImpactofProjects, levels = 1:3,
             labels = c('Positive','Negative','Neutral')),
  
  Attendance = factor(Attendance, levels = 1:3,
             labels = c('Always', 'Sometimes', 'Never')),
  
  MidtermPrepGroup = factor(MidtermPrepGroup, levels = 1:3,
             labels = c('Alone', 'Group', 'NA')),
  
  MidtermPrepStudy = factor(MidtermPrepStudy, levels = 1:3,
             labels = c('Last minute', 'Regularly', 'Never')),
  
  NoteTaking = factor(NoteTaking, levels = 1:3,
             labels = c('Never','Sometimes','Always')),
  
  Listening = factor(Listening, levels = 1:3,
             labels = c('Never','Sometimes','Always')),
  
  DiscussionViews = factor(DiscussionViews, levels = 1:3,
             labels = c('Never','Sometimes','Always')),
  
  FlipClassroomViews = factor(FlipClassroomViews, levels = 1:3,
             labels = c('Useful', 'Not useful', 'NA')),
  
  LastSemesterGPA = factor(LastSemesterGPA, levels = 1:5,
             labels = c('<2.00','2.00-2.49','2.50-2.99', '3.00-3.49','+3.49')),
  
  ExpectedGPA = factor(ExpectedGPA, levels = 1:5,
             labels = c('<2.00','2.00-2.49','2.50-2.99', '3.00-3.49','+3.49')),
  
  CourseID = factor(CourseID, levels = 1:9,
             labels = as.character(1:9)),
  
  Grade = factor(Grade, levels = 0:7,
             labels = c('Fail','DD','DC','CC', 'CB', 'BB', 'BA', 'AA'))
  
)

write.csv(df2, "StudentPerformance.csv")

# maybe turn some factors into numerics?
# 1, 8, 17, 29,30


fit0 <- clm(Grade~1, data=df2, link='logit')
summary(fit.0)
fitall <- clm(Grade~., data=df2, link='logit')
summary(fitall)

anova(fit.0,fit.all)


stepmodel <- fitall %>% stepAIC(trace=T)
coef(stepmodel)

stepfit <- clm(Grade~Age + Sex + AdditionalWork + Extracurriculars + Transportation + 
      MotherEdu + ImpactofProjects + MidtermPrepGroup + LastSemesterGPA + 
      CourseID, data=df2, link='logit')





ologit <- lrm(Grade~.,data=df2)


