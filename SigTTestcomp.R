

set.seed(88)
n <-  15
df <- data.frame("Gender" = c(rep("M",n),rep("F",n)),
                 "BMI" = c(runif(n*2,min=70,max=72))) 
var.test(BMI~Gender,data=df)
t.test(BMI~Gender,var.equal=T,data=df)


set.seed(31)
n <-  150
df <- data.frame("Gender" = c(rep("M",n),rep("F",n)),
                 "BMI" = c(runif(n*2,min=70,max=72))) 
var.test(BMI~Gender,data=df)
t.test(BMI~Gender,var.equal=T,data=df)





"A lower p -value is sometimes interpreted as meaning there is a stronger relationship between two variables. 
However, statistical significance 
means that it is unlikely that the null hypothesis is true (less than 5%).
Therefore, a significant p -value tells us that an intervention works, whereas an 
effect size tells us how much it works.
"