# Import dataset, set heading to 'yes' and row names to 'automatic' if not already
# Alt + Enter to run line
# datasetOGF is Our Guardian Feature
# Both ID & ID_OGF go from 1-15 so the y-axis can draw 1-15.

library(ggplot2)

mean(dataset$Age)  # 24.73
mean(dataset$Age_OGF) # 26.2
median(dataset$Age) # 22
median(dataset$Age_OGF) # 24

# NASA RAW of Standard Guardian Feature
# How mentally demanding was the task?
# Histogram
hGramMD <- ggplot(dataset, aes(x=dataset$NASA_MD_OGF)) 
hGramMD + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How mentally demanding was the task?' of Our Guardian Feature") +
  labs(x= "1 - Low, 20 - High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_MD, dataset$NASA_MD_OGF,
        main = "How mentally demanding was the task?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_MD)
mean(dataset$NASA_MD_OGF)
median(dataset$NASA_MD)
median(dataset$NASA_MD_OGF)

# How physically demanding was the task?
# Histogram
hGramPD <- ggplot(dataset, aes(x=dataset$NASA_PD_OGF))
hGramPD + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How physically demanding was the task?' of Our Guardian Feature") +
  labs(x= "1 - Low, 20 - High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_PD, dataset$NASA_PD_OGF,
        main = "How physically demanding was the task?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_PD)
mean(dataset$NASA_PD_OGF)
median(dataset$NASA_PD)
median(dataset$NASA_PD_OGF)

# How hurried or rushed was the pace of the task?
hGramRUSH <- ggplot(dataset, aes(x=dataset$NASA_RUSH_OGF))
hGramRUSH + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How hurried or rushed was the pace of the task?' of Our Guardian Test") +
  labs(x= "1 - Low, 20 - High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_RUSH, dataset$NASA_RUSH_OGF,
        main = "How hurried or rushed was the pace of the task?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_RUSH)
mean(dataset$NASA_RUSH_OGF)
median(dataset$NASA_RUSH)
median(dataset$NASA_RUSH_OGF)
# How hard did you have to work to accomplish your level of performance?
# Histogram
hGramLOP <- ggplot(dataset, aes(x=dataset$NASA_PERFORMANCEDEMAND_OGF))
hGramLOP + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How hard did you have to work to accomplish your level of performance?' of Our Guardian Feature") +
  labs(x= "1 - Very Low, 20 - Very High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_PERFORMANCEDEMAND, dataset$NASA_PERFORMANCEDEMAND_OGF,
        main = "How hard did you have to work to accomplish your level of performance?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_PERFORMANCEDEMAND)
mean(dataset$NASA_PERFORMANCEDEMAND_OGF)
median(dataset$NASA_PERFORMANCEDEMAND)
median(dataset$NASA_PERFORMANCEDEMAND_OGF)

# How successful were you in accomplishing what you were asked to do?
hGramLOS <- ggplot(dataset, aes(x=dataset$NASA_SUBJECTIVE_SUCCES_OGF))
hGramLOS + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How successful were you in accomplishing what you were asked to do?' of Our Guardian Feature") +
  labs(x= "1 - Low, 20 - High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_SUBJECTIVE_SUCCES, dataset$NASA_SUBJECTIVE_SUCCES_OGF,
        main = "How successful were you in accomplishing what you were asked to do?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_SUBJECTIVE_SUCCES)
mean(dataset$NASA_SUBJECTIVE_SUCCES_OGF)
median(dataset$NASA_SUBJECTIVE_SUCCES)
median(dataset$NASA_SUBJECTIVE_SUCCES_OGF)
# How insecure, discouraged, irritated, stressed, and annoyed were you?
hGramIDISA <- ggplot(dataset, aes(x=dataset$NASA_IDISA))
hGramIDISA + geom_histogram(binwidth = 1, colour = "black", fill="red") +
  ggtitle("Histogram for 'How insecure, discouraged, irritated, stressed and annoyed were you?' of Our Guardian Feature") +
  labs(x= "1 - Low, 20 - High") +
  coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$NASA_IDISA, dataset$NASA_IDISA_OGF,
        main = "How insecure, discouraged, irritated, stressed, and annoyed were you?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20)
)
mean(dataset$NASA_IDISA)
mean(dataset$NASA_IDISA_OGF)
median(dataset$NASA_IDISA)
median(dataset$NASA_IDISA_OGF)
library(moments)
# Kurtosis & Skewness

# System Usability Scale
# https://github.com/ekapros/sus.compute.R
# Git kode til at regne SUS
# >80.3       Excellent Score
# 68 – 80.3   Good Score
# 68          Okay Score
# 51 – 68     Poor Score
# <51         Awful Score

# Husk at tjekke kurtosis og skewness og den slags
# Måske er det muligt at få mere normalt fordelt data ved LOG el. squareroot af vores data
# Graf med boxplots af NASA Raw spørgsmål???

# Mann Whitney Test & T-Test
wilcox.test(dataset$NASA_MD, dataset$NASA_MD_OGF)
# w = 41.5, p = 0.003353
wilcox.test(dataset$NASA_PD, dataset$NASA_PD_OGF) 
# w = 83, p = 0.2229
wilcox.test(dataset$NASA_RUSH, dataset$NASA_RUSH_OGF) 
# w = 91.5, p = 0.3908
wilcox.test(dataset$NASA_PERFORMANCEDEMAND, dataset$NASA_PERFORMANCEDEMAND_OGF) 
# w = 39.5, p = 0.002539
wilcox.test(dataset$NASA_SUBJECTIVE_SUCCES, dataset$NASA_SUBJECTIVE_SUCCES_OGF) 
# 118.8, p = 0.8185
wilcox.test(dataset$NASA_IDISA, dataset$NASA_IDISA_OGF) 
# w = 55, p = 0.01775
wilcox.test(dataset$VRHOURS, dataset$VRHOURS_OGF)
# W = 115.5, p-value = 0.9155
wilcox.test(dataset$FRUSTRATION_SCAPE, dataset$FRUSTRATION_SCAPE_OGF)
# W = 179.5, p-value = 0.004854
wilcox.test(dataset$HM_USE, dataset$HM_USE_OGF)
# W = 46, p-value = 0.004065
wilcox.test(dataset$ACTIVATION_TIME, dataset$ACTIVATION_TIME_OGF)
# w = 116.5, p-value = 0.8794
t.test(dataset$NASA_MD, dataset$NASA_MD_OGF)
# data:  dataset$NASA_MD and dataset$NASA_MD_OGF
# t = -3.4172, df = 27.02, p-value = 0.002018
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -9.069029 -2.264304
# sample estimates:
#   mean of x mean of y 
# 8.466667 14.133333 

t.test(dataset$NASA_PD, dataset$NASA_PD_OGF)
# data:  dataset$NASA_PD and dataset$NASA_PD_OGF
# t = -1.0679, df = 27.862, p-value = 0.2947
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.864179  1.530846
# sample estimates:
#   mean of x mean of y 
# 4.466667  6.133333 

t.test(dataset$NASA_RUSH, dataset$NASA_RUSH_OGF)
# data:  dataset$NASA_RUSH and dataset$NASA_RUSH_OGF
# t = -0.93883, df = 27.499, p-value = 0.356
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.032630  1.499297
# sample estimates:
#   mean of x mean of y 
# 4.666667  5.933333 

t.test(dataset$NASA_PERFORMANCEDEMAND, dataset$NASA_PERFORMANCEDEMAND_OGF)
# data:  dataset$NASA_PERFORMANCEDEMAND and dataset$NASA_PERFORMANCEDEMAND_OGF
# t = -3.7849, df = 21.01, p-value = 0.001085
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -10.123011  -2.943655
# sample estimates:
#   mean of x mean of y 
#5.80000  12.33333 

t.test(dataset$NASA_SUBJECTIVE_SUCCES, dataset$NASA_SUBJECTIVE_SUCCES_OGF)
# data:  dataset$NASA_SUBJECTIVE_SUCCES and dataset$NASA_SUBJECTIVE_SUCCES_OGF
# t = 0.99897, df = 18.148, p-value = 0.3309
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.836416  5.169749
#  estimates:
#   mean of x mean of y 
# 14.26667  12.60000 

t.test(dataset$NASA_IDISA, dataset$NASA_IDISA_OGF)
# :  dataset$NASA_IDISA and dataset$NASA_IDISA_OGF
# t = -2.5509, df = 27.766, p-value = 0.01655
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -9.0165595 -0.9834405
# sample estimates:
#   mean of x mean of y 
# 7.2      12.2 

hGramVRHOURS <- ggplot(dataset, aes(x=dataset$VRHOURS))
hGramVRHOURS + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for how many hours participants have used Virtual Reality before (SGF)") +
  labs(x= "1 = <1 hour, 2 = 1-5 hours, 3 = 5-20 hours, 20-60 hours,  5 = >60 hours")
  #coord_cartesian(xlim = c(0,20))

# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$FRUSTRATION_SCAPE, dataset$FRUSTRATION_SCAPE_OGF,
        main = "How frustrating did you find the space?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20),
        xlab = "0 = Not very frustrating, 20 = Very frustrating"
)
# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$HM_USE, dataset$HM_USE_OGF,
        main = "How much did you make use of the feature?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20),
        xlab = "1 = Very much, 20 = Not very much")
)
# Boxplot
par(mar = c(5, 4, 4, 2) + 0.1)
boxplot(dataset$ACTIVATION_TIME, dataset$ACTIVATION_TIME_OGF,
        main = "Did the feature activate in good time?",
        names = c("Standard Guardian Feature", "Our Guardian Feature"),
        horizontal = FALSE,
        col = "red",
        notch = FALSE,
        ylim = c(0, 20),
        xlab = "1 = Too early, 20 = Too late")
)