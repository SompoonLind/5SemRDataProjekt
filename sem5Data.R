# Import dataset, set heading to 'yes' and row names to 'automatic' if not already
# Alt + Enter to run line
# datasetOGF is Our Guardian Feature
# Both ID & ID_OGF go from 1-15 so the y-axis can draw 1-15.
options(stringsAsFactors=FALSE)

library(ggplot2)

mean(dataset$Age)  # 24.73
mean(dataset$Age_OGF) # 26.2
median(dataset$Age) # 22
median(dataset$Age_OGF) # 24

# NASA RAW of Standard Guardian Feature
# How mentally demanding was the task?
hGramMD <- ggplot(dataset, aes(x=dataset$NASA_MD)) 
hGram + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for the mental demand required for use the standard guardian feature") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

# How physically demanding was the task?
hGramPD <- ggplot(dataset, aes(x=dataset$NASA_PD))
hGram + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for the physical demand required for use the standard guardian feature") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

# How hurried or rushed was the pace of the task?
hGramRUSH <- ggplot(dataset, aes(x=dataset$NASA_RUSH))
hGram + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for how rushed the users felt during the test") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

# How hard did you have to work to accomplish your level of performance?
hGramLOP <- ggplot(dataset, aes(x=dataset$NASA_PERFORMANCEDEMAND))
hGramLOP + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for the users subjective performance level") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

# How successful were you in accomplishing what you were asked to do?
hGramLOS <- ggplot(dataset, aes(x=dataset$NASA_SUBJECTIVE_SUCCES))
hGram + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for participants subjective level of success") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

# How insecure, discouraged, irritated, stressed, and annoyed were you?
hGramIDISA <- ggplot(dataset, aes(x=dataset$NASA_IDISA))
hGram + geom_histogram(binwidth = 1, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for how insecure, discouraged, irritated, stressed and annoyed the participant felt during the test") +
  labs(x= "Scale from 1-20") +
  coord_cartesian(xlim = c(0,20))

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
