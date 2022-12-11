                  # For at køre kode nemt tryk på Alt + Enter.
library(ggplot2)  # Importer ggplot2 library
# Scatter Plot for condition A med en regression
scatterPlotTTATLXA <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(A)`, y = DataSheet$`TLX(A)`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTATLXA + 
  geom_point(colour = DataSheet$`TotalTime(A)`) + 
  geom_smooth(method = lm) + 
  ggtitle("Regression between Cognitive Load & Total Time Taken in Condition A (mirrored)") +
  labs(x="Total Time for Condition A (mirrored)", y="Cognitive Load (TLX(A))") #Briger geom_point(), geom_smooth() og labs() på scatterPlot Dataen

# Scatter plot for condition B med en regression
scatterPlotTTBTLXB <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(B)`, y = DataSheet$`TLX(B)`)) #Tilføjer TotalTime(B) og TLX(B) til variablen scatterPlot
scatterPlotTTBTLXB + 
  geom_point(colour = DataSheet$`TotalTime(B)`) + 
  geom_smooth(method = lm) + 
  labs(x="Total Time for condition B (imitated)", y = "Cognitive Load (TLX(B)") +
  ggtitle("Regression between Cognitive Load & Total Time Taken in Condition B (imitated)")

# Scatterplotter for TLX A & B med regression
scatterPlotTLXAB <- ggplot(DataSheet, aes(x = DataSheet$`TLX(A)`, y = DataSheet$`TLX(B)`))
scatterPlotTLXAB + 
  geom_point(color = "blue") +
  geom_smooth(method = lm) +
  labs(x = "Condition A Cognitive load(mirrored)", y = "Condition B Cognitive Load(imitated)")

# Scatterplotter for Total Time for både A og B
scatterPlotTTAB <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(B)`, y = DataSheet$`TLX(B)`))
scatterPlot +
  geom_point(color = DataSheet$`TotalTime(A)`) +
  geom_smooth(method = lm) +
  labs(x = "Total Time for Condition A(mirrored)", y = "Total Time for condition B(imitated)")

boxplot(DataSheet$`TotalTime(A)`, DataSheet$`TotalTime(B)`)
bPlot <- ggplot(DataSheet, aes(DataSheet$`TotalTime(A)`, DataSheet$`TotalTime(B)`)) +
  geom_boxplot()
install.packages(moments)
library(moments)

#Kurtosis og skewness for den uøndrede data
kurtosis(DataSheet$`TotalTime(A)`)
kurtosis(DataSheet$`TotalTime(B)`)
skewness(DataSheet$`TotalTime(A)`)
skewness(DataSheet$`TotalTime(B)`)

#Histogram for condition A's independent variable
hGram <- ggplot(DataSheet, aes(x=DataSheet$`TotalTime(A)`))
hGram + geom_histogram(binwidth = 5, colour = "black", fill="orangered2") +
  ggtitle("Histogram for Total time condition A(mirrored)") +
  labs(x= "Total Time for condition A (mirrored)")

#Histogram for condition B's independent variable
hGram <- ggplot(DataSheet, aes(x=DataSheet$`TotalTime(B)`))
hGram + geom_histogram(binwidth = 7, colour = "black", fill="steelblue2") +
  ggtitle("Histogram for Total time condition A(mirrored)") +
  labs(x= "Total Time for condition A (imitated)")

# Mean og standard deviation for normal data
mean(DataSheet$`TotalTime(A)`)
sd(DataSheet$`TotalTime(A)`)
mean(DataSheet$`TotalTime(B)`)
sd(DataSheet$`TotalTime(B)`)

#Square root transformation
DataSheet$`TotalTime(A)_sqrt` <- sqrt(DataSheet$`TotalTime(A)`)
DataSheet$`TotalTime(B)_sqrt` <- sqrt(DataSheet$`TotalTime(B)`)
DataSheet$`TLX(A)_sqrt` <- sqrt(DataSheet$`TLX(A)`)
DataSheet$`TLX(B)_sqrt` <- sqrt(DataSheet$`TLX(B)`)

#Log transformation
DataSheet$`TotalTime(A)_log` <- log(DataSheet$`TotalTime(A)`)
DataSheet$`TotalTime(B)_log` <- log(DataSheet$`TotalTime(B)`)
DataSheet$`TLX(A)_log` <- log(DataSheet$`TLX(A)`)
DataSheet$`TLX(B)_log` <- log(DataSheet$`TLX(B)`)

#Inverse Transformation
DataSheet$`TotalTime(A)_reciprocal` <- 1/(DataSheet$`TotalTime(A)`)
DataSheet$`TotalTime(B)_reciprocal` <- 1/(DataSheet$`TotalTime(B)`)
DataSheet$`TLX(A)_reciprocal` <- 1/(DataSheet$`TLX(A)`)
DataSheet$`TLX(B)_reciprocal` <- 1/(DataSheet$`TLX(B)`)

#Kurtosis for den log transformarede data
kurtosis(DataSheet$`TotalTime(A)_log`)
kurtosis(DataSheet$`TotalTime(B)_log`)

#Kurtosis for den squareroot transformerede data
kurtosis(DataSheet$`TotalTime(A)_sqrt`)
kurtosis(DataSheet$`TotalTime(B)_sqrt`)

#kurtosis for den inverse transformerede data
kurtosis(DataSheet$`TotalTime(A)_reciprocal`)
kurtosis(DataSheet$`TotalTime(B)_reciprocal`)

#Scatterplot for reciprocal transformation over condition A
scatterPlotTTATLXA_rp <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(A)_reciprocal`, y = DataSheet$`TLX(A)_reciprocal`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTATLXA_rp + 
  geom_point(colour = DataSheet$`TotalTime(A)_reciprocal`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition A after a Inverse Transformation") +
  labs(x="Total Time for Condition A (mirrored)", y="Cognitive Load (TLX(A))")

#Scatter plot for reciprocal transformation over condition B
scatterPlotTTBTLXB_rp <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(B)_reciprocal`, y = DataSheet$`TLX(B)_reciprocal`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTBTLXB_rp + 
  geom_point(colour = DataSheet$`TotalTime(B)_reciprocal`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition B after a Inverse Transformation") +
  labs(x="Total Time for Condition B (mirrored)", y="Cognitive Load (TLX(B))")

#Scatter plot for den log transformerede data over condition A
scatterPlotTTATLXA_log <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(A)_log`, y = DataSheet$`TLX(A)_log`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTATLXA_log + 
  geom_point(colour = DataSheet$`TotalTime(A)_log`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition A after a log transformation") +
  labs(x="Total Time for Condition A (mirrored)", y="Cognitive Load (TLX(A))")

#Scatter plot for den log transformerede data over condition A
scatterPlotTTBTLXB_log <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(B)_log`, y = DataSheet$`TLX(B)_log`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTBTLXB_log + 
  geom_point(colour = DataSheet$`TotalTime(B)_log`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition B after a log transformation") +
  labs(x="Total Time for Condition B (mirrored)", y="Cognitive Load (TLX(B))")

#Scatter plot for den square root transformerede data over condition A
scatterPlotTTATLXA_sqrt <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(A)_sqrt`, y = DataSheet$`TLX(A)_sqrt`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTATLXA_sqrt + 
  geom_point(colour = DataSheet$`TotalTime(A)_sqrt`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition A after a Square Root transformation") +
  labs(x="Total Time for Condition A (mirrored)", y="Cognitive Load (TLX(A))")

#Scatter plot for den square root transformerede data over condition B
scatterPlotTTBTLXB_sqrt <- ggplot(DataSheet, aes(x = DataSheet$`TotalTime(B)_sqrt`, y = DataSheet$`TLX(B)_sqrt`)) # Tilføjer ggplot data til scatterPlot variabel
scatterPlotTTBTLXB_sqrt + 
  geom_point(colour = DataSheet$`TotalTime(B)_sqrt`) + 
  geom_smooth(method = lm) + 
  ggtitle("Condition B after a Square Root transformation") +
  labs(x="Total Time for Condition B (mirrored)", y="Cognitive Load (TLX(B))")


###############

chisq.test(DataSheet$`TotalTime(A)`, DataSheet$`TotalTime(B)`, correct = FALSE)

mauchly.test(DataSheet$`TotalTime(A)`, DataSheet$`TotalTime(B)`)

shapiro.test(DataSheet$`TotalTime(A)`)
shapiro.test(DataSheet$`TotalTime(B)`)
