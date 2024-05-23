library(ggplot2)
library(car)
library(MASS)
library(psych)


install.packages("dplyr")
library(dplyr)


setwd("C:/Users/abhil/OneDrive/Desktop/drivers/predictive-modelling")
data<-read.csv("Dataset_2024.csv")
print(data)
column_names<-names(data)
print(column_names)


pairs(data)

ggplot(data, aes(x = Age..years., y = Body.fat....)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body.fat.... vs Age..years.", x = "Age..years. (years)", y = "Body.fat.... (%)") +
  theme_minimal()

ggplot(data, aes(x = Chest.circumference..cm., y = Body.fat....)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body.fat.... vs Chest.circumference..cm.", x = "Chest.circumference..cm.", y = "Body.fat....") +
  theme_minimal()

ggplot(data, aes(x = Density..g.cm.., y = Body.fat....)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Density", x = "Density (g/cm³)", y = "Body Fat (%)") +
  theme_minimal()

ggplot(data, aes(x = Knee.circumference..cm., y = Body.fat....)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Knee Circumference", x = "Knee Circumference (cm)", y = "Body Fat (%)") +
  theme_minimal()

ggplot(data, aes(x = Weight..lbs., y = Body.fat....)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Body Fat vs Weight", x = "Weight (lbs)", y = "Body Fat (%)") +
  theme_minimal()

windows(20,10)
pairs.panels(data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)        # If TRUE, adds confidence intervals


# Examine linearity in more detail using scatter plots



windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = data$Body.fat....,
               y = data$Age..years.,
               xlab = "body fat (,000)",
               ylab = "age %", main = "Correlation of age ~ body fat")

scatter.smooth(x = data$Chest.circumference..cm. ,
               y = data$Age..years.,
               xlab = "chest circumferrneece  (,000)",
               ylab = "age %", main = "Correlation of murder ~ Income ")


scatter.smooth(x = data$Density..g.cm..,
               y = data$Age..years.,
               main = "Correlation of age ~ density",
               xlab = "density %",
               ylab = "age %")

scatter.smooth(x = data$Knee.circumference..cm.,
               y = data$Age..years.,
               main = "Correlation of age ~ knee circumference ",
               xlab = "Knee.circumference..cm. ",
               ylab = "age %")

scatter.smooth(x = data$Weight..lbs. ,
               y = data$Age..years.,
               main = "Correlation of age ~ weight ",
               xlab = "weight ",
               ylab = "age %")



# Examining correlation between age and Independent variables


cor(data)

attach(data)
# Examining the other variables
paste("Correlation for age and bodyfat: ", round(cor(Age..years., Body.fat....),2))
paste("Correlation for Age and Chest.circumference: ", round(cor(Age..years., Chest.circumference..cm.),2))
paste("Correlation for Age and density: ", round(cor(Age..years., Density..g.cm..),2))
paste("Correlation for Age and knee circumference: ", round(cor(Age..years., Knee.circumference..cm.),2))
paste("Correlation for Age and weight: ", round(cor(Age..years., Weight..lbs.),2))

# Check for outliers


windows(20,10)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(data)

boxplot(Age..years.,
        main = "Age..years.") # box plot for 'age'

boxplot(Body.fat....,
        main = "Body.fat....") # box plot for 'body fat'

boxplot(Chest.circumference..cm.,
        main = "Chest.circumference..cm.") # box plot for 'chest cirucmnfernce'

boxplot(Density..g.cm..,
        main = "Density..g.cm..") # box plot for 'densityy'


boxplot(Weight..lbs.,
        main = "Weight..lbs.") # box plot for 'weight'


correlation_matrix<-cor(data)
windows(20,16)
corPlot(correlation_matrix)

cor(data$Age..years.,data$Chest.circumference..cm.)
cor(data$Age..years.,data$Body.fat....)
cor(data$Age..years.,data$Weight..lbs.)


##Changing all columns to proper names.
colnames(data)[colnames(data) == "Age..years."] <- "Age (years)"
colnames(data)[colnames(data) == "Body.fat...."] <- "Body fat (%)"
colnames(data)[colnames(data) == "Chest.circumference..cm."] <- "Chest circumference (cm)"
colnames(data)[colnames(data) == "Density..g.cm.."] <- "Density (g/cm)"
colnames(data)[colnames(data) == "Knee.circumference..cm."] <- "Knee circumference (cm)"
colnames(data)[colnames(data) == "Weight..lbs."] <- "Weight (lbs)"


##Calculating min, median, mean and 3ed quartile.
summary(data$"Age (years)")
summary(data$"Body fat (%)")
summary(data$"Chest circumference (cm)")
summary(data$"Density (g/cm)")
summary(data$"Knee circumference (cm)")
summary(data$"Weight (lbs))")

## checking for the linearity of data
library(psych)
windows(20,10)

pairs.panels(data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

print(data)
plot(density(data$`Age (years)`),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Age (years)"), 2)))
polygon(density(data$`Age (years)`), col = "red")


plot(density(data$"Body fat (%)"),
     main = "Density plot : bodyfat",
     ylab = "Frequency", xlab = "bodyfat",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Body fat (%)"), 2)))
polygon(density(data$"Body fat (%)"), col = "red")

plot(density(data$"Chest circumference (cm)"),
     main = "Density plot : Chest circumference (cm)",
     ylab = "Frequency", xlab = "Chest circumference (cm)",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Chest circumference (cm)"), 2)))
polygon(density(data$"Chest circumference (cm)"), col = "red")


plot(density(data$"Density (g/cm)"),
     main = "Density plot : Density (g/cm)",
     ylab = "Frequency", xlab = "Density (g/cm)",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Density (g/cm)"), 2)))
polygon(density(data$"Density (g/cm)"), col = "red")


plot(density(data$"Knee circumference (cm)"),
     main = "Density plot : Knee circumference (cm)",
     ylab = "Frequency", xlab = "Knee circumference (cm)",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Knee circumference (cm)"), 2)))
polygon(density(data$"Knee circumference (cm)"), col = "red")


plot(density(data$"Weight (lbs)"),
     main = "Density plot : Weight",
     ylab = "Frequency", xlab = "weight",
     sub = paste("Skewness : ", round(e1071::skewness(data$"Weight (lbs)"), 2)))
polygon(density(data$"Weight (lbs)"), col = "red")



# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -.05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0.5 = approx symetric.

paste("Skewness for age : ", round(e1071::skewness(data$"Age (years)"), 2))
paste("Skewness for body fat : ", round(e1071::skewness(data$"Body fat (%)"), 2))
paste("Skewness for chest circumferncece : ", round(e1071::skewness(data$"Chest circumference (cm)"), 2))
paste("Skewness for density : ", round(e1071::skewness(data$"Density (g/cm)"), 2))
paste("Skewness for knee circumference : ", round(e1071::skewness(data$"Knee circumference (cm)"), 2))
paste("Skewness for weight : ", round(e1071::skewness(data$"Weight (lbs)"), 2))


initial_model <- lm(Body.fat.... ~ Age..years. + Chest.circumference..cm. + Density..g.cm.. + Knee.circumference..cm. + Weight..lbs., data = data)


summary(initial_model)

vif(initial_model)

final_model <- stepAIC(initial_model, direction = "both")
summary(final_model)



