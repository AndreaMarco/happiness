install.packages("tidyverse")
library(tidyverse)
library(dplyr)

list.files(path = "../input")

felicitat <- read.csv(file='2019.csv', header = TRUE) # read file

attributes(felicitat)
str(felicitat)

df <- as.data.frame(felicitat[,c('Country.or.region', 'Score', 'GDP.per.capita', 'Social.support', 'Healthy.life.expectancy', 'Freedom.to.make.life.choices', 'Generosity','Perceptions.of.corruption')])

fel <- as.data.frame(felicitat[,c( 'Score', 'GDP.per.capita', 'Social.support', 'Healthy.life.expectancy', 'Freedom.to.make.life.choices', 'Generosity', 'Perceptions.of.corruption')])

colnames(fel) <- c('Score', 'GDP', 'Social', 'Healthy', 'Freedom', 'Generosity','Corruption')
colnames(fel)

sum(is.na(fel)) # no NA

# Correlation

df.num <- select_if(df, is.numeric)
M_cor <- cor(df.num)
M_cor

# Forta correlació entre Healthy life expectancy i GDP per Capita

# Primer, fit a un model de regresió lineal multiple. El Score es la variable dependent

fit <- lm(Score ~ GDP + Social + Healthy + Freedom + Generosity + Corruption, data = fel)

sumn_fit <- summary(fit)
sumn_fit


residplot <- function(fit, nbreaks = 20) {
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean = mean(z), sd = sd(z)), add = TRUE, col = "blue", lw = 1)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright", legend = c("Normal Curve", "Kernel Density Curve"), lty = 1:4, col = c("blue", "red"), cex = 0.7)
}
residplot(fit)

library('ggplot2')

ggplot(felicitat) + geom_point(aes(x=Score, y=GDP.per.capita, col = Healthy.life.expectancy))
plot(fit, which = 1)

zstates <- as.data.frame(scale(fel)) #standardized
zfit <- lm(Score ~ GDP + Social + Healthy + Freedom + Generosity + Corruption, data=fel)
plot(zfit$coefficients,col='dark red', ylab = 'Regression Coefficients',
     pch=19,cex = 2,xlab = '')
grid()
text(seq(1, 7, by=1),par("usr")[3] -0.1,labels = (names(zfit$coefficients)),
     srt = 45, pos = 1, xpd = TRUE)

fel$Score.index <- factor(fel$Score> mean(fel$Score), levels=c(TRUE,FALSE),labels=c("High", "Low"))

fit.full<- glm(Score.index ~ GDP + Social + Healthy + Freedom + Generosity + Corruption, family=binomial(), data=fel)
summary(fit.full)

fit.null <- glm(Score.index ~ 1, data = fel, family = binomial())
anova(fit.null, fit.full, test = "Chisq")

fit.reduced <- glm(Score.index ~ Social + Healthy + Freedom, data = fel, family = binomial())
summary(fit.reduced)
