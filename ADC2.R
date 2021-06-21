library(dplyr)
library(corrgram)
library(ggplot2)
library(emmeans)
library(interactions)
library(readxl)

df <- read_excel('DataForFigure2.1WHR2021C2.xls')

attributes(df)
str(df)

df$`Country name` <- as.factor(df$`Country name`)
df$`Regional indicator` <- as.factor(df$`Regional indicator`)

colnames(df) <- c('Country', 'Region', 'Score', 'SD', 'upperwhisker', 'lowerwhisker', 'GDP', 'Social', 'Healthy', 'Freedom', 'Generosity', 'Corruption', 'Dystopia', 'EXP.GDP', 'EXP.Social', 'EXP.Healthy', 'EXP.Freedom', 'EXP.Generosity', 'EXP.Corr', 'DYS+Res')
colnames(df)

fel <- df[, -c(4:6, 13:20)]
colnames(fel)

fel <- mutate(fel, Score = row_number())

sum(is.na(fel)) # No NA

# Correlation

fel_num <- select_if(fel, is.numeric)
M <- cor(fel_num)
corrgram(M, order = TRUE, upper.panel = panel.cor, main='Happiness Correlation Matrix')

set.seed(10)

smp_size <- floor(0.75 * nrow(fel))

train_ind <- sample(seq_len(nrow(fel)), size = smp_size)

train <- fel[train_ind,]
test <- fel[-train_ind,]

str(fel$Region)
levels(fel$Region)
fel$Region <- relevel(fel$Region, ref="Western Europe")

# Modeling

summary(train)
colnames(train)
m1 <- lm(Score ~ ., data = train[, -1])
summary(m1)

null <- lm(Score ~ 1, data = train[, -1])
summary(null)

step(m1)

m1.step <- lm(formula = Score ~ Region + GDP + Social + Freedom + Corruption, data = train[, -1])
summary(m1.step)
plot(m1.step)

ggplot(train[], aes(Score,GDP)) + geom_point()
ggplot(train[], aes(Score,Freedom)) + geom_point()
ggplot(train[], aes(GDP,Freedom)) + geom_point()

ggplot(fel[which(fel[,"GDP"] <= 8.3),], aes(x=Freedom, y=Score))+geom_line(size=2, aes(color=GDP))

#interactions
#slop of Freedom changes for every one increase in GDP

int.mod <- lm(formula = Score ~ Region + GDP + Social + Freedom + GDP*Freedom, data = train[, -1])

summary(int.mod)#as GDP increases, slope of freedom decreases

interact_plot(int.mod, pred = GDP, modx = Freedom)
