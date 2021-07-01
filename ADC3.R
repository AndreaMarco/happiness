library(tidyverse)
library(dplyr)

df <- read_excel("DataPanelWHR2021C2.xls")
attributes(df)
str(df)

df$`Country name` <- as.factor(df$`Country name`)

colnames(df) <- c('Country', 'Year', 'Score', 'GDP', 'Social', 'Healthy', 'Freedom','Generosity', 'Corruption', 'Possitive', 'Negative')
colnames(df)

fel <- df[,-c(2, 10, 11)]
colnames(fel)

fel <- fel[!is.na(fel$Country),]
fel <- fel[!is.na(fel$Score),]
fel <- fel[!is.na(fel$GDP),]
fel <- fel[!is.na(fel$Social),]
fel <- fel[!is.na(fel$Healthy),]
fel <- fel[!is.na(fel$Freedom),]
fel <- fel[!is.na(fel$Generosity),]
fel <- fel[!is.na(fel$Corruption),]

sum(is.na(fel))

fel_g1 <- fel[which(fel$Score >= mean(fel$Score)),]
fel_g2 <- fel[which(fel$Score < mean(fel$Score)),]

permu_test <- function(fel_g1, fel_g2) {
  nr = 10000
  st <- numeric(nr)
  n1 = length(fel_g1)
  n2 = length(fel_g2)
  total <- n1 + n2
  sttrue = abs(mean(fel_g1)- mean(fel_g2))
  print("Stadistic: ")
  print(sttrue)
  cnt = 0
  
  vect = c(fel_g1, fel_g2)
  for(i in 1:nr) {
    d = sample(vect, n1 + n2)
    ne <- d[1:n1]
    a <- n1 + 1
    co <- d[a:total]
    st[i] <- abs(mean(ne) - mean(co))
    if (st[i] > sttrue) cnt = cnt + 1
  }
  
  return(cnt/nr)
}

nums <- unlist(lapply(fel, is.numeric))
fel_g1_num <- fel_g1[, nums]
fel_g2_num <- fel_g2[, nums]

print("Mean test")

p_values_mean_test <- c()
columnes <- colnames(fel_g1_num)
ncols <- length(columnes)

for(i in 1:ncols){
  c <- columnes[i]
  print(c)
  p_values_mean_test[i] <- permu_test(fel_g1_num %>% pull(c), fel_g2_num %>% pull(c))
  print(p_values_mean_test[i])
}

parametric_Boostrap <- function(data, type){
  n <- length(data)
  n_simulation <- 100000
  if (type == "mean"){
    mu <- mean(data)
    sd <- sqrt(var(data))
    bootstrapMeans <- numeric(n_simulation)
    for(i in 1:n_simulation){
      m_sim <- rnorm(n, mu, sd)
      bootstrapMeans[i] <- mean(m_sim)
    }
    return(quantile(bootstrapMeans, probs = c(0.025, 0.975)))
  }
  else if(type == "proportion") {
    mu <- length(data[data==1])/length(data)
    sd <- sqrt(mu * (1-mu)/n)
    bootstrapProp <- numeric(n_simulation)
    for(i in 1:n_simulation) {
      propSimul <- rnorm(n, mu, sd)
      bootstrapProp[i] <- mean(propSimul)
    }
    return(quantile(bootstrapProp, probs = c(0.025, 0.975)))
  }
  else return("Error")
}

interval_GDP_g1<-parametric_Boostrap(fel_g1_num$GDP,type = "mean")
interval_GDP_g2<-parametric_Boostrap(fel_g2_num$GDP,type = "mean")
interval_Social_g1<-parametric_Boostrap(fel_g1_num$Social,type = "mean")
interval_Social_g2<-parametric_Boostrap(fel_g2_num$Social,type = "mean")
interval_Healthy_g1<-parametric_Boostrap(fel_g1_num$Healthy,type = "mean")
interval_Healthy_g2<-parametric_Boostrap(fel_g2_num$Healthy,type = "mean")
interval_Freedom_g1<-parametric_Boostrap(fel_g1_num$Freedom,type = "mean")
interval_Freedom_g2<-parametric_Boostrap(fel_g2_num$Freedom,type = "mean")
interval_Generosity_g1<-parametric_Boostrap(fel_g1_num$Generosity,type = "mean")
interval_Generosity_g2<-parametric_Boostrap(fel_g2_num$Generosity,type = "mean")
interval_Corruption_g1<-parametric_Boostrap(fel_g1_num$Corruption,type = "mean")
interval_Corruption_g2<-parametric_Boostrap(fel_g2_num$Corruption,type = "mean")

print(interval_GDP_g1)
print(interval_GDP_g2)
print(interval_Social_g1)
print(interval_Social_g2)
print(interval_Healthy_g1)
print(interval_Healthy_g2)
print(interval_Freedom_g1)
print(interval_Freedom_g2)
print(interval_Generosity_g1)
print(interval_Generosity_g2)
print(interval_Corruption_g1)
print(interval_Corruption_g2)
