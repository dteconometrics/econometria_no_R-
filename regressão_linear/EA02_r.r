########################################################
####### Aula 02 - Regressão Linear Simples #############
########################################################


library(wooldridge)
library(ggplot2)
library(stargazer)
library(foreign)
library(tidyverse)


### Acessar dataset

data(wage1)
names(wage1)


wage1 %>%
  ggplot(aes(x=educ, y=wage))+
  geom_point(stat='identity')+
  geom_smooth(method='lm', se=FALSE)

ggplot(wage1, aes(x=educ, wage))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)


wage1 %>%
  dplyr::select(wage, educ) %>%
  cor()

plot(wage1$wage)

### Modelo de regressão linear

reg_simples = lm(wage~exper, data=wage1)
summary(reg_simples)

# Plotando a reta de regressão

wage1 %>%
  ggplot(aes(x=exper, y=wage))+
  geom_point(stat='identity')+
  geom_smooth(method='lm', se=FALSE)

# Entendendo melhor a ideia de aproximação linear

aprox_linear = lm(wage~female, data=wage1)
summary(aprox_linear)

# Exemplo 2.6 do Wooldridge

data(ceosal1)

# Extrair as variáveis
sal = ceosal1$salary
roe = ceosal1$roe
# Regressão com vetores
CEOregression = lm(sal~roe)
# Obtendo os valores preditos
sal.hat = fitted(CEOregression)
u.hat = resid(CEOregression)

par(mfrow=c(1,2))
plot(sal.hat)
abline(CEOregression, col='red')
plot(u.hat)
