library(tidyverse)
library(skimr)
wood_density <- read.csv("Data/wood_density.csv") ### reading in data
view(wood_density) ### looking at the data
skim(wood_density) ### get an overview of the data 

###drawing linear model
wood_density %>%
  ggplot(aes(x= Density, y = Hardness))+
  geom_smooth(method="lm")+
  geom_point()

density_model <- lm(Hardness~Density, data=wood_density)
density_model

coef(density_model)[1]+
  coef(density_model)[2]*24.7

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)
  fitted(density_model)
  
library(broom)
  broom::glance(density_model)
  broom::tidy(density_model, conf.int=TRUE)
  broom::augment(density_model, wood_density, interval="confidence") 
  