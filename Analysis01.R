library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)
library(broom.helpers)
library(tableone)
library(nhanesA)
library(pROC)
library(ggcorrplot)
library(gt)
#import data
risk_factors <- read.csv("MSc_Mammography_Dataset_20240529.csv")
density <- read.csv("One2One_Density.csv")
df <- left_join(density, risk_factors, by = join_by(QCode == qcode))
#Correlation between left and right 
Li_corr<-cor(df$Density_RMLO_Li,df$Density_LMLO_Li,use="complete.obs")
print(Li_corr) # Libra
Cu_corr<-cor(df$Density_RMLO_Cu,df$Density_LMLO_Cu,use="complete.obs")
print(Cu_corr) # Cumulus
# Visulalise the relationship of right and left
ggplot(df,aes(x= df$Density_RMLO_Li,y=df$Density_LMLO_Li))+
  geom_point(color ='blue',size=2,shape=16) +
  # geom_label(aes(label = df$QCode))
  labs(title = 'Libra scatter plot (RMLO & LMLO)',
       x = 'RMLO density reading',
       y = 'LMLO density reading') +
  theme_minimal()

ggplot(df,aes(x= df$Density_RMLO_Cu,y=df$Density_LMLO_Cu))+
  geom_point(color ='green',size=3,shape=16) +
  labs(title = 'Cumulus scatter plot (RMLO & LMLO)',
       x = 'RMLO density reading',
       y = 'LMLO density reading') +
  theme_minimal()
