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
# import dataset
df <- read.csv("Wholedata.csv")
ggplot(df,aes(x= df$Avg_Li,y=df$Avg_Cu))+
  geom_point(color ='blue',size=3,shape=16) +
  labs(title = 'The correlation plot between Cumulus and LIBRA',
       x = 'LIBRA density reading',
       y = 'Cumulus density reading') +
  theme_minimal()
# correlation between LIBRA and Cumulus 
cor <- cor(df$Avg_Li,df$Avg_Cu,use="complete.obs")
print(cor)
### recategorical the varibales ###
# bmi entry group 

df$bmi_entry_group[df$bmi_entry_group == 1] <- "Underweight"
df$bmi_entry_group[df$bmi_entry_group == 2] <- "Normal"
df$bmi_entry_group[df$bmi_entry_group == 3] <- "Overweight"
df$bmi_entry_group[df$bmi_entry_group > 3 & df$bmi_entry_group <=6 ] <- "Obesity"
df$bmi_entry_group[df$bmi_entry_group == 888] <- "Missing"
df$bmi_entry_group[is.na(df$bmi_entry_group)] <- "Missing"

# age at menarche

df$age_menarche[df$age_menarche <= 12] <- "<= 12"
df$age_menarche[df$age_menarche == 13] <- "13"
df$age_menarche[df$age_menarche >= 14 & df$age_menarche <= 16 ] <- ">= 14"
df$age_menarche[df$age_menarche == 888] <- "Missing"
df$age_menarche[is.na(df$age_menarche)] <- "Missing"

#Parity
df$parity[df$parity == 888] <- "Missing"
df$parity[is.na(df$parity)] <- "Missing"
df$parity[df$parity >=3 & df$parity < 888 ] <- "3+"

#BBD
df$bbd[is.na(df$bbd)]<- "Missing"

#family history
df$famhist_bc[is.na(df$famhist_bc)]<- "Missing"

# menopause
df$menopause[is.na(df$menopause)]<- "Missing"
df$menopause[df$menopause == 1] <- 1
df$menopause[df$menopause == 3] <- 1
df$menopause[df$menopause == 2] <- 0
df$menopause[df$menopause == 4] <- 0
# HRT 
df <- df %>%
  mutate(hrt_status = if_else(menopause == 0,0,hrt_status))

df$hrt_status[is.na(df$hrt_status)] <- "Missing"
####### extract subset for Sensitivity analysis  #######
con= df%>%filter(df$CaseControlRequestStatus_Li == 0) # control group 
table(con$Manufacturer_Li,con$qua_Li)
cas= df%>%filter(df$CaseControlRequestStatus_Li == 1) # case group 
table(cas$Manufacturer_Li,cas$qua_Li)
Hologic <- df %>% filter(Manufacturer_Li == "HOLOGIC  Inc.") # only using Hologic 
# recategory by quartiles 
control_gorup <- Hologic[Hologic$CaseControlRequestStatus_Li == 0,]
quartiles_Li<- quantile(control_gorup$Avg_Li,probs=c(0.25,0.5,0.75))
quartiles_Li <- as.numeric(quartiles_Li)
Hologic$QUA_Li <- cut(Hologic$Avg_Li,breaks = c(-Inf,quartiles_Li[1],quartiles_Li[2],quartiles_Li[3],Inf),
                      label = c("Q1","Q2","Q3","Q4"))
table(Hologic$Manufacturer_Li,Hologic$QUA_Li)
### Sensitivity analysis ----- Basic model for LIBRA #########
M1 <- glm(CaseControlRequestStatus_Li ~ QUA_Li+age_entry, family = 'binomial',data = Hologic)
summary(M1)
exp(coef(M1)[-1])
exp(confint.default(M1))
# Fully adjusted model for LIBRA #
M2 <- glm(CaseControlRequestStatus_Li ~QUA_Li+age_entry+bmi_entry_group+age_menarche+parity+bbd+famhist_bc+menopause+hrt_status, family = 'binomial',data = Hologic)
summary(M2)
exp(coef(M2)[-1])
exp(confint.default(M2))
################### Correlation plot ###########
# extract some needed  variables 
sub <- data.frame(
  Libra = df$Avg_Li,
  Cumulus = df$Avg_Cu,
  Age = df$age_entry,
  BMI = df$bmi_entry_group,
  AgeMenarche = df$age_menarche,
  parity = df$parity,
  BBD = df$bbd,
  famhist = df$famhist_bc,
  menupause = df$menopause,
  hrt = df$hrt_status
)
# recategrory  to numberic variable
sub$Age <- as.numeric(sub$Age)
sub$BMI[sub$BMI == "Normal"] <- 1
sub$BMI[sub$BMI == "Overweight"] <- 2
sub$BMI[sub$BMI == "Obesity" ] <- 3
sub$BMI[sub$BMI == "Missing"] <- 4
sub$BMI <- as.numeric(sub$BMI)
sub$AgeMenarche[sub$AgeMenarche == "<= 12"] <- 1
sub$AgeMenarche[sub$AgeMenarche == "== 13"] <- 2
sub$AgeMenarche[sub$AgeMenarche == ">= 14"] <- 3
sub$AgeMenarche[sub$AgeMenarche == "Missing"] <- 4
sub$AgeMenarche <- as.numeric(sub$AgeMenarche)
sub$parity[sub$parity == "3+"] <- 3
sub$parity[sub$parity == "Missing"] <- 4
sub$parity <- as.numeric(sub$parity)
sub$BBD[sub$BBD == "Missing"] <- 2
sub$BBD <- as.numeric(sub$BBD)
sub$famhist[sub$famhist == "Missing"] <- 2
sub$famhist <- as.numeric(sub$famhist)
sub$menupause[sub$menupause == "Missing"] <- 2
sub$menupause <- as.numeric(sub$menupause)
sub$hrt[sub$hrt == "Missing"] <- 3
sub$hrt <- as.numeric(sub$hrt)
cormatrix <- cor(sub,use="complete.obs")
library(corrplot)
corrplot(cormatrix,method = "circle",addCoef.col = "black",number.cex = 0.5)