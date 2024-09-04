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
#import density data
density <- read.csv("One2One_Density.csv")
all(density$CaseControlRequestStatus_Cu == density$CaseControlRequestStatus_Li)
density <- density %>% select(- CaseControlRequestStatus_Cu)
density$Avg_Cu <- (density$Density_LMLO_Cu+density$Density_RMLO_Cu)/2
# if one of the reading is NA, average reading is the non-NA reading without average
density$Avg_Cu <- ifelse(is.na(density$Avg_Cu),
                         ifelse(is.na(density$Density_RMLO_Cu),density$Density_LMLO_Cu, density$Density_RMLO_Cu),density$Avg_Cu)
density$Avg_Li <- (density$Density_LMLO_Li+density$Density_RMLO_Li)/2
density$Avg_Li<- ifelse(is.na(density$Avg_Li),
                        ifelse(is.na(density$Density_RMLO_Li),density$Density_LMLO_Li, density$Density_RMLO_Li),density$Avg_Li)
# Cut in measures 
density$Cat_Li <- cut(
  density$Avg_Li, breaks = c(0,25,50,75,100),
  labels = c("1","2","3","4"))
density$Cat_Cu <- cut(
  density$Avg_Cu, breaks = c(0,25,50,75,100),
  labels = c("1","2","3","4"))

# Cut in quartiles 
control_gorup <- density[density$CaseControlRequestStatus_Li == 0,]
# For libra
quartiles_Li<- quantile(control_gorup$Avg_Li,probs=c(0.25,0.5,0.75))
quartiles_Li <- as.numeric(quartiles_Li)
density$qua_Li <- cut(density$Avg_Li,breaks = c(-Inf,quartiles_Li[1],quartiles_Li[2],quartiles_Li[3],Inf),
                      label = c("Q1","Q2","Q3","Q4"))
# For Cumulus
quantiles_Cu<- quantile(control_gorup$Avg_Cu,probs=c(0.25,0.5,0.75))
quantiles_Cu <- as.numeric(quantiles_Cu)

density$qua_Cu <- cut(density$Avg_Cu,breaks = c(-Inf,quantiles_Cu[1],quantiles_Cu[2],quantiles_Cu[3],Inf),
                      label = c("Q1","Q2","Q3","Q4"))
# combine the dataset
risk_factors <- read.csv("MSc_Mammography_Dataset_20240529.csv")
df <- left_join(density, risk_factors, 
                by = join_by(QCode == qcode))
write.csv(df,file = "Wholedata.csv",row.names = FALSE)
# Table for manufacturers and Quatiles 
table(df$Manufacturer_Li,df$qua_Li)

##################################### recategorical the varibales#################################################
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
df$menopause[df$menopause == 1] <- '1-post'
df$menopause[df$menopause == 3] <- '1-post'
df$menopause[df$menopause == 2] <- '0-pre'
df$menopause[df$menopause == 4] <- '0-pre'
# HRT 
df <- df %>%
  mutate(hrt_status = if_else(menopause == "0-pre",0,hrt_status))
# Create a tableone
df$hrt_status[is.na(df$hrt_status)] <- "Missing"
myVars <- c("Avg_Li","Cat_Li","qua_Li","Avg_Cu","Cat_Cu","qua_Cu","age_entry",
            "bmi_entry","bmi_entry_group", 
            "age_menarche","parity","bbd","famhist_bc","menopause","hrt_status")
catVars <- c("Cat_Li","qua_Li","Cat_Cu","qua_Cu","bmi_entry_group","parity","bbd","famhist_bc","menopause","hrt_status")
tab1 <- CreateTableOne(vars=myVars,data=df,factorVars=catVars, strata = 'CaseControlRequestStatus_Li', test =F)
print(tab1)
### change the struchtures for some variables
df$Cat_Li <- as.factor(df$Cat_Li)
df$qua_Li <- as.factor(df$qua_Li)
df$Cat_Cu <- as.factor(df$Cat_Cu)
df$qua_Cu <- as.factor(df$qua_Cu)
df$age_entry <- as.numeric(df$age_entry)
df$bmi_entry_group <- as.factor(df$bmi_entry_group)
df$age_menarche <- as.factor(df$age_menarche)
df$parity <- as.factor(df$parity)
df$bbd <- as.factor(df$bbd)
df$famhist_bc <- as.factor(df$famhist_bc)
df$menopause <- as.factor(df$menopause)
df$hrt_status <- as.factor(df$hrt_status)
df$CaseControlRequestStatus_Li <- as.factor(df$CaseControlRequestStatus_Li)
# change the reference levels 
df$bmi_entry_group <- relevel(df$bmi_entry_group,ref = "Normal")
df$age_menarche <- relevel(df$age_menarche,ref= "<= 12")
############################ LIBRA models ###############################
# baisc model 
L0 <- glm(CaseControlRequestStatus_Li ~ Cat_Li+age_entry, family = 'binomial',data = df)
L1 <- glm(CaseControlRequestStatus_Li ~ qua_Li+age_entry, family = 'binomial',data = df)
L2 <- glm(CaseControlRequestStatus_Li ~ qua_Li+age_entry+bmi_entry_group, family = 'binomial',data = df)
# fully adjusted model 
L3 <- glm(CaseControlRequestStatus_Li ~ qua_Li+age_entry+bmi_entry_group+age_menarche+parity+bbd+famhist_bc+menopause+hrt_status,family = 'binomial',data = df)
# Table to show the results 
tidy_M1 <- tidy(L0,conf.int=TRUE)
tidy_M2 <- tidy(L1,conf.int=TRUE)
tidy_M3 <- tidy_and_attach(L3,conf.int=TRUE) |>
  tidy_add_reference_rows() |>
  tidy_detach_model() |>
  select(all_of(colnames(tidy_M1)))
tidy_M1$Model <- "Category variable --- cut in measures"
tidy_M2$Model <- "Category variable --- cut in quartile"
tidy_M3$Model <- "Adjust Model --- with all risk factors"
result_Li <- rbind(tidy_M1,tidy_M2,tidy_M3)
result_Li <- result_Li %>% 
  select(Model,term,estimate,conf.low, conf.high,p.value)
result_Li <- result_Li %>%
  filter(term != "(Intercept)") %>%
  summarise(
    `Model` = Model,
    `Term` = term,
    `Odds Ratio` = round(exp(estimate),3),
    `95% CI`= paste0(round(exp(conf.low),3),"-",round(exp(conf.high),3)),
    `P value` = round(p.value,3)
  )

########################### Cumulus Models #################################
# basic model
C0 <- glm(CaseControlRequestStatus_Li ~ Cat_Cu+age_entry, family = 'binomial',data = df)
C1 <- glm(CaseControlRequestStatus_Li ~ qua_Cu+age_entry, family = 'binomial',data = df)
C2 <- glm(CaseControlRequestStatus_Li ~ qua_Cu+age_entry+bmi_entry_group, family = 'binomial',data = df)
# fully adjusted model
C3 <- glm(CaseControlRequestStatus_Li ~ qua_Cu+age_entry+bmi_entry_group + age_menarche + parity + bbd + famhist_bc + menopause + hrt_status, family = 'binomial',data = df)
# Table to show the results 
tidy_M11 <- tidy(C0,conf.int=TRUE)
tidy_M22 <- tidy(C1,conf.int=TRUE)
tidy_M33 <- tidy_and_attach(C3,conf.int=TRUE) |>
  tidy_add_reference_rows() |>
  tidy_detach_model() |>
  select(all_of(colnames(tidy_M11)))
tidy_M11$Model <- "Category variable --- cut in measures"
tidy_M22$Model <- "Category variable --- cut in quartile"
tidy_M33$Model <- "Adjust Model --- with all risk factors"
result_Cu <- rbind(tidy_M11,tidy_M22,tidy_M33)
result_Cu <- result_Cu %>% 
  select(Model,term,estimate,conf.low, conf.high,p.value)
result_Cu <- result_Cu %>%
  filter(term != "(Intercept)") %>%
  summarise(
    `Model` = Model,
    `Term` = term,
    `Odds Ratio` = round(exp(estimate),3),
    `95% CI`= paste0(round(exp(conf.low),3),"-",round(exp(conf.high),3)),
    `P value` = round(p.value,3)
  )

#### complementary model ~ Category by BI-RADS + others ##########
# Cumulus 
C4 <- glm(CaseControlRequestStatus_Li ~ Cat_Cu+age_entry+bmi_entry_group + age_menarche + parity + bbd + famhist_bc + menopause + hrt_status, family = 'binomial',data = df)
summary(C4)
exp(coef(C4)[-1])
exp(confint.default(C4))
# LIBRA
L4 <- glm(CaseControlRequestStatus_Li ~ Cat_Li+age_entry+bmi_entry_group + age_menarche + parity + bbd + famhist_bc + menopause + hrt_status, family = 'binomial',data = df)
summary(L4)
exp(coef(L4)[-1])
exp(confint.default(L4))





