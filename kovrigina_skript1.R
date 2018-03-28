rm(list=ls()) 
library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
## Читаем файл 
setwd ("D:/Kovrigina_R") 
getwd() 
eddy = read_csv("eddypro.csv",skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddy
names(eddy)
# удаляем из таблицы столбик ролл 
eddy = select(eddy, -(roll)) 
eddy = eddy[,c(-1,-3,-6,-7,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
names(eddy) 
#оставляем лето, дневное время
eddy<-eddy[eddy$DOY>152 & eddy$DOY<244 & eddy$daytime == TRUE, c(1:ncol(eddy))] 
# Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения 
eddy = eddy %>% mutate_if(is.character, factor)
# Убираем проблему со знаками в переменных
names(eddy) = str_replace_all(names(eddy), "[!]","_emph_") 
names(eddy) = names(eddy) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(eddy) 
##корреляция
sapply(eddy,is.numeric) 
eddy_numeric = eddy[,sapply(eddy,is.numeric) ] 
eddy_nonnumeric = eddy[,!sapply(eddy,is.numeric) ] 
cor_td = cor(eddy_numeric) 
cor_td 
cor_td = cor(drop_na(eddy_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
#создание обучающей выборки
row_numbers = 1:length(eddy$date) 
teach = sample(row_numbers, floor(length(eddy$date)*.7)) 
test = row_numbers[-teach] 
teaching_eddy_unq = eddy[teach,] 
testing_eddy_unq = eddy[test,] 
mod = lm(formula, data=eddy) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod)
anova(mod)
#взаимодействиеспеременными
model1 = lm(h2o_flux ~ (rand_err_Tau + H + LE + rand_err_LE + h2o_flux +rand_err_h2o_flux + 
  co2_molar_density + co2_mixing_ratio + RH + VPD + max_speed + 
  u_star_ + TKE + T_star_ + un_H + un_LE + un_h2o_flux + u_var + 
  v_var + w_div_ts_cov + w_div_h2o_cov + co2_signal_strength_7200), data = eddy)
model1
coef(model1) 
resid(model1) 
confint(model1) 
summary(model1)
anova(model1)
model2= lm(h2o_flux ~ (H + LE + rand_err_LE + h2o_flux +rand_err_h2o_flux + 
                         co2_molar_density + co2_mixing_ratio + max_speed + 
                         TKE + un_H + un_LE + un_h2o_flux + u_var + 
                         v_var + co2_signal_strength_7200) - rand_err_Tau - RH - VPD - max_speed - 
                         u_star_ - T_star_ - w_div_ts_cov - w_div_h2o_cov, data = eddy)
anova(model2)
summary(model2)
model3= lm(h2o_flux ~ (H + LE + rand_err_LE + h2o_flux +rand_err_h2o_flux + 
                       co2_molar_density + co2_mixing_ratio + max_speed + 
                       TKE + un_H + un_LE + un_h2o_flux + u_var) - rand_err_Tau - RH - VPD - max_speed - 
                       u_star_ - T_star_ - w_div_ts_cov - w_div_h2o_cov -  u_var - co2_signal_strength_7200, data = eddy)
anova(model3)
summary(model3)
model4=lm(h2o_flux ~ (H + LE + rand_err_LE + h2o_flux +rand_err_h2o_flux + 
                                 co2_molar_density + co2_mixing_ratio + max_speed + 
                                 TKE + un_H + un_LE + un_h2o_flux + u_var) - rand_err_Tau - RH - VPD - max_speed - 
                                 u_star_ - T_star_ - w_div_ts_cov - w_div_h2o_cov -  
                                 u_var - co2_signal_strength_7200 ^2, data = eddy)
anova(model4)
summary(model4)
