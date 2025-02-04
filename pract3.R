library("lmtest")
library("dplyr")
library("GGally")
library("car")
library("sandwich")

data <- haven::read_spss("r17i_os26b.sav")

#отберем признаки и избавимся от пропусков в данных
data2 = select(data, mj13.2, mh5, m_marst, m_educ, m_age, status, mj6.2, mj1.1.2, mj32.1 )
data2 = data2 %>% mutate_all(~ifelse(. > 9999996, NA, .))
data2 = na.omit(data2)
glimpse(data2)

#зарплата
data2$mj13.2
sal1 = as.character(data2$mj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]

#возраст
age1 = as.character(data2$m_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
q=mean(age3)
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]

#пол
data2["sex"]=data2$mh5
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)

#образование
data2["h_educ"] = data2$m_educ
data2["higher_educ"] = data2$m_educ
data2["higher_educ"] = 0
data2$higher_educ[which(data2$h_educ=='21')] <- 1
data2$higher_educ[which(data2$h_educ=='22')] <- 1
data2$higher_educ[which(data2$h_educ=='23')] <- 1

#населенный пункт
data2["status1"]=data2$status
data2["status2"] = 0
data2$status2[which(data2$status1=='1')] <- 1
data2$status2[which(data2$status1=='2')] <- 1
data2$status2 = as.numeric(data2$status2)

#длительность рабочей недели
dur1 = as.character(data2$mj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))

#семейное положение
data2["wed"]= data2$m_marst
data2$wed1 = 0
data2$wed1[which(data2$wed=='1')] <- 1
data2$wed1[which(data2$wed=='6')] <- 1
data2$wed1 = as.numeric(data2$wed1)

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='2')] <- 1
data2$wed2[which(data2$wed=='3')] <- 1
data2$wed2 = as.numeric(data2$wed2)

data2["wed3"]=data2$m_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='4')] <- 1
data2$wed3 = as.numeric(data2$wed3)

data2["wed4"]=data2$m_marst
data2$wed4 = 0
data2$wed4[which(data2$wed=='5')] <- 1
data2$wed4 = as.numeric(data2$wed4)


#удовлетворенность
data2["sat"]=data2$mj1.1.2
data2["sat"] = lapply(data2["sat"], as.character)
data2["satisfy"] = 0
data2$satisfy[which(data2$sat=='1')] <- 1
data2$satisfy[which(data2$sat=='2')] <- 1
data2$satisfy = as.numeric(data2$satisfy)

#вторая работа
data2["sj"] = data2$mj32.1
data2["sj"] = lapply(data2["sj"], as.character)
data2["second_job"] = 0
data2$second_job[which(data2$sj=='1')] <- 1
data2$second_job = as.numeric(data2$second_job)

data2 = na.omit(data2)

data3 = select(data2, salary, age, sex, higher_educ, status2, dur, wed1, wed2, wed3, satisfy, second_job)

model1 = lm(data = data3, salary~age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3 + satisfy+ second_job)
summary(model1) #R^2=0.1841
vif(model1)
#age         sex higher_educ     status2         dur        wed1        wed2        wed3     satisfy  second_job 
#1.377863    1.123041    1.057912    1.024013    1.062654    4.498949    4.837793    2.639107    1.041215    1.003992

#wed2 имеет сильную взаимосвязь с другими переменными, исключим его
model2 = lm(data = data3, salary~age + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model2) #R^2=0.1842
vif(model2)
#age         sex higher_educ     status2         dur        wed1        wed3     satisfy  second_job 
#1.300100    1.080865    1.056957    1.023833    1.062653    1.319178    1.058824    1.041061    1.003879 


model3 = lm(data = data3, salary~log(age) + sex + higher_educ + status2 + log(dur) + wed1  + wed3 + satisfy+ second_job)
summary(model3)#R^2=0.23

model4 = lm(data = data3, salary~log(age) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model4)#R^2=0.2028

model5 = lm(data = data3, salary~age + sex + higher_educ + status2 + log(dur) + wed1  + wed3 + satisfy+ second_job)
summary(model5)#R^2=0.1718

model6 = lm(data = data3, salary~I(dur*age) + sex + higher_educ + status2 + wed1  + wed3 + satisfy+ second_job)
summary(model6)#R^2=0.1506


model8 = lm(data = data3, salary~I(age^0.1) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model8)#0.2046

model9 = lm(data = data3, salary~I(age^0.3) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model9)#0.2078

model10 = lm(data = data3, salary~I(age^0.5) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model10)#0.2106

model11 = lm(data = data3, salary~I(age^0.7) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model11)#0.2128

model12 = lm(data = data3, salary~I(age^0.9) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model12)#0.2144

model13 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model13)#0.2154

model14 = lm(data = data3, salary~I(age^1.3) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model14)#0.2159

model15 = lm(data = data3, salary~I(age^1.5) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model15)#0.216

model16 = lm(data = data3, salary~I(age^1.7) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model16)#0.2159

model17 = lm(data = data3, salary~I(age^2) + sex + higher_educ + status2 + dur + wed1  + wed3 + satisfy+ second_job)
summary(model17)#0.2019


model18 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^0.1) + wed1  + wed3 + satisfy+ second_job)
summary(model18)#0.1727

model19 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^0.4) + wed1  + wed3 + satisfy+ second_job)
summary(model19)#0.175

model20 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^0.7) + wed1  + wed3 + satisfy+ second_job)
summary(model20)#0.1763

model21 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^0.9) + wed1  + wed3 + satisfy+ second_job)
summary(model21)#0.1768

model22 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^1.2) + wed1  + wed3 + satisfy+ second_job)
summary(model22)#0.1771

model23 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^1.5) + wed1  + wed3 + satisfy+ second_job)
summary(model23)#0.177

model24 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^1.8) + wed1  + wed3 + satisfy+ second_job)
summary(model24)#0.1766

model25 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^2) + wed1  + wed3 + satisfy+ second_job)
summary(model25)#0.1645

model26 = lm(data = data3, salary~I(age^1.5) + sex + higher_educ + status2 + I(dur^1.5) + wed1  + wed3 + satisfy+ second_job)
summary(model26)#0.2389

model27 = lm(data = data3, salary~I(age^1.5) + sex + higher_educ + status2 + I(dur^1.4) + wed1  + wed3 + satisfy+ second_job)
summary(model27)#0.2392

model28 = lm(data = data3, salary~I(age^1.4) + sex + higher_educ + status2 + I(dur^1.4) + wed1  + wed3 + satisfy+ second_job)
summary(model28)#0.2397

model29 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + wed1  + wed3 + satisfy+ second_job)
summary(model29)#R^2 = 0.2413

# после перебора разных степеней получим лучший результат I(age^1.1), I(dur^0.7). Попробуем исключить незначаящие параметры
#wed1 wed3 и second job очень слабо описывают salary(большой p-value, нет звезд)
model30 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + wed1  + wed3 + satisfy)
summary(model30)#0.2427

model31 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + wed1 + satisfy+ second_job)
summary(model31)#0.2418

model32 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + wed3 + satisfy+ second_job)
summary(model32)#0.2425

model33 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7)  + satisfy+ second_job)
summary(model33)#0.2431

model34 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + wed3 + satisfy)
summary(model34)#0.2439

model35 = lm(data = data3, salary~I(age^1.1) + sex + higher_educ + status2 + I(dur^0.7) + satisfy)
summary(model35)#R^2 = 0.2444

#Получим лучшую модель: model35 с R-squared = 0.2444 
vif(model35) # Линейной зависимости между регрессорами нет

#пункт 4
#3670-2=3668 степеней свободы у всех парных регрессий
t_critical = qt(0.975, df = 3668) 

m1 = lm(salary~age,data3)
summary(m1) # salary = -7.220e-02 * age + 4.447e-16 ** - есть зависимость(отрицательная)
b_m1 = c(-7.220e-02-t_critical*1.647e-02, -7.220e-02+t_critical*1.647e-02)
# (-0.1045 -0.0339) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное
plot(x = data3$age, y = data3$salary, xlab = "Age",ylab = "Salary", main = "Age VS Salary")
     
m2 = lm(salary~sex,data3)
summary(m2) # salary = 0.41984 * sex - 0.18807 *** - сильная положительная зависимость
confint(m2, level = 0.95) # (0.3561859  0.4834959) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

m3 = lm(salary~higher_educ,data3)
summary(m3) # salary = 0.50604 * higher_educ - 0.13609 *** - сильная положительная зависимость
confint(m3, level = 0.95) # ( 0.4349011  0.57717463) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

m4 = lm(salary~status2,data3)
summary(m4) # salary = 0.43121 * status2 - 0.31971  *** - сильная положительная зависимость
confint(m4, level = 0.95) # (0.3586152  0.5038036) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

m5 = lm(salary~dur,data3)
summary(m5) # salary = 1.674e-01 * dur) + 3.364e-16 *** - положительная зависимость
confint(m5, level = 0.95) # (0.13551605 0.19934717) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное

m6 = lm(salary~satisfy,data3)
summary(m6) # salary = 0.35450 * satisfy - 0.21434  *** - сильная положительная зависимость
confint(m6, level = 0.95) # (0.2892974  0.4196969) - доверительный интервал не содержит 0, наиболее вероятное значение коэффициента положительное
#у всех моделей очень маленький R^2 (<0.05), зависимости имеются, но для такой большой выборки у нас в модели слишком мало признаков, описывающих переменную salary
# из наших регрессоров можно сделать вывод, что, вероятно,более высокая зарплата у людей с такими признаками:
#- немного моложе среднего возраста(средний возраст - 41 год)
#- мужской пол
#- наличие высшего образования
#- тип населенного пункта-город
#- удовлетворен рабочими условиями

#пункт 5   
#1.c высшим образованием, не из города

data4 = subset(data2,status!=2)
data5 = subset(data4, m_educ > 20)

model_1_subset = lm(data = data5, salary~I(age^1.1) + sex + I(dur^0.7) + satisfy)
summary(model_1_subset)
confint(model_1_subset, level = 0.95)
#I(age^1.1)  -1.5806333 -0.2354781 не содержит 0, наиболее вероятное значение коэффициента отрицательное
#sex          0.5549599  2.1287694 не содержит 0, наиболее вероятное значение коэффициента положительное
#I(dur^0.7)  -0.8328808  0.9452197 содержит 0
#satisfy     -0.3219368  1.4680415 содержит 0
# Наибольшая зависимость от переменных sex и age  
# Наибольшая зарплата у мужчин людей моложе 41 (с высшим образорванием, не из города)


#2.женщины, с высшим образованием
data6 = subset(data2,sex!=1)
data7 = subset(data6, m_educ > 20)

model_2_subset = lm(data = data7,  salary~I(age^1.1) + status2 + I(dur^0.7) + satisfy)
summary(model_2_subset)
confint(model_2_subset, level = 0.95)
#I(age^1.1)  -0.4472137 0.42430828 содержит 0
#status2     -0.2389539 0.77023013 содержит 0
#I(dur^0.7)  -0.4083954 0.53206746 содержит 0
#satisfy      0.2293060 1.16260602 не содержит 0, наиболее вероятное значение коэффициента положительное
# Наибольшая зависимость от переменной satisfy
# Наибольшая зарплата среди женщин с высшим образованием, у тех, кто удовлетворен условиями труда