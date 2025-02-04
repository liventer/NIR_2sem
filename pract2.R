#вариант 7
library("lmtest")
library("car")
data = mtcars
help(mtcars)
#mpg ~ cyl + disp + hp + drat
#проверим регрессоры на линейную зависимость
m1 = lm(cyl~disp,data)
summary(m1)
# R-squared: 0.81, одна из переменных достаточно хорошо описывает другую, поэтому не будем использовать их вместе
m2 = lm(hp~cyl,data)
summary(m2)
# R-squared: 0.68
m3 = lm(drat~cyl,data)
summary(m3)
# R-squared: 0.47
m4 = lm(disp~hp,data)
summary(m4)
# R-squared: 0.61
plot(data$disp~data$hp)
m5 = lm(disp~drat,data)
summary(m5)
# R-squared: 0.49
m6 = lm(hp~drat,data)
summary(m6)
# R-squared: 0.17
m7 = lm(mpg~drat+hp+disp+cyl,data)
vif(m7)
m8 = lm(mpg~drat+hp+cyl,data)
vif(m8)
m9 = lm(mpg~drat+hp+disp,data)
vif(m9)
#cyl больше зависима от других регрессоров, чем disp, поэтому исключим cyl.

mod1=lm(mpg~disp+hp+drat,data)
summary(mod1)
# R-squared: 0.7509; *, *, . - у всех регрессоров есть зависимость, но она небольшая

mod2=lm(mpg~disp,data)
summary(mod2)
# R-squared: 0.709;  *** - Зависимость есть, но R^2 низкий, поэтому модель недостаточно хорошая(disp хорошо описывает mpg, но одного регрессора недостаточно)

mod3=lm(mpg~drat,data)
summary(mod3)
# R-squared: 0.4461; *** - Зависимость есть, но R^2 низкий, поэтому модель недостаточно хорошая

mod4=lm(mpg~hp,data)
summary(mod4)
# R-squared: 0.5892; *** - Зависимость есть, но R^2 низкий, поэтому модель недостаточно хорошая

mod5=lm(mpg~disp+hp,data)
summary(mod5)
# R-squared: 0.7309; ***, . - зависимость есть, R^2 неплохой, но можно найти модель лучше

mod6=lm(mpg~disp+drat,data)
summary(mod6)
# R-squared: 0.7125; у drat нет зависимости(большой p-value:0.2521), R^2 неплохой

mod7=lm(mpg~hp+drat,data)
summary(mod7)
# R-squared: 0.7233; ***,*** - зависимость есть R^2 неплохой, но можно найти модель лучше

#переберем несколько моделей для выявлени лучшей коэффициенту детерминации
mod8=lm(mpg~log(disp)+log(hp)+log(drat),data)
summary(mod8)
# R-squared: 0.824

mod9=lm(mpg~log(disp)+log(hp)+drat,data)
summary(mod9)
# R-squared: 0.8241
#у этих модель drat не имеет зависимости, mod6, тоже, в остальных предыдущих моделях он имеет слабую зависимость, поэтому, скорее всего регрессор drat незначимый, исключим его

mod10=lm(mpg~log(disp)+log(hp),data)
summary(mod10)
# R-squared: 0.8293 модель стала лучше

mod11=lm(mpg~I(disp^2)+I(hp^2)+I(drat^2),data)
summary(mod11)
# R-squared: 0.6966, маленький 

mod12=lm(mpg~I(disp*hp)+drat,data)
summary(mod12)
# R-squared: 0.7158, маленький 

mod13=lm(mpg~I(disp*hp),data)
summary(mod13)
# R-squared: 0.6348, маленький 

mod14=lm(mpg~disp+I(hp*drat),data)
summary(mod14)
# R-squared: 0.7208, маленький 

mod15=lm(mpg~I(disp*drat)+hp,data)
summary(mod15)
# R-squared: 0.687, маленький 

mod16=lm(mpg~log(disp)+log(hp)+I(drat^2),data)
summary(mod16)
# R-squared: 0.8243

#Наилучшая модель mod10, R^2=0.8293

model1 = lm(mpg~log(disp),data)
summary(model1) #mpg = -9.2935*log(disp)+69.2050; *** - сильная отрицательная зависимость

model2 = lm(mpg~log(hp),data)
summary(model2) #mpg = -10.764*log(hp)+72.640; *** - сильная отрицательная зависимость


#===================================================================================================
#2.1 mod10=lm(mpg~log(disp)+log(hp),data)
#32 наблюдения, оценивалось 3 коэффициентами: 32 - 3 = 29 степени свободы
bestmodel=lm(mpg~log(disp)+log(hp),data)
summary(bestmodel)

t_critical = qt(0.975, df = 29) #2.045

b_disp = c(-6.993-t_critical*1.499, -6.993+t_critical*1.499)
# [-10.06, -3.93]  b не может равняться 0, наиболее вероятное значение коэффициента отрицательное

b_hp = c(-3.304-t_critical*1.855, -3.304+t_critical*1.855)
# [-7.1, 0.49] содержит 0, связь отсутсвует, не опровергается гипотеза о том, что коэфициент равен нулю

# Проверка
confint(bestmodel, level = 0.95)
#             2.5 %     97.5 %
#log(disp)   -10.05821 -3.9283018
#log(hp)      -7.09806  0.4899552

# Доверительный интервал для прогноза
new.data = data.frame(disp = 150, hp = 100)
predict(bestmodel, new.data, interval = "confidence")
#      fit      lwr      upr
#  22.92258 21.87455 23.9706


t_critical2 = qt(0.975, df = 30)
modela = lm(mpg ~ disp, data)
summary(modela) # выявленная связь между объясняемой переменной и регрессором отрицательная
c_disp = c(-0.041215-t_critical2*0.004712,-0.041215+t_critical2*0.004712)
# [-0.0508, -0.0316] доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное

modelb = lm(mpg ~ hp, data)
summary(modelb) # выявленная связь между объясняемой переменной и регрессором отрицательная
c_hp = c(-0.06823-t_critical2*0.01012,-0.06823+t_critical2*0.01012)
# [-0.0889, -0.0476] доверительный интервал не содержит 0, наиболее вероятное значение коэффициента отрицательное