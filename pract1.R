# Вариант 7
library("lmtest")

data = swiss
help(swiss)

mean_Education = mean(data$Education) # Среднее значение Education
var_Education = var(data$Education)   # Дисперсия Education 
CKO_Education = sd(data$Education)    # Среднеквадратическое отклонение Education 
#Итог: Среднее значение 11%(мало), СКО 9.62%(большое), большая дисперсия (значения в среднем отклоняются от среднего на 87%)


mean_Examination = mean(data$Examination)  # Среднее значение Examination
var_Examination = var(data$Examination)    # Дисперсия Examination
CKO_Examination = sd(data$Examination)     # Среднеквадратическое отклонение Examination   
#Итог: Среднее значение 16%(мало),СКО 8%(малое), большая дисперсия (значения в среднем отклоняются от среднего на 50%)


mean_Fertility = mean(data$Fertility)  # Среднее значение Fertility
var_Fertility = var(data$Fertility)    # Дисперсия Fertility
CKO_Fertility = sd(data$Fertility)     # Среднеквадратическое отклонение Fertility
#Итог:Среднее значение 70%(очень высокий), СКО 12.5%(мало), маленькая дисперсия (значения в среднем отклоняются от среднего на 18%)


model_1 = lm(Examination~Fertility, data)

# Examination = -0.41250 * Fertility + 45.42289
plot(data$Examination~data$Fertility, ylab = "Examination", xlab = "Fertility") + abline(45.42289, -0.41250, col = "red") 
summary(model_1)
# Отрицательная зависимость т.к. коэф < 0
# Модель неплохая для парной регрессии (R-squared:  0.4042)
# '***' Доля получивших высокие оценки на экзамене зависит от рождаемостти (сильная зависимость) 


model_2 = lm(Examination~Education, data)

# Examination =  0.57947 * Education + 10.12748
plot(data$Examination~data$Education, ylab = "Examination", xlab = "Education") + abline(10.12748,  0.57947, col = "red") 
summary(model_2)
# Положительная зависимость т.к. коэф > 0
# Модель неплохая для парной регрессии(R-squared:  0.4764 )
# '***'  Доля получивших высокие оценки на экзамене зависит от получивших образование (сильная зависимость) 





