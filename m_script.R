# В этом скрипте представлен анализ
library(foreign)
df <- read.spss(file.choose(),use.value.labels = F, to.data.frame=T, header= T) # загружаем
names(df) # имена пременных, их очень много/
str(df$cntry) # страны, их тут должно быть 39
head(df$freehms) # отношение к геям, на английском звучит так: Gays and lesbians free to live life as they wish
# (геи и лесбиянки могут жить так, как хотят) 1 - полностью согласен, 5 - совершенно не согласен
attach(df) #
summary(freehms) # стат саммари переменной
str(freehms) # метки переменной
class(freehms) # класс переменной

df$gndr <- as.factor(gndr) # чтобы R не считало перемнную числовой

countries_gays<-prop.table(table(cntry, freehms), margin = 1) * 100 # кросстаб
countries_gays<-round(countries_gays, 1) # округляем
head(countries_gays) 
gays<- data.frame(countries_gays) # в датафрейм
head(gays) # не тот датафрейм, который мне нужен, надо обработать

df2<- subset(gays, freehms == 1) # создаем новый
library(dplyr)
df2 <- df2 %>% rename(agree_strongly = Freq) # переименовываю пременную
df2 <- subset(df2, select = -freehms) # удаляем лишнюю переменную
df2$agree <- subset(gays, freehms == 2)$Freq # добавляем
df2$neither_agree_nor_disagree <- subset(gays, freehms == 3)$Freq # добавляем
df2$disagree <- subset(gays, freehms == 4)$Freq # добавляем
df2$disagree_strongly <- subset(gays, freehms == 5)$Freq # добавляем
head(df2)

df4 <- df2[1]
df4$positive <- df2$agree_strongly + df2$agree
df4$equal <- df2$neither_agree_nor_disagree
df4$negative <- df2$disagree + df2$disagree_strongly
head(df4) 
df4[order(df4$positive, decreasing = TRUE), ] # ну и рейтинг стран по лояльности к геям, лесбиянкам
df4
library(ggplot2)

# Не самый понятный график конечно
ggplot(data = gays, aes(x = cntry, y = Freq, fill = factor(freehms))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Frequency", fill = "Free LGBT") +
  scale_fill_discrete(name = "Free LGBT")

head(df)
# Первая модель с основными социально-демографическими предикторами: пол, возраст, доход, образование, наличие детей
# Модель без учета межстрановой вариации
# Значимо влияют число лет образования (чем больше лет - тем лояльнее относятся к геям и лесбиянкам) и возраст (чем старше - тем хуже относятся к геям и лесбиянкам)
m1 <- lm(df$freehms ~ df$agea + df$eduyrs + df$gndr + df$chldhm + df$hinctnta)
summary(m1)

# Модель со странами в качестве дамии-переменных, R-квадрат вырос на 13%! Правда, конечно, было использовано много степеней свободы
m2 <- lm(df$freehms ~ df$agea + df$eduyrs + df$gndr + df$chldhm + df$hinctnta+df$cntry)
summary(m2)

install.packages("arm")
library ("arm")
M3 <- lmer(freehms ~ agea + eduyrs + gndr + chldhm + hinctnta +  (1 | cntry), data = df)
display (M3)
summary(M3)

coef(M3) # коэффициенты предикторов в разных странах ведут себя одинаково
fixef (M3)
ranef(M3)

# только страны
m4 <- lm(freehms ~ cntry)
summary(m4)
coef(m4)
plot(coef(m4))

anova(m2, m4)
