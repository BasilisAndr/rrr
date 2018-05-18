# 1.0
schwa <- read.csv('Documents/maga/r/Der.csv', sep = '\t')
library(tidyverse)
library(ggplot2)

# график 1.1
ggplot(data=schwa, aes(f2, f1)) + geom_point(aes(colour=factor(schwa$vowel)), show.legend = F) + scale_y_reverse() + scale_x_reverse() + labs(title='f2 and f1 of the reduced and stressed vowels', caption="Data from Duryagin 2018")
# график 1.2
ggplot(data=schwa, aes(vowel, f1)) + geom_boxplot(aes(colour=factor(schwa$vowel)), show.legend = F) + coord_flip() + labs(title='f1 distribution for each vowel', caption="Data from Duryagin 2018")
# график 1.2
ggplot(data=schwa, aes(vowel, f2)) + geom_boxplot(aes(colour=factor(schwa$vowel)), show.legend = F) + coord_flip() + labs(title='f2 distribution for each vowel', caption="Data from Duryagin 2018")

# 1.3
# те две точки из а, 1248 и 1270

# 1.4
cor(schwa$f1, schwa$f2, method='pearson')
# 1.5
cor(schwa[schwa$vowel=='a',]$f1, schwa[schwa$vowel=='a',]$f2)
cor(schwa[schwa$vowel=='y',]$f1, schwa[schwa$vowel=='y',]$f2)
cor(schwa[schwa$vowel=='A',]$f1, schwa[schwa$vowel=='A',]$f2)

# 1.6
model1 <- lm(f1~f2, data=schwa)
summary(model1) # отсюда брать все вот эти коэффициенты

ggplot(data=schwa, aes(f2, f1)) + 
  geom_point(aes(colour=factor(schwa$vowel)), show.legend = F) +
  # это для такой же картинки
  geom_line(aes(f2, predict(model1)), colour='grey') +
  scale_y_reverse() + 
  scale_x_reverse() + 
  labs(title='f2 and f1 of the reduced and stressed vowels', caption="Data from Duryagin 2018")

# 1.7
library(lme4)
# это если такую же картинку
model2 <- lmer(f1~f2+(1|vowel), data=schwa)
summary(model2) # отсюда брать все вот эти коэффициенты

schwa$model <- predict(model2)
schwa %>% ggplot(aes(f2, f1)) + 
  geom_point(aes(colour=factor(vowel)), show.legend = F) +
  # это для картинки
  geom_line(aes(f2, model, color = vowel), show.legend = F) +
  scale_y_reverse() + 
  scale_x_reverse() + 
  labs(title='f2 and f1 of the reduced and stressed vowels', caption="Data from Duryagin 2018")


# 2.0 
elp <- read.csv('https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/ELP.csv')


# 2.1 
cor(elp$SUBTLWF, elp$Length)
cor(elp$Mean_RT, elp$SUBTLWF)
cor(elp$Mean_RT, elp$Length) # ну вот эти (0.53)

# второй способ
elp %>%
  select(Length, SUBTLWF, Mean_RT) %>%
  cor(use='all.obs', method='pearson')

# 2.2 
elp %>%
  ggplot(aes(SUBTLWF, Mean_RT)) +
  geom_point(aes(colour = Length)) +
  scale_color_continuous(low = "lightblue", high = "red") +
  facet_wrap(~POS, scales = "free_x") +
  scale_x_log10() +
  theme_bw() +
  labs(caption="data from (Balota et al. 2007)")


# 2.3 
fit <- lm(Mean_RT ~ log(SUBTLWF) + POS, data=elp)
coefficients(fit)

# 2.3.1 
# 2.3.2 
summary(fit)$adj.r.squared

# 2.3.3 
new_fit <- lm(Mean_RT~log(SUBTLWF), data=elp)
fit_prediction <- predict(new_fit)

elp %>%
  ggplot(aes(log(SUBTLWF), Mean_RT)) +
  geom_point(aes(colour = Length)) +
  scale_color_continuous(low = "lightblue", high = "red") +
  geom_line(aes(log(SUBTLWF), fit_prediction), color='black') +
  theme_bw() +
  labs(caption="data from (Balota et al. 2007)")


# 2.4 
fit2 <- lmer(Mean_RT~log(SUBTLWF) + (1 | POS), data=elp)
summary(fit2)

fixef(fit2)

# 2.4.2
# 414.4

# 2.4.3
fit2_new <- lmer(Mean_RT~log(SUBTLWF) + (1 | POS), data=elp)
elp$fit2_new_prediction <- predict(fit2_new)

elp %>% 
  ggplot(aes(log(SUBTLWF), Mean_RT, color=POS)) + 
  geom_point() + 
  facet_wrap(~POS) + 
  geom_line(aes(log(SUBTLWF), fit2_new_prediction), color='black') + 
  labs(caption='data from (Balota et al. 2007)') + 
  theme(legend.position='none')


data <- read.csv('https://raw.githubusercontent.com/agricolamz/2018-MAG_R_course/master/data/dutch_causatives.csv', sep = ',')
# 3.1
# Хи-квадрат нельзя использовать, если хотя бы в одной из клеток ожидаемое значение меньше 5
table <- table(data$Aux, data$CeSynt) 
chisq.test(table)
# Warning: In chisq.test(table) : Chi-squared approximation may be incorrect
chisq.test(table)$expected
# в одной из клеток значение 3.74
# В итоге надо Aux ~ CeSynt
fisher.test(table)
# p-value < 2.2e-16 
# значимо

# 3.2 
table <- table(data$Aux, data$EPTrans) 
chisq.test(table)
# p-value = 0.0001553
# они зависимые

# 3.3 
chisq.test(table)$expected

# 3.4 
fisher.test(table)$estimate
# odds ratio = 2.601174

# 3.5 
library(questionr)
cramer.v(table)
# 0.1744882

# 3.6 Report the results of independence test using the following template:
# We have found a significant association between variables `Aux` and `EPTrans` (p < 0.001).  The odds of ... were times higher in (group ....) than in (group ....). Effect size is small (Cramer's V = 0.1744882).

# 3.7 Visualize the distribution using mosaic plot.
library(vcd)
mosaic(~ Aux + CrSem + Country, data=data, shade=TRUE, legend=TRUE)
