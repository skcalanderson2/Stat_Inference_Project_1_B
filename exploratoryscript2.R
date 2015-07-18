library(ggplot2)
library(dplyr)

tg <- ToothGrowth

tg_grouped_by_supp_dose <- tg %>% group_by(supp, dose)
summary_supp_dose <- tg_grouped_by_supp_dose %>% summarise(means = mean(len), sd = sd(len))

summary_supp_dose <- summary_supp_dose %>% mutate(label = paste(supp, dose, sep = "-"))
summary_supp_dose$label <- as.factor(summary_supp_dose$label)


tg_grouped_by_supp <- tg %>% group_by(supp)
summary_supp <- tg_grouped_by_supp %>% summarise(means = mean(len), sd = sd(len))

tg_grouped_by_dose <- tg %>% group_by(dose)
summary_dose <- tg_grouped_by_dose %>% summarise(means = mean(len), sd = sd(len))

## ggplot needs to have both x and y or you get a weird error!!
summary_supp_dose$lbl2 <- reorder(summary_supp_dose$label, summary_supp_dose$means)
g <- ggplot(summary_supp_dose, aes(y = means, x = lbl2, fill = supp))
g + geom_bar(stat = "identity") + xlab("Suppliment-Dose Category") + ylab("Means of Length")


summary_supp_dose <- summary_supp_dose %>% mutate(label = paste(supp, dose, sep = "-"))
## Does Not Work!! -------------------------------------------------------
# Example
mtcars3 <- mtcars
mtcars3$car <-factor(mtcars3$car, levels=mtcars3[order(mtcars3$mpg), "car"])
summary_supp_dose$label <- factor(summary_supp_dose$label, 
                                  levels = summary_supp_dose[order(summary_supp_dose$means), "label"])
##------------------------------------------------------------------------

t.test(len ~ supp, data = tg, conf.level = 0.95)
tg_2 <- tg %>% filter(dose == 2.0)
tg_5 <- tg %>% filter(dose == .5)
tg_1 <- tg %>% filter(dose == 1.0)
t.test(len ~ supp, data = tg_2, conf.level = 0.95)
tg_1_2 <- tg %>% filter(dose == 2.0 | dose == 1.0)
t.test(len ~ dose, data = tg_1_2, conf.level = 0.95)
tg_5_1 <- tg %>% filter(dose == .5 | dose == 1.0)
t.test(len ~ dose, data = tg_5_1, conf.level = 0.95)
tg_5_2 <- tg %>% filter(dose == .5 | dose == 2.0)
t.test(len ~ dose, data = tg_5_2, conf.level = 0.95)

tg_1_2 <- tg %>% filter(dose == 2.0 | dose == 1.0)
t_tg_1_2 <- t.test(dose ~ len, data = tg_1_2, conf.level = 0.95, alternative = "greater")$p.value
tg_5_1 <- tg %>% filter(dose == .5 | dose == 1.0)
t_tg_5_1 <- t.test(dose ~ len, data = tg_5_1, conf.level = 0.95, alternative = "greater")$p.value
tg_5_2 <- tg %>% filter(dose == .5 | dose == 2.0)
t_tg_5_2 <- t.test(dose ~ len, data = tg_5_2, conf.level = 0.95, alternative = "greater")$p.value
