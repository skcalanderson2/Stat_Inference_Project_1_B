library(ggplot2)

tg_grouped_by_supp_dose <- tg %>% group_by(supp, dose)
summary_supp_dose <- tg_grouped_by_supp_dose %>% summarise(means = mean(len), sd = sd(len))

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
