data_Set <- read.csv("data/Numeric.csv")
#View(data_Set)
library(dplyr)
library(tidyverse)
data_Set
data_Set %>% drop_na()
#View(data_Set)


Mercs <- filter(data_Set, Make == "Mercedes")
#View(Mercs)
all_wheel_drive <- filter(data_Set, Drive == "All Wheel Drive")
#View(all_wheel_drive)
rear_wheel_drive <- filter(data_Set, Drive == "Rear Wheel Drive")
#View(rear_wheel_drive)
front_wheel_drive <- filter(data_Set, Drive == "Front Wheel Drive")
#View(front_wheel_drive)


t.test(front_wheel_drive$EfficiencyWHPKM, rear_wheel_drive$EfficiencyWHPKM, paired = F,alternative = "greater")

anova_result <- aov(TopSpeedKMPH ~ Drive, data_Set)
summary(anova_result)
TukeyHSD(anova_result)

data_clean <- data_Set |> filter(!is.na(Make), !is.na(Drive))
result <- chisq.test(table(data_Set$Make, data_Set$Drive))
result$p.value
result$residuals