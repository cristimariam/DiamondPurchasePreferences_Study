
# BAMA 580A Project Code 
# Authors: Angga Adi Saputra, Christi Mariam Denny, Gagan Singh, Shiwei Li, Young Ji Tuen

library(dplyr)
library(ggplot2)

data <- read.csv("data/data.csv")
demo <- read.csv("data/demographics.csv")

# DATA CLEANING

# check for duplicate respondents 
duplicates <- data$PROLIFIC_PID[duplicated(data$PROLIFIC_PID)]
print(duplicates) 

# count number of respondents in each condition
condition_n <- table(data$FL_6_DO)
print(condition_n)

# count number of responses to attention check
attention_n <- table(data$X11_EnvQuestion_2)
print(attention_n)

# FEATURE ENGINEERING 

# create a new variable for environmental consciousness
data$env <- (data$X11_EnvQuestion_1 + data$X11_EnvQuestion_3) / 2

# regroup environmental consciousness values 
data$env2 <- ifelse(data$env %in% c(1, 1.5, 2, 2.5), 1,
                          ifelse(data$env == 3, 2,
                                 ifelse(data$env %in% c(3.5, 4, 4.5, 5), 3, NA)))

# DESCRIPTIVE STATISTICS

# descriptive statistics for advertisement group
group_by(data, FL_6_DO) %>%
  summarise(
    count = n(),
    mean = mean(X11_DiamondQuestion1, na.rm = TRUE),
    sd = sd(X11_DiamondQuestion1, na.rm = TRUE)
  )

# descriptive statistics for environmental consciousness
group_by(data, env2) %>%
  summarise(
    count = n(),
    mean = mean(X11_DiamondQuestion1, na.rm = TRUE),
    sd = sd(X11_DiamondQuestion1, na.rm = TRUE)
  )

# plot preference for lab-grown diamonds by condition and environmental consciousness
ggplot(data = data, aes(x=env2, y=X11_DiamondQuestion1, fill=FL_6_DO)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = 
                 position_dodge(width = 0.9), width = 0.2) +
  coord_cartesian(ylim = c(1, 5)) +
  xlab("Environmental Consciousness") +  
  ylab("Preference for Lab-grown Diamond") +
  theme_light() +  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  labs(fill = "Advertisement") +
  scale_fill_manual(labels = c("Environmental", "Control"),
                      values = c("lightblue", "cornflowerblue"))

# INFERENTIAL STATISTICS 

# convert variables to factors
data$env2 <- factor(data$env2)
data$FL_6_DO <- factor(data$FL_6_DO)

# 2-way ANOVA (preference ~ condition + env + condition : env)
res.aov <- aov(X11_DiamondQuestion1 ~ FL_6_DO + env2 + FL_6_DO : env2, data = data)
summary(res.aov)

# Tukey's HSD test
res.tukey <- TukeyHSD(res.aov, "env2")
res.tukey

# EXPLORATORY ANALYSIS

# merge demographic data to main data 
merged <- merge(data, demo, by = "PROLIFIC_PID")

# remove rows where Income is equal to -99
merged <- merged[merged$Income != -99,]

# plot preference for lab-grown diamonds by income and condition
ggplot(data = merged, aes(x=Income, y=X11_DiamondQuestion1, fill=FL_6_DO)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = 
                 position_dodge(width = 0.9), width = 0.2) +
  coord_cartesian(ylim = c(1, 5)) +
  xlab("Income") +  
  ylab("Preference for Lab-grown Diamond") +
  theme_light() +  
  theme(panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  labs(fill = "Advertisement") +
  scale_fill_manual(labels = c("Environmental", "Control"),
                    values = c("lightblue", "cornflowerblue"))

# 2-way ANOVA (preference ~ condition + income + condition : income)
res.aov.2 <- aov(X11_DiamondQuestion1 ~ FL_6_DO + Income + FL_6_DO : Income, data = merged)
summary(res.aov.2)
