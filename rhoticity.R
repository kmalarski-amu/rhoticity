# This is the R code used for analyses in Chapters 4, 5 and 6 of the PhD entitled Loss of Rhoticity in South-West England by Kamil Malarski #


###### Chapter 4 ######


### class and age ###
df3 <- data.frame(class = rep(c("LMC", "MMC", "UMC"), each=3),
                  age=c("18-30", "31-50", "50+"),
                  percent_r=c(15.4, 25.7, 47.6, 11.1, 25.0, 34.1, 7.7, 8.3, 26.4) )
head(df3)

### plotting 1 ###
ggplot(data=df3, aes(x=age, y=percent_r, group=class)) +
  geom_line()+
  geom_point()

ggplot(data=df3, aes(x=age, y=percent_r, group=class)) +
  geom_line(linetype="dashed")+
  geom_point()

ggplot(data=df3, aes(x=age, y=percent_r, group=class)) +
  geom_line(aes(linetype=class))+
  geom_point()+
  scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  theme(legend.position="top")

# Change x and y axis labels, and limits
sp + scale_x_continuous(name="Speed of cars", limits=c(0, 30)) +
  scale_y_continuous(name="Stopping distance", limits=c(0, 150))


###### Chapter 5 ######

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

install.packages('gsheet')
library(gsheet)
gsheet2tbl('https://docs.google.com/spreadsheets/d/1b12S1YJLgGVJFYX0Nu2_YtpQ4oW2YNgs5Ni6f7hk9us/edit#gid=0')

### r rates vs. age ###

name <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13",
          "F14", "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", "F23", "M1", "M2", "M3", "M4",
          "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13","M14", "M15", "M16", "M18", "M19", "M20",
          "M22", "M23", "M24", "M25")
age <- c(44, 21, 33, 23, 27, 22, 22, 23, 76, 47, 66, 26, 72, 70, 27, 35, 51, 61, 52, 63, 75,
         40, 41, 64, 22, 38, 58, 27, 23, 54, 25, 39, 35, 23, 68, 67, 34, 72, 23, 68, 67, 20, 53, 26, 44, 19)
rhoticity_rate <- c(0, 0, 0, 1, 8, 0, 9, 4, 4, 23, 83, 0, 14, 96, 1, 3, 18, 41, 1, 54, 92, 9,
                    0, 96, 0, 4, 0, 0, 0, 15, 6, 2, 1, 3, 100, 91, 3, 86, 0, 8, 99, 39, 41, 1, 87, 1)

rates <- data.frame(name, age, rhoticity_rate)

library(ggplot2)
library(hrbrthemes)


### basic scatter plot ###
(basic_mm_scatter <- ggplot(rates, aes(age, rhoticity_rate)) +
    geom_point() +
    theme_bw())

### scatter plot with trendline ###
(basic_mm_scatter_line <- ggplot(rates, aes(age, rhoticity_rate)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

### improved scatter plot ###
(improved_mm_scat <- ggplot(rates, aes(age, rhoticity_rate, colour = age)) +
    geom_point() +
    theme_bw() +
    ylab("rhoticity rate") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))


data(gapminder, package="gapminder")

read.table("rhoticity_a.csv",
           header = TRUE,
           sep = ",",
           stringsAsFactors = FALSE)

library(dplyr)
ggplot(rhoticity_a, 
       aes(x= rhoticity, y = Speaker)) +
  geom_point()

ggplot(rhoticity_b, 
       aes(x=rhoticity, 
           y=reorder(Speaker, rhoticity))) +
            ylab("Speaker") +
  geom_point()

n<-dim(rhoticity_b)[1]
rhoticity_b<-rhoticity_b[1:(n-2),]

library(dplyr)


### test for normal distribution of rhoticity ###

shapiro.test(rhoticity_b$rhoticity)

n<-dim(rhoticity2)[1]
rhoticity2<-rhoticity2[1:(n-1),]

library(ggplot2)
library(reshape2)
library(plyr)
library(hrbrthemes)


rhoticity2_task = c("pairs", "wordlist", "passage", "dialogue", "avg_total")
rhoticity2$task = factor(rhoticity2$task, levels = all_counties)
rhoticity2$order = as.numeric(rhoticity2$county)/100 # a hack, but it works

ggplot(data = rhoticity2, aes(x = factor(order), fill = sex)) + 
  geom_bar(subset=.(sex=="F")) + 
  geom_bar(subset=.(sex=="M"),aes(y=..count..*(-1))) + 
  scale_x_discrete(labels = all_counties) +
  xlab("role") +
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  scale_fill_brewer(palette="dark2") +
  facet_wrap(~county)

### importing data ###

head(rhoticity)
summary(rhoticity)


frequency <- mutate(tapped, tapped.num=as.numeric(tapped)-1)
log(frequency)

rhoticity <- rhoticity %>%
  mutate(across(where(is.character), factor))

glmer(X.r. ~ position + context + word.class + log(frequency + 1) + (1 | speaker) + (1 | word), data = rhoticity)
m <- glmer(X.r. ~ position + context + word.class + log(frequency + 1) + stress + task + gender + age + class + prof_mother + prof_father + county + (1 | speaker) + (1 | word), data = rhoticity, family=binomial)
n <- glmer(X.r. ~ position + context + word.class + log(frequency + 1) + stress + gender + age + class + prof_mother + prof_father + county + (1 | speaker), data = rhoticity, family=binomial)
print(m, correlation=TRUE)
summary(m)$coef
summary (m)


###### Chapter 6: Discussion ######

self_identity <- c(8, 6, 5, 7, 7, 6, 5, 5, 7, 9, 6, 5, 9, 9, 7, 1, 5, 9, 7, 9, 9, 9,
                   7, 8, 5, 9, 3, 5, 5, 9, 7, 8, 4, 9, 8, 8, 7, 9, 5, 4, 9, 6, 9, 1, 9, 8)

region_rating <- c(9, 8, 8, 9, 7, 8, 8, 6, 9, 9, 9, 6, 9, 9, 8, 8, 9, 9, 8, 9, 7, 9, 9, 9,
                   5, 9, 7, 8, 7, 9, 6, 7, 9, 9, 9, 9, 8, 9, 9, 8, 9, 6, 9, 9, 9, 7)

accent_rating <- c(9, 6, 4, 9, 2, 7, 9, 4, 7, 9, 5, 3, 9, 9, 7, 1, 1, 5, 5, 6, 8, 9, 8, 8, 7, 9, 5, 9,
                   6, 8, 9, 6, 7, 7, 2, 9, 7, 8, 7, 5, 8, 7, 4, 2, 9, 8)

accent_awareness <- c(1, 2, 5, 3, 4, 2, 4, 2, 4, 9, 5, 3, 6, 9, 2, 2, 8, 5, 2, 6, 6, 5, 1, 9,
                      5, 5, 1, 4, 2, 6, 4, 6, 2, 2, 6, 8, 3, 7, 5, 5, 6, 4, 7, 3, 8, 1)

### Let's build a data frame with these four data series ###

identities <- data.frame(self_identity, region_rating, accent_rating, accent_awareness, rhoticity_rate)

### Let us now show how averaged rhoticity rates correlate with each of these 4 parameters ###

(identity_plot <- ggplot(identities, aes(self_identity, rhoticity_rate, colour = self_identity)) +
    geom_point() +
    theme_bw() +
    ylab("rhoticity rate") +
    xlab("self-identity") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

(region_plot <- ggplot(identities, aes(region_rating, rhoticity_rate, colour = region_rating)) +
    geom_point() +
    theme_bw() +
    ylab("rhoticity rate") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

(improved_mm_scat <- ggplot(identities, aes(accent_rating, rhoticity_rate, colour = accent_rating)) +
    geom_point() +
    theme_bw() +
    ylab("rhoticity rate") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

(improved_mm_scat <- ggplot(identities, aes(accent_awareness, rhoticity_rate, colour = accent_awareness)) +
    geom_point() +
    theme_bw() +
    ylab("rhoticity rate") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

