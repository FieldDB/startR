library(reshape)

# Reading data, naming objects
list.a <- read.csv("listA.csv")
a
a <- read.table("listA.csv")
a <- read.table("listA.csv", sep = ",")
a <- read.table("listA.csv", sep = ",", header = TRUE)
a <- read.table("listA.csv", sep = ",", he = TRUE)
list.b <- read.csv("listB.csv")
dataListB <- read.csv("listB.csv")
myDataB <- read.csv("listB.csv")
my.data.b <- read.csv("listB.csv")
ListB <- read.csv("listB.csv")
ls()
rm(myDataB)
ls()
rm(list = ls())

a <- read.csv("listA.csv")
a.nohead <- read.csv("listA.csv", header = FALSE)
b <- read.csv("listB.csv")

c
c(3, 7, 103)
c <- 1:10
c
c(3, 476, 2)
data
data <- a
data
data()
head(tips)
data(tips)
head(tips)

#item.info <- read.delim("item_info.txt")
item.info <- read.delim("item_info.txt", na.strings = c("", "na"))
subject.info <- read.delim("subject_info.txt")

# Structure of data frames
head(a)
head(a.nohead)
summary(a)
str(a)
class(a)
class(a$RT)
class(a$Subject)

head(a$RT)
head(a$RT, 30)
head(a, 20)
a[ 1, 3]
a[ 1, "RT"]
colnames(a)
a[  , "RT"]
a[1:10, "RT"]
a[200:210, c("RT", "ACC")]

# Getting data frames together
head(b)

ldt <- rbind(a, b)

a$list
a$list <- "A"
head(a)
summary(a)

ldt <- rbind(a, b)

b$list <- "B"

ldt <- rbind(a, b)

head(item.info)

ldt2 <- merge(ldt, item.info)
nrow(ldt2)
ldt3 <- merge(ldt, item.info, all = TRUE)
nrow(ldt3)
summary(ldt3)

ldt <- merge(ldt, item.info, all = TRUE)
ldt <- merge(ldt, subject.info, all = TRUE)

# Clean-up and subsetting
head(ldt$Item, 50)
head(ldt, 50)

unique(ldt$Item)
sort(unique(ldt$Item))
head(ldt$Item, 10)
ldt$Item[1:10]
ldt$Item[1:10] < 1000
head(ldt[ldt$Item > 1000, ])

ldt <- ldt[ldt$Item < 1000, ]
summary(ldt)
summary(ldt$condition)

# Summarizing and re-formatting
xtabs(~ Subject, ldt)
xtabs(~ ACC, ldt)
xtabs(~ Subject + ACC, ldt)
subj.by.acc <- xtabs(~ Subject + ACC, ldt)
write.csv(subj.by.acc, "subject_by_accuracy.csv")
write.csv(xtabs(~ Subject + ACC, ldt), "subject_by_accuracy2.csv")

cast(ldt, type ~ ., value = "RT", mean)
cast(ldt, type ~ ., value = "RT", mean, na.rm = TRUE)

# more clean-up and subsetting
ldt.rt <- ldt[ldt$ACC == 1, ]
ldt.rt <- ldt.rt[ldt.rt$RT != NA, ] #ACK!
ldt.rt <- ldt[ldt$ACC == 1, ]
ldt.rt <- ldt.rt[!is.na(ldt.rt$RT), ]

summary(ldt.rt$RT)

# more summarizing, vectorized calculations
cast(ldt.rt, type ~ ., value = "RT", mean)
cast(ldt.rt, condition ~ ., value = "RT", mean)
cast(ldt.rt, condition ~ ., value = "RT", sd)
cast(ldt.rt, condition ~ ., value = "RT", c(mean, sd))

std.err <- function(x) {
  n <- length(x)
  std.err <- sd(x)/sqrt(n)
  return(std.err) # or just std.err
}

std.err <- function(x) sd(x)/sqrt(length(x))

rt.table1 <- cast(ldt.rt, condition ~ ., value = "RT", c(mean, sd, std.err))

rt.table2 <- cast(ldt.rt, condition ~ ., value = "RT", c(mean, sd, length))

rt.table2$std.err <- rt.table2$sd/sqrt(rt.table2$length)

# melting
ldt.rt$logRT <- log(ldt.rt$RT)
ldt.rt.melt <- melt(ldt.rt, measure.vars = c("RT", "logRT"))

cast(ldt.rt.melt, condition ~ variable, mean)
cast(ldt.rt.melt, condition ~ variable, c(mean, std.err))

# cleaning up frequency


# base graphics plots
plot(log(ldt.rt$frequency), ldt.rt$RT)
plot(log(RT) ~ log(frequency), data = ldt.rt)
abline(lm(log(RT) ~ log(frequency), data = ldt.rt))

summary(lm(log(RT) ~ log(frequency), data = ldt.rt))

# correlation
cor(log(ldt.rt$RT), log(ldt.rt$frequency), use = "pairwise.complete.obs")
cor.test(log(ldt.rt$RT), log(ldt.rt$frequency), use = "pairwise.complete.obs")

# linear regression
rt.freq.lm <- lm(log(RT) ~ log(frequency), data = ldt.rt)
summary(rt.freq.lm)
str(rt.freq.lm)
plot(rt.freq.lm$resid, log(ldt.rt$RT[!is.na(ldt.rt$frequency)]))

# t-tests
boxplot(log(RT) ~ type, data = ldt.rt)

ldt.rt.wnw <- ldt.rt[ldt.rt$type == c("word", "nonce"), ]
summary(ldt.rt.wnw)
summary(ldt.rt)
ldt.rt.wnw <- ldt.rt[ldt.rt$type %in% c("word", "nonce"), ]
summary(ldt.rt.wnw)
ldt.rt.wnw <- droplevels(ldt.rt[ldt.rt$type %in% c("word", "nonce"), ])

t.test(log(RT) ~ type, data = ldt.rt.wnw)
rt.lm <- lm(log(RT) ~ type, data = ldt.rt.wnw)
summary(rt.lm)

anova(rt.lm)

rt.aov1 <- aov(log(RT) ~ type, data = ldt.rt.wnw)
summary(rt.aov1)

ldt.rt.bysubj <- cast(ldt.rt.wnw, Subject + type ~ ., value = "logRT", mean)
ldt.rt.bysubj
ldt.rt.bysubj <- cast(ldt.rt.wnw, Subject + type ~ ., value = "logRT", c(mean, sd))
rt.aov2.bysubj <- aov(mean ~ type + Error(Subject/type), data = ldt.rt.bysubj)
summary(rt.aov2.bysubj)

ldt.rt.byitem <- cast(ldt.rt.wnw, Item + type ~ ., value = "logRT", c(mean, length))
head(ldt.rt.byitem)
rt.aov2.byitem <- aov(mean ~ type + Error(Item), data = ldt.rt.byitem)
summary(rt.aov2.byitem)

library(lme4)
rt.lmer <- lmer(log(RT) ~ type + (1 + type | Subject) + (1|Item), data = ldt.rt.wnw)
summary(rt.lmer)
summary(rt.lm)
