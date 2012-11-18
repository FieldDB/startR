library(reshape)
library(plyr)
library(ggplot2)
library(lme4)

# Read data
list.a <- read.csv("listA.csv")
list.b <- read.csv("listB.csv")

item.info <- read.delim("item_info.txt", na.strings = c("", "na"))
subject.info <- read.delim("subject_info.txt")

# Merge data files
list.a$list <- "A"
list.b$list <- "B"
ldt <- rbind(list.a, list.b)

head(item.info)

nrow(ldt)
ldt <- merge(ldt, item.info, all = TRUE)
nrow(ldt)
ldt <- merge(ldt, subject.info, all = TRUE)
nrow(ldt)
colnames(ldt)
summary(ldt)
write.csv(ldt, "all_ldt.csv", row.names = FALSE)
ldt2 <- read.csv("all_ldt.csv")

# Clean-up and subsetting
ldt <- ldt[ldt$Item < 1000, ]
nrow(ldt)
summary(ldt)
summary(ldt$condition)
# (move write/read code here)

#####################
# Continuous data
# RT analysis: are words faster than nonwords?

# More cleaning/munging
xtabs(~ Subject, ldt)
xtabs(~ ACC, ldt)
xtabs(~ Subject + ACC, ldt)

ldt.rt <- ldt[ldt$ACC == 1, ]
ldt.rt <- ldt.rt[ldt.rt$RT != NA, ] #ACK!
ldt.rt <- ldt[ldt$ACC == 1, ]
ldt.rt <- ldt.rt[!is.na(ldt.rt$RT), ]

summary(ldt.rt)

ldt.rt2 <- ldt.rt[ldt.rt$type %in% c("nonce", "word"), ]
ldt.rt3 <- ldt.rt[ldt.rt$type == c("nonce", "word"), ]
summary(ldt.rt3)
summary(ldt.rt2)
ldt.rt  <- droplevels(ldt.rt[ldt.rt$type %in% c("nonce", "word"), ])
summary(ldt.rt)

# Outliers
# Look at distributions!
ggplot(ldt.rt, aes(x = RT)) + geom_histogram()
hist(ldt.rt$RT)

ggplot(ldt.rt, aes(x = RT)) + geom_histogram(binwidth = 10)
hist(ldt.rt$RT, breaks = 250)

ggplot(ldt.rt, aes(x = log(RT))) + geom_histogram()
hist(log(ldt.rt$RT))

# qqnorm(log(ldt.rt$RT))

# Remove outliers
# Define outliers: lots possible!
mean(log(ldt.rt$RT))
sd(log(ldt.rt$RT))
high.cut <- mean(log(ldt.rt$RT)) + 3*sd(log(ldt.rt$RT))
high.cut
exp(high.cut)
low.cut <- mean(log(ldt.rt$RT)) - 3*sd(log(ldt.rt$RT))
low.cut
exp(low.cut)
hist(log(ldt.rt$RT))
abline(v = high.cut, col = "red")
abline(v = low.cut, col = "red")

hist(log(ldt.rt$RT), xlim = c(log(500), log(4000)))
abline(v = high.cut, col = "red")
abline(v = low.cut, col = "red")
#pdf("myplot.pdf")

#dev.off()

hist(ldt.rt$RT, xlim = c(500, 4000))
abline(v = exp(high.cut), col = "red")
abline(v = exp(low.cut), col = "red")

nrow(ldt.rt[log(ldt.rt$RT) < low.cut, ])
nrow(ldt.rt[log(ldt.rt$RT) > high.cut, ])

ldt.rt.trim <- ldt.rt[log(ldt.rt$RT) >= low.cut & log(ldt.rt$RT) <= high.cut, ]
nrow(ldt.rt)
nrow(ldt.rt.trim)

# By-subject cutoffs
cutoffs <- ddply(ldt.rt, "Subject", summarise, low.cut.bysub = mean(log(RT)) - 3*sd(log(RT)), high.cut.bysub = mean(log(RT)) + 3*sd(log(RT)))
cutoffs

ldt.rt <- merge(ldt.rt, cutoffs, all = TRUE)
nrow(ldt.rt)
ldt.rt.trim2 <- ldt.rt[log(ldt.rt$RT) >= ldt.rt$low.cut.bysub & log(ldt.rt$RT) <= ldt.rt$high.cut.bysub, ]

# Summary stats
# Cell means
cast(ldt.rt, type ~ ., value = "RT", mean)
cast(ldt.rt, type ~ ., value = "RT", c(mean, sd))
cast(ldt.rt, type + condition ~ ., value = "RT", c(mean, sd))
cast(ldt.rt, condition + type ~ ., value = "RT", c(mean, sd))
ldt.rt$logRT <- log(ldt.rt$RT)
cast(ldt.rt, type ~ ., value = "logRT", c(mean, sd))

std.err <- function(x) {
  n <- length(x)
  std.err <- sd(x)/sqrt(n)
  return(std.err) # or just std.err
}

std.err <- function(x) sd(x)/sqrt(length(x))

rt.table1 <- cast(ldt.rt.trim, type ~ ., value = "RT", c(mean, sd, std.err))
rt.table2 <- cast(ldt.rt.trim, type ~ ., value = "RT", c(mean, sd, length))
rt.table2$std.err <- rt.table2$sd/sqrt(rt.table2$length)
rt.table1
rt.table2

ddply(ldt.rt.trim, "type", summarise, mean = mean(RT), sd = sd(RT), std.err = std.err(RT))
ddply(ldt.rt.trim, "type", summarise, mean = mean(RT), sd = sd(RT), std.err = sd(RT)/sqrt(length(RT)))

rt.table <- ddply(ldt.rt.trim, "type", summarise, mean = mean(log(RT)), sd = sd(log(RT)), std.err = std.err(log(RT)))
rt.table

# Summary plot comparisons
# Boxplots
boxplot(RT ~ type, ldt.rt.trim)
ggplot(ldt.rt.trim, aes(x = type, y = RT)) + geom_boxplot()

boxplot(log(RT) ~ type, ldt.rt.trim)
ggplot(ldt.rt.trim, aes(x = type, y = log(RT))) + geom_boxplot()

# dot + errorbars
rt.table
ggplot(rt.table, aes(x = type, y = mean)) + geom_point() + geom_errorbar(aes(ymin = mean - 1.96*std.err, ymax = mean + 1.96*std.err))

ggplot(rt.table, aes(x = type, y = mean)) + geom_bar() + geom_errorbar(aes(ymin = mean - 1.96*std.err, ymax = mean + 1.96*std.err))
ggplot(rt.table, aes(x = type, y = mean)) + geom_bar(fill = "white") + geom_errorbar(aes(ymin = mean - 1.96*std.err, ymax = mean + 1.96*std.err))
ggplot(rt.table, aes(x = type, y = mean)) + geom_bar(fill = "white") + geom_errorbar(aes(ymin = mean - 1.96*std.err, ymax = mean + 1.96*std.err)) + ylim(7, 7.2)

ggplot(rt.table, aes(x = type, y = mean)) + geom_point() + geom_errorbar(aes(ymin = mean - 1.96*std.err, ymax = mean + 1.96*std.err))
# Analyses
# t-test
t.test(log(RT) ~ type, ldt.rt.trim)
t.test(log(ldt.rt.trim$RT[ldt.rt.trim$type == "nonce"]))
t.test(log(RT) ~ type, ldt.rt.trim, var.equal = TRUE)

?t.test

# correlation
ldt.rt.freq <- ldt.rt.trim[!is.na(ldt.rt.trim$frequency), ]
cor(log(ldt.rt.freq$RT), log(ldt.rt.freq$frequency))
cor.test(log(ldt.rt.freq$RT), log(ldt.rt.freq$frequency))

plot(log(RT) ~ log(frequency), data = ldt.rt.freq)

# lm
rtfreq.lm <- lm(log(RT) ~ log(frequency), data = ldt.rt.freq)

rtfreq.lm
summary(rtfreq.lm)
coef(rtfreq.lm)
rtfreq.lm$coef
str(rtfreq.lm)

abline(rtfreq.lm)

ggplot(ldt.rt.freq, aes(log(frequency), log(RT))) + geom_point() + geom_smooth(method = "lm")
ggplot(ldt.rt.freq, aes(log(frequency), log(RT))) + geom_point() + geom_smooth()

rttype.lm <- lm(log(RT) ~ type, data = ldt.rt.trim)
summary(rttype.lm)
t.test(log(RT) ~ type, ldt.rt.trim)


# ANOVA
anova(rttype.lm)
rttype.anova <- aov(log(RT) ~ type, data = ldt.rt.trim)
summary(rttype.anova)

# By-subject ANOVA
ldt.rt.trim.bysubj <- ddply(ldt.rt.trim, c("Subject", "type"), summarise, meanlogRT = mean(log(RT)), std.err = std.err(log(RT)))
ldt.rt.trim.bysubj

ggplot(ldt.rt.trim.bysubj, aes(type, meanlogRT)) + geom_point() + geom_errorbar(aes(ymin = meanlogRT - 1.96*std.err, ymax = meanlogRT + 1.96*std.err)) + facet_wrap(~ Subject)

rttype.anova.bysubj <- aov(meanlogRT ~ type + Error(Subject/type), data = ldt.rt.trim.bysubj)
summary(rttype.anova.bysubj)

# lmer
rttype.lmer <- lmer(log(RT) ~ type + (1 + type | Subject) + (1 | Item), data = ldt.rt.trim)
summary(rttype.lmer)
summary(rttype.lm)
ranef(rttype.lmer)

coef(summary(rttype.lmer))
write.csv(coef(summary(rttype.lmer)), "lmer_summary.csv")

################################################
# Categorical
ldt.wnw <- droplevels(ldt[ldt$type %in% c("word", "nonce") & !is.na(ldt$ACC), ])
xtabs(~ ACC + type, ldt.wnw)
chisq.test(xtabs(~ ACC + type, ldt.wnw))
plot(xtabs(~ ACC + type, ldt.wnw))

ddply(ldt.wnw, "type", summarise, meanACC = mean(ACC))

ldt.acc.glm <- glm(ACC ~ type, data = ldt.wnw, family = "binomial")
summary(ldt.acc.glm)


ldt.acc.glmer <- lmer(ACC ~ type + (1 + type | Subject) + (1 | Item), data = ldt.wnw, family = "binomial")
summary(ldt.acc.glmer)