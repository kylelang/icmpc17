### Title:    Attention & Sound Analysis
### Author:   Kyle M. Lang & Eriko Fukuda
### Created:  2023-01-17
### Modified: 2023-05-18

rm(list = ls(all = TRUE))

library(magrittr)
library(dplyr)
library(lme4)
library(performance)
library(ggplot2)

dataDir <- "../data/"
fn1     <- "PerMusA_N73.csv"
fn2     <- "Combined_sample size 73.csv"

###-Data Ingest--------------------------------------------------------------###

dat0 <- read.csv2(paste0(dataDir, fn1))

## Check data:
head(dat0)
summary(dat0)
str(dat0)

## Any missing data?
colMeans(is.na(dat0))

## Frequencies of categorical variables:
table(dat0$correct)
table(dat0$response)
table(dat0$title)

## Drop the bird sound condition:
dat1 <- dat0 %>%
    filter(title != "2022_NE") %>%
    rename(id = multichar_response) %>%
    mutate(cond = factor(title,
                         levels = c("2022_NS", "2022_HJ", "2022_HP"),
                         labels = c("none", "jazz", "pop")
                         )
           )

head(dat1)
summary(dat1)

levels(dat1$cond)
dat1 %$% table(cond, title)

###-Modeling-----------------------------------------------------------------###

## Calculate ICC:
fit0 <- glmer(correct ~ (1|id), data = dat1, family = "binomial")
summary(fit0)
icc(fit0)

## Fit the hypothesized model:
fit1 <- glmer(correct ~ cond + (1|id), data = dat1, family = "binomial")
summary(fit1)
fixef(fit1) %>% exp()

## Change the reference group to evaluate the other comparison:
fit2 <- dat1 %>%
    mutate(cond = relevel(cond, ref = "jazz")) %$%
    glmer(correct ~ cond + (1|id), family = "binomial")
summary(fit2)
fixef(fit2) %>% exp()

###-Check Assumptions--------------------------------------------------------###

## Estimate (deviance) residuals and fitted values (on the latent scale):
res  <- resid(fit2, type = "deviance")
yHat <- predict(fit2, type = "link")

## Plot residuals against fitted values:
data.frame(yHat, res) %>%
  ggplot(aes(yHat, res)) +
  geom_point() +
  geom_smooth()

## Check homogeneity of residual variances:
resid(fit0, type = "deviance") %>% tapply(dat1$cond, var)

## Check homogeneity and linearity:
data.frame(residual  = resid(fit0, type = "deviance"),
           condition = dat1$cond) %>%
ggplot(aes(condition, residual)) +
       geom_boxplot()

###-Conclusions--------------------------------------------------------------###

### 1. The assumptions seem to be satisfied.
### 2. The clustering effect is not very strong (ICC = 0.1)
### 3. Pop music is significantly better than no sound
### 4. Pop music is significantly better than jazz music
### 5. There is no significant difference between no sound and jazz
