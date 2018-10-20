## This is the reproduction file for "When do the Rich Win" by J.
## Alexander Branham, Stuart Soroka, and Christopher Wlezien. It will
## reproduce all of the figures and tables in the article as well as
## the appendix.

## This is the session information used to create the figures, tables,
## and other statistics reported in the paper:

## Session info ----------------------------------
##  setting  value
##  version  R version 3.3.0 (2016-05-03)
##  system   x86_64, linux-gnu
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  tz       America/Chicago
##  date     2016-06-06

## Packages --------------------------------------
##  package    * version date       source
##  dplyr      * 0.4.3   2015-09-01 CRAN (R 3.3.0)
##  haven        0.2.0   2015-04-09 CRAN (R 3.3.0)
##  lavaan     * 0.5-20  2015-11-07 CRAN (R 3.3.0)

rm(list = ls())

library(dplyr) # for data munging
library(lavaan) # for SEM modelling
library(haven) # for importing Stata file

if (as.character(packageVersion("lavaan")) != "0.5.20") {
  stop('lavaan version 0.5.20 is required.
       You may install it with the command install.packages("https://cran.r-project.org/src/contrib/Archive/lavaan/lavaan_0.5-20.tar.gz",repos=NULL,type="source")')
}

## Here we load in the data (version 1) provided by Gilens (see
## Affluence and Influence)
gilens <- haven::read_dta("./DS1_0.dta")

############################################################
## Variable creation
############################################################

range01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

gilens <- gilens %>%
  filter(OUTCOME != 99, OUTCOME != 2.5,
         OUTCOME != 3.5, OUTCOME != 4.5) %>%
  mutate(rich_want = pred90_sw > 0.5,
         middle_want = pred50_sw > 0.5,
         poor_want = pred10_sw > 0.5,
         rangepred50 = range01(qlogis(pred50_sw)),
         rangepred90 = range01(qlogis(pred90_sw)),
         rangediff = range01(qlogis(pred90_sw) - qlogis(pred50_sw)),
         disagreement = rich_want != middle_want,
         policy_change = as.numeric(OUTCOME > 0),
         disagreement = as.numeric(disagreement),
         mid_support_greater = pred50_sw > pred90_sw,
         rich_opp_greater = pred50_sw < pred90_sw,
         rich_opinion = pred90_sw * 100,
         mid_opinion = pred50_sw * 100,
         difference = rich_opinion - mid_opinion,
         interest_align = log(INTGRP_STFAV +
                                (0.5 * INTGRP_SWFAV) + 1) -
           log(INTGRP_STOPP +
                 (0.5 * INTGRP_SWOPP) + 1),
         rangeinterest = range01(interest_align),
         who_wants = rich_want - middle_want,
         winner = ifelse(disagreement == FALSE, NA,
           ifelse(who_wants == -1 & policy_change == 1, 1,
           ifelse(
             who_wants == 1 & policy_change == 1, 2,
             ifelse(
               who_wants == 1 & policy_change == 0, 1,
               ifelse(who_wants == -1 & policy_change == 0,
                      2, NA))))),
         agreement = ifelse(poor_want == rich_want &
                              rich_want == middle_want &
                              poor_want == TRUE, 4,
                            ifelse(rich_want == middle_want &
                                   rich_want == TRUE, 3,
                            ifelse(rich_want == poor_want &
                                     rich_want == TRUE, 2,
                            ifelse(middle_want == poor_want &
                                     middle_want == TRUE, 1,
                            ifelse(poor_want == rich_want &
                                     rich_want == middle_want &
                                     poor_want == FALSE, -3,
                            ifelse(rich_want == middle_want &
                                     rich_want == FALSE, -2,
                            ifelse(rich_want == poor_want &
                                     rich_want == FALSE, -1,
                            ifelse(middle_want == poor_want &
                                     middle_want == FALSE, 0, NA)))))))),
         agreement = factor(agreement,
                            levels = -3:4,
                            labels = c("No one want",
                                       "R-M no want",
                                       "R-P no want",
                                       "M-P no want",
                                       "M-P want",
                                       "R-P want",
                                       "R-M want",
                                       "All want")))

############################################################
## Begin figures
############################################################

## Figure 1
bw_pass <- "black"
bw_nopass <- gray(0.6)
pdf("figure1.pdf", width = 7, height = 7)
plot(gilens$mid_opinion[gilens$policy_change == FALSE],
     gilens$rich_opinion[gilens$policy_change == FALSE],
     ylim = c(0, 100),
     xlim = c(0, 100), axes = FALSE, ann = FALSE, pch = 1,
     cex = 0.5, col = bw_nopass)
points(gilens$mid_opinion[gilens$policy_change == TRUE],
       gilens$rich_opinion[gilens$policy_change == TRUE],
       cex = 0.5, pch = 1, col = bw_pass)
abline(0, 1, lty = 2, lwd = 3, col = "black")
axis(side = 2, las = 1, cex.axis = 0.8, col = "gray")
axis(side = 1, las = 1, cex.axis = 0.8, col = "gray")
title(xlab = "Estimated Support, 50th Income Percencile", cex.lab = 1)
title(ylab = "Estimated Support, 90th Income Percencile", cex.lab = 1)
text(50, 14, "Support from middle and high",
     col = "black", cex = 0.8, pos = 4)
text(50, 10, "income groups when the bill...", col = "black",
     cex = 0.8, pos = 4)
text(50, 6, "Passed", col = "black", cex = 0.8, pos = 4)
text(65, 6, "/", col = "black", cex = 0.8, pos = 4)
text(70, 6, "Did not Pass", col = bw_nopass, cex = 0.8, pos = 4)
dev.off()

## Figure 2
bw_pass <- "black"
bw_nopass <- gray(0.6)
bw_agree <- gray(0.85)
pdf("figure2.pdf", width = 7, height = 7)
plot(gilens$mid_opinion[gilens$policy_change == FALSE &
                          gilens$disagreement == TRUE],
     gilens$rich_opinion[gilens$policy_change == FALSE &
                           gilens$disagreement == TRUE],
     ylim = c(0, 100),
     xlim = c(0, 100), axes = FALSE, ann = FALSE, pch = 0,
     cex = 0.6, col = bw_nopass)
points(gilens$mid_opinion[gilens$policy_change == TRUE &
                            gilens$disagreement == TRUE],
       gilens$rich_opinion[gilens$policy_change == TRUE &
                             gilens$disagreement == TRUE],
       cex = 0.6, pch = 0, col = bw_pass)
points(gilens$mid_opinion[gilens$disagreement == FALSE],
       gilens$rich_opinion[gilens$disagreement == FALSE],
       cex = 0.5, pch = 1, col = bw_agree)
abline(0, 1, lty = 2, lwd = 3, col = "black")
axis(side = 2, las = 1, cex.axis = 0.8, col = "gray")
axis(side = 1, las = 1, cex.axis = 0.8, col = "gray")
title(xlab = "Estimated Support, 50th Income Percencile", cex.lab = 1)
title(ylab = "Estimated Support, 90th Income Percencile", cex.lab = 1)
text(50, 13, "Support from middle and high",
     col = "black", cex = 0.8, pos = 4)
text(50, 10, "income groups when the bill...", col = "black",
     cex = 0.8, pos = 4)
text(50, 6, "Passed", col = bw_pass, cex = 0.8, pos = 4)
text(65, 6, "/", col = "black", cex = 0.8, pos = 4)
text(70, 6, "Did not Pass", col = bw_nopass, cex = 0.8, pos = 4)
text(50, 2, "excluding policies for which middle", col = bw_agree,
     cex = .8, pos = 4)
text(50, -1, "& high income groups agree", col = bw_agree,
     cex = .8, pos = 4)
dev.off()

## Get the ideologically-coded data, create a few needed variables
gilens_ideo <- read.csv("./ideo-coded.csv")
gilens_ideo <- gilens_ideo %>%
  filter(OUTCOME != 99, OUTCOME != 2.5,
         OUTCOME != 3.5, OUTCOME != 4.5) %>%
  mutate(policy_change = as.numeric(OUTCOME > 0),
         rich_want = pred90.sw > 0.5,
         middle_want = pred50.sw > 0.5,
         who_wants = rich_want - middle_want,
         winner = ifelse(who_wants == -1 & policy_change == 1, 1,
           ifelse(
             who_wants == 1 & policy_change == 1, 2,
             ifelse(
               who_wants == 1 & policy_change == 0, 1,
               ifelse(who_wants == -1 & policy_change == 0,
                      2, NA)))),
         ideology = ifelse(ideology == 99, NA, ideology),
         ideology = ifelse(ideology == -1, 3,
                           ifelse(ideology == 0, 2,
                                  ifelse(ideology == 1, 1, NA))))
T2 <- with(gilens_ideo,
           table(policy_change[winner == 1],
            ideology[winner == 1]))
T4 <- with(gilens_ideo,
           table(change[winner == 2],
            ideology[winner == 2]))

## Figure 3
bw_pass <- "black"
bw_nopass <- gray(0.6)
pdf("figure3.pdf", width = 7, height = 5.5)
layout(matrix(c(1, 2), 2, 2, byrow = TRUE),
       widths = c(1, 1), heights = c(1, 1))
barplot(T2, beside = TRUE, ylim = c(0, 27),
        names.arg = c("Liberal", "Neither", "Conservative"),
        col = c(bw_nopass, bw_pass),
        axes = FALSE, border = NA)
axis(side = 2, at = c(0, 5, 10, 15, 20, 25), las = 1,
     col = "gray", cex.axis = 0.8)
mtext(side = 3, "The Ideological \nDistribution of Middle 'Wins'")
text(5, 38, "*Coded where available", cex = 0.8)
text(2, 25, "Total = 26")
text(5, 25, "Total = 29")
text(8, 25, "Total = 20")
barplot(T4, beside = TRUE, ylim = c(0, 27),
        names.arg = c("Liberal", "Neither", "Conservative"),
        col = c(bw_nopass, bw_pass), axes = FALSE, border = NA)
axis(side = 2, at = c(0, 5, 10, 15, 20, 25), las = 1,
     col = "gray", cex.axis = 0.8)
mtext(side = 3, "The Ideological \nDistribution of Rich 'Wins'")
text(5, 38, "*Coded where available", cex = 0.8)
text(2, 25, "Total = 28")
text(5, 25, "Total = 37")
text(8, 25, "Total = 26")
dev.off()

T4[2, 3] / sum(T4[2, ])

############################################################
### End figures
############################################################


############################################################
## Begin Tables & Statistics in Text
############################################################

## Table 1
table1 <- with(gilens, table(middle_want, rich_want))
dimnames(table1) <- list(c("Middle Oppose",
                           "Middle Favor"),
                         c("Rich Oppose",
                           "Rich Favor"))
table1

# calculate textual notes:
gilens %>%
  mutate(gap = abs(pred90_sw * 100 - pred50_sw * 100)) %>%
  group_by(disagreement) %>%
  summarize(mean = mean(gap),
            sd = sd(gap),
            n = n())

gilens %>%
  mutate(gap = abs(pred90_sw * 100 - pred50_sw * 100)) %>%
  with(., t.test(gap[disagreement == FALSE], gap[disagreement == TRUE]))

## Table 2
table2 <- with(gilens, table(winner, who_wants))
table2 <- table2[, -2]
table2 <- addmargins(table2, margin = 2)
table2 <- cbind(table2, round(table2[, 3] / 185 * 100))
dimnames(table2) <- list(c("Middle Win",
                           "Rich Win"),
                         c("Middle Want",
                           "Rich Want",
                           "Total Wins",
                           "Win Rate"))
table2

# Calculate textual information
p_val <- function(pbar, n){
  z <- (pbar - 0.5) / sqrt(0.25 / n)
  pval <- 2 * pnorm(-abs(z))
  pval
}

# Is gap in win rates stat diff?
p_val(98 / 185, 185)

# Difference between rich and middle changing policy
p_val_for_rich_mid_passage <- function(){
  mid_prop <- table2[1, 1] / (sum(table2[, 1]))
  rich_prop <- table2[2, 2] / (sum(table2[, 2]))
  p <- (mid_prop * sum(table2[, 1]) + rich_prop * sum(table2[, 2])) /
    sum(table2[, 1:2])
  se <- sqrt(p * (1 - p) * ((1 / sum(table2[, 1]) + (1 / sum(table2[, 2])))))
  z <- (mid_prop - rich_prop) / se
  pnorm(-abs(z))
}

p_val_for_rich_mid_passage()

# Preferences differ by > 10 percentage points?
diff_table <- gilens %>%
  filter(abs(difference) > 10) %>%
  with(., table(winner, who_wants))
diff_table <- addmargins(diff_table, margin = 2)
diff_table

p_val(56 / 101, 101)

## Table 3
table3 <- with(gilens, table(agreement, policy_change))
table3 <- cbind(table3, round(table3[, 2] /
                                (table3[, 1] + table3[, 2]) * 100, 1))
dimnames(table3) <- list(c("None Favor",
                           "Poor Favor",
                           "Middle Favor",
                           "Rich Favor",
                           "Middle/Poor Favor",
                           "Rich/Poor Favor",
                           "Rich/Middle Favor",
                           "All Favor"),
                         c("Blocked",
                           "Passed",
                           "Passage Rate"))
table3

with(gilens, cor(pred50_sw, pred90_sw))
with(gilens, cor(pred50_sw, pred10_sw))
with(gilens, cor(pred10_sw, pred90_sw))



############################################################
## End Tables & Statistics in Text
############################################################


############################################################
## Begin Appendix
############################################################

## Table A1: Strength of public preferences

table_a1 <- cbind(
  with(subset(gilens, rich_want == TRUE & disagreement == FALSE),
       table(policy_change, mid_support_greater)),
  with(subset(gilens, rich_want == FALSE & disagreement == FALSE),
       table(policy_change, rich_opp_greater)))
dimnames(table_a1) <- list(c("Blocked", "Passed"),
                           both_support_then_both_oppose =
                             c("Rich Support Greater",
                               "Middle Support Greater",
                               "Rich Opposition Greater",
                               "Middle Opposition Greater"))
table_a1

## Table A2: Economic vs social issues
gilens <- gilens %>%
  mutate(econ = ifelse(
    XL_AREA == "budget" | XL_AREA == "econ & labor" |
      XL_AREA == "taxation", TRUE,
    ifelse(XL_AREA == "defense" | XL_AREA == "terrorism", NA, FALSE)))

table_a2 <- cbind(
  with(subset(gilens, disagreement == TRUE & econ == TRUE),
       table(winner, rich_want)),
  with(subset(gilens, disagreement == TRUE & econ == FALSE),
       table(winner, rich_want)))

dimnames(table_a2) <- list(c("Middle Win", "Rich Win"),
                           economc_then_social = c("Middle Favor",
                                                   "Rich Favor",
                                                   "Middle Favor",
                                                   "Rich Favor"))
table_a2

## Calculate economic and social win-rates
econ_win <- sum(table_a2[2, 1:2]) / sum(table_a2[, 1:2])
soc_win <- sum(table_a2[2, 3:4]) / sum(table_a2[, 3:4])

## Is that difference statistically sig?
phat <- sum(table_a2[2, ]) / sum(table_a2)
z_stat <- (econ_win - soc_win) /
  sqrt(phat * (1 - phat) *
         ((1 / sum(table_a2[, 1:2])) +
          (1 / sum(table_a2[, 3:4]))))

2 * pnorm(-abs(z_stat))

# Table A3

model <- "
# measurement
  L5 =~ 1 * rangepred50
  L9 =~ 1 * rangepred90
  IG_bal =~ 1 * rangeinterest
  # regressions
  policy_change ~ L5 + L9 + IG_bal
  # cov and var
  rangepred50 ~~ .0026380 * rangepred90
  L5 ~~ IG_bal
  L9 ~~ IG_bal
  L5 ~~ L9
"

model1 <- sem(model,
           data = gilens,
           estimator = "WLS")
summary(model1, rsquare = TRUE)

model2  <- sem(model,
               data = gilens[gilens$disagreement == FALSE, ],
               estimator = "WLS")
summary(model2, rsquare = TRUE)

model3  <- sem(model,
               data = gilens[gilens$disagreement == TRUE, ],
               estimator = "WLS")
summary(model3, rsquare = TRUE)

model4 <- sem(model,
              data = gilens[gilens$disagreement == TRUE, ],
              estimator = "WLS",
              ordered = "policy_change")
summary(model4, rsquare = TRUE)

model5 <- lm(policy_change ~ rangepred50 +
             rangepred90 + rangeinterest,
             data = gilens[gilens$disagreement == TRUE, ])
summary(model5)

model6 <- glm(policy_change ~ rangepred50 + rangepred90 + rangeinterest,
            data = gilens[gilens$disagreement == TRUE, ],
            family = binomial(link = "probit"))
summary(model6)

# F test of joint sig of rich & middle for model 3
reg_test <- lm(policy_change ~ rangeinterest,
            data = gilens[gilens$disagreement == TRUE, ])
anova(reg_test, model5)

## Table A4: Alternative regression results
model7 <- glm(policy_change ~ rangediff * disagreement + rangeinterest,
              data = gilens,
              family = binomial(link = "probit"))
summary(model7)

# Calculate p value for marginal effect of diff when disagree==1
se_b1 <- vcov(model7)[2, 2]
se_b3 <- vcov(model7)[5, 5]
se_cov <- vcov(model7)[2, 5]
margin_se <- sqrt(se_b1 + 1 ^ 2 * se_b3 + 2 * 1 * se_cov)
the_t <- (coef(model7)[2] + coef(model7)[5]) / margin_se
2 * pt(-abs(the_t), df = model7$df.residual)

## Include interactions, then calculate p value
model8 <- update(model7, . ~ rangediff * disagreement +
                           rangepred50 * disagreement +
                           rangeinterest * disagreement)
summary(model8)

# p value
se_b1 <- vcov(model8)[2, 2]
se_b3 <- vcov(model8)[6, 6]
se_cov <- vcov(model8)[2, 6]
margin_se <- sqrt(se_b1 + 1 ^ 2 * se_b3 + 2 * 1 * se_cov)
the_t <- (coef(model8)[2] + coef(model8)[6]) / margin_se
2 * pt(-abs(the_t), df = model8$df.residual)
