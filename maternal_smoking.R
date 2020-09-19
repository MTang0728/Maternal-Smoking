# load libraries
library(ggplot2)
library(GGally)
library(knitr)
library(xtable)
library(rms)

# import data
babies_data <- read.csv('./smoking.csv')
# remove columns 
babies_data <- babies_data[, !names(babies_data) %in% c('id', 'date', 'gestation')]
# turn mother race into a factor variable
babies_data$mrace <- factor(babies_data$mrace)
levels(babies_data$mrace) <- list('1'=c('0','1','2','3','4','5'), '2'=c('6'), '3'=c('7'), '4'=c('8'), '5'=c('9'))
# turn mother edu into a factor variable
babies_data$med <- factor(babies_data$med)
# turn income and smoke to factor variable
babies_data$inc <- factor(babies_data$inc)
babies_data$smoke <-factor(babies_data$smoke)
# check if response variable is normal
ggplot(babies_data,aes(x=bwt.oz)) +
  geom_histogram(color = 'lightblue', fill = 'white', binwidth = 5) +
  labs(title="Distribution of body weight")
## the distribution looks normal

# explore the relationship between body weight and other discrete/continuous variables
ggplot(babies_data,aes(x=parity, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weights vs Parity",x="Parity",y="Body Weight")
## seems to be a quadratic pattern
ggplot(babies_data,aes(x=mage, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weights vs Mother Age",x="Mother Age",y="Body Weight")
## looks somewhat linear
ggplot(babies_data,aes(x=mht, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weights vs Mother Height",x="Mother Height",y="Body Weight")
## looks like there might be a quadratic relationship
ggplot(babies_data,aes(x=mpregwt, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weights vs Mother Pre-pregnancy Weight",x="Mother Pre-pregnancy Weight",y="Body Weight")
## looks like there is a linear relationship

# explore the relationship between body weight and other categorical/factor variables
ggplot(babies_data,aes(x=mrace, y=bwt.oz, fill=mrace)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weights vs Mother Race",x="Mother Race",y="Body Weight") + 
  theme_classic() + theme(legend.position="none")
## Looks like we may have some difference in the means, specifically for black and asian
ggplot(babies_data,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weights vs Mother Education",x="Mother Education",y="Body Weight") + 
  theme_classic() + theme(legend.position="none")
## lower education seems to have a difference on the body weight
ggplot(babies_data,aes(x=inc, y=bwt.oz, fill=inc)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weights vs Mother Income",x="Mother Income",y="Body Weight") + 
  theme_classic() + theme(legend.position="none")
#@ the median weight looks somewhat close for each income level, lower income do have lower weights
ggplot(babies_data,aes(x=smoke, y=bwt.oz, fill=smoke)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weights vs Mother Smokes",x="Mother Smokes",y="Body Weight") + 
  theme_classic() + theme(legend.position="none")
## mothers who smokes tend to give birth to babes with lower weight

# need to now look at the interaction:
# weight and parity by mrace
ggplot(babies_data,aes(x=parity, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Parity by Mother Race",x="Parity",y="Body Weight") +
  facet_wrap( ~ mrace,ncol=5)
## a lot of difference shown by race, there is an interaction
# weight and mage by mrace
ggplot(babies_data,aes(x=mage, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Age by Mother Race",x="Mother Age",y="Body Weight") +
  facet_wrap( ~ mrace,ncol=5)
## different pattern for race group 4, there is an interaction
# weight and mht by mrace
ggplot(babies_data,aes(x=mht, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Height by Mother Race",x="Mother Height",y="Body Weight") +
  facet_wrap( ~ mrace,ncol=5)
## different pattern for race group 4, there is an interaction
# weight and mpregwt by mrace
ggplot(babies_data,aes(x=mpregwt, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Pre-Praganancy Weight by Mother Race",x="Mother Pre-Praganancy Weight",y="Body Weight") +
  facet_wrap( ~ mrace,ncol=5)
## different pattern for race group 4, there is an interaction
# weight and med by mrace
ggplot(babies_data,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Mother Education by Mother Race",x="Mother Education",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ mrace,ncol=5)
## different trends, there is an interaction
# weight and inc by mrace
ggplot(babies_data,aes(x=inc, y=bwt.oz, fill=inc)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Mother Income by Mother Race",x="Mother Income",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ mrace,ncol=5)
## different trends, there is an interaction
# weight and smoke by mrace
ggplot(babies_data,aes(x=smoke, y=bwt.oz, fill=smoke)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Smoke by Mother Race",x="Smoke",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ mrace,ncol=5)
## NO interaction


# weight and parity by smoke
ggplot(babies_data,aes(x=parity, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Parity by Smoke",x="Parity",y="Body Weight") +
  facet_wrap( ~ smoke,ncol=2)
## looks ok, same pattern
# weight and mage by smoke
ggplot(babies_data,aes(x=mage, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Age by Smoke",x="Mother Age",y="Body Weight") +
  facet_wrap( ~ smoke,ncol=2)
## looks ok, same pattern
# weight and mht by smoke
ggplot(babies_data,aes(x=mht, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Height by Smoke",x="Mother Height",y="Body Weight") +
  facet_wrap( ~ smoke,ncol=2)
## looks ok, same pattern
# weight and mpregwt by smoke
ggplot(babies_data,aes(x=mpregwt, y=bwt.oz)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Body Weight vs Mother Pre-Praganancy Weight by Smoke",x="Mother Pre-Praganancy Weight",y="Body Weight") +
  facet_wrap( ~ smoke,ncol=2)
## looks ok, same pattern
# weight and mrace by smoke
ggplot(babies_data,aes(x=mrace, y=bwt.oz, fill=mrace)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Mother Race by Smoke",x="Mother Race",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2)
## there is an interaction
# weight and med by smoke
ggplot(babies_data,aes(x=med, y=bwt.oz, fill=med)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Mother Education by Smoke",x="Mother Education",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2)
## there is NO interaction
# weight and income by smoke
ggplot(babies_data,aes(x=inc, y=bwt.oz, fill=inc)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Body Weight vs Mother Income by Smoke",x="Mother Income",y="Body Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2)
## there is an interaction

# center mage, mht, mpregwt
babies_data$magec <- babies_data$mage - mean(babies_data$mage)
babies_data$mhtc <- babies_data$mht - mean(babies_data$mht)
babies_data$mpregwtc <- babies_data$mpregwt - mean(babies_data$mpregwt)
# fit a linear model using the original variables
babies_model <- lm(bwt.oz ~ parity + mrace + magec + med + mhtc + mpregwtc + inc + smoke, data = babies_data)
summary(babies_model)
# assess normality
plot(babies_model, which = 2, col = c('blue4')) ##  normality assumption holds
# assess linearity
ggplot(babies_data,aes(x=parity, y=babies_model$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Parity",x="Parity",y="Residuals") ## need a quadratic term
ggplot(babies_data,aes(x=magec, y=babies_model$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Mother Age (Centered)",x="Mother Age (Centered)",y="Residuals") ## no pattern
ggplot(babies_data,aes(x=mhtc, y=babies_model$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Mother Height (Centered)",x="Mother Height (Centered)",y="Residuals") ## need a log term
ggplot(babies_data,aes(x=mpregwtc, y=babies_model$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Mother Prep-pregnancy Weight (Centered)",x="Mother Prep-pregnancy Weight (Centered)",y="Residuals") ## no pattern
# access independence and equal variance
plot(babies_model, which = 1, col = c('blue4')) ## no obvious patterns and the points look equally spread out
## The regression assumptions are met. The model summary shows that some predictors are statistically significant. Namely, they are mrace, mhtc, mpregwtc and smoke. Even though other predictor variables don't show any statistical significance, it could that the linear relationship between each one of these variable is not strong enough to be detected by this sample. Next, I will perform model selection, with null_model only capturing the two predictors that we or our client care about - mrace and smoke. And the full_model will include all main effects as well as the interactions that we deemed important through visual inspection. If the any variables that were previously determined to be statistically significant are removed thorugh the model selection process, we will perform F-test on these terms to evaluate their significance.


# model selection using forward selection selection, with AIC as the criterion
null_model <- lm(bwt.oz ~ mrace + smoke, data = babies_data)
full_model <- lm(bwt.oz ~ parity + mrace + magec + med + mhtc + mpregwtc + inc + smoke + parity*mrace + magec*mrace + mhtc*mrace + mpregwtc*mrace + med*mrace + inc*mrace + mrace*smoke + inc*smoke, data = babies_data)
model_forward <- step(null_model, scope = formula(full_model), direction = 'forward', trace = 0)
model_forward$call
# repeat the process using stepwise selection
model_stepwise <- step(null_model, scope = formula(full_model),direction="both", trace=0)
model_stepwise$call
## both model shows the same result
# check model assumption of the produced model
model_select = lm(formula = bwt.oz ~ mrace + smoke + mpregwtc + mhtc + parity + mrace*mhtc, data = babies_data)
plot(model_select, which = 2, col = c('blue4')) ## normality assumption holds
ggplot(babies_data,aes(x=mpregwtc, y=model_select$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Mother Prep-pregnancy Weight (Centered)",x="Mother Prep-pregnancy Weight (Centered)",y="Residuals")## no pattern
ggplot(babies_data,aes(x=mhtc, y=model_select$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Mother Height (Centered)",x="Mother Height (Centered)",y="Residuals")## no pattern
ggplot(babies_data,aes(x=parity, y=model_select$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Parity",x="Parity",y="Residuals")## no pattern
plot(model_select, which = 1, col = c('blue4'))
## all model assumptions are valid

# check if parity important
model_select_2 = lm(formula = bwt.oz ~ mrace + smoke + mpregwtc + mhtc + mrace*mhtc, data = babies_data)
anova(model_select_2, model_select)
## the p-value is 0.0725, which is greater than 0.05, fail to reject null hypothesis, keep model_select

# based on EDA, check if interactions with smoke are significant
model_select_3 = lm(formula = bwt.oz ~ mrace + smoke + mpregwtc + mhtc + mrace*mhtc + mrace*smoke + inc*smoke, data = babies_data)
anova(model_select_3, model_select)
## the p-value is 0.5584, which is greater than 0.05, fail to reject null hypothesis, keep model_select
# repeat the same process for interactions with mrace
model_select_4 = lm(formula = bwt.oz ~ mrace + smoke + mpregwtc + mhtc + parity + mrace*mhtc + parity*mrace + magec*mrace + mpregwtc*mrace + med*mrace + inc*mrace, data = babies_data)
anova(model_select_4, model_select)
## the p-value is 0.608, which is greater than 0.05, fail to reject null hypothesis, keep model_select
## so the final model would contain 6 predictors, which are mrace, smoke, mpregwtc, mhtc, parity and an interaction term mrace*mhtc. We can check the model summary
summary(model_select)
## Most variables are significant at 0.05, parity is significant at 0.1

# check for outliers, leverage point and influence points
plot(model_select, which = 5, col = c('blue4'))
## there are lots of outliers in the sample. There is also a high leverage point labeled 510, but its cook's distance is below 0.5, so it will be kept.
# check multicolinearity
vif(model_select)
## the result shows there is light to moderate correlation, and it's not worth noting