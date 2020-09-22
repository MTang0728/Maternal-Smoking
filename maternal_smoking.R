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


######### Smoking Vs. Birth Weight #########
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



######### Smoking Vs. Pre-term Birth #########
## perform EDA
# check if data is imbalanced
table(babies_data$Premature_fac) ## a little bit imbalanced
## on continuous variables
p1 <- ggplot(babies_data,aes(x=Premature_fac, y=parity, fill=Premature_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Parity vs Born Premature",
       x="Born Premature",y="Parity") + 
  theme_classic() + theme(legend.position="none") ## similar median and distribution, maybe not an significant predictor

p2 <- ggplot(babies_data,aes(x=Premature_fac, y=mage, fill=Premature_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Maternal Age vs Born Premature",
       x="Born Premature",y="Maternal Age") + 
  theme_classic() + theme(legend.position="none") ## median lower in Premature, Premature has higher distribution

p3 <- ggplot(babies_data,aes(x=Premature_fac, y=mht, fill=Premature_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Maternal Height vs Born Premature",
       x="Born Premature",y="Maternal Height") + 
  theme_classic() + theme(legend.position="none") ## similar median, Premature has lower distribution

p4 <- ggplot(babies_data,aes(x=Premature_fac, y=mpregwt, fill=Premature_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Maternal Pre-pragnancy Weight vs Born Premature",
       x="Born Premature",y="Maternal Pre-pragnancy Weight") + 
  theme_classic() + theme(legend.position="none") ## similar median, Premature has lower distribution
grid.arrange(p1, p2, p3, p4, ncol = 2)

## on categorical variables
apply(table(babies_data[,c("Premature_fac","mrace")])/sum(table(babies_data[,c("Premature_fac","mrace")])),
      2,function(x) x/sum(x)) ## ratios between premature and nonpremature vary a lot between each race

apply(table(babies_data[,c("Premature_fac","med")])/sum(table(babies_data[,c("Premature_fac","med")])),
      2,function(x) x/sum(x)) ## ratios between premature and nonpremature vary a lot between each education

apply(table(babies_data[,c("Premature_fac","inc")])/sum(table(babies_data[,c("Premature_fac","inc")])),
      2,function(x) x/sum(x)) ## ratios between premature and nonpremature doesn't vary as much between each income level

apply(table(babies_data[,c("Premature_fac","smoke")])/sum(table(babies_data[,c("Premature_fac","smoke")])),
      2,function(x) x/sum(x)) ## ratios between premature and nonpremature vary based on smoke


# center mage, mht, mpregwt
babies_data$magec <- babies_data$mage - mean(babies_data$mage)
babies_data$mhtc <- babies_data$mht - mean(babies_data$mht)
babies_data$mpregwtc <- babies_data$mpregwt - mean(babies_data$mpregwt)
# build a logistic model using the main effects
babies_model_1 <- glm(Premature ~ parity + mrace + magec + med + mhtc + mpregwtc + inc + smoke, data = babies_data, family = binomial)
summary(babies_model_1)

# assess independence and whole fit
#save the raw residuals
rawresid1 <- residuals(babies_model_1,"resp")
#binned residual plots
binnedplot(x=fitted(babies_model_1),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## all data inside the red line, no obvious patterns. Independence assumption is ok!

# check if inverse logit function fits parity
binnedplot(x=babies_data$parity,y=rawresid1,xlab="Parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy") 
## no patterns, there are not many points, and one point is outside of the red line. Might need to do something 
# check if inverse logit function fits parity
binnedplot(x=babies_data$magec,y=rawresid1,xlab="Maternal Age (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mhtc
binnedplot(x=babies_data$mhtc,y=rawresid1,xlab="Maternal Height (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mpregwtc
binnedplot(x=babies_data$mpregwtc,y=rawresid1,xlab="Maternal Pre-pregnancy Weight (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy") 
## no patterns, most points are within the red line 

## Model assessment shows that the regression assumptions are met, so there are much needs to transform the response variable as well as any one of the predictors. based on the model constructed using all main effects, we could tell that most variables are actually insignificant. The only variables that are significant are mrace4 (Maternal Race: Asian) and mpregwtc (Maternal Pre-pregnancy Weight) at level 0.1 and mrace4 (Maternal Race: black) at 0.05. Even though other predictor variables don't show any statistical significance, it could be that the linear relationship between each one of these variable and log odds of the response variable is not strong enough to be detected by this sample. 


## Model Validation
#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(babies_model_1) >= 0.5, "1","0")),
                            babies_data$Premature_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] ## sensitivity is really low

#first, let's repeat with the marginal percentage in the data
mean(babies_data$Premature)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(babies_model_1) >= mean(babies_data$Premature), "1","0")),
                            babies_data$Premature_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] ## accuracy a lot lower, but sensitivity and specificity are more even
#look at ROC curve
roc(babies_data$Premature,fitted(babies_model_1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

## Using the 'best' threshold value which is 0.189, we do get a better performance in sensitivity (0.620), and the AUC is 0.667. But there is still much to improve. One thing that is worth noting is that there aren't much data for higher values of Parity, and it makes more sense to convert it to categorical variables.


# convert parity to categorical variable
## based on a study, multiparae may have a higher risk of preterm birth. Therefore, we will collapse parity into 2 levels, with 0 being level 0, which indicates primiparae, and the rest as the 1. The new parity variable will be a factor type as well.
babies_data$parity_new <- rep(nrow(babies_data$parity))
# babies_data$parity_new[babies_data$parity <= 3] <- 0
# babies_data$parity_new[babies_data$parity > 3] <- 1
babies_data$parity_new[babies_data$parity <= 1] <- 0
babies_data$parity_new[babies_data$parity > 1] <- 1
# convert variable to factor
babies_data$parity_new <- factor(babies_data$parity_new, levels = c(0, 1), labels = c('0', '1'))

# build a logistic model using the updated main effects
babies_model_2 <- glm(Premature ~ parity_new + mrace + magec + med + mhtc + mpregwtc + inc + smoke, data = babies_data, family = binomial)
summary(babies_model_2)
# assess independence and whole fit
#save the raw residuals
rawresid2 <- residuals(babies_model_2,"resp")
#binned residual plots
binnedplot(x=fitted(babies_model_2),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## one data point outside the red line, no obvious patterns. Independence assumption is ok!
# check if inverse logit function fits parity
binnedplot(x=babies_data$magec,y=rawresid2,xlab="Maternal Age (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mhtc
binnedplot(x=babies_data$mhtc,y=rawresid2,xlab="Maternal Height (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mpregwtc
binnedplot(x=babies_data$mpregwtc,y=rawresid2,xlab="Maternal Pre-pregnancy Weight (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy") 
## no patterns, most points are within the red line 
#look at ROC curve
roc(babies_data$Premature,fitted(babies_model_2),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
## Based on the binned residual plots, the new model seems to satisfies the assumptions reasonably well, except that there 2 now 2 points outside of the red line in the predicted possibilities plot. The new model summary indicates that smoke is now also statistically significant on top of the 3 significant variables previously observed. Also note that the p-value for parity has already decreased considerably after converting it to a factor variable with just 2 levels. Last but not the least, for ROC vurve of the updated model, the new 'best' threshold is 0.149, which produced a sensitivity-specificity combination of (0.428, 0.829), and an AUC of 0.672. The overall AUC has increased by a little bit. Next, we will explore interactions to check if there are any interesting insights. 


## investigate interactions between categorical and continuous predictors
# #premature and parity by smoke
# ggplot(babies_data,aes(x=parity, y=Premature_fac, fill=parity)) +
#   geom_boxplot() + coord_flip() +
#   scale_fill_brewer(palette="Blues") +
#   labs(title="Born Premature vs Parity by Smoke",x="Parity",y="Born Premature") + 
#   theme_classic() + theme(legend.position="none") +
#   facet_wrap( ~ smoke,ncol=2) ## different trend, could be some interactions

# premature and mage by smoke
g1 <- ggplot(babies_data,aes(x=magec, y=Premature_fac, fill=magec)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Born Premature vs Maternal Age (Centerd) by Smoke",x="Maternal Age (Centerd)",y="Born Premature") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2) ## different trend, could be some interactions
# premature and mht by smoke
g2 <- ggplot(babies_data,aes(x=mhtc, y=Premature_fac, fill=mhtc)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Born Premature vs Maternal Height (Centerd) by Smoke",x="Maternal Height (Centerd)",y="Born Premature") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2) ## different trend, could be some interactions
# premature and mpregwt by smoke
g3 <- ggplot(babies_data,aes(x=mpregwtc, y=Premature_fac, fill=mpregwtc)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Born Premature vs Maternal Pre-pragnancy Weight (Centerd) by Smoke",x="Maternal Pre-pragnancy Weight (Centerd)",y="Born Premature") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke,ncol=2) ## different trend, could be some interactions
grid.arrange(g1, g2, g3, ncol = 1)

## investigate interactions between categorical and categorical predictors
# premature and parity_new by smoke
apply(table(babies_data[babies_data$smoke == 0,c("Premature_fac","parity_new")])/sum(table(babies_data[babies_data$smoke == 0,c("Premature_fac","parity_new")])),
      2,function(x) x/sum(x))
apply(table(babies_data[babies_data$smoke == 1,c("Premature_fac","parity_new")])/sum(table(babies_data[babies_data$smoke == 1,c("Premature_fac","parity_new")])),
      2,function(x) x/sum(x)) ## no interaction
# premature and mrace by smoke
apply(table(babies_data[babies_data$smoke == 0,c("Premature_fac","mrace")])/sum(table(babies_data[babies_data$smoke == 0,c("Premature_fac","mrace")])),
      2,function(x) x/sum(x))
apply(table(babies_data[babies_data$smoke == 1,c("Premature_fac","mrace")])/sum(table(babies_data[babies_data$smoke == 1,c("Premature_fac","mrace")])),
      2,function(x) x/sum(x)) ## no interaction
# premature and med by smoke
apply(table(babies_data[babies_data$smoke == 0,c("Premature_fac","med")])/sum(table(babies_data[babies_data$smoke == 0,c("Premature_fac","med")])),
      2,function(x) x/sum(x))
apply(table(babies_data[babies_data$smoke == 1,c("Premature_fac","med")])/sum(table(babies_data[babies_data$smoke == 1,c("Premature_fac","med")])),
      2,function(x) x/sum(x)) ## there is interaction for when education level is equal to 0
# premature and inc by smoke
apply(table(babies_data[babies_data$smoke == 0,c("Premature_fac","inc")])/sum(table(babies_data[babies_data$smoke == 0,c("Premature_fac","inc")])),
      2,function(x) x/sum(x))
apply(table(babies_data[babies_data$smoke == 1,c("Premature_fac","inc")])/sum(table(babies_data[babies_data$smoke == 1,c("Premature_fac","inc")])),
      2,function(x) x/sum(x)) ## might be some interaction

### Next, I will perform model selection, with null_model only capturing the two predictors that we or our client care about - smoke. And the full_model will include all main effects as well as the interactions that we deemed important through visual inspection. If any variables that were previously determined to be statistically significant are removed through the model selection process, we will perform F-test on these terms to evaluate their significance.

## model selection using forward selection selection, with AIC as the criterion
null_model <- glm(Premature ~ smoke + mrace*smoke, data = babies_data, family = binomial)
full_model <- glm(Premature ~ parity_new + mrace + med + inc + smoke + magec + mhtc + mpregwtc + magec*smoke + mhtc*smoke+ mpregwtc*smoke + mrace*smoke + med*smoke + inc*smoke, data = babies_data, family = binomial)
model_forward <- step(null_model, scope = formula(full_model), direction = 'forward', trace = 0)
model_forward$call
# repeat the process using stepwise selection
model_stepwise <- step(null_model, scope = formula(full_model), direction="both", trace=0)
model_stepwise$call ## same result as forward selection
# repeat the process using backward selection
model_backward <- step(full_model, direction ="backward", trace=0)
model_backward$call ## backward selection gave a different result

# use F-test to check if difference between forward and stepwise selection are significant
babies_model_f = glm(Premature ~ smoke + mrace + mpregwtc + med + smoke:mrace, 
                     family = binomial, data = babies_data)
babies_model_s = glm(Premature ~ smoke + mrace + mpregwtc + med, 
                     family = binomial, data = babies_data)
anova(babies_model_f, babies_model_s, test = 'Chisq')
## since the p-value of F-test is 0.2659, which is greater than 0.1. Thus we fail to reject the null hypothesis, and will keep the result of stepwise selection.

babies_model_b = glm(Premature ~ mrace + med + smoke + mpregwtc + smoke:mhtc, 
                     family = binomial, data = babies_data)
anova(babies_model_s, babies_model_b, test = 'Chisq')
## since the p-value of F-test is 0.1544, which is greater than 0.1. Thus we fail to reject the null hypothesis, and will keep the result of stepwise selection.

### based on EDA, we concluded that several terms might be significant, which are magec and mhtc. However, mhtc has been negated by our F-test. In the similar fashion, we also thought some interaction terms might be significant, these include interactions between smoke and magec, mhtc, mpregwtc, mrace as well as inc, where the interaction between smoke and mhtc as well as smoke and mrace were negated by our F-test. In the next step, we will test if any one of the remaining terms could be statistically significant

# create a model that includes magec and perform F-test
babies_model_5 = glm(formula = Premature ~ smoke + mrace + med + mpregwtc + magec, 
                     family = binomial, data = babies_data)
anova(babies_model_s, babies_model_5, test = 'Chisq') ## the p-value of F-test is 0.4167, fail to reject the null hypothesis
# create a model that includes interactions with magec and mpregwtc
babies_model_6 = glm(formula = Premature ~ smoke + mrace + med + mpregwtc + smoke:magec + smoke:mpregwtc, 
                     family = binomial, data = babies_data)
anova(babies_model_s, babies_model_6, test = 'Chisq') ## the p-value of F-test is 0.7594, fail to reject the null hypothesis
# create a model that includes interactions with and inc
babies_model_7 = glm(formula = Premature ~ smoke + mrace + med + mpregwtc + smoke:med + smoke:inc, 
                     family = binomial, data = babies_data)
anova(babies_model_s, babies_model_7, test = 'Chisq') ## the p-value of F-test is 0.5593, fail to reject the null hypothesis

# perform model assessment and validation on babies_model_3
summary(babies_model_s)
# assess independence and whole fit
#save the raw residuals
rawresid3 <- residuals(babies_model_s,"resp")
#binned residual plots
binnedplot(x=fitted(babies_model_s),y=rawresid3,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## one data point outside the red line, no obvious patterns. Independence assumption is ok!
# check if inverse logit function fits parity
binnedplot(x=babies_data$magec,y=rawresid3,xlab="Maternal Age (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mhtc
binnedplot(x=babies_data$mhtc,y=rawresid3,xlab="Maternal Height (Centered)",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
## no patterns, pretty much all within the red line
# check if inverse logit function fits mpregwtc
binnedplot(x=babies_data$mpregwtc,y=rawresid3,xlab="Maternal Pre-pregnancy Weight",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy") 
## no patterns, most points are within the red line 

mean(babies_data$Premature)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(babies_model_s) >= mean(babies_data$Premature), "1","0")),
                            babies_data$Premature_fac,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#look at ROC curve
roc(babies_data$Premature,fitted(babies_model_s),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

# # check confidence interval
confint.default(babies_model_s)   #on log odds scale
exp(confint.default(babies_model_s))- 1   #on odds scale
# # check multicolinearity
vif(babies_model_s)