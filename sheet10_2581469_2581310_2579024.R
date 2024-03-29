##########################
#Week 11: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 

## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

######################################################################################################################

library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(reshape2)


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.




#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.

dat = read.csv('SpeedDatingData.csv')
head(dat)

model <- glm(dec ~ attr + sinc + intel + fun, family = binomial, data = dat)
summary(model)

# odds == p(outcome) / 1 - p(outcome)
# ln(odds) == ln(P(outcome)/1-p(outcome))
# to reverse, take e^ln(odds)

# interpretation, the slope coefficient is in the ln scale, so a positive slope increases the ln(odds) of the response
# variable 
# in this model, intercept is non sensical, so no need to interprete

# we use logistic regression because the response variable is binary, only 2 possible outcomes;
# linear regression doesn't make much sense


# attraction
mdat <- melt(dat, id.vars = c("attr"),
             measure.vars = c("dec"))
cdat <- dcast(mdat, attr ~ variable, mean, na.rm = T)
ggplot(cdat, aes(attr, dec)) + geom_point() + geom_smooth(method = 'glm', se = FALSE)

# sincerity
mdat_s <- melt(dat, id.vars = c("sinc"),
             measure.vars = c("dec"))
cdat_s <- dcast(mdat_s, sinc ~ variable, mean, na.rm = T)
ggplot(cdat_s, aes(sinc, dec)) + geom_point() + geom_smooth(method = 'glm', se = FALSE)

# intelligence
mdat_i <- melt(dat, id.vars = c("intel"),
               measure.vars = c("dec"))
cdat_i <- dcast(mdat_i, intel ~ variable, mean, na.rm = T)
ggplot(cdat_i, aes(intel, dec)) + geom_point() + geom_smooth(method = 'glm', se = FALSE)

# fun
mdat_f <- melt(dat, id.vars = c("fun"),
               measure.vars = c("dec"))
cdat_f <- dcast(mdat_f, fun ~ variable, mean, na.rm = T)
ggplot(cdat_f, aes(fun, dec)) + geom_point() + geom_smooth(method = 'glm', se = FALSE)

# According to the linear model, only attractiveness, sincerity, and fun predictor variables are deemed to be 
# significiant. Only attraction and fun are positively correlated with the decision for a follow-up date. Sincerity,
# on the contrary, is negatively correlated with the decision response variable.
# This suggests that, all else being equal, an increase in attraction rating by 1 point increases the expected 
# chance of a decision to follow-up date by 0.5623 unit points while an increase in fun by 1 point increases the 
# likelihood of a decision to date by 0.33711.

#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

# Does not converge
# adding a random intercept per participant (iid is the rater) rating
# one person might have a tendency to say yes/no in general regardless of predictor
model_mixed <- glmer(dec ~ attr + sinc + intel + fun + (1|iid), 
                     family = 'binomial', data = dat)
summary(model_mixed)
# Converges
model_mixed_c <- glmer(dec ~ attr + sinc + intel + fun + (1|id) + (1|iid), 
                   family = 'binomial', data = dat)
summary(model_mixed_c)

# pid is the person being rated
model_mixed_rated <- glmer(dec ~ attr + sinc + intel + fun + (1|iid) + (1|pid), 
                           family = 'binomial', data = dat)
summary(model_mixed_rated)

# when comparing 2 models, if one converges and the other doesn't, pick the one that converges, even if AIC
# score is about the same
# usually a model that converges will likely have a lower AIC than the one that doesn't

# In the general mixed effect model, all predictors are considered significant factors affecting
# decision to follow-up date. The slopes of these variables change quite a bit, but the attraction variable
# still has the highest slope (0.9), followed by fun (0.6). For intelligence and sincerity an increase in 
# intelligence and sincerity rating by 1 point increases the decision-to-date by 0.17 and 0.1 units respectively.

# Curiously, the model doesn't converge with id (which should be redundant knowing iid) not included.

# The AIC for this model of 6700 is lower than that of the non-mixed effect model (8328).

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.

model_3 <- glmer(dec ~ attr + sinc + intel + fun + (1|id) + (1|iid) + (1|pid), 
                     family = 'binomial', data = dat)
summary(model_3)

# There is an effect of pid, but only a small one (0.27).

# The AIC for this model of 6665 is again lower, but not as much.

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)


# random effects:

# random intercept: (1|subject) --> 1 always refers to intercept, imagine each subject having their own regression
# line with their own intercept, but all have the same slope

# random intercept & slope: (1 + fixed variable|random effect variable); each subject has their own regression line
# with different slope and intercept
# this is the most used model unless you have reasons not to

# random slope: (fixed effect|subject or another random effect variable) ; each subject has the same intercept
# for their regression line, but different slope.

# no correlation between random intercept and slope: (1 + fixed ||random effect) this is rarely used

# treating intercepts and slopes separately by random factor; does not converge
model_mixed_2 <- glmer(dec ~ attr + sinc + intel + fun + amb + (attr||iid) + (fun||iid) + (intel||iid) + (amb||iid), 
                       family = 'binomial', data = dat)
summary(model_mixed_2)

# considering only attraction as a fixed effect; converged
model_mixed_5 <- glmer(dec ~ attr + (attr||id) + (fun||id), family = 'binomial', data = dat)
summary(model_mixed_5)

# considering attraction and fun as fixed effects; converged
model_mixed_4 <- glmer(dec ~ attr + fun + (attr||id) + (attr||iid), family = 'binomial', data = dat)
summary(model_mixed_4)

# treating intercepts and slopes together by random factor; does not converge
model_mixed_3 <- glmer(dec ~ attr + sinc + intel + fun + amb + (1 + attr|id) + (1 + fun|id) + (1 + intel|id) + 
                         (1 + amb|id), family = 'binomial', data = dat)
summary(model_mixed_3)

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?

# In the above models, the attraction variable still has the highest slope. When considering the intercepts and slopes together
# in model_mixed_3, the only variables that are signficant are attraction, fun, sincerity, and ambition, although ambition and sincerity 
# are negatively correlated with decision. 
# However, the models that did not converge (mixed_2 - mixed_3) both had higher AIC scores (7894 and 7849) than 
# the classical model in the previous section.

# When considering only attractio and fun as predictor variables and participant and Wave as random effects,
# we were able to get the model to converge in model_mixed_4. This model's AIC is 6831, still not as good
# as the classical model in the previous section. According to this model, for each unit increase in
# attraction rating, the expected likelihood of a decision to follow-up date increases by 0.92150 units. For each
# unit increase in fun rating, the expected likelihood of a decision to follow-up date increases by 0.6266 units.


####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.


# response variable is numerical (number of awards) or count of awards (starting at 0 to inf) discrete
# no negative values possible. thus, you cannot use linear regression (continuous value response variable)
# if unsure, it also helps to plot a histogram/density plot of your data to see what type of distribution it
# resembles

# model families: linear (response variable continuous and non-binary)
# assumptions for linear regression: normally distributed  residuals, homoschedasticity (homogeneity of variances)
# relationship between predictor and response variables has to be LINEAR

# logistic (binary response variable)
# poisson (response variable is discrete, e.g. count)

# you use regression because you want to PREDICT outcomes in relation to a data point you do not have
# an extrapolation based on a model from existing to predict an outcome from unknown data

p = read.csv("poisson_sim.csv", sep=";")

#p <- within(p, {
#  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
#  id <- factor(id)
#})

summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.


library(ggplot2)
plot <- ggplot(p, aes(x = math, y = num_awards, color = prog)) + geom_point()
plot + geom_smooth(method = 'glm', se = FALSE)

#(7) Run a generalized linear model to test for significance of effects.

mod <- glm(num_awards ~ math + prog, data = p, family = 'poisson')
summary(mod)

#(8) Do model comparisons do find out whether the predictors significantly improve model fit.

mod_null <- glm(num_awards ~ math, data = p, family = 'poisson')
summary(mod_null)

mod_inter <- glm(num_awards ~ math * prog, data = p, family = 'poisson')
summary(mod_inter)

mod_mixed <- glmer(num_awards ~ math*prog + (1 + math||ï..id), data = p, family = 'poisson')
summary(mod_mixed)

AIC(mod, mod_null, mod_inter, mod_mixed)

# It appears that the type of program predictor variable improves the model fit as it has the lowest AIC score among
# all the models. However, the difference in AIC scores among all the 4 models are < 10, which does not seem to
# be a huge difference.

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.

mod_gaussian <- glm(num_awards ~ math + prog, data = p, family = 'gaussian')
summary(mod_gaussian)
AIC(mod, mod_gaussian)
# clearly the non-gaussian model has a signficantly better fit as the AIC score is almost 200 points lower than
# the gaussian model.

