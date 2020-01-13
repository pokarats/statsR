### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Monday, January 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are required to work together in groups of three students, but everybody 
## needs to submit the group version of the homework via moodle individually.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Noon Pokaratsiri Goldstein, Pauline Sander, Axel Allen
## Matriculation number: 2581469, 2581310, 2579024

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)

# a)There are 3 datasets on moodle, you can choose one of them to work with on this
#   assignment.  
#   Read in the data file of your choice (gender.Rdata, sem.Rdata OR relclause.Rdata) 
#   and assign it to a variable called "dat". 
#   See a description of the items in the datasets below.
dat <- read.delim('gender.txt', fileEncoding="UTF-8", sep = ' ')
summary(dat)
head(dat)
# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German'):

# Christina schie?t / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "shoot", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consisten or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

# b) Inspect "dat" and provide 2 plots. 
#    The first plot should provide insights about the relationship between WORD_TIME 
#    and ITEM_TYPE. 
#    For the second plot you should first subset the data using only RELWDINDEX == 0 and
#    then plot the WORD_TIME for the different conditions (ITEM_TYPE).
library(ggplot2)
library(reshape2)

# plot of average by item_type per participant
mdat <- melt(dat, id.vars = c("PARTICIPANT", "ITEM_TYPE"),
             measure.vars = c("WORD_TIME"))
cdat <- dcast(mdat, ITEM_TYPE + PARTICIPANT ~ variable, mean, na.rm = T)
ggplot(cdat, aes(ITEM_TYPE, WORD_TIME, group = ITEM_TYPE)) + 
  geom_boxplot() +
  ggtitle('Plot of average WORD_TIME per PARTICIPANT by ITEM_TYPE')

# first plot for relationship between WORD_TIME and ITEM_TYPE
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data = dat, aes(WORD_TIME, color = ITEM_TYPE, fill=ITEM_TYPE)) +
  ggtitle('Plot of WORD_TIME Distribution by ITEM_TYPE') +
  geom_density(alpha = 0.6) + 
  facet_wrap(~ITEM_TYPE)

ggplot(dat, aes(ITEM_TYPE, WORD_TIME, group = ITEM_TYPE)) + 
  geom_boxplot() +
  ggtitle('Boxplot of WORD_TIME by ITEM_TYPE')

# second plot for RELWDINDEX == 0
dat2 <- subset(dat, RELWDINDEX == 0)
ggplot(dat2, aes(ITEM_TYPE, WORD_TIME, group = ITEM_TYPE)) + 
  geom_boxplot() +
  ggtitle('Boxplot of WORD_TIME by ITEM_TYPE at Critical Word Index')

# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation). 
#    Note that we are evaluating WORD_TIME as our reponse variable. 
#    What time intervals make sense for such an experiment?

# Considering the length of the sample sentences described, I'm assuming that WORD_TIME unit is in milliseconds
# as 1000-6000 ms (1-6 seconds) seems reasonable for proficient speakers to read these sentences.
# From our density plot and the summary of the data, ~90% of the WORD_TIME distribution lie below 2000 ms.
# Almost all of the data points for WORD_TIME at RELWDINDEX == 0 are also under 3000ms as observed in our boxplot.
# Considering that it is reasonable to expect readers to take more time to read 'grammatically bad' sentences
# excluding the outlier data points > 2000 ms, which are mostly in the 'grammatically good' category, seems
# reasonable. The boxplot of the average WORD_TIME per PARTICIPANT by ITEM_TYPE supports this rationale as well.
# As observed, in contrast to the Boxplot of WORD_TIME by ITEM_TYPE at Critical Word Index, which shows the 'GB'
# group to have overall longer WORD_TIME, the average WORD_TIME in the per PARTICIPANT bloxplot is higher in 
# the 'GG' group than 'GB' group.

dat_excluded2 <- subset(dat, WORD_TIME <= 2000)
ggplot(dat_excluded2, aes(ITEM_TYPE, WORD_TIME, group = ITEM_TYPE)) + 
  geom_boxplot() +
  ggtitle('Boxplot of WORD_TIME by ITEM_TYPE')

ggplot(data = dat_excluded2, aes(WORD_TIME, color = ITEM_TYPE, fill=ITEM_TYPE)) +
  ggtitle('Plot of WORD_TIME Distribution by ITEM_TYPE') +
  geom_density(alpha = 0.6) + 
  facet_wrap(~ITEM_TYPE)

# d) Make a scatter plot where for each index word as the sentence progresses (RELWDINDEX),
#    the average reading time is shown for each of the two conditions (ITEM_TYPE).
#    Please use two different colours for the different conditions.
ggplot(data = dat_excluded2, aes(x = RELWDINDEX, y = WORD_TIME, col = ITEM_TYPE)) +
  geom_point(alpha = 0.8, shape = 16) +
  ggtitle('Scatter Plot of Reading time(ms) and Word Index between GG and GB groups')

# e) You do not need to use ggplot here, just follow the example below.
#    The code is a plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?
summary(dat_excluded2)
?xyplot
# melting and recasting for average word_time
mdat_e <- melt(dat_excluded2, id.vars = c("PARTICIPANT", "RELWDINDEX", "ITEM_TYPE"),
             measure.vars = c("WORD_TIME"))
cdat_e <- dcast(mdat_e,RELWDINDEX + PARTICIPANT + ITEM_TYPE ~ variable, mean, na.rm = T)

print(xyplot(WORD_TIME ~ RELWDINDEX | PARTICIPANT, cdat_e, aspect = 'fill',
             main = 'Relationship between ART and Critical Word',
             groups = ITEM_TYPE, auto.key = list(space = 'right'),
             par.settings = list(superpose.symbol = list(col = c("blue", "orange")), 
                                 superpose.line = list(col = c("blue", "orange"))),
             layout = c(8,3), type = c('g', 'p', 'r'), 
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = 'Index Word as the Sentence Progresses',
             ylab = 'ART (ms) by Sentence Type: GG vs GB'))

# Observation from the plot:
# Reading time tends to increase after the critical word (i.e. word at index 0), more so for the GB group than
# the GG group. However, the general tendency is not so clear (as it is the case for the sleep study example)

# f) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).

dat_model = lmer(WORD_TIME ~ RELWDINDEX + ITEM_TYPE + (1 + RELWDINDEX | PARTICIPANT), cdat_e, REML=FALSE)
dat_model_null = lmer(WORD_TIME ~ RELWDINDEX + (1 + RELWDINDEX | PARTICIPANT), cdat_e, REML=FALSE)
summary(dat_model)

# The average WORD_TIME in the GB group is approximately 621.2 ms (fixed effects intercept) and 
# the GG group WORD_TIME is around 17 ms longer. For each unit increase in RELWDINDEX, the estimated WORD_TIME
# also increases by 3 ms.
# The std. Dev of 142 in the participant random effects suggest that there's quite a large variability among
# participants in terms of WORD_TIME.

coef(dat_model)
# The slopes for the RELWDINDEX that change from positive to negative in different participants suggest that
# the relationship between WORD_TIME and RELWDINDEX is different across test participants and that the effect
# is not consistant. In 7 participants, WORD_TIME decreases as RELWDINDEX increases; in others, the relationship
# is reversed. 

anova(dat_model_null, dat_model)
# Based on the Chi-sqaured statistic of 4.5381 and p-value of 0.1125, the effect of ITEM_TYPE on WORD_TIME
# is not statistically significant.

# g) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.
print(dotplot(ranef(dat_model, condVar=TRUE), scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])
# Based on the plot, it appears that there is variability in slopes between participants.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])
