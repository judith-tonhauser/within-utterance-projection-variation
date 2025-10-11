# projection of C and BEL in Exp 2 of Scontras & Tonhauser 2025
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(forcats)
library(emmeans)

# load helper functions
source('../../helpers.R')

# projection of BEL and CC in neg-know  ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# set reference levels
proj = d %>%
  filter(utterance == "know-neg") %>%
  mutate(qud = fct_relevel(qud, "CC?"), 
         responseTo = fct_relevel(responseTo, "CC"))
nrow(proj) #152

# linear model
m = lmer(rating ~ responseTo*qud + (1|content), data=proj)
summary(m)

# pairwise comparison for each predictor while holding the other one constant
pairs(emmeans(m, ~ responseTo*qud), simple = "each")

# qud = CC?:
#   contrast estimate     SE  df t.ratio p.value
# CC - BEL    0.279 0.0575 147   4.853  <.0001
# CC is more projective than BEL under CC? 

# qud = BEL?:
#   contrast estimate     SE  df t.ratio p.value
# CC - BEL    0.483 0.0575 147   8.406  <.0001
# CC is more projective than BEL under BEL?

# responseTo = CC:
#   contrast   estimate     SE  df t.ratio p.value
# CC? - BEL?  -0.1097 0.0589 147  -1.862  0.0646
# no support for QUD-sensitivity of projection of CC

# responseTo = BEL:
#   contrast   estimate     SE  df t.ratio p.value
# CC? - BEL?   0.0945 0.0589 147   1.603  0.1111
# no support for QUD-sensitivity of projection of BEL


# projection of BEL in neg-know and neg-think ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# set reference levels
proj = d %>%
  filter(utterance == "know-neg" | utterance == "think-neg") %>%
  filter(responseTo == "BEL") %>%
  mutate(qud = fct_relevel(qud, "CC?"), 
         utterance = fct_relevel(utterance, "think-neg"))
nrow(proj) #148

# linear model
m = lmer(rating ~ utterance*qud + (1|content), data=proj)
summary(m)

# utteranceknow-neg           0.21455    0.05002 144.00000   4.289 3.27e-05 ***
# qudBEL?                     0.05662    0.05237 144.00000   1.081 0.281377    
# utteranceknow-neg:qudBEL?  -0.15110    0.07286 144.00000  -2.074 0.039874 * 


# set reference levels
proj = d %>%
  filter(utterance == "know-neg" | utterance == "think-neg") %>%
  filter(responseTo == "BEL") %>%
  mutate(qud = fct_relevel(qud, "BEL?"), 
         utterance = fct_relevel(utterance, "think-neg"))
nrow(proj) #148

# linear model
m = lmer(rating ~ utterance*qud + (1|content), data=proj)
summary(m)

# utteranceknow-neg          0.06345    0.05298 144.00000   1.198   0.2330    
# qudCC?                    -0.05663    0.05237 144.00000  -1.081   0.2814    
# utteranceknow-neg:qudCC?   0.15110    0.07286 144.00000   2.074   0.0399 * 

# OR REPORT SIMPLE EFFECT ANALYSIS

# pairwise comparison for each predictor while holding the other one constant
pairs(emmeans(m, ~ utterance*qud), simple = "each")

# qud = CC?:
#   contrast                 estimate     SE  df t.ratio p.value
# (think-neg) - (know-neg)  -0.2146 0.0535 129  -4.011  0.0001
# BEL is more projective in neg-know than neg-think under CC? QUD

# qud = BEL?:
#   contrast                 estimate     SE  df t.ratio p.value
# (think-neg) - (know-neg)  -0.0635 0.0540 144  -1.175  0.2421
# no support for BEL being projective under BEL? QUD

# utterance = think-neg:
#   contrast   estimate     SE  df t.ratio p.value
# CC? - BEL?  -0.0566 0.0553 135  -1.024  0.3076
# no QUD-sensitivity for BEL under neg-think

# utterance = know-neg:
#   contrast   estimate     SE  df t.ratio p.value
# CC? - BEL?   0.0945 0.0520 143   1.816  0.0714
# no QUD-sensitivity for BEL under neg-think

# Is CC more projective than BEL in neg-utterances? ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# set reference levels
d = d %>%
  mutate(qud = fct_relevel(qud, "CC?"), 
         responseTo = fct_relevel(responseTo, "BEL"))
contrasts(d$qud) # CC? 0
contrasts(d$responseTo) # BEL 0

m = lmer(rating ~ responseTo*qud + (1|content), data=d[d$utterance == "know-neg",])
summary(m)
# warning singular fit because random effect is 0
# responseToCC           0.27895    0.05748 148.00000   4.853 3.05e-06 ***
# qudBEL?               -0.09447    0.05748 148.00000  -1.644   0.1024    
# responseToCC:qudBEL?   0.20421    0.08129 148.00000   2.512   0.0131 * 

means = with(d[d$utterance == "know-neg",],tapply(rating,list(responseTo,qud), mean, na.rm=T))
means

#      CC?      BEL?
#BEL 0.3410526 0.2465789
#CC  0.6200000 0.7297368
# the difference we care about is the vertical 

#rating = 0.34105 + 0.27895*responseTo + -0.09447*qud + 0.20421*responseTo*qud

# reference levels
#responseTo: BEL = 0
#qud: CC? = 0

# intercept: BEL 0 / CC? 0 
#rating = 0.34105 (estimated average for BEL under CC? QUD)

# responseToCC: CC 1 / CC? 0
# rating = 0.34105 + 0.27895*1 = .62 (estimated average for CC under CC? QUD, ***)
# so CC ratings are higher than BEL ratings under CC? QUD

# qudBEL?: BEL 0 / BEL? 1
#rating = 0.34105 + -0.09447*1 = .24 (estimated average for BEL under BEL? QUD, n.s.)
# so keep the null hypothesis that BEL ratings under the two QUDs don't differ

# responseToCC:qudBEL?: CC 1 / BEL? 1
# rating = 0.34105 + 0.27895*1 + -0.09447*1 + 0.20421*1 = .72 (estimated average for CC under BEL? QUD, *)
# BEL projects less under CC? than CC under BEL? (not useful)

# change reference levels
d = d %>%
  mutate(qud = fct_relevel(qud, "BEL?"), 
         responseTo = fct_relevel(responseTo, "BEL"))

m = lmer(rating ~ responseTo*qud + (1|content), data=d[d$utterance == "know-neg",])
summary(m)
# responseToCC          0.48316    0.05748 148.00000   8.406 3.25e-14 ***
# qudCC?                0.09447    0.05748 148.00000   1.644   0.1024    
# responseToCC:qudCC?  -0.20421    0.08129 148.00000  -2.512   0.0131 * 
# so under BEL? QUD, ratings for CC are higher than ratings for BEL
# (and again no effect of QUD for BEL ratings)

# Is BEL more projective in neg-know than neg-think? ----

##### comparison of BEL-ratings under BEL? QUD ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# limit the data to BEL? QUD, BEL ratings and the relevant utterances
d = d %>%
  filter(qud == "BEL?") %>%
  filter(responseTo == "BEL") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(d) #70

# set reference levels
d = d %>%
  mutate(utterance = fct_relevel(utterance, "think-neg"))
#view(d)

m = lmer(rating ~ utterance + (1+utterance|content), data=d)
summary(m)
# utteranceknow-neg  0.07003    0.06680  1.44030   1.048 0.438787
# n.s

##### comparison of BEL-ratings under CC? QUD ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# limit the data to CC? QUD, BEL ratings and the relevant utterances
d = d %>%
  filter(qud == "CC?") %>%
  filter(responseTo == "BEL") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(d) #78

# set reference levels
d = d %>%
  mutate(utterance = fct_relevel(utterance, "think-neg"))
#view(d)

m = lmer(rating ~ utterance + (1+utterance|content), data=d)
summary(m)
# utteranceknow-neg  0.21455    0.05066 76.00000   4.235 6.33e-05 ***
  
##### comparison of BEL-ratings aggregated over QUDs ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# limit the data to BEL ratings and the relevant utterances
d = d %>%
  filter(responseTo == "BEL") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(d) #148

# set reference levels
d = d %>%
  mutate(utterance = fct_relevel(utterance, "think-neg"))
#view(d)

m = lmer(rating ~ utterance + (1|content) + (1+utterance|qud), data=d)
summary(m)
#utteranceknow-neg  0.14135    0.05317 1.64581   2.659   0.1429  
 
# Bayesian analysis ----

library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(brms)
library(knitr)
library(emmeans)
library(lme4)
library(lmerTest)
library(padr)
library(performance)
library(MuMIn)
library(xtable)

##### comparison of BEL-ratings under BEL? QUD ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# target data is BEL ratings for think-neg and know-neg
t = d %>%
  filter(responseTo == "BEL") %>%
  filter(qud == "BEL?") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(t) #70

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(t$rating)
t$betarating = (t$rating*(nrow(t)-1) + .5)/nrow(t)
summary(t$betarating)

# set reference levels
t = t %>%
  mutate(utterance = fct_relevel(utterance, "think-neg")) 
levels(t$utterance)
#levels(t$qud)

# fit the model
prior = get_prior(betarating ~ utterance + (1|content),family = Beta(),data=t)
prior

betamodel = bf(betarating ~ utterance + (1+utterance|content),
               phi ~ utterance, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=t, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-BEL-under-BELqud.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-BEL-under-BELqud.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "utterance", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# hypothesis testing

#estimate
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Estimate #0.4250258
# posterior probability
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Post.Prob #0.7296071
# Bayes factor
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Evid.Ratio #2.698323

##### comparison of BEL-ratings under CC? QUD ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# target data is BEL ratings for think-neg and know-neg under CC? QUD
t = d %>%
  filter(responseTo == "BEL") %>%
  filter(qud == "CC?") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(t) #78

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(t$rating)
t$betarating = (t$rating*(nrow(t)-1) + .5)/nrow(t)
summary(t$betarating)

# set reference levels
t = t %>%
  mutate(utterance = fct_relevel(utterance, "think-neg")) 
levels(t$utterance)
#levels(t$qud)

# fit the model
prior = get_prior(betarating ~ utterance + (1|content),family = Beta(),data=t)
prior

betamodel = bf(betarating ~ utterance + (1+utterance|content),
               phi ~ utterance, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=t, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-BEL-under-CCqud.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-BEL-under-CCqud.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "utterance", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# hypothesis testing
m.b

#estimate
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Estimate #1.266503
# posterior probability
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Post.Prob #0.9330357
# Bayes factor
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Evid.Ratio #13.93333

##### comparison of BEL-ratings aggregated over the QUDs----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# target data is BEL ratings for think-neg and know-neg under CC? QUD
t = d %>%
  filter(responseTo == "BEL") %>%
  filter(utterance == "think-neg" | utterance == "know-neg")
nrow(t) #148

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(t$rating)
t$betarating = (t$rating*(nrow(t)-1) + .5)/nrow(t)
summary(t$betarating)

# set reference levels
t = t %>%
  mutate(utterance = fct_relevel(utterance, "think-neg")) 
levels(t$utterance)
#levels(t$qud)

# fit the model
prior = get_prior(betarating ~ utterance + (1|qud) + (1|content),family = Beta(),data=t)
prior

betamodel = bf(betarating ~ utterance + (1+utterance|qud) + (1+utterance|content),
               phi ~ utterance, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=t, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-BEL-aggregatedQUDs.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-BEL-aggregatedQUDs.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "utterance", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# hypothesis testing
m.b

#estimate
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Estimate #0.9448045
# posterior probability
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Post.Prob #0.7785714
# Bayes factor
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Evid.Ratio #3.516129
