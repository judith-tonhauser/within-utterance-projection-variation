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

# frequentist analyses ----

#### projection of BEL in neg-know and neg-think ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# filter to relevant utterances and set reference levels
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

#### projection of BEL and CC in neg-know by QUD  ----

# load datas
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

# Bayesian analyses ----

library(tidyverse)
library(tidybayes)
library(dplyr)
library(dichromat)
library(forcats)
library(brms)
library(emmeans)

#### projection of BEL in neg-know and neg-think (CC? QUD as reference level) ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(d$rating)
d$betarating = (d$rating*(nrow(d)-1) + .5)/nrow(d)
summary(d$betarating)

# filter to relevant utterances and set reference levels
proj = d %>%
  filter(utterance == "know-neg" | utterance == "think-neg") %>%
  filter(responseTo == "BEL") %>%
  mutate(qud = fct_relevel(qud, "CC?"), 
         utterance = fct_relevel(utterance, "think-neg"))
nrow(proj)

# fit the model
prior = get_prior(betarating ~ utterance*qud + (1|content),family = Beta(),data=proj)
prior

betamodel = bf(betarating ~ utterance*qud + (1|content),
               phi ~ utterance+qud, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=proj, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-BEL-in-neg-know-and-neg-think-CCqud.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-BEL-in-neg-know-and-neg-think-CCqud.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "utterance", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# hypothesis testing

#estimate
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Estimate #1.3
# posterior probability
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Post.Prob #1
# Bayes factor
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Evid.Ratio #Inf

#### projection of BEL in neg-know and neg-think (BEL? QUD as reference level) ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(d$rating)
d$betarating = (d$rating*(nrow(d)-1) + .5)/nrow(d)
summary(d$betarating)

# filter to relevant utterances and set reference levels
proj = d %>%
  filter(utterance == "know-neg" | utterance == "think-neg") %>%
  filter(responseTo == "BEL") %>%
  mutate(qud = fct_relevel(qud, "BEL?"), 
         utterance = fct_relevel(utterance, "think-neg"))
nrow(proj)

# fit the model
prior = get_prior(betarating ~ utterance*qud + (1|content),family = Beta(),data=proj)
prior

betamodel = bf(betarating ~ utterance*qud + (1|content),
               phi ~ utterance+qud, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=proj, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-BEL-in-neg-know-and-neg-think-BELqud.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-BEL-in-neg-know-and-neg-think-BELqud.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "utterance", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# hypothesis testing

#estimate
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Estimate #.2
# posterior probability
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Post.Prob #.8
# Bayes factor
hypothesis(m.b,"utteranceknowMneg > 0")$hypothesis$Evid.Ratio #3.9

#### projection of BEL and CC in neg-know by QUD  ----

# load datas
d = read_csv("../data/cd.csv")
nrow(d) #327

# transform the data
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?"))
nrow(d) #654

# because rating assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
summary(d$rating)
d$betarating = (d$rating*(nrow(d)-1) + .5)/nrow(d)
summary(d$betarating)

# filter to relevant utterance and set reference levels
proj = d %>%
  filter(utterance == "know-neg") %>%
  mutate(qud = fct_relevel(qud, "CC?"), 
         responseTo = fct_relevel(responseTo, "CC"))
nrow(proj) #152

# fit the model
prior = get_prior(betarating ~ responseTo*qud + (1|content),family = Beta(),data=proj)
prior

betamodel = bf(betarating ~ responseTo*qud + (1|content),
               phi ~ responseTo+qud, # beta distribution's precision 
               family = Beta())

m.b = brm(formula = betamodel,
          family=Beta(),
          data=proj, 
          cores = 4, iter = 8000, warmup = 1000,
          control = list(adapt_delta = .9999999,max_treedepth=20))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/projection-of-CC-and-BEL-in-neg-know.rds")

# read the model
m.b <- readRDS(file="../models/projection-of-CC-and-BEL-in-neg-know.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "responseTo", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# pairwise comparison for each predictor while holding the other one constant
pairs(emmeans(m.b, ~ responseTo*qud), simple = "each")

# $`simple contrasts for responseTo`
# qud = CC?:
#   contrast estimate lower.HPD upper.HPD
# CC - BEL     1.37     0.809      1.93
# HPD does not include 0

# qud = BEL?:
#   contrast estimate lower.HPD upper.HPD
# CC - BEL     2.40     1.784      3.05
# HPD does not include 0

# $`simple contrasts for qud`
# responseTo = CC:
#   contrast   estimate lower.HPD upper.HPD
# CC? - BEL?   -0.507   -1.0642    0.0605
# HPD includes 0 

# responseTo = BEL:
#   contrast   estimate lower.HPD upper.HPD
# CC? - BEL?    0.527   -0.0394    1.1186
# HPD includes 0

# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 