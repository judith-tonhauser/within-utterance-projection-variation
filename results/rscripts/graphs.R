# projection of C and BEL in Exp 2 of Scontras & Tonhauser 2025
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

# mean rating for BEL and CC by utterance and QUD, violin plot ----

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #327

# identify mean ratings
means.C.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.BEL.utt.qud = d %>%
  group_by(utterance, qud) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "BEL")
means.BEL.utt.qud

# bind the data
means.by.qud = rbind(means.C.utt.qud,means.BEL.utt.qud)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "think-neg" = "neg-think", 
                            "think-pos" = "pos-think", 
                            "know-neg" = "neg-know", 
                            "know-pos" = "pos-know"))
means.by.qud
table(means.by.qud$utterance)

# transform the data to long
d = d %>%
  gather(responseTo, rating, responseCC:responseMC) %>%
  mutate(responseTo = recode(responseTo, "responseCC" = "CC", "responseMC" = "BEL")) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(utterance = recode(utterance, "think-neg" = "neg-think", 
                            "think-pos" = "pos-think", 
                            "know-neg" = "neg-know", 
                            "know-pos" = "pos-know"))
  
nrow(d) #654

# order the utterances by mean projection strength of CC
tmp = d %>%
  filter(responseTo == "CC") %>%
  group_by(utterance) %>%
  summarize(Mean = mean(rating))
tmp

means.by.qud$utterance = factor(means.by.qud$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)
d$utterance = factor(d$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)

levels(means.by.qud$utterance)
str(means.by.qud$utterance)
levels(d$utterance)
str(d$utterance)

# library for nested facets
library(ggh4x)

ggplot(data=means.by.qud, aes(x=content, y=Mean)) +
  geom_violin(data = d, aes(x = responseTo, y = rating), fill = "gray90") +
  geom_point() +
  geom_errorbar(aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested(. ~ utterance + qud) +
  ylab("Mean inference rating") +
  xlab("Inference") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud.pdf",height=3,width=8)

