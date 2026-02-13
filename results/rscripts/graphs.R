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

# import data from repo
d <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/SuB29-Scontras-Tonhauser/refs/heads/main/results/main/main03/data/cd.csv")
summary(d)
nrow(d) #327

# Fig 4: mean rating for BEL and CC by utterance and QUD, violin plot ----

# identify mean ratings
means.C.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC? QUD", "nai" = "BEL? QUD")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.BEL.utt.qud = d %>%
  group_by(utterance, qud) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC? QUD", "nai" = "BEL? QUD")) %>%
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
  mutate(qud = recode(qud, "ai" = "CC? QUD", "nai" = "BEL? QUD")) %>%
  mutate(utterance = recode(utterance, "think-neg" = "neg-think", 
                            "think-pos" = "pos-think", 
                            "know-neg" = "neg-know", 
                            "know-pos" = "pos-know"))
  
nrow(d) #654

# order the utterances by mean projection strength of CC
# tmp = d %>%
#   filter(responseTo == "CC") %>%
#   group_by(utterance) %>%
#   summarize(Mean = mean(rating))
# tmp

# order the utterances by mean projection strength of BEL
tmp = d %>%
  filter(responseTo == "BEL") %>%
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
  geom_violin(data = d, aes(x = responseTo, y = rating, fill=responseTo)) +
  geom_point() +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  geom_errorbar(aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="none") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested(. ~ utterance + qud) +
  ylab("Mean inference rating") +
  xlab("Inference") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud.pdf",height=3,width=8)

# Additional figures ----

## By-participant ratings for neg-know ----

# import data from repo
d <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/SuB29-Scontras-Tonhauser/refs/heads/main/results/main/main03/data/cd.csv")
summary(d)
nrow(d) #327

nrow(d[d$utterance == "know-neg",]) #76
nrow(d[d$utterance == "know-pos",]) #80

ggplot(d[d$utterance == "know-neg",], aes(x=responseMC, y=responseCC)) +
  geom_point() +
  theme(legend.position="none") +
  theme(axis.text.y = element_text(size=10)) +
  ylab("CC") +
  xlab("BEL") +
  geom_hline(yintercept=.5, linetype="dashed", color = "red") +
  geom_vline(xintercept=.5, linetype="dashed", color = "red") +
  coord_fixed(ratio = 1) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_reverse(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1"))
ggsave("../graphs/neg-know-BEL-and-CC-ratings.pdf",height=3,width=3)

ggplot(d[d$utterance == "know-pos",], aes(x=responseMC, y=responseCC)) +
  geom_point() +
  theme(legend.position="none") +
  theme(axis.text.y = element_text(size=10)) +
  ylab("CC") +
  xlab("BEL") +
  geom_hline(yintercept=.5, linetype="dashed", color = "red") +
  geom_vline(xintercept=.5, linetype="dashed", color = "red") +
  coord_fixed(ratio = 1) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  scale_x_reverse(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1"))
ggsave("../graphs/pos-know-BEL-and-CC-ratings.pdf",height=3,width=3)



