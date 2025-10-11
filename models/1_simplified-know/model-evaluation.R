# 1_simplified-know

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)
library(ggh4x) # nested facets

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

source('../../helpers.R')

# read in the  model 
model <- read_file("model.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

# define the utterances
utterances = c("pos-know-pos-dance", 
               "pos-think-pos-dance",
               "neg-know-pos-dance", 
               "neg-think-pos-dance",
               "pos-bare-pos-dance",
               "neg-bare-pos-dance")
utterances

# define the qudBias (from context)
qudBias = c("BEL?", "CC?")
qudBias

#define the quds
#quds = c("mcQUD","ccQUD")
#quds

# define the world states
states = c("{BEL: 1, CC: 1}",
           "{BEL: 0, CC: 1}",
           "{BEL: 1, CC: 0}",
           "{BEL: 0, CC: 0}")
states

# define the plausible private assumptions
cgs = c("[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}, {BEL: 1, CC: 0}, {BEL: 0, CC: 0}]",
        "[{BEL: 1, CC: 1}, {BEL: 1, CC: 0}]", # BEL:1
        "[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}]", # CC:1
        "[{BEL: 0, CC: 1}, {BEL: 0, CC: 0}]", # BEL:0
        "[{BEL: 1, CC: 0}, {BEL: 0, CC: 0}]" # CC:0
)
cgs

# run the speaker ----

# input to speaker: state, cg, qud
# we only input those state/cg combinations that are compatible with one another

# create data frame of all possible state/cg combinations
tmp = data.frame(state = character(), cg = character())
tmp

#produce all combinations of s and cg
for (s in states) {
  for (cg in cgs) {
    tmp = tmp %>% 
      add_row(state = s, cg = cg)
  }
}
tmp

# filter out those rows where the state is not in the cg
stateCG = tmp %>%
  filter(mapply(grepl, tmp$state, tmp$cg, fixed=TRUE))
#view(stateCG)
stateCG # these are the 12 combinations shown in the tables

#### call the speaker ----

#(state, cg, qud)
#eval_webppl(paste("speaker(","{BEL: 1, CC: 1}",",","[{BEL: 1, CC: 1},{BEL: 1, CC: 0}]",",'","maxQUD","')",sep=""))

S = data.frame(state = character(), commonGround = character(), qud = character(), utterance = character(), prob = numeric())
S

for (n in 1:nrow(stateCG)) {
  print(stateCG$state[[n]])
  print(stateCG$cg[[n]])
  for (q in quds) {
    S_tmp = eval_webppl(paste("speaker(",stateCG$state[[n]],",",stateCG$cg[[n]],",'",q,"')",sep=""))
    for (i in 1:nrow(S_tmp)) {
      S = S %>% 
        add_row(state = stateCG$state[[n]], commonGround = stateCG$cg[[n]], qud = q, utterance = S_tmp$support[i], prob = S_tmp$prob[i])
    }
  }
}
S
#view(S)

# save raw speaker data
write_csv(S, file="data/S_raw.csv")

#### clean up the speaker data ----

# read raw speaker
S = read_csv(file="data/S_raw.csv")

# create a more compact state column for plotting
table(S$state)
S$state = gsub("\\{","",S$state)
S$state = gsub("\\}","",S$state)
S$state = gsub(" ", "",S$state)
table(S$state)

# code the commonGround column into private assumptions  
S <- S %>% 
  mutate(CC = case_when(!grepl("CC: 0", commonGround) ~ "CC:1",
                        !grepl("CC: 1", commonGround) ~ "CC:0",
                        TRUE ~ "CC:?"))
S <- S %>%
  mutate(BEL = case_when(!grepl("BEL: 0", commonGround) ~ "BEL:1",
                         !grepl("BEL: 1", commonGround) ~ "BEL:0",
                         TRUE ~ "BEL:?"))
table(S$BEL,S$CC)

S$pa = paste(S$BEL,S$CC,sep=" ")
table(S$pa)

# recode the utterance names
S = S %>%
  mutate(utterance = recode(utterance, "neg-bare-pos-dance" = "not c", "pos-bare-pos-dance" = "c",
                            "pos-know-pos-dance" = "pos-know",
                            "neg-know-pos-dance" = "neg-know",
                            "pos-think-pos-dance" = "pos-think",
                            "neg-think-pos-dance" = "neg-think"))

# recode the QUD names
S = S %>%
  mutate(qud = recode(qud, "ccQUD" = "CC? QUD", "mcQUD" = "BEL? QUD"))

# save cleaned up speaker data
write_csv(S, file="data/S.csv")
S

# speaker plots ----

#### plot probability of neg-know by PA, state and QUD ----

# read cleaned up speaker
S = read_csv(file="data/S.csv")

# BEL? QUD
S_agg = aggregate(prob~BEL*CC*state,data=S[S$qud == "BEL? QUD" & S$utterance == "neg-know",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$CC, S_agg$BEL, sep="-")
S_agg$combined = paste(S_agg$CC, S_agg$BEL, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = combined)) +
  geom_bar(stat="identity") +
  ylab("private assumptions about BEL and CC, \n state observed by speaker") +
  xlab("Probability of neg-know utterance under BEL? QUD") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-know-probability-by-PA-and-stateCombined-qudBEL.pdf",width=6,height=4)

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity") +
  ylab("Private assumption") +
  #xlab("BEL? QUD") +
  facet_grid(state ~ .) +
  ggtitle("Observed state") +
  scale_x_continuous(breaks = c(0,.02,.04),labels = c("0", ".02", ".04")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-know-probability-by-PA-and-state-qudBEL.pdf",width=5,height=4)


# CC? QUD
S_agg = aggregate(prob~BEL*CC*state,data=S[S$qud == "CC? QUD" & S$utterance == "neg-know",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$CC, S_agg$BEL, sep="-")
S_agg$combined = paste(S_agg$BEL, S_agg$CC, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = combined)) +
  geom_bar(stat="identity") +
  ylab("private assumptions about BEL and CC, \n state observed by speaker") +
  xlab("Probability of neg-know utterance under CC? QUD") +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-know-probability-by-PA-and-stateCombined-qudCC.pdf",width=6,height=4)

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity") +
  ylab("Private assumption") +
  #xlab("CC? QUD") +
  facet_grid(state ~ .) +
  ggtitle("Observed state") +
  scale_x_continuous(breaks = c(0,.00004),labels = c("0", ".00004")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-know-probability-by-PA-and-state-qudCC.pdf",width=5,height=4)


#### plot probability of all utterances, by PA, state and QUDs ----

# read cleaned up speaker
S = read_csv(file="data/S.csv")

# BEL? QUD
S_agg = aggregate(prob~BEL*CC*state*utterance,data=S[S$qud == "BEL? QUD",],FUN=mean)
S_agg

S_agg$combined = paste(S_agg$BEL, S_agg$CC, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = combined)) +
  geom_bar(stat="identity") +
  ylab("private assumptions about BEL and CC, \n state observed by speaker") +
  xlab("Probability of neg-know utterance under BEL? QUD") +
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  facet_wrap(. ~ utterance)
ggsave("graphs/utterance-probability-under-qudBEL.pdf",width=6,height=4)

# CC? QUD
S_agg = aggregate(prob~BEL*CC*state*utterance,data=S[S$qud == "CC? QUD",],FUN=mean)
S_agg

S_agg$combined = paste(S_agg$BEL, S_agg$CC, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = combined)) +
  geom_bar(stat="identity") +
  ylab("private assumptions about BEL and CC, \n state observed by speaker") +
  xlab("Probability of utterances under CC? QUD") +
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  facet_wrap(. ~ utterance)
ggsave("graphs/utterance-probability-under-qudCC.pdf",width=6,height=4)


# run the pragmatic listener ----

# input to PL: utterance, qudBias

#### call PL ----

PL = data.frame(utterance = character(), qudBias = character(), 
                state.CC = numeric(), state.BEL = numeric(), prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"')",sep=""))
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qudBias = q,
                  state.CC = PL_tmp$CC[i],
                  state.BEL = PL_tmp$BEL[i],
                  prob = PL_tmp$prob[i])
      }
  }
}
PL
#view(PL)
nrow(PL) #24

write_csv(PL, file="data/PL.csv")

# pragmatic listener plots ----

#### plot predictions for neg-know, aggregating over QUDs ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.BEL=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  mutate(state = recode(state, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  group_by(utterance,state) %>%
  summarize(prob = mean(prob))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-neg-know-QUD-aggregated.pdf",height=2,width=2)

#### plot predictions for neg-know by QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.BEL=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  rename("qud" = "qudBias") %>%
  mutate(state = recode(state, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD"))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_wrap(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-by-QUD.pdf",height=2,width=3)

# plot comparison to human data ----

#### plot comparison of all utterances, by QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# load clean data from the experiment
d = read_csv("../../results/data/cd.csv")
nrow(d) #327

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.BEL=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  droplevels() %>%
  mutate(utterance = recode(utterance, "pos-know-pos-dance"="pos-know", "neg-know-pos-dance"="neg-know", 
                                         "pos-think-pos-dance"="pos-think", "neg-think-pos-dance"="neg-think"))
PL2
#view(PL2)
nrow(PL2) #24

# calculate model predictions by utterance and contextual qud bias
PL_agg.utt = PL2 %>%
  group_by(utterance,qudBias,state) %>%
  summarize(prob = sum(prob))
PL_agg.utt
nrow(PL_agg.utt) #20

PL_agg.utt = PL_agg.utt %>%
  mutate(state = recode(state, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  rename("content" = "state", "qud" = "qudBias") %>%
  mutate(utterance = recode(utterance, "pos-know-pos-dance"="pos-know", "neg-know-pos-dance"="neg-know", 
                            "pos-think-pos-dance"="pos-think", "neg-think-pos-dance"="neg-think")) %>%
  filter(utterance != "neg-bare-pos-dance" & utterance != "pos-bare-pos-dance")
PL_agg.utt

# add "neg-think-pos-dance" and state.BEL with value 0 to model predictions
PL_agg.utt <- as.data.frame(PL_agg.utt)
tmp.data = data.frame(utterance = "neg-think", content = "BEL", qud = "BEL?", prob = 0)
tmp.data2 = data.frame(utterance = "neg-think", content = "BEL", qud = "CC?", prob = 0)
PL_agg.utt = rbind(PL_agg.utt, tmp.data, tmp.data2)
PL_agg.utt

# sort utterances by increasing inference to CC
tmp = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC)) %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                            "think-pos"="pos-think", "think-neg"="neg-think"))
tmp

PL_agg.utt$utterance = factor(PL_agg.utt$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = FALSE)
levels(PL_agg.utt$utterance)

# plot 
ggplot(data=PL_agg.utt, aes(x=content, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested_wrap(utterance ~ qud, nrow=2) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities.pdf",height=2,width=3)

# experiment data

means.C.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.BEL.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "BEL") 
means.BEL.utt.qud

# bind the data
means.by.qud = rbind(means.C.utt.qud,means.BEL.utt.qud)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                             "think-pos"="pos-think", "think-neg"="neg-think"))
means.by.qud

# sort utterances by increasing inference to CC
tmp = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC)) %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                            "think-pos"="pos-think", "think-neg"="neg-think"))
tmp

means.by.qud$utterance = factor(means.by.qud$utterance, levels = means.by.qud$utterance[order(tmp$Mean)], ordered = FALSE)
levels(means.by.qud$utterance)

ggplot() +
  geom_bar(data=PL_agg.utt,aes(x=content, y=prob),stat = "identity",width = 0.3,position = position_nudge(x = -.15)) +
  geom_bar(data=means.by.qud,aes(x=content, y=Mean), stat = "identity",width = 0.3, alpha = .7,position = position_nudge(x = .15)) +
  geom_errorbar(data=means.by.qud,aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, linewidth=.5,position = position_nudge(x = .15)) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested_wrap(utterance ~ qud, nrow=2) +
  ylab("Predicted probability \n Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/comparison.pdf",height=4,width=8)

#### comparison of neg-know predictions to human data ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("content"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.BEL=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  mutate(content = recode(content, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  rename("qud" = "qudBias") %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD"))
PL2


# load clean data from the experiment
d = read_csv("../../results/data/cd.csv")
nrow(d) #327

means.C.utt.qud = d %>%
  filter(utterance == "know-neg") %>%
  group_by(qud) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC? QUD", "nai" = "BEL? QUD")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.BEL.utt.qud = d %>%
  filter(utterance == "know-neg") %>%
  group_by(qud) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC? QUD", "nai" = "BEL? QUD")) %>%
  mutate(content = "BEL") 
means.BEL.utt.qud

# bind the data
means.by.qud = rbind(means.C.utt.qud,means.BEL.utt.qud)
means.by.qud

# plot
ggplot() +
  geom_bar(data=PL2,aes(x=content, y=prob),stat = "identity",width = 0.3,position = position_nudge(x = -.15)) +
  geom_bar(data=means.by.qud,aes(x=content, y=Mean), stat = "identity",width = 0.3, alpha = .7,position = position_nudge(x = .15)) +
  geom_errorbar(data=means.by.qud,aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, linewidth=.5,position = position_nudge(x = .15)) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_grid(. ~ qud) +
  ylab("Predicted probability (black) \n Mean inference rating (gray)") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/comparison-neg-know.pdf",height=2.5,width=3.5)



