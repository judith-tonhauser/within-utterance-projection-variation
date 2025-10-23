# 2b_simplified-know-noisy-QUD-fitted
# model evaluation script

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)

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

# define the QUDs
#quds = c("ccQUD", "mcQUD")
#quds

# define the qudBias (from context)
qudBias = c("BEL?", "CC?")
qudBias

# pragmatic listener ----

# input to PL: utterance, qud

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

# Fig 6b: Comparison of neg-know predictions to human data ----

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

# aggregate over ccPrior
PL2 = PL2 %>%
  group_by(qud,content)  %>%
  summarize(prob = mean(prob))
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
  geom_bar(data=PL2,aes(x=content, y=prob),stat = "identity",width = 0.3,
           position = position_nudge(x = -.15),
           color="black",fill="black") +
  geom_bar(data=means.by.qud,aes(x=content, y=Mean), stat = "identity",width = 0.3, alpha = .7,position = position_nudge(x = .15)) +
  geom_errorbar(data=means.by.qud,aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, linewidth=.5,position = position_nudge(x = .15)) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_grid(. ~ qud) +
  ylab("Predicted probability (black) \n Mean inference rating (grey)") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/Fig6b-comparison-neg-know.pdf",height=2.5,width=3.5)

# Appendix: predictions for neg-know by QUD ----

# read PL
PL = read_csv("data/PL.csv")
nrow(PL) #24

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
  mutate(qudBias = recode(qudBias, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD")) %>%
  rename(qud = qudBias) %>%
  select(-c(trueFalse))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3,color="black",fill="black") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_wrap(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/neg-know-predictions-by-QUD.pdf",height=2,width=3)
