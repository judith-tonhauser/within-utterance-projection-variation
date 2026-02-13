# graphs.R to create Fig 1 in "Predicting within-utterance projection variation"

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../helpers.R')

# Fig 1: plot of mean certainty ratings from Exp 1a of Degen & Tonhauser 2022 -----
# import data from repo
cd <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
summary(cd)

# mean projectivity by predicate, including the main clause controls
means = cd %>%
  group_by(verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(verb),Mean))
means

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
         ifelse(cols$V %in% c("MC"),"MC","NF")))

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#D55E00",
                      ifelse(cols$VeridicalityGroup == "NF", "black",'grey40'))


cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
                                   ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                          ifelse(means$verb  %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

subjmeans = cd %>%
  group_by(verb,workerid) %>%
  summarize(Mean = mean(response)) 
subjmeans$verb <- factor(subjmeans$verb, levels = unique(levels(means$verb)))
subjmeans$VeridicalityGroup = factor(x=
                                   ifelse(subjmeans$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", 
                                          ifelse(subjmeans$verb  %in% c("MC"),"MC","NF")),levels=rev(c("F","NF","MC")))

levels(subjmeans$verb)
#view(subjmeans)

# version of Figure 2 Degen & Tonhauser 2022 Language paper
# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
  geom_violin(data=subjmeans,scale="width",linewidth = 0, alpha = .4) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax,fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(c("#D55E00","black","grey40")),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean certainty rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("mean-certainty-by-predicateType.pdf",height=4.5,width=7)

