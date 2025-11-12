#analyze cleaned data
##CCF 4.2024 updated KLS 11.11.25

library(here)         # relative paths
library(tidyverse)    # tidy functions
library(lme4)         # mixed-effects regressions
library(lmerTest)     # mixed-effects regressions
library(sjPlot)       # clean tables

# Load source functions
source(here('src', 'exclude.r'))

#import cleaned data
dt <- read.csv(here('data', 'agePE_data.csv'))

#exclude participants
dt <- exclude_participants(dt)

##ensure correct format of variables
dt$age_group<-as.factor(dt$age_group)
dt$offer<-as.factor(dt$offer)
dt$offer_value<-as.factor(dt$offer_value)
dt$id<-as.factor(dt$id)

#scale (but not mean center) variables
dt$rpe_s<-scale(dt$rpe, center=FALSE)
dt$vpe_s<-scale(dt$vpe, center=FALSE)
dt$ape_s<-scale(dt$ape, center=FALSE)
dt$age_s<-scale(dt$age, center=FALSE)

################# Model A: Hypotheses 1 and 2 ##################
  modela<-glmer(choice~ rpe_s*age_s + vpe_s*age_s + ape_s*age_s+ (1 + rpe_s + vpe_s + ape_s | id), 
                data=dt, 
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa',
                                       optCtrl = list(maxfun=2e5)))
  summary(modela)
  tab_model(modela, file= here("output", "model2table.doc"), transform = NULL, title = "Emotion Prediction Errors", 
            pred.labels = c("Intercept", "Reward PE", "Age", "Valence PE", "Arousal PE",
                            "Reward PE:Age", "Valence PE:Age", "Arousal PE:Age"),
            dv.labels = c("Estimates"), string.est = "Log-Odds", string.se = "SE", string.stat = "Z", 
            show.se = TRUE, show.stat = TRUE, show.ci = FALSE, 
            show.re.var = FALSE, show.aic = FALSE, show.dev = FALSE, 
            show.r2 = FALSE, show.icc = FALSE, show.obs = TRUE,
            CSS = css_theme("regression"))

  # beta comparison test
  coefsa <- fixef(modela)
  coefsa_se <- sqrt(diag(vcov(modela)))
  z_scorea <- (as.numeric(coefsa["vpe_s"]) - as.numeric(coefsa["rpe_s"])) / (sqrt(coefsa_se[2]^2 + coefsa_se[4]^2))
  pvaluea <- pnorm(-abs(z_scorea))

############### Model B: Hypothesis 3 ###############
  modelb<-lm(cbind(rpe_s,vpe_s, ape_s) ~ age_s, data=dt)
  summary(modelb)
