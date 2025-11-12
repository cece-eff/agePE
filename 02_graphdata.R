##Graph primary analyses
##CCF 3.18.24. updated 8/8/25 
## KLS updated 11.11.25 

#open packages
library(here)         # relative paths
library(tidyverse)    # tidy functions
library(knitr)        # knit functions
library(kableExtra)   # extra markdown functions
library(lsr)          # cohenD()
library(lme4)         # mixed-effects regressions
library(lmerTest)     # mixed-effects regressions
library(AICcmodavg)   # predictSE()
library(cowplot)      # plot_grid()
library(gridExtra)    # aesthetics
library(sjPlot)       # clean tables
library(ggstance)     # vertical position dodge
library(forcats)      # needed for relevel


# Load source functions
source(here::here('src', 'exclude.r'))

#import cleaned data
dt <- read.csv(here('data', 'agePE_data.csv'))

#exclude participants
dt <- exclude_participants(dt)

##correct format of variables
dt$age_group<-as.factor(dt$age_group)
dt$offer<-as.factor(dt$offer)
dt$offer_value<-as.factor(dt$offer_value)
dt$id<-as.factor(dt$id)

#scale (but not mean center) variables
dt$rpe_s<-scale(dt$rpe, center=FALSE)
dt$vpe_s<-scale(dt$vpe, center=FALSE)
dt$ape_s<-scale(dt$ape, center=FALSE)
dt$age_s<-scale(dt$age, center=FALSE)

################ Model 1: VPE vs. APE vs. RPE
##Hypothesis 1: exact replication of Heffner
model1<-glmer(choice~ rpe_s+vpe_s+ape_s+(1 +rpe_s + vpe_s + ape_s | id), 
              data=dt, 
              family = binomial,
              control = glmerControl(optimizer = 'bobyqa',
                                     optCtrl = list(maxfun=2e5)))
xRange1 <- with(dt, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
xRange2 <- with(dt, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
xRange3 <- with(dt, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
predict1 <- with(dt, expand.grid(vpe_s=xRange1, ape_s=0, rpe_s = 0))
predict2 <- with(dt, expand.grid(vpe_s=0, ape_s=xRange2, rpe_s = 0))
predict3 <- with(dt, expand.grid(vpe_s=0, ape_s=0, rpe_s = xRange3))
predictedInterval1 <- data.frame(AICcmodavg::predictSE(model1, newdata=predict1, type="response", print.matrix=T))
predictedInterval2 <- data.frame(AICcmodavg::predictSE(model1, newdata=predict2, type="response", print.matrix=T))
predictedInterval3 <- data.frame(AICcmodavg::predictSE(model1, newdata=predict3, type="response", print.matrix=T))
plot_data1 <- bind_cols(predict1, predictedInterval1)
plot_data2 <- bind_cols(predict2, predictedInterval2)
plot_data3 <- bind_cols(predict3, predictedInterval3)


plot_data2$ape_s<-round(plot_data2$ape_s, digits=2)


pe_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = vpe_s, y = fit, color = "vpe_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = vpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "vpe_s"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = ape_s, y = fit, color = "ape_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = ape_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "ape_s"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = rpe_s, y = fit, color = "rpe_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = rpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "rpe_s"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     text = element_text(size = 14), title = element_text(size = 20))+
  ggtitle("All Participants")
pe_plot1
ggsave(filename = here("output", "pe_plot_combined.pdf"), plot=pe_plot1, width = 8, height = 6, useDingbats = F)

################ Model 2: Compare PEs separately by age group
#####younger adults
  tableya_data <- dt %>% 
    filter(age_group == "ya")

  tableya <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tableya_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))

  xRange1 <- with(tableya_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  xRange2 <- with(tableya_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  xRange3 <- with(tableya_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  predict1 <- with(tableya_data, expand.grid(vpe_s=xRange1, ape_s=0, rpe_s = 0))
  predict2 <- with(tableya_data, expand.grid(vpe_s=0, ape_s=xRange2, rpe_s = 0))
  predict3 <- with(tableya_data, expand.grid(vpe_s=0, ape_s=0, rpe_s = xRange3))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict1, type="response", print.matrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict2, type="response", print.matrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict3, type="response", print.matrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)

  ya_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = vpe_s, y = fit, color = "vpe_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = vpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "vpe_s"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = ape_s, y = fit, color = "ape_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = ape_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "ape_s"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = rpe_s, y = fit, color = "rpe_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = rpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "rpe_s"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     text = element_text(size = 14), title = element_text(size = 20))+
  ggtitle("Younger Adults")
  ya_plot1

#####middle-aged adults
  tablema_data <- dt %>% 
  filter(age_group == "ma")

  tablema <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tablema_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))

  xRange1 <- with(tablema_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  xRange2 <- with(tablema_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  xRange3 <- with(tablema_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  predict1 <- with(tablema_data, expand.grid(vpe_s=xRange1, ape_s=0, rpe_s = 0))
  predict2 <- with(tablema_data, expand.grid(vpe_s=0, ape_s=xRange2, rpe_s = 0))
  predict3 <- with(tablema_data, expand.grid(vpe_s=0, ape_s=0, rpe_s = xRange3))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict1, type="response", print.matrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict2, type="response", print.matrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict3, type="response", print.matrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)

  ma_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = vpe_s, y = fit, color = "vpe_s"),linewidth = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = vpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "vpe_s"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = ape_s, y = fit, color = "ape_s"), linewidth = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = ape_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "ape_s"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = rpe_s, y = fit, color = "rpe_s"), linewidth = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = rpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "rpe_s"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     text = element_text(size = 14), title = element_text(size = 20))+
  ggtitle("Middle Aged Adults")
  ma_plot1

#####older adults plot all 3 pe
  tableoa_data <- dt %>% 
    filter(age_group == "oa")

  tableoa <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tableoa_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))

  xRange1 <- with(tableoa_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  xRange2 <- with(tableoa_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  xRange3 <- with(tableoa_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  predict1 <- with(tableoa_data, expand.grid(vpe_s=xRange1, ape_s=0, rpe_s = 0))
  predict2 <- with(tableoa_data, expand.grid(vpe_s=0, ape_s=xRange2, rpe_s = 0))
  predict3 <- with(tableoa_data, expand.grid(vpe_s=0, ape_s=0, rpe_s = xRange3))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict1, type="response", print.oatrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict2, type="response", print.oatrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict3, type="response", print.oatrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)

  oa_plot1 <- ggplot() + 
  # Valence PE
  geom_line(data = plot_data1, aes(x = vpe_s, y = fit, color = "vpe_s"), linewidth = 1.5) + 
  geom_ribbon(data = plot_data1, aes(x = vpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "vpe_s"), alpha = .2) + 
  # Arousal PE
  geom_line(data = plot_data2, aes(x = ape_s, y = fit, color = "ape_s"), linewidth = 1.5) + 
  geom_ribbon(data = plot_data2, aes(x = ape_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "ape_s"), alpha = .2) + 
  # Reward PE
  geom_line(data = plot_data3, aes(x = rpe_s, y = fit, color = "rpe_s"), linewidth = 1.5) + 
  geom_ribbon(data = plot_data3, aes(x = rpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = "rpe_s"), alpha = .2) + 
  scale_color_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + # arousal, reward, valence order (Paul Tol's colors)
  scale_fill_manual(name = "Color", values = c("#EE6677", "#228833", "#4477AA")) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  coord_cartesian(xlim = c(-5.0, 5.), ylim = c(0, 1)) + 
  scale_x_continuous(name = "Prediction Errors") +
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     text = element_text(size = 14), title = element_text(size = 20))+
  ggtitle("Older Adults")
  oa_plot1

#####combine 3 plots into one figure
  pe_plot_agegroup <- cowplot::plot_grid(ya_plot1, ma_plot1, oa_plot1, nrow = 1)
  pe_plot_agegroup
  ggsave(filename = here("output", "pe_plot_agegroup.pdf"), plot=pe_plot_agegroup, width = 16, height = 6, useDingbats = F)

##################Model 2: Compare PEs by age group
##first 3 code chunks is to create age group models if code above was not ran
  ##YA
  tableya_data <- dt %>% 
    filter(age_group == "ya")
  tableya <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tableya_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
  #MA
  tablema_data <- dt %>% 
    filter(age_group == "ma")
  tablema <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tablema_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
  #OA
  tableoa_data <- dt %>% 
    filter(age_group == "oa")
  tableoa <- glmer(choice ~ rpe_s+vpe_s+ape_s+(1 + rpe_s + vpe_s + ape_s | id), 
                 data = tableoa_data,
                 family = binomial, 
                 control = glmerControl(optimizer = "bobyqa"))
##### Valence PE 
  xRange1<-with(tableya_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  xRange2<-with(tablema_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  xRange3<-with(tableoa_data, seq(round(min(vpe_s, na.rm=T), 1), round(max(vpe_s, na.rm=T), 1), by = .1))
  predict1<-with(tableya_data, expand.grid(vpe_s=xRange1, ape_s = 0, rpe_s = 0))
  predict2<-with(tablema_data, expand.grid(vpe_s=xRange2, ape_s = 0, rpe_s = 0))
  predict3<-with(tableoa_data, expand.grid(vpe_s=xRange3, ape_s = 0, rpe_s = 0))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict1, type="response", print.matrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict2, type="response", print.matrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict3, type="response", print.matrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)
  colnames(plot_data1)[4]<-"yax.fit"
  colnames(plot_data1)[5]<-"yax.se.fit"
  colnames(plot_data2)[4]<-"max.fit"
  colnames(plot_data2)[5]<-"max.se.fit"
  colnames(plot_data3)[4]<-"oax.fit"
  colnames(plot_data3)[5]<-"oax.se.fit"

  #Combine YA, MA, OA dataframes
  plot_data<-left_join(plot_data1, plot_data2)
  plot_data<-left_join(plot_data, plot_data3)
  plot_data$ape_s<-NULL
  plot_data$rpe_s<-NULL
  plot_data<- plot_data %>% pivot_longer(cols = 2:7) %>% separate(name, into = c("age", "test"), sep = "x.") %>% pivot_wider(names_from ="test", values_from = "value")
  plot_data$age<-as.factor(plot_data$age)
  plot_data$age<-fct_relevel(plot_data$age, c("ya", "ma", "oa"))
  
  #graph by age group
  vpe_all<- ggplot() + 
  # Valence PE
  geom_line(data = plot_data, aes(x = vpe_s, y = fit, color = age), size = 1) + 
  geom_ribbon(data = plot_data, aes(x = vpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = age), alpha = .2) + 
  scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
  xlab("Valence PE") + 
  scale_color_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
  scale_fill_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
  coord_cartesian(ylim = c(0, 1)) + 
  geom_vline(xintercept = 0, lty = 3) + 
  theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### Arousal Prediction Error 
  xRange1<-with(tableya_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  xRange2<-with(tablema_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  xRange3<-with(tableoa_data, seq(round(min(ape_s, na.rm=T), 1), round(max(ape_s, na.rm=T), 1), by = .1))
  predict1<-with(tableya_data, expand.grid(vpe_s=0, ape_s = xRange1, rpe_s = 0))
  predict2<-with(tablema_data, expand.grid(vpe_s=0, ape_s = xRange2, rpe_s = 0))
  predict3<-with(tableoa_data, expand.grid(vpe_s=0, ape_s = xRange3, rpe_s = 0))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict1, type="response", print.matrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict2, type="response", print.matrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict3, type="response", print.matrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)
  colnames(plot_data1)[4]<-"yax.fit"
  colnames(plot_data1)[5]<-"yax.se.fit"
  colnames(plot_data2)[4]<-"max.fit"
  colnames(plot_data2)[5]<-"max.se.fit"
  colnames(plot_data3)[4]<-"oax.fit"
  colnames(plot_data3)[5]<-"oax.se.fit"
  
  #Combine YA, MA, OA dataframes
  plot_data1$ape_s<-round(plot_data1$ape_s, digits=2)
  plot_data2$ape_s<-round(plot_data2$ape_s, digits=2)
  plot_data3$ape_s<-round(plot_data3$ape_s, digits=2)
  plot_data<-left_join(plot_data1, plot_data2)
  plot_data<-left_join(plot_data, plot_data3)
  plot_data$vpe_s<-NULL
  plot_data$rpe_s<-NULL
  plot_data<- plot_data %>% pivot_longer(cols = 2:7) %>% separate(name, into = c("age", "test"), sep = "x.") %>% pivot_wider(names_from ="test", values_from = "value")
  plot_data$age<-as.factor(plot_data$age)
  plot_data$age<-fct_relevel(plot_data$age, c("ya", "ma", "oa"))
  
  #graph by age group
  ape_all<- ggplot() + 
    # Arousal PE
    geom_line(data = plot_data, aes(x = ape_s, y = fit, color = age), size = 1) + 
    geom_ribbon(data = plot_data, aes(x = ape_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = age), alpha = .2) + 
    scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
    xlab("Arousal PE") + 
    scale_color_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
    scale_fill_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
    coord_cartesian(ylim = c(0, 1)) + 
    geom_vline(xintercept = 0, lty = 3) + 
    theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### Reward Prediction Error 
  xRange1<-with(tableya_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  xRange2<-with(tablema_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  xRange3<-with(tableoa_data, seq(round(min(rpe_s, na.rm=T), 1), round(max(rpe_s, na.rm=T), 1), by = .1))
  predict1<-with(tableya_data, expand.grid(vpe_s=0, ape_s = 0, rpe_s = xRange1))
  predict2<-with(tablema_data, expand.grid(vpe_s=0, ape_s = 0, rpe_s = xRange2))
  predict3<-with(tableoa_data, expand.grid(vpe_s=0, ape_s = 0, rpe_s = xRange3))
  predictedInterval1 <- data.frame(AICcmodavg::predictSE(tableya, newdata=predict1, type="response", print.matrix=T))
  predictedInterval2 <- data.frame(AICcmodavg::predictSE(tablema, newdata=predict2, type="response", print.matrix=T))
  predictedInterval3 <- data.frame(AICcmodavg::predictSE(tableoa, newdata=predict3, type="response", print.matrix=T))
  plot_data1 <- bind_cols(predict1, predictedInterval1)
  plot_data2 <- bind_cols(predict2, predictedInterval2)
  plot_data3 <- bind_cols(predict3, predictedInterval3)
  colnames(plot_data1)[4]<-"yax.fit"
  colnames(plot_data1)[5]<-"yax.se.fit"
  colnames(plot_data2)[4]<-"max.fit"
  colnames(plot_data2)[5]<-"max.se.fit"
  colnames(plot_data3)[4]<-"oax.fit"
  colnames(plot_data3)[5]<-"oax.se.fit"
  
  #Combine YA, MA, OA dataframes
  plot_data<-left_join(plot_data1, plot_data2)
  plot_data<-left_join(plot_data, plot_data3)
  plot_data$vpe_s<-NULL
  plot_data$ape_s<-NULL
  plot_data<- plot_data %>% pivot_longer(cols = 2:7) %>% separate(name, into = c("age", "test"), sep = "x.") %>% pivot_wider(names_from ="test", values_from = "value")
  plot_data$age<-as.factor(plot_data$age)
  plot_data$age<-fct_relevel(plot_data$age, c("ya", "ma", "oa"))
  
  #graph by age group
  rpe_all<- ggplot() + 
    # Arousal PE
    geom_line(data = plot_data, aes(x = rpe_s, y = fit, color = age), size = 1) + 
    geom_ribbon(data = plot_data, aes(x = rpe_s, ymax = fit + se.fit, ymin = fit - se.fit, fill = age), alpha = .2) + 
    scale_y_continuous(labels = scales::percent, name = "p(Reject)") +
    xlab("Reward PE") + 
    scale_color_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
    scale_fill_manual(name = "Age Group", values = c("#0033FF", "#339966", "#FF3399")) + # Paul Tol
    coord_cartesian(ylim = c(0, 1)) + 
    geom_vline(xintercept = 0, lty = 3) + 
    theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### Combine plots into one figure
  pe_plot_by_age <- cowplot::plot_grid(vpe_all, ape_all, rpe_all, nrow = 1)
  pe_plot_by_age
  ggsave(filename = here("output", "pe_plot_by_age.pdf"), plot=pe_plot_by_age, width = 16, height = 6, useDingbats = F)

############## Model 2: main effect of age on ultimatum game decision to punish
#plot probability to punish by age
prob<-dt %>% group_by(id, age) %>% summarize(prob_rej=mean(choice))
choice_by_age<-ggplot(prob, aes(y=prob_rej, x=age)) + geom_jitter()+ geom_smooth(method = 'lm')+ylab("p[Reject]") + xlab("Age")+theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(choice_by_age, file=here("output", "choice_by_age.pdf"))

############## Model 3: age differences in PE
##plot each prediction error by age
id_pe_age<-dt %>% group_by(id, age) %>% dplyr::summarise(mean_rpe = mean(rpe), mean_vpe = mean(vpe), mean_ape=mean(ape))
age_rpe<-ggplot(id_pe_age, aes(y=age, x=mean_rpe)) + geom_jitter()+ geom_smooth(method = 'lm')+ylab("Age") + xlab("Reward PE") + theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
age_vpe<-ggplot(id_pe_age, aes(y=age, x=mean_vpe)) + geom_jitter()+ geom_smooth(method = 'lm')+ylab("Age") + xlab("Valence PE")+theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
age_ape<-ggplot(id_pe_age, aes(y=age, x=mean_ape)) + geom_jitter()+ geom_smooth(method = 'lm')+ylab("Age") + xlab("Arousal PE")+theme_bw() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
age_pe_all<-cowplot::plot_grid(age_vpe, age_ape, age_rpe, nrow=1)
ggsave(age_pe_all, filename = here("output", "age_pe_all.pdf"))

