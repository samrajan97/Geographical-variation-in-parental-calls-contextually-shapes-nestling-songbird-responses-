## Clear memory
rm(list = ls())

## Set up packages ##
library(readxl) #import excel files
library(dplyr) #Data formatting
library(tidyverse) #Data formatting
library(MASS) #for LDA analysis
library(ggplot2) #plotting
library(ggpubr) # arrange multiple plots
library(glmmTMB) #Linear mixed models
library(emmeans) #Posthoc tests
library(DHARMa) #Model diagnostics
library(ggridges) ##for LDA plot

## Import dataset 
alarm_mds <- read_excel("~/data_call_mds.xlsx") 
attach(alarm_mds)
str(alarm_mds)

## Make variables into the correct format
alarm_mds$Population <- as.factor(alarm_mds$Population)
alarm_mds$Individual <- as.factor(alarm_mds$Individual)
alarm_mds$Recording <- as.factor(alarm_mds$Recording)

## Overall sample size
alarm_mds %>%
  summarise(No_ofindividuals = n_distinct(Individual),
            No_callsanalysed = n())

## Number of calls analysed per individual
alarm_mds %>%
  group_by(Individual) %>%
  summarise(n = n()) %>%
  summarise(min(n), max(n), median(n))

## Supplementary Table 1: Sample size:
alarm_mds %>%
  group_by(Population) %>%
  summarise(No_ofindividuals = n_distinct(Individual),
            No_callsanalysed = n())

##make sure that the population levels are in this order:
alarm_mds$Population <- factor(alarm_mds$Population, levels = c('Sweden_T', 'Sweden_L', 'Netherlands_W', 'Netherlands_D', 'UK', 
                                                                'Spain_H', 'Spain_V'))


## Calculate Population centroid for first two PC values
alarm_pop <- alarm_mds %>%
  dplyr::select(Population, pc_1_value, pc_2_value) %>%
  group_by(Population) %>%
  summarise_all(funs(mean))

## Calculate Individual centroid for first two PC values
alarm_popind <- alarm_mds %>%
  dplyr::select(Population, Individual, pc_1_value, pc_2_value) %>%
  group_by(Population, Individual) %>%
  summarise_all(funs(mean))

##Replicate figure 1B: PCA of alarm calls per population with population centroid
Figure_1B <- ggplot(alarm_popind, aes(pc_1_value, pc_2_value, group=Population, fill = Population)) +
  geom_point(size=9.0, alpha = .4, colour= 'black',pch = 21) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))   +
  geom_point(data = alarm_pop, aes(pc_1_value, pc_2_value, group = Population, fill = Population), 
             colour = 'black', pch=24,size = 13)   + 
  scale_colour_manual(values = c("#F8766D", "#C49A00" ,"#53B400" ,"#00C094", "#00B6EB" ,"#A58AFF" ,"#FB61D7"))+
  theme(legend.position = "none") +  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "PC1 (61.78%)", y = "PC2 (11.12%)") + 
  theme(axis.text = element_text(size = 25))  + 
  theme(axis.title = element_text(size = 30), axis.title.x = element_text(vjust =-1), axis.title.y = element_text(vjust =2)) +
  theme(axis.ticks.length=unit(0.3,"cm")) + theme(legend.position = "none")
Figure_1B

##Q1) Can the alarm calls from the different populations be distinguished from each other based on PC scores
#Likelihood ratio tests for the 10 PCs: Supplemenatry Table 4
##PC1 driven mainly by variation in Sweden_T from Netherlands_D and UK
m_intpc1 <- glmmTMB(pc_1_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc1 <- glmmTMB(pc_1_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc1,m_pc1)
emmeans(m_pc1, list(pairwise~Population))

##PC2 driven mainly by variation in both Spainish populations from other populations
m_intpc2 <- glmmTMB(pc_2_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc2 <- glmmTMB(pc_2_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc2,m_pc2)
emmeans(m_pc2, list(pairwise~Population))

##PC3 driven mainly by variation in UK and Nl from other countries and between each other
m_intpc3 <- glmmTMB(pc_3_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc3 <- glmmTMB(pc_3_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc3,m_pc3)
emmeans(m_pc3, list(pairwise~Population))

##PC4 driven mainly by variation in Sweden  (especially Sweden_L) from other populations
m_intpc4 <- glmmTMB(pc_4_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc4 <- glmmTMB(pc_4_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc4,m_pc4)
emmeans(m_pc4, list(pairwise~Population))

##PC5 driven mainly by Spain_H
m_intpc5 <- glmmTMB(pc_5_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc5 <- glmmTMB(pc_5_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc5,m_pc5)
emmeans(m_pc5, list(pairwise~Population))

##PC6 not significant
m_intpc6 <- glmmTMB(pc_6_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc6 <- glmmTMB(pc_6_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc6,m_pc6)

##PC7 not significant
m_intpc7 <- glmmTMB(pc_7_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc7 <- glmmTMB(pc_7_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc7,m_pc7)

##PC8 not significant
m_intpc8 <- glmmTMB(pc_8_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc8 <- glmmTMB(pc_8_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc8,m_pc8)

##PC9  significant: Uk and Sweden_L different
m_intpc9 <- glmmTMB(pc_9_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc9 <- glmmTMB(pc_9_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc9,m_pc9)
emmeans(m_pc9, list(pairwise~Population))

##PC10  not significant
m_intpc10 <- glmmTMB(pc_10_value~ 1 + (1|Individual), dispformula = ~Population, alarm_mds)
m_pc10 <- glmmTMB(pc_10_value~ Population + (1|Individual), dispformula = ~Population, alarm_mds)
anova(m_intpc10,m_pc10)

##Q2) Run a linear discriminant analysis to see how the populations differ from each other

#CV = TRUE allows you to quickly get % correct classification 
#for each category. CV = FALSE gives you the LD loadings and means across categories
##Need CV=TRUE to get to the confusion matrix
alarm_lda <- lda(formula = Population ~ pc_1_value + pc_2_value + pc_3_value
                +pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+pc_10_value,
                data = alarm_mds, CV = TRUE)

##Supplementary Table 5: confusion matrix and percentage accuracy for each population
ct <- table(alarm_mds$Population, alarm_lda$class)
ct #Confusion matrix
diag(prop.table(ct, 1)) ##accuracy of LDA for each group

#correctly over all categories: for overall accuracy of LDA model
sum(diag(ct))/sum(ct)

####these lines project a new data set using the previous LD functions. First, you have to re-run the LDA with CV = FALSE
alarm_lda <- lda(formula = Population ~ pc_1_value + pc_2_value + pc_3_value +
                  pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+
                  pc_10_value, data = alarm_mds, CV = FALSE)
##Check how the different PC's load onto the LDA: PC2, PC4,  PC9, PC5
alarm_lda #LD1 explains 85%

##Prediction to get the LD scores for calls from the different populations
alarm.ld1 <- predict(object = alarm_lda, newdata = alarm_mds) 
population_lda <- cbind(alarm_mds, alarm.ld1)

model_interceptlda <- glmmTMB(x.LD1 ~ 1 + (1|Individual), data = population_lda, 
                     dispformula = ~Population,family = gaussian())
model_lda <- glmmTMB(x.LD1 ~ Population + (1|Individual), data = population_lda, 
                       dispformula = ~Population,family = gaussian())
anova(model_interceptlda, model_lda) #LRT for effect of population
#Supplementary Table 6: Post-hoc results
emmeans(model_lda, list(pairwise~Population)) 

##Check residuals
simulation_residuals <- simulateResiduals(fittedModel = model_lda, plot = F)
plot(simulation_residuals) #mostly good

##Replicate Figure 1C: LDA of calls from the different populations

#reorder the populations to get it in the correct order
population_lda$Population <- factor(population_lda$Population, 
                                    levels = c('Spain_V', 'Spain_H', 'UK',  'Netherlands_D', 
                                               'Netherlands_W', 'Sweden_L', 'Sweden_T'))


Figure_1C <- ggplot(population_lda, aes(x = x.LD1, y = Population, fill = Population)) + 
  geom_density_ridges()  + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  labs(x = "LD1 (85.19%)", y = "") + 
  theme(axis.text = element_text(size = 25))  + 
  theme(axis.title = element_text(size = 30), axis.title.x = element_text(vjust =-1)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, .2))) +
  scale_fill_manual(values = c("#FB61D7","#A58AFF","#00B6EB" , "#00C094", "#53B400" , "#C49A00", "#F8766D"))+
  theme(axis.ticks.length=unit(0.3,"cm")) 
Figure_1C
