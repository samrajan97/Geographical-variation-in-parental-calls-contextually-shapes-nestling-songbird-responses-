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
library(corrplot) #correlation matrix

## Import dataset 
nestling_responses <- read_excel("~/Desktop/Chapter2,3_Alarmcall/Github_codes_data_MS/data_nestling_responses.xlsx") 
attach(nestling_responses)
str(nestling_responses)

##Convert variables to correct format
nestling_responses$Year <- as.factor(nestling_responses$Year)
nestling_responses$Date <- as.factor(nestling_responses$Date)
nestling_responses$Treatment <- as.factor(nestling_responses$Treatment)
nestling_responses$Playback_male <- as.factor(nestling_responses$Playback_male)
nestling_responses$Playback_ID <- as.factor(nestling_responses$Playback_ID)
nestling_responses$Order <- as.factor(nestling_responses$Order)
nestling_responses$Nestling_RN <- as.factor(nestling_responses$Nestling_RN)
nestling_responses$Nestbox <- as.factor(nestling_responses$Nestbox)
nestling_responses$Mass <- as.numeric(nestling_responses$Mass)

#################### SAMPLE SIZE ##################################

## 1) Sample size for playbacks per year: Overall number of nestlings and nestboxes tested
nestling_responses %>% group_by(Year) %>%
  summarise(n_distinct(Nestbox), n_distinct(Nestling_RN))

# 2) Sample size for paired playback treatments each year
#First, create a summary dataset with the number of nestlings per Nestbox and Year
nest_summary <- nestling_responses %>%
  group_by(Year, Nestbox) %>%
  summarize(n_Nestlings = n_distinct(Nestling_RN))

# Next, create a dataset with the treatments received by each Nestbox and Year (pairing it)
treatment_summary <- nestling_responses %>%
  dplyr::select(Year, Nestbox, Treatment) %>%
  distinct() %>%
  arrange(Year, Nestbox) %>%
  group_by(Year, Nestbox) %>%
  summarise(treatments = paste(Treatment, collapse = "-"))

# Now, merge the two summary datasets
nest_summary <- merge(nest_summary, treatment_summary, by = c("Year", "Nestbox"))
nest_summary$treatments <- as.factor(nest_summary$treatments)
##Recode the factor of treatment so that the order doesn't matter 
nest_summary$treatments <- recode_factor(nest_summary$treatments, 
                                            'Fast foreign-Fast collared' = "Fast collared-Fast foreign",
                                            'Fast collared-Fast foreign' = "Fast collared-Fast foreign",
                                            'Fast collared-Fast local' = "Fast collared-Fast local",
                                            'Fast local-Fast collared' =  "Fast collared-Fast local",
                                            'Fast foreign'= "Fast foreign",
                                            'Fast foreign-Fast local' = "Fast foreign-Fast local" ,
                                            'Fast local' = "Fast local",
                                            'Fast local-Fast foreign' = "Fast foreign-Fast local",
                                            'Fast local-Slow foreign'  =  "Fast local-Slow foreign",
                                            'Fast local-Slow local'  = "Fast local-Slow local",
                                            'Slow foreign-Fast local'  = "Fast local-Slow foreign",
                                            'Slow foreign-Slow local' = "Slow foreign-Slow local",
                                            'Slow local-Fast local' = "Fast local-Slow local",
                                            'Slow local-Slow foreign' = "Slow foreign-Slow local")


##Supplementary table 2: Now calcuate the sample size for the different paired playback treatments performed each year
nest_summary %>% group_by(treatments) %>%
  summarise(n_distinct(Nestbox), sum(n_Nestlings))

## 3) Supplementary Table 2: Sample size for model comparisons
nestling_responses %>% group_by(Year, Treatment) %>%
  summarise(no_nests = n_distinct(Nestbox),
            no_individual = n_distinct(Nestling_RN))

##NESTBOX 78 2022: All nestlings do not have mass values
##Nestbox 678 and 679 2023: 2 nestlings from both of them have missing mass values

#################### MODEL WITH ALL THE DATA: NORMAL BEGGING CALLS ##################################

model_all1 <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size + Year + 
                       (1|Nestbox/Nestling_RN), family = poisson(), data = nestling_responses)
model_all2 <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size + Year + 
                       (1|Nestbox/Nestling_RN) , family = nbinom1(), data = nestling_responses) #lowest AIC
model_all3 <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size + Year + 
                       (1|Nestbox/Nestling_RN), family = nbinom2(), data = nestling_responses)
model_all4 <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size + Year + 
                        (1|Nestbox/Nestling_RN) + (1|Playback_ID), family = nbinom1(), data = nestling_responses) #Playback male included, but has a higher AIC

AIC(model_all1, model_all2, model_all3, model_all4)
summary(model_all2)

##Model without treatment as an effect
model_intercept <- glmmTMB(During_NBC ~ Before_NBC + Order +  Mass + Clutch_size + Year + 
                             (1|Nestbox/Nestling_RN), family = nbinom1(), data = nestling_responses)

##Likelihood ratio test for the effect of Treatment 
anova(model_intercept, model_all2) ##Significant effect of treatment

##Check the residuals of the model
residuals_model_all2 <- simulateResiduals(fittedModel = model_all2, plot = F) 
testZeroInflation(residuals_model_all2)
testDispersion(residuals_model_all2)
plot(residuals_model_all2) #all good

#Posthoc
posthoc = emmeans(model_all2, specs = ~ Treatment)
posthoc ##View it to see in the order the treatments appear to specify contrasts:
fastcollared = c(1,0,0,0,0)
fastforeign = c(0,1,0,0,0)
fastlocal = c(0,0,1,0,0)
slowforeign = c(0,0,0,1,0)
slowlocal = c(0,0,0,0,1)

contrast(posthoc, method = list('FL-FF' = fastlocal - fastforeign, 
                             'FF-FC' = fastforeign - fastcollared,
                             'FL-FC' = fastlocal - fastcollared,
                             'SL-FL' = slowlocal - fastlocal,
                             'SL-SF' = slowlocal - slowforeign), adjust = 'mvt')

#################### OTHER RESPONSE VARIABLES ##################################
## Correlation of normal begging calls with the other response variables during playback period:
cor_matrix <- cor(nestling_responses[c(18:21)],method = 'spearman')
corrplot(cor_matrix, type="lower") #Visualise: moderately correlated


##Which is the most common behaviour??
# Define a function to summarize data for each response variable
summarize_response <- function(data, response_var) {
  data %>%
    filter(!!sym(response_var) != '0') %>%
    summarize(nestbox = n_distinct(Nestbox),
              nestling_rn = n_distinct(Nestling_RN)) %>%
    mutate(response_variable = response_var)
}

# List of response variables
response_variables <- c("Before_NBC", "Before_HIBC", "Before_preen", "Before_gape", "Before_lookup",
                        "During_NBC", "During_HIBC", "During_preen", "During_gape", "During_lookup")

# Iterate over the response variables and summarize the data
summary_results <- lapply(response_variables, function(response_var) {
  summarize_response(nestling_responses, response_var)
})

# Combine all the summary results into one dataframe
summary_table <- bind_rows(summary_results)


# 1) HIBC:
model_hibc <- glmmTMB(During_HIBC ~ Before_HIBC+ Treatment + Order +  Mass + Clutch_size + Year + 
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = nestling_responses) 
model_hibc_intercept <- glmmTMB(During_HIBC ~ Before_HIBC + Order +  Mass + Clutch_size + Year + 
                             (1|Nestbox/Nestling_RN), family = nbinom1(), data = nestling_responses)

anova(model_hibc_intercept, model_hibc)
summary(model_hibc)
residuals_hibc <- simulateResiduals(fittedModel = model_hibc, plot = F) #s perfect
plot(residuals_hibc) 

posthoc_hibc = emmeans(model_hibc, specs = ~ Treatment)
contrast(posthoc_hibc, method = list('FL-FF' = fastlocal - fastforeign, 
                             'FF-FC' = fastforeign - fastcollared,
                             'FL-FC' = fastlocal - fastcollared,
                             'SL-FL' = slowlocal - fastlocal,
                             'SL-SF' = slowlocal - slowforeign), adjust = 'mvt')

# 2) Gape:
model_gape <- glmmTMB(During_gape ~ Before_gape+ Treatment + Order +  Mass + Clutch_size + Year + 
                        (1|Nestbox), family = nbinom1(), data = nestling_responses) 
model_gape_intercept <- glmmTMB(During_gape ~ Before_gape + Order +  Mass + Clutch_size + Year + 
                                  (1|Nestbox), family = nbinom1(), data = nestling_responses)

anova(model_gape_intercept, model_gape)

residuals_gape <- simulateResiduals(fittedModel = model_gape, plot = F) #s perfect
plot(residuals_gape) #good

posthoc_gape = emmeans(model_gape, specs = ~ Treatment) #Nothing significant
contrast(posthoc_gape, method = list('FL-FF' = fastlocal - fastforeign, 
                                     'FF-FC' = fastforeign - fastcollared,
                                     'FL-FC' = fastlocal - fastcollared,
                                     'SL-FL' = slowlocal - fastlocal,
                                     'SL-SF' = slowlocal - slowforeign), adjust = 'mvt')

# 3) Lookup:
model_lookup <- glmmTMB(During_lookup ~ Before_lookup+ Treatment + Order +  Mass + Clutch_size + Year + 
                        (1|Nestbox), family = nbinom1(), data = nestling_responses) 
model_lookup_intercept <- glmmTMB(During_lookup ~ Before_lookup + Order +  Mass + Clutch_size + Year + 
                                  (1|Nestbox), family = nbinom1(), data = nestling_responses)

anova(model_lookup_intercept, model_lookup) #not significant

residuals_lookup <- simulateResiduals(fittedModel = model_lookup, plot = F) #s perfect
plot(residuals_lookup) #good

#################### MODEL FOR SLOW VS FAST LOCAL CALLS ##################################

#Dataset for slow versus fast call rates on nestling responses
data_local_slowfast <- nestling_responses %>%
  filter(Year == '2023') %>%
  filter(Treatment != 'Slow foreign')

model_rate <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size +
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_local_slowfast) #lowest aic
model_intercept_rate <- glmmTMB(During_NBC ~ Before_NBC + Order +  Mass + Clutch_size +
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_local_slowfast) ##Effect of treatment
model_intercept_baseline <- glmmTMB(During_NBC ~ Treatment + Order +  Mass + Clutch_size +
                                  (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_local_slowfast) ##Effect of treatment
summary(model_rate) #get estimate for baseline

anova(model_rate, model_intercept_rate) #likelihood ratio test for treatment
anova(model_rate, model_intercept_baseline) #likelihood ratio test for baseline begging behaviour

residuals_rate <- simulateResiduals(fittedModel = model_rate, plot = F) #residuals
plot(residuals_rate) #good

emmeans(model_rate, list(pairwise~Treatment), adjust = "tukey") #Posthoc

##Replicate Figure 2:
#Dataset for plotting:average normal begging calls by nestbox
data_local_slowfast_plot <-  data_local_slowfast%>%
  group_by(Treatment, Nestbox) %>%
  summarise(mean_beforeNBC = mean(Before_NBC), mean_duringNBC = mean(During_NBC))

data_local_slowfast_plot %>% ungroup() %>% summarise(mean = mean(mean_beforeNBC)) ##Average number of begging calls duringbaseline

rateboxplot <-ggplot(data_local_slowfast_plot, aes(y= mean_duringNBC, x = Treatment, colour = NULL, fill = Treatment)) + 
  geom_hline(yintercept=4.78,linetype=2) +
  geom_boxplot(aes(colour = Treatment, alpha = 0.5), width = 0.5, outlier.shape = NA) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#9a3fcc",  "#fba810")) +  
  scale_colour_manual(values = c("#9a3fcc",  "#fba810")) +
  geom_jitter(aes(colour = Treatment), size = 9, width = 0.1, height = 0.2, alpha = 0.5,pch = 21, colour = 'black') +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text = element_text(size = 25)) +  
  theme(axis.ticks.length=unit(.3, "cm"),  axis.title.x = element_text(vjust =-1), axis.title.y = element_text(vjust =2)) +
  labs(x = "Repetition rate in local alarm calls", y = "Mean number of Begging calls \n per nestbox") +
  geom_hline(yintercept=4.78,linetype=2)
rateboxplot

#################### MODEL FOR GEO VARIATION IN FAST CALLS ##################################

#Dataset for slow versus fast call rates on nestling responses
data_geofast <- nestling_responses %>%
  filter(Year == '2022') 

model_fast <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size +  
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geofast) #lower AIC
model_intercept_fast <- glmmTMB(During_NBC ~ Before_NBC + Order +  Mass + Clutch_size +  
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geofast)
model_intercept_baseline <- glmmTMB(During_NBC ~ Treatment + Order +  Mass + Clutch_size +  
                                  (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geofast)
summary(model_fast)

anova(model_fast,model_intercept_baseline) #Likelihood ratio test for baseline
anova(model_fast,model_intercept_fast) #Likelihood ratio test for treatment

residuals_fast <- simulateResiduals(fittedModel = model_fast, plot = F) #residuals
plot(residuals_fast)

emmeans(model_fast, list(pairwise~Treatment), adjust = "tukey") #Posthoc

##Replicate Figure 3A:
#Dataset for plotting:average normal begging calls by nestbox
data_geofast_plot <-  data_geofast%>%
  group_by(Treatment, Nestbox) %>%
  summarise(mean_beforeNBC = mean(Before_NBC), mean_duringNBC = mean(During_NBC))

geofast_boxplot <- ggplot(data_geofast_plot, aes(y= mean_duringNBC, x = Treatment, colour = NULL, fill = Treatment)) + 
  geom_boxplot(aes(colour = Treatment, alpha = 0.5), width = 0.5,  outlier.shape = NA) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +
scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D", "#7CAE00", "#00BFC4")) +
  geom_jitter(aes(colour = Treatment), size = 9, width = 0.1, height = 0.2, alpha = 0.5,pch = 21, colour = 'black') +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text = element_text(size = 25)) +  
  theme(axis.ticks.length=unit(.3, "cm"),  axis.title.x = element_text(vjust =-1), axis.title.y = element_text(vjust =2)) +
  labs(x = "Geographic variation in fast alarm calls", y = "Mean number of Begging calls \n per nestbox")
geofast_boxplot


#################### MODEL FOR GEO VARIATION IN SLOW CALLS ##################################

#Dataset for slow versus fast call rates on nestling responses
data_geoslow <- nestling_responses %>%
  filter(Year == '2023') %>%
  filter(Treatment != 'Fast local')

model_slow <- glmmTMB(During_NBC ~ Before_NBC+ Treatment + Order +  Mass + Clutch_size +  
                        (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geoslow) #lower AIC
model_intercept_slow <- glmmTMB(During_NBC ~ Before_NBC + Order +  Mass + Clutch_size +  
                                  (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geoslow)
model_intercept_baseline <- glmmTMB(During_NBC ~ Treatment + Order +  Mass + Clutch_size +  
                                  (1|Nestbox/Nestling_RN), family = nbinom1(), data = data_geoslow)

anova(model_slow,model_intercept_baseline) #likelihood ratio test for baseline begging
anova(model_slow,model_intercept_slow) #Likelihood ratio test for treatment

residuals_slow <- simulateResiduals(fittedModel = model_slow, plot = F) #residuals
plot(residuals_slow)

emmeans(model_slow, list(pairwise~Treatment), adjust = "tukey")

##Replicate Figure 3B:
#Dataset for plotting:average normal begging calls by nestbox
data_geoslow_plot <-  data_geoslow%>%
  group_by(Treatment, Nestbox) %>%
  summarise(mean_beforeNBC = mean(Before_NBC), mean_duringNBC = mean(During_NBC))

geoslow_boxplot <- ggplot(data_geoslow_plot, aes(y= mean_duringNBC, x = Treatment, colour = NULL, fill = Treatment)) +
  geom_boxplot(aes(colour = Treatment, alpha = 0.5), width = 0.5,  outlier.shape = NA) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c( "#7CAE00", "#00BFC4")) +  
  scale_colour_manual(values = c( "#7CAE00", "#00BFC4")) +
  geom_jitter(aes(colour = Treatment), size = 9, width = 0.1, height = 0.2, alpha = 0.5,pch = 21, colour = 'black') +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text = element_text(size = 25)) +  
  theme(axis.ticks.length=unit(.3, "cm"),  axis.title.x = element_text(vjust =-1), axis.title.y = element_text(vjust =2)) +
  labs(x = "Geographic variation in slow alarm calls", y = "Mean number of Begging calls \n per nestbox")
geoslow_boxplot

