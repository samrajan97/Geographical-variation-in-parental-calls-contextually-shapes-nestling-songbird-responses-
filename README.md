# Geographic_variation_calls_nestling_responses

# Overview
This repository hosts the code and data required to replicate the results in the MS 'To be or not to be alarmed? Pied flycatcher nestlings show context-dependent discrimination of within-species call variation'

## Table of Contents
- [Data and File overview](#data-and-file-overview)
- [File specific information](#file-specific-information)

## Data and File overview

This repository contains the following:
1. Data: 
- `data_call_mds.xlsx`
- `data_nestling_responses.xlsx`
- `data_interobserver_reliability.xlsx`
2. Codes:
- `Code_alarmcall_geographic_variation.R`
- `Code_nestling_responses.R`
- `Code_interobserver_reliability.R`

Description of files and relationship between files:
1. The dataset `data_alarm_mds.xlsx` was used for the analysis of geographic variation in calls of the pied flycatcher and analyses can be reproduced using the R script `Code_alarmcall_geographic_variation.R`. The code can also be used to replicate Figure 1 and Supplementary Table 1, 4, 5 and 6. 
2. The dataset `data_nestling_responses.xlsx` was used for the analysis of nestling responses of within-species call variation in alarm and non-alarm contexts and can be reproduced using the R script `Code_nestling_responses.R` The code can also be used to replicae Figure 2 and Figure 3 and Supplementary Tables 2 and 3. 
3. The dataset `data_interobserver_reliability.xlsx` was used for the analysis of interobserver reliability of behavioral counts during the baseline silence period and during the call playbacks and analysis can  be reproduced using the R script `Code_interobserver_reliability.R`.  

Instrument- or software-specific information needed to interpret the data: 
R v.4.2.0 https://www.r-project.org/

## File specific information
1) data_call_mds.xlsx
Number of variables: 13

Number of cases/rows: 798

1. Variable List:
- "Population" = Population where calls were recorded
- "Individual" = Individual identity to whom calls belong to 
- "Recording" = Recording from which calls were extracted 
- "pc_1_value" = First principal component scores for each call
- "pc_2_value" = Second principal component scores for each call
- "pc_3_value" = Third principal component scores for each call
- "pc_4_value" = Fourth principal component scores for each call
- "pc_5_value" = Fifth principal component scores for each call
- "pc_6_value" = Sixth principal component scores for each call
- "pc_7_value" = Seventh principal component scores for each call
- "pc_8_value" = Eighth principal component scores for each call
- "pc_9_value" = Ninth principal component scores for each call
- "pc_10_value" = Tenth principal component scores for each call

Missing data codes: NA

2) data_nestling_responses.xlsx
Number of variables: 21

Number of cases/rows: 784

1. Variable List:

- "Year" = Year of experimemnt
- "Nestbox" = Clutch that nestlings belong to
- "Date" = Experiment date
- "DVR" = Video camera used to record behavior
- "Time" = Time of experiment
- "Nestling_RN" = Nestling identity
- "Marker" = Visual marking to track nestling in video
- "Mass" = Measured mass on experiment day
- "Treatment" = Call playback treatment. Five levels: Fast local, fast foreign, fast heterospecific, slow local and slow foreign
- "Order" = Playback order. Two levels: first and second 
- "Playback_male" = Identity of male used in the call playback
- "Playback_ID" = Identity of call playback 
- "Clutch_size" = Number of nestlings in the recording
- "Before_NBC" = No. of Normal begging calls during baseline silence period
- "Before_HIBC" = No of. High intensity begging calls during baseline silence period
- "Before_gape" = No of. gapes during baseline silence period
- "Before_lookup" = No of. looking up behaviour during baseline silence period
- "During_NBC" = No. of Normal begging calls during call playback
- "During_HIBC" = No. of High intensity begging calls during call playback
- "During_gape" = No. of gapes during call playback
- "During_lookup" = No. of looking up behavior during call playback

Missing data codes: NA

3) data_interobserver_reliability.xlsx
Number of variables: 18

Number of cases/rows: 62

1. Variable List:

- "Nestbox" = Clutch that nestlings belong to
- "Nestling_RN" = Nestling identity
- Obs1_before_NBC: Observer 1 counts of normal begging calls during baseline silence
- Obs1_during_NBC: Observer 1 counts of normal begging calls during call playback
- Obs2_before_NBC: Observer 2 counts of normal begging calls during baseline silence
- Obs2_during_NBC: Observer 2 counts of normal begging calls during call playback
- Obs1_before_HIBC: Observer 1 counts of high intensity begging calls during baseline silence
- Obs1_during_HIBC: Observer 1 counts of high intensity begging calls during call playback
- Obs2_before_HIBC: Observer 2 counts of high intensity begging calls during baseline silence
- Obs2_during_HIBC: Observer 2 counts of high intensity begging calls during call playback
- Obs1_before_lookup: Observer 1 counts of look ups during baseline silence
- Obs1_before_lookup: Observer 1 counts of look ups during call playback
- Obs2_before_lookup: Observer 2 counts of look ups during baseline silence
- Obs2_during_lookup: Observer 2 counts of look ups during call playback
- Obs1_before_gape: Observer 1 counts of gapes during baseline silence
- Obs1_before_gape: Observer 1 counts of gapes during call playback
- Obs2_before_gape: Observer 2 counts of gapes during baseline silence
- Obs2_during_gape: Observer 2 counts of gapes during call playback
