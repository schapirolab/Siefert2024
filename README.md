# Siefert2024

README for Siefert2024 behavioral data and code. Corresponds to Siefert et al. (2024): https://doi.org/10.1523/JNEUROSCI.0022-24.2024
Contact Liz Siefert (sieferte@pennmedicine.upenn.edu) if you have further questions.

**```Siefert2024_Analysis.R``` generates all figures and analyses.** Uses the following csv data files: ```Siefert2024_behavdata.csv```, ```learning_data.csv```, ```cluster_array.csv```, ```sleep_information.csv```, ```cueing_stages.csv```. See below for descriptions of each data file.

## Data details

### Siefert2024_behavdata.csv
Trial level data from test, with some additional information added. This is the main data frame that the analyses are based on.

**SubNum**: subject number\
**TrialNum**: trial number\
**SatNum**: satellite number\
	- Satellites 1-5 were the studied satellites from the blocked category. 16-19 were the novel satellites in this category.\
	- Satellites 6-10 were the studied satellites from the interleaved category. 20-23 were the novel satellites in this category.\
	- Satellites 11-15 were the studied satellites from the uncued category. 24-27 were the novel satellites in this category.\
**FirstOrSecondPart**: whether the trial is testing the first or second part that is covered up, 1 = first part, 2 = second part\
**Accuracy**: 1 = correct, 0 = incorrect\
**WhichPart**: part being tested in that trial. 1 = head, 2 = back, 3 = tail, 4 = back legs, 5 = front legs.\
**IsUnique**: 1 = unique feature is queried, 0 = shared feature is queried\
**Confidence**: confidence in decision, ranging from 1 = very unsure to 5 = very sure, 0 indicates no confidence response\
**NumberofTimesCued**: number of times an individual item was cued\
**IsNovel**: if that item is novel or not\
**SessionNum**: 1 = prenap; 2 = postnap.\
**TestType**: indicates whether the trial tested a shared feature from a studied satellite, a unique feature from a studied satellite, or a feature from a novel satellite (all parts queried on novel satellites were shared features).\
**CueStyle**: indicates whether that item was cued in interleaved order, blocked order, or left uncued\
**Cued**: -1 = uncued, 1 = cued

### learning_data.csv
Trial level data from the learning blocks.

**SubNum**: subject number\
**TrialNum**: trial number\
**SatNum**: satellite number\
**BlockNum**: learning block\
**Accuracy**: accuracy on that trial (1 = correct; 0 = incorrect; accuracy on first answer/before follow-up corrections implemented)\
**WhichPart**: which part was queried\
**IsUnique**: 1 = unique feature is queried, 0 = shared feature is queried

### cluster_array.csv
Power in each cluster for each item within each participant.

**SubNum**: subject number\
**SatNum**: satellite number\
**Clust1**: power in cluster 1\
**Clust2**: power in cluster 2

### sleep_information.csv
Basic sleep statistics for each participant. Here, time is measured in epochs. Each epoch is 30s.

**SubNum**: subject number\
**InBed**: total epochs spent in bed\
**TotalSleep**: total epochs spent sleeping (N1, N2, N3, REM)\
**W**: epochs spent in wake\
**N1**: epochs spent in N1\
**N2**: epochs spent in N2\
**N3**: epochs spent in N3\
**REM**: epochs spent in REM\
**MVT**: epochs spent moving (epochs scored as movement, and not as any other sleep stage)\
**SleepFrag**: sleep fragmentation index, based on Whitmore et al. (2022; https://doi.org/10.1038/s41539-021-00119-2), Haba-Rubio et al. (2004; https://doi.org/10.1016/j.sleep.2004.06.007)

### cueing_stages.csv

Number of times an individual item was cued in N2, N3, N1, or W, for each item within each participant.

**SubNum**: subject number\
**SatNum**: satellite number\
**N2**: number of cues delivered in N2 sleep\
**N3**: number of cues delivered in N3 sleep\
**N1**: number of cues delivered in N1 sleep\
**W**: number of cues delivered in W\
*no cues were ever delivered in epochs marked as REM\
*all cues marked in epochs of N1 or wake were followed up and confirmed to occur during N2 or N3 and just prior to a change in stage to N1 or wake. Analyses on effects of sleep stage were performed with these cueing edge cases excluded.
