Assignment 3 - Part 1 - Assessing voice in schizophrenia
--------------------------------------------------------

Individuals with schizophrenia (SCZ) tend to present voice
atypicalities. Their tone is described as “inappropriate” voice,
sometimes monotone, sometimes croaky. This is important for two reasons.
First, voice could constitute a direct window into cognitive, emotional
and social components of the disorder, thus providing a cheap and
relatively non-invasive way to support the diagnostic and assessment
process (via automated analyses). Second, voice atypicalities play an
important role in the social impairment experienced by individuals with
SCZ, and are thought to generate negative social judgments (of
unengaged, slow, unpleasant interlocutors), which can cascade in more
negative and less frequent social interactions.

Several studies show *significant* differences in acoustic features by
diagnosis (see meta-analysis in the readings), but we want more. We want
to know whether we can diagnose a participant only from knowing the
features of their voice.

The corpus you are asked to analyse is a relatively large set of voice
recordings from people with schizophrenia (just after first diagnosis)
and matched controls (on gender, age, education). Each participant
watched several videos of triangles moving across the screen and had to
describe them (so you have several recordings per person). We have
already extracted the pitch once every 10 milliseconds as well as
several duration related features (e.g. number of pauses, etc).

N.B. For the fun of it, I threw in data from 3 different languages: 1)
Danish (study 1-4); 2) Mandarin Chinese (Study 5-6); 3) Japanese (study
7). Feel free to only use the Danish data, if you think that Mandarin
and Japanese add too much complexity to your analysis.

In this assignment (A3), you will have to discuss a few important
questions (given the data you have). More details below.

Part 1 - Can we find a difference in acoustic features in schizophrenia?
- Describe your sample (n of studies, n of participants, age, gender,
clinical and cognitive features of the two groups) and critically assess
whether the groups (schizophrenia and controls) are balanced. N.B. you
need to take studies into account. - Discuss the analysis necessary to
replicate the meta-analytic findings: which fixed and random effects
should be included, given your dataset? E.g. what about language and
study, age and gender? Discuss also how studies and languages should
play a role in your analyses. E.g. should you analyze each study
individually? Or each language individually? Or all together? Each of
these choices makes some assumptions about how similar you expect the
studies/languages to be. - Describe the acoustic profile of a
schizophrenic voice: which features are different? E.g. People with
schizophrenia tend to have high-pitched voice, and present bigger swings
in their prosody than controls. N.B. look also at effect sizes. How do
these findings relate to the meta-analytic findings? - Your report
should look like a methods paragraph followed by a result paragraph in a
typical article (think the Communication and Cognition paper)

Part 2 - Can we diagnose schizophrenia from voice only?

-   Should you run the analysis on all studies and both languages at the
    same time?
-   Choose your best acoustic feature from part 1. How well can you
    diagnose schizophrenia just using it?
-   Identify the best combination of acoustic features to diagnose
    schizophrenia using logistic regression.
-   Discuss the “classification” process: which methods are you using?
    Which confounds should you be aware of? What are the strength and
    limitation of the analysis?

Bonus question: Logistic regression is only one of many classification
algorithms. Try using others and compare performance. Some examples:
Discriminant Function, Random Forest, Support Vector Machine, Penalized
regression, etc. The packages caret and glmnet provide them. Tidymodels
is a set of tidyverse style packages, which take some time to learn, but
provides a great workflow for machine learning.

Learning objectives
-------------------

-   Critically design, fit and report multilevel regression models in
    complex settings
-   Critically appraise issues of replication

Overview of part 1
------------------

In the course of this part 1 of Assignment 3 you have to: - combine the
different information from multiple files into one meaningful dataset
you can use for your analysis. This involves: extracting descriptors of
acoustic features from each pitch file (e.g. mean/median, standard
deviation / interquartile range), and combine them with duration and
demographic/clinical files - describe and discuss your sample - analyze
the meaningful dataset to assess whether there are indeed differences in
the schizophrenic voice and compare that to the meta-analysis

There are three pieces of data:

1- Demographic data
(<a href="https://www.dropbox.com/s/6eyukt0r5du0xif/DemoData.txt?dl=0" class="uri">https://www.dropbox.com/s/6eyukt0r5du0xif/DemoData.txt?dl=0</a>).
It contains

-   Study: a study identifier (the recordings were collected during 6
    different studies with 6 different clinical practitioners in 2
    different languages)
-   Language: Danish, Chinese and Japanese
-   Participant: a subject ID
-   Diagnosis: whether the participant has schizophrenia or is a control
-   Gender
-   Education
-   Age
-   SANS: total score of negative symptoms (including lack of
    motivation, affect, etc). Ref: Andreasen, N. C. (1989). The Scale
    for the Assessment of Negative Symptoms (SANS): conceptual and
    theoretical foundations. The British Journal of Psychiatry, 155(S7),
    49-52.
-   SAPS: total score of positive symptoms (including psychoses, such as
    delusions and hallucinations):
    <a href="http://www.bli.uzh.ch/BLI/PDF/saps.pdf" class="uri">http://www.bli.uzh.ch/BLI/PDF/saps.pdf</a>
-   VerbalIQ:
    <a href="https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale" class="uri">https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale</a>
-   NonVerbalIQ:
    <a href="https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale" class="uri">https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale</a>
-   TotalIQ:
    <a href="https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale" class="uri">https://en.wikipedia.org/wiki/Wechsler_Adult_Intelligence_Scale</a>

1.  Articulation.txt
    (<a href="https://www.dropbox.com/s/v86s6270w39g0rd/Articulation.txt?dl=0" class="uri">https://www.dropbox.com/s/v86s6270w39g0rd/Articulation.txt?dl=0</a>).
    It contains, per each file, measures of duration:

-   soundname: the name of the recording file
-   nsyll: number of syllables automatically inferred from the audio
-   npause: number of pauses automatically inferred from the audio
    (absence of human voice longer than 200 milliseconds)
-   dur (s): duration of the full recording
-   phonationtime (s): duration of the recording where speech is present
-   speechrate (nsyll/dur): average number of syllables per second
-   articulation rate (nsyll / phonationtime): average number of
    syllables per spoken second
-   ASD (speakingtime/nsyll): average syllable duration

1.  One file per recording with the fundamental frequency of speech
    extracted every 10 milliseconds (excluding pauses):
    <a href="https://www.dropbox.com/sh/b9oc743auphzxbg/AAChUsvFc6dIQSlM9eQTL53Aa?dl=0" class="uri">https://www.dropbox.com/sh/b9oc743auphzxbg/AAChUsvFc6dIQSlM9eQTL53Aa?dl=0</a>

-   time: the time at which fundamental frequency was sampled
-   f0: a measure of fundamental frequency, in Herz

NB. the filenames indicate: - Study: the study, 1-6 (1-4 in Danish, 5-6
in Mandarin Chinese) - D: the diagnosis, 0 is control, 1 is
schizophrenia - S: the subject ID (NB. some controls and schizophrenia
are matched, so there is a 101 schizophrenic and a 101 control). Also
note that study 5-6 have weird numbers and no matched participants, so
feel free to add e.g. 1000 to the participant ID in those studies. - T:
the trial, that is, the recording ID for that participant, 1-10 (note
that study 5-6 have more)

### Getting to the pitch data

You have oh so many pitch files. What you want is a neater dataset, with
one row per recording, including a bunch of meaningful descriptors of
pitch. For instance, we should include “standard” descriptors: mean,
standard deviation, range. Additionally, we should also include less
standard, but more robust ones: e.g. median, iqr, mean absoluted
deviation, coefficient of variation. The latter ones are more robust to
outliers and non-normal distributions.

Tip: Load one file (as a sample) and: - write code to extract the
descriptors - write code to extract the relevant information from the
file names (Participant, Diagnosis, Trial, Study) Only then (when
everything works) turn the code into a function and use map\_df() to
apply it to all the files. See placeholder code here for help.

``` r
# read_pitch <- function(filename){
#     # load data
#     data <- read.delim(filename)
#     # parse filename to extract study, diagnosis, subject and trial
#     df <- as.data.frame(t(unlist(regmatches(filename, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", filename)))))
#     #df <- df %>% rename(study=V1, diagnosis = V2, subj= V3, trial = V4, f=V5) %>% select(-c(f))
#     
#     # extract pitch descriptors (mean, sd, iqr, etc)
#     df$mean_frequency<-mean(data$f0)
#     df$sd_frequency<-sd(data$f0)
#     df$inner_quartil_range<-IQR(data$f0)
#     
#     # combine all this data in one dataset
#     return(df)
# }
# 
# # test it on just one file while writing the function
# #test_data = read_pitch("Pitch/Study1D0S101T1_f0.txt")
# 
# #We load all the files (we have already done this)
# listed<-list.files(pattern="*.txt")%>%purrr::map_df(read_pitch)
# 
# ### We rename the columns for the pitch data
# pitch_data <- listed %>% select(-c(V5)) %>% rename(study = V1, diagnosis = V2, subj = V3, trial = V4, f = V6) %>%select(-c(f))
# 
# #We write the csv 
# write_csv(pitch_data, "pitch_data.csv")
```

### Now you need to merge demographic/clinical, duration and pitch data

``` r
# Let's start with the demographic and clinical data
demo <- read.csv("DemographicData.csv", sep = ";")
# We will rename columns to match pitch data
demo <- demo %>% rename(subj = Participant, diagnosis = Diagnosis, study = Study)

#We change the diagnosis into numeric 
demo$diagnosis <- ifelse(demo$diagnosis == "Control", 0, 1)

# then duration data
dur <- read.delim("Data/Articulation.txt", sep = ",")

soundname <- regmatches(dur$soundname, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", dur$soundname)) 


soundname <- as.data.frame(t(stri_list2matrix(soundname)))
#soundname <- as.data.frame(soundname)
#colnames(soundname) <- unique(unlist(sapply(soundname, names)))


#We rename the columns
soundname <- soundname %>%  select(-c(V5)) %>% rename(study = V1, diagnosis = V2, subj = V3, trial = V4)

# Finally the pitch data
pitch_data <- read.csv("pitch_data.csv")

# Now we merge them
#First we merge together the duration df and the soundname df we just created
dur <- cbind(soundname, dur)
#We remove the column soundname
dur <- dur %>% rename(dur = dur..s., phonationtime=phonationtime..s., speechrate = speechrate..nsyll.dur., articulation= articulation.rate..nsyll...phonationtime., ASD_speakingtime = ASD..speakingtime.nsyll.) %>% select(-soundname)

#Now we all dataframes together
df <- merge(dur, pitch_data, all = TRUE)
```

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(101L, 101L, 101L, 101L,
    ## 101L, : invalid factor level, NA generated

    ## Warning in `[<-.factor`(`*tmp*`, ri, value = c(101L, 101L, 101L, 101L,
    ## 101L, : invalid factor level, NA generated

``` r
#Now we need to merge in the demo df
df <- merge(demo, df, all = TRUE)

#We remove study 6 and 7 which are chinese and japanese
df <- subset(df, Language == "Danish")

# Now we save them
write.csv(df, "df.csv")
```

Now we need to describe our sample
----------------------------------

First look at the missing data: we should exclude all recordings for
which we do not have complete data. We also add unique ID’s and Unique
pair ID’s

``` r
df <- read.csv("df.csv")
#We remove rows with NAs
#df <- na.omit(df) #This removes all missing values. Instead we try with complete.cases and only remove from the ones we will use
df <- df[complete.cases(df$diagnosis), ] #Only removes from one row at a time
df <- df[complete.cases(df$speechrate), ] #Only removes from one row at a time
df <- df[complete.cases(df$ASD_speakingtime), ] #Only removes from one row at a time
df <- df[complete.cases(df$inner_quartil_range), ] #Only removes from one row at a time
df <- df[complete.cases(df$dur), ] #Only removes from one row at a time
df <- df[complete.cases(df$npause), ] #Only removes from one row at a time

#This made diagnosis into a numeric variable. We want it as a factor
df$diagnosis <- as.factor(df$diagnosis)

### We will make unique pairs and unique IDs for participants (because controls have same ID as skitzophrenic)
df <- df %>% mutate(uPairID = paste(subj, study, sep = "_"), #Create unique pair ID
                    uPairID = as.numeric(as.factor(uPairID)), 
                    uID = paste(subj, study, diagnosis, sep = "_"), #Create unique ID
                    uID = as.numeric(as.factor(uID)))

## We rename these columns
#df <- df %>% rename(uPairID = `uPairID <- as.numeric(as.factor(uPairID))`, PairID = `uPairID <- paste(subj, study, sep = "_")`, ID = `uID <- paste(subj, study, diagnosis, sep = "_")`, uID = `uID <- as.numeric(as.factor(uID))`)

df %>% select(uPairID, study) %>% unique() %>% group_by(uPairID) %>% summarise(n()) #Check whether they all say 1
```

    ## # A tibble: 122 x 2
    ##    uPairID `n()`
    ##      <dbl> <int>
    ##  1       1     1
    ##  2       2     1
    ##  3       3     1
    ##  4       4     1
    ##  5       5     1
    ##  6       6     1
    ##  7       7     1
    ##  8       8     1
    ##  9       9     1
    ## 10      10     1
    ## # ... with 112 more rows

Then count the participants and recordinsgs by diagnosis, report their
gender, age and symptom severity (SANS, SAPS and Social) Finally, do the
same by diagnosis and study, to assess systematic differences in
studies. I like to use group\_by() %&gt;% summarize() for quick
summaries

``` r
#We make the first summarised version with the symptom severity
#### How to also fix gender but might exclude some data
desc_stats <- df %>% select(uID, Gender, Age, SANS, SAPS, diagnosis) 
desc_stats <- unique(desc_stats)

desc_stats %>% group_by(diagnosis) %>% dplyr::summarise(N = length(unique(uID)), MeanAge = round(mean(Age, na.rm=TRUE), 2), SDAge = round(sd(Age, na.rm = TRUE), 2), FemaleN=sum(Gender=="F"), Mean_SANS= round(mean(SANS, na.rm=TRUE), 2), Mean_SAPS= round(mean(SAPS, na.rm=TRUE), 2))
```

    ## # A tibble: 2 x 7
    ##   diagnosis     N MeanAge SDAge FemaleN Mean_SANS Mean_SAPS
    ##   <fct>     <int>   <dbl> <dbl>   <int>     <dbl>     <dbl>
    ## 1 0           116    26.7  9.19      50      0.42      0.08
    ## 2 1           105    26.7  9.02      45      9.69     10.4

``` r
#### Systematic differences in studies
#We count the number of trial for each study for ASD and TD
desc_stats_2 <- df %>% select(uID, Gender, Age, SANS, SAPS, diagnosis, study) 
desc_stats_2 <- unique(desc_stats_2)

desc_stats_2 %>% group_by(diagnosis, study) %>% summarise(N = length(unique(uID)), MeanAge = round(mean(Age, na.rm=TRUE), 2), SDAge = round(sd(Age, na.rm = TRUE), 2), FemaleN=sum(Gender=="F"), Mean_SANS= round(mean(SANS, na.rm=TRUE), 2), Mean_SAPS= round(mean(SAPS, na.rm=TRUE), 2))
```

    ## # A tibble: 8 x 8
    ## # Groups:   diagnosis [2]
    ##   diagnosis study     N MeanAge SDAge FemaleN Mean_SANS Mean_SAPS
    ##   <fct>     <int> <int>   <dbl> <dbl>   <int>     <dbl>     <dbl>
    ## 1 0             1    36    22.7  3.19      17      0         0   
    ## 2 0             2    23    23.6  3.61       7      0         0   
    ## 3 0             3    28    37.5 13.1       13    NaN       NaN   
    ## 4 0             4    29    24.4  4.58      13      1.28      0.24
    ## 5 1             1    34    22.8  3.13      16     10.3      10.4 
    ## 6 1             2    23    23.4  3.94       6     10.0      14.5 
    ## 7 1             3    19    40.8 12.4       11    NaN       NaN   
    ## 8 1             4    29    24.8  3.66      12      8.66      7.03

``` r
#We see that study 3 has NAs for the SANS and SAPS
```

Now we can analyze the data
---------------------------

If we look at the meta-analysis, there are differences (measured as
Hedges’ g, very close to Cohen’s d, that is, in standard deviations) in
- pitch variability (lower, Hedges’ g: -0.55, 95% CIs: -1.06, 0.09)
inner\_quartil\_range –&gt; differences in lower and higher pitches in
the inner quartile

-   proportion of spoken time (lower, Hedges’ g: -1.26, 95% CIs: -2.26,
    0.25) ASD\_speakingtime (speakingtime/nsyll): average syllable
    duration

-   speech rate (slower, Hedges’ g: -0.75, 95% CIs: -1.51, 0.04)
    speechrate (nsyll/dur): average number of syllables per second

-   pause duration (longer, Hedges’ g: 1.89, 95% CIs: 0.72, 3.21).
    (Duration - Spoken Duration) / PauseN

We need therefore to set up 4 models to replicate the findings. Feel
free of course to test more features.

N.B. the meta-analytic findings are on scaled measures. If you want to
compare your results with them, you need to scale your measures as well:
subtract the mean, and divide by the standard deviation. N.N.B. We want
to think carefully about fixed and random effects in our model. In
particular: how should study be included? Does it make sense to have all
studies put together? Does it make sense to analyze both languages
together? Relatedly: does it make sense to scale all data from all
studies together? N.N.N.B. If you want to estimate the studies
separately, you can try this syntax: Feature \~ 0 + Study +
Study:Diagnosis + \[your randomEffects\]. Now you’ll have an intercept
per each study (the estimates for the controls) and an effect of
diagnosis per each study

-   Bonus points: cross-validate the models and report the betas and
    standard errors from all rounds to get an idea of how robust the
    estimates are.

``` r
## We will set up 4 models to replicate the findings
#We will scale the variables
iqr <- scale(df$inner_quartil_range)
speakingtime <- scale(as.numeric(df$ASD_speakingtime))
speechrate <- scale(df$speechrate)

df$ASD_speakingtime <- as.numeric(as.character(df$ASD_speakingtime))

#we calculate pause duration
df$pause_dur <- (df$dur-df$ASD_speakingtime)/df$npause

df$pause_dur <- ifelse(df$pause_dur == "Inf", 0, df$pause_dur)

pause_dur <- scale(df$pause_dur)

#We save the df for part 2
write.csv(df, "df_2.csv")

## We will make models with uPairID and ID 

model_iqr <- lmer(iqr ~ diagnosis + (1 + diagnosis|uPairID), data = df, REML = F)
#This makes us able to see the two studies each 
summary(model_iqr)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: iqr ~ diagnosis + (1 + diagnosis | uPairID)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4437.0   4470.3  -2212.5   4425.0     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5564 -0.2575 -0.1108  0.0149 14.3060 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.9574   0.9785        
    ##           diagnosis1  1.0517   1.0255   -0.99
    ##  Residual             0.4985   0.7060        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  0.13879    0.09359   1.483
    ## diagnosis1  -0.26907    0.10056  -2.676
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## diagnosis1 -0.962

``` r
#We try with the unique ID
model_2 <- lmer(iqr ~ diagnosis + (1|uID), data = df, REML = F)
summary(model_2)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: iqr ~ diagnosis + (1 | uID)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4572.0   4594.2  -2282.0   4564.0     1892 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0739 -0.1844 -0.0751  0.0202 13.0747 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  uID      (Intercept) 0.5114   0.7151  
    ##  Residual             0.4987   0.7062  
    ## Number of obs: 1896, groups:  uID, 221
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  0.13600    0.07014   1.939
    ## diagnosis1  -0.26631    0.10175  -2.617
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## diagnosis1 -0.689

``` r
anova(model_iqr, model_2) 
```

    ## Data: df
    ## Models:
    ## model_2: iqr ~ diagnosis + (1 | uID)
    ## model_iqr: iqr ~ diagnosis + (1 + diagnosis | uPairID)
    ##           Df  AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
    ## model_2    4 4572 4594.2 -2282.0     4564                             
    ## model_iqr  6 4437 4470.3 -2212.5     4425 139.02      2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#The model with unique pairs is significantly better than the model with unique ID. We will continue with this

#We will now make a model for speakingtime

model_speakingtime <- lmer(speakingtime ~ diagnosis + (1 + diagnosis|uPairID), data = df, REML = F)
summary(model_speakingtime)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: speakingtime ~ diagnosis + (1 + diagnosis | uPairID)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5113.6   5146.9  -2550.8   5101.6     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3042 -0.4361 -0.1058  0.2559 14.3053 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.2411   0.4911        
    ##           diagnosis1  0.3999   0.6324   -0.61
    ##  Residual             0.7371   0.8586        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept) -0.11563    0.05318  -2.175
    ## diagnosis1   0.22976    0.07282   3.155
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## diagnosis1 -0.618

``` r
#Here we see that only study 2 shows significantly different results

#We will do for speechrate
model_speechrate <- lmer(speechrate ~ diagnosis + (1 + diagnosis|uPairID), data = df, REML = F)
summary(model_speechrate)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: speechrate ~ diagnosis + (1 + diagnosis | uPairID)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   3846.4   3879.7  -1917.2   3834.4     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5038 -0.5846 -0.0106  0.5579  4.2481 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 0.2762   0.5256        
    ##           diagnosis1  0.4834   0.6953   -0.57
    ##  Residual             0.3452   0.5875        
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  3.17680    0.05225  60.805
    ## diagnosis1  -0.25153    0.07254  -3.468
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## diagnosis1 -0.574

``` r
#We will do for pause duration
model_pause_dur <- lmer(pause_dur ~ diagnosis + (1 + diagnosis|uPairID), data = df, REML = F)

summary(model_pause_dur)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: pause_dur ~ diagnosis + (1 + diagnosis | uPairID)
    ##    Data: df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8980.8   9014.0  -4484.4   8968.8     1890 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -7.0798 -0.2402 -0.0758  0.1398 12.2652 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  uPairID  (Intercept) 2.520    1.588         
    ##           diagnosis1  4.301    2.074    -0.57
    ##  Residual             5.472    2.339         
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   2.7614     0.1651  16.721
    ## diagnosis1   -0.1236     0.2281  -0.542
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## diagnosis1 -0.585

For assignment A3\_P2

Describe the acoustic profile of a schizophrenic voice *Note* in this
section you need to describe the acoustic profile of a schizophrenic
voice and compare it with the meta-analytic findings (see 2 and 3 in
overview of part 1).

See the written report:
<a href="https://docs.google.com/document/d/11DQqMdC33JOjShAPvn6snAsEmwdoEiS3LJycVRIX0Oc/edit?usp=sharing" class="uri">https://docs.google.com/document/d/11DQqMdC33JOjShAPvn6snAsEmwdoEiS3LJycVRIX0Oc/edit?usp=sharing</a>
