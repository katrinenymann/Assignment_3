Assignment 3 - Part 2 - Diagnosing schizophrenia from voice
-----------------------------------------------------------

In the previous part of the assignment you generated a bunch of
“features”, that is, of quantitative descriptors of voice in
schizophrenia. We then looked at whether we could replicate results from
the previous literature. We now want to know whether we can
automatically diagnose schizophrenia from voice only, that is, relying
on the set of features you produced last time, we will try to produce an
automated classifier. Again, remember that the dataset containst 7
studies and 3 languages. Feel free to only include Danish (Study 1-4) if
you feel that adds too much complexity.

Issues to be discussed your report: - Should you run the analysis on all
languages/studies at the same time? - Choose your best acoustic feature
from part 1. How well can you diagnose schizophrenia just using it? -
Identify the best combination of acoustic features to diagnose
schizophrenia using logistic regression. - Discuss the “classification”
process: which methods are you using? Which confounds should you be
aware of? What are the strength and limitation of the analysis? - Bonus
question: Logistic regression is only one of many classification
algorithms. Try using others and compare performance. Some examples:
Discriminant Function, Random Forest, Support Vector Machine, etc. The
package caret provides them. - Bonus Bonus question: It is possible
combine the output of multiple classification models to improve
classification accuracy. For inspiration see,
<a href="https://machinelearningmastery.com/machine-learning-ensembles-with-r/" class="uri">https://machinelearningmastery.com/machine-learning-ensembles-with-r/</a>
The interested reader might also want to look up ‘The BigChaos Solution
to the Netflix Grand Prize’

Learning objectives
-------------------

-   Learn the basics of classification in a machine learning framework
-   Design, fit and report logistic regressions
-   Apply feature selection techniques

### Let’s start

We first want to build a logistic regression to see whether you can
diagnose schizophrenia from your best acoustic feature. Let’s use the
full dataset and calculate the different performance measures (accuracy,
sensitivity, specificity, PPV, NPV, ROC curve). You need to think
carefully as to how we should (or not) use study and subject ID.

Then cross-validate the logistic regression and re-calculate performance
on the testing folds. N.B. The cross-validation functions you already
have should be tweaked: you need to calculate these new performance
measures. Alternatively, the groupdata2 and cvms package created by
Ludvig are an easy solution.

N.B. the predict() function generates log odds (the full scale between
minus and plus infinity). Log odds &gt; 0 indicates a choice of 1, below
a choice of 0.

1.  We first want to build a logistic regression to see whether you can
    diagnose schizophrenia from your best acoustic feature.

``` r
#We read the dataset from part 1
df_2 <- read.csv("df_2.csv")
df_2 <- df_2 %>% 
  rename(
    pitch = inner_quartil_range
    )
#We scale variables
df_2$iqr_s <- scale(df_2$pitch)
df_2$speakingtime_s <- scale(as.numeric(df_2$ASD_speakingtime))
df_2$speechrate_s <- scale(df_2$speechrate)

df_2$ASD_speakingtime <- as.numeric(as.character(df_2$ASD_speakingtime))

#we calculate pause duration
df_2$pause_dur <- (df_2$dur-df_2$ASD_speakingtime)/df_2$npause

df_2$pause_dur <- ifelse(df_2$pause_dur == "Inf", 0, df_2$pause_dur)

df_2$pause_dur <- scale(df_2$pause_dur)

#As we had the speechrate feature produce the best model in part 1, we will use this feature for the logistic regression

diagnose <- glmer(diagnosis ~ speechrate_s + (1|uPairID), data = df_2, family = binomial, control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))

summary(diagnose)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: diagnosis ~ speechrate_s + (1 | uPairID)
    ##    Data: df_2
    ## Control: glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2589.5   2606.2  -1291.8   2583.5     1893 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4724 -0.9379 -0.7568  1.0136  1.9766 
    ## 
    ## Random effects:
    ##  Groups  Name        Variance Std.Dev.
    ##  uPairID (Intercept) 0        0       
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.09676    0.04648  -2.082   0.0374 *  
    ## speechrate_s -0.29826    0.04761  -6.264 3.75e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## speechrat_s 0.011

Let’s use the full dataset and calculate the different performance
measures (accuracy, sensitivity, specificity, PPV, NPV, ROC curve). You
need to think carefully as to how we should (or not) use study and
subject ID.

``` r
#We will make a confusion matrix

#First we predict values with our diagnose model
df_2$PredictionsPerc=predict(diagnose)
#We categorize into either diagnosis 1 or 0
df_2$Predictions[df_2$PredictionsPerc>0]=1
df_2$Predictions[df_2$PredictionsPerc<=0]=0

#We change into factors
df_2$Predictions <- as.factor(df_2$Predictions)
df_2$diagnosis <- as.factor(df_2$diagnosis)

#We make a confusion Matrix
confusionMatrix(data = df_2$Predictions, reference = df_2$diagnosis, positive = "1") 
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 695 517
    ##          1 298 386
    ##                                           
    ##                Accuracy : 0.5701          
    ##                  95% CI : (0.5475, 0.5926)
    ##     No Information Rate : 0.5237          
    ##     P-Value [Acc > NIR] : 2.767e-05       
    ##                                           
    ##                   Kappa : 0.1288          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.237e-14       
    ##                                           
    ##             Sensitivity : 0.4275          
    ##             Specificity : 0.6999          
    ##          Pos Pred Value : 0.5643          
    ##          Neg Pred Value : 0.5734          
    ##              Prevalence : 0.4763          
    ##          Detection Rate : 0.2036          
    ##    Detection Prevalence : 0.3608          
    ##       Balanced Accuracy : 0.5637          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
#We change into factors again
df_2$Predictions <- as.factor(df_2$Predictions)
df_2$diagnosis <- as.factor(df_2$diagnosis)

#We make a roc Curve
rocCurve <- roc(response = df_2$diagnosis, predictor = df_2$PredictionsPerc)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
rocCurve
```

    ## 
    ## Call:
    ## roc.default(response = df_2$diagnosis, predictor = df_2$PredictionsPerc)
    ## 
    ## Data: df_2$PredictionsPerc in 993 controls (df_2$diagnosis 0) < 903 cases (df_2$diagnosis 1).
    ## Area under the curve: 0.5855

``` r
#We plot the curve
plot(rocCurve, main = "ROC Curve")
```

![](A3_P2_DiagnosingSchizophrenia_instructions_files/figure-markdown_github/unnamed-chunk-2-1.png)
We try with another model with more features

``` r
#We add inner quartile range
diagnose_2 <- glmer(diagnosis ~ speechrate_s + iqr_s + (1|uPairID), data = df_2, family = binomial, control = glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE))

summary(diagnose_2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: diagnosis ~ speechrate_s + iqr_s + (1 | uPairID)
    ##    Data: df_2
    ## Control: glmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2557.6   2579.7  -1274.8   2549.6     1892 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5976 -0.9392 -0.5799  0.9889  4.2299 
    ## 
    ## Random effects:
    ##  Groups  Name        Variance Std.Dev.
    ##  uPairID (Intercept) 0        0       
    ## Number of obs: 1896, groups:  uPairID, 122
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.10817    0.04718  -2.293   0.0219 *  
    ## speechrate_s -0.30220    0.04819  -6.270 3.60e-10 ***
    ## iqr_s        -0.32789    0.06685  -4.905 9.34e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) spchr_
    ## speechrat_s 0.007        
    ## iqr_s       0.115  0.002

``` r
#We make a confusion Matrix
#First we predict values with our diagnose model
df_2$PredictionsPerc2=predict(diagnose_2)
#We categorize into either diagnosis 1 or 0
df_2$Predictions2[df_2$PredictionsPerc2>0]=1
df_2$Predictions2[df_2$PredictionsPerc2<=0]=0
#We produce the confusion matrix
df_2$Predictions2 <- as.factor(df_2$Predictions2)
df_2$diagnosis <- as.factor(df_2$diagnosis)
confusionMatrix(data = df_2$Predictions2, reference = df_2$diagnosis, positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 654 453
    ##          1 339 450
    ##                                           
    ##                Accuracy : 0.5823          
    ##                  95% CI : (0.5597, 0.6046)
    ##     No Information Rate : 0.5237          
    ##     P-Value [Acc > NIR] : 1.736e-07       
    ##                                           
    ##                   Kappa : 0.1579          
    ##                                           
    ##  Mcnemar's Test P-Value : 5.938e-05       
    ##                                           
    ##             Sensitivity : 0.4983          
    ##             Specificity : 0.6586          
    ##          Pos Pred Value : 0.5703          
    ##          Neg Pred Value : 0.5908          
    ##              Prevalence : 0.4763          
    ##          Detection Rate : 0.2373          
    ##    Detection Prevalence : 0.4161          
    ##       Balanced Accuracy : 0.5785          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
rocCurve2 <- roc(response = df_2$diagnosis, predictor = df_2$PredictionsPerc2)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
rocCurve2
```

    ## 
    ## Call:
    ## roc.default(response = df_2$diagnosis, predictor = df_2$PredictionsPerc2)
    ## 
    ## Data: df_2$PredictionsPerc2 in 993 controls (df_2$diagnosis 0) < 903 cases (df_2$diagnosis 1).
    ## Area under the curve: 0.6116

Then cross-validate the logistic regression and re-calculate performance
on the testing folds. N.B. The cross-validation functions you already
have should be tweaked: you need to calculate these new performance
measures. Alternatively, the groupdata2 and cvms package created by
Ludvig are an easy solution.

N.N.B. you need to decide whether calculate performance on each single
test fold or save all the prediction for test folds in one datase, so to
calculate overall performance. N.N.N.B. Now you have two levels of
structure: subject and study. Should this impact your cross-validation?
N.N.N.N.B. A more advanced solution could rely on the tidymodels set of
packages (warning: Time-consuming to learn as the documentation is
sparse, but totally worth it)

``` r
# Set seed for reproducibility
set.seed(7)

# Fold data 
data <- fold(df_2, k = 4,
             cat_col = 'diagnosis',
             id_col = 'uID') %>% 
  arrange(.folds)

# Show first 15 rows of data
data %>% head(15) %>% kable()
```

|  uID|  X.1|    X|  study| diagnosis |  subj| Language | Gender |  Age|  Education|  SANS|  SAPS|  VerbalIQ|  NonVerbalIQ|  TotalIQ|  trial|  nsyll|  npause|    dur|  phonationtime|  speechrate|  articulation|  ASD\_speakingtime|  mean\_frequency|  sd\_frequency|    pitch|  uPairID|  pause\_dur|      iqr\_s|  speakingtime\_s|  speechrate\_s|  PredictionsPerc| Predictions |  PredictionsPerc2| Predictions2 | .folds |
|----:|----:|----:|------:|:----------|-----:|:---------|:-------|----:|----------:|-----:|-----:|---------:|------------:|--------:|------:|------:|-------:|------:|--------------:|-----------:|-------------:|------------------:|----------------:|--------------:|--------:|--------:|-----------:|-----------:|----------------:|--------------:|----------------:|:------------|-----------------:|:-------------|:-------|
|   11|   61|   61|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      3|     12|       1|   4.10|           2.90|        2.93|          4.14|              0.241|         236.8697|       15.04718|  23.5375|        7|   0.4140054|  -0.1964717|        0.6943851|     -0.1513853|       -0.0516055| 0           |         0.0020028| 1            | 1      |
|   11|   62|   62|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      2|     42|       5|  12.30|           7.59|        3.41|          5.53|              0.181|         238.9829|       18.16149|  18.8900|        7|  -0.0899533|  -0.2661211|       -0.6717601|      0.4363823|       -0.2269144| 0           |        -0.1527807| 0            | 1      |
|   11|   63|   63|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      6|     40|       5|  11.40|           7.22|        3.51|          5.54|              0.180|         231.5583|       15.30891|  17.6300|        7|  -0.1530885|  -0.2850040|       -0.6945292|      0.5588339|       -0.2634371| 0           |        -0.1835936| 0            | 1      |
|   11|   64|   64|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|     10|     21|       1|   4.70|           2.91|        4.47|          7.21|              0.139|         239.8501|       10.83512|  16.6425|        7|   0.6605069|  -0.2998030|       -1.6280617|      1.7343691|       -0.6140550| 0           |        -0.5339826| 0            | 1      |
|   11|   65|   65|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      4|     46|       6|  14.80|           9.62|        3.11|          4.78|              0.209|         238.1923|       16.17526|  20.2875|        7|  -0.0871324|  -0.2451775|       -0.0342257|      0.0690276|       -0.1173463| 0           |        -0.0486349| 0            | 1      |
|   11|   66|   66|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      5|     18|       3|   6.40|           3.81|        2.81|          4.73|              0.212|         230.1597|       16.69764|  27.7700|        7|  -0.2167622|  -0.1330417|        0.0340816|     -0.2983272|       -0.0077783| 0           |         0.0256101| 1            | 1      |
|   11|   67|   67|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      1|     17|       0|   4.90|           3.75|        3.47|          4.54|              0.220|         245.0297|       22.16223|  30.9025|        7|  -0.9410506|  -0.0860967|        0.2162343|      0.5098532|       -0.2488280| 0           |        -0.2340111| 0            | 1      |
|   11|   68|   68|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      8|     24|       4|   8.30|           3.81|        2.89|          6.30|              0.159|         226.8401|       17.77909|  19.4150|        7|  -0.2263894|  -0.2582532|       -1.1726800|     -0.2003659|       -0.0369964| 0           |         0.0370620| 1            | 1      |
|   11|   69|   69|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      9|     57|       4|  13.10|           9.62|        4.35|          5.93|              0.169|         227.8968|       18.15233|  19.3350|        7|   0.1941029|  -0.2594521|       -0.9449891|      1.5874272|       -0.5702278| 0           |        -0.5028080| 0            | 1      |
|   11|   70|   70|      1| 0         |   107| Danish   | F      |   20|         13|     0|     0|        97|           93|       95|      7|     30|       4|   8.30|           4.91|        3.61|          6.11|              0.164|         228.7382|       20.14694|  21.4550|        7|  -0.2268283|  -0.2276809|       -1.0588345|      0.6812855|       -0.2999598| 0           |        -0.2393934| 0            | 1      |
|   15|   81|   81|      1| 0         |   109| Danish   | F      |   22|         13|     0|     0|        97|          100|       98|      3|     30|       4|   7.30|           5.44|        4.11|          5.51|              0.181|         240.2321|       38.79666|  59.1400|        9|  -0.3161061|   0.3370823|       -0.6717601|      1.2935434|       -0.4825733| 0           |        -0.6095939| 0            | 1      |
|   15|   82|   82|      1| 0         |   109| Danish   | F      |   22|         13|     0|     0|        97|          100|       98|      1|     34|       4|   9.98|           6.68|        3.41|          5.09|              0.196|         237.9441|       37.59155|  45.3400|        9|  -0.0821579|   0.1302697|       -0.3302238|      0.4363823|       -0.2269144| 0           |        -0.2827524| 0            | 1      |
|   15|   83|   83|      1| 0         |   109| Danish   | F      |   22|         13|     0|     0|        97|          100|       98|      2|     34|       5|   9.60|           5.92|        3.54|          5.74|              0.174|         243.3669|       39.78969|  57.0700|        9|  -0.2790782|   0.3060604|       -0.8311437|      0.5955694|       -0.2743939| 0           |        -0.3884976| 0            | 1      |
|   23|  111|  111|      1| 0         |   113| Danish   | F      |   18|         12|     0|     0|        84|           79|       80|      1|     16|       1|   5.00|           3.84|        3.20|          4.17|              0.240|         197.6107|       17.58817|  28.1325|       13|   0.7303841|  -0.1276091|        0.6716160|      0.1792340|       -0.1502168| 0           |        -0.1204880| 0            | 1      |
|   23|  112|  112|      1| 0         |   113| Danish   | F      |   18|         12|     0|     0|        84|           79|       80|      4|     21|       2|   7.76|           4.80|        2.70|          4.37|              0.229|         186.0033|       21.48357|  25.6900|       13|   0.3811736|  -0.1642134|        0.4211560|     -0.4330239|        0.0323967| 1           |         0.0765357| 1            | 1      |

``` r
### Now we cross validate

CV <- cross_validate(data, "diagnosis~speechrate_s", 
                     fold_cols = '.folds', 
                     family='binomial')

# Show results
CV
```

    ## # A tibble: 1 x 28
    ##   `Balanced Accur~    F1 Sensitivity Specificity `Pos Pred Value`
    ##              <dbl> <dbl>       <dbl>       <dbl>            <dbl>
    ## 1            0.563 0.488       0.432       0.694            0.562
    ## # ... with 23 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
    ## #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
    ## #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
    ## #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>,
    ## #   Coefficients <list>, Folds <int>, `Fold Columns` <int>, `Convergence
    ## #   Warnings` <int>, `Singular Fit Messages` <int>, `Other
    ## #   Warnings` <int>, `Warnings and Messages` <list>, Family <chr>,
    ## #   Link <chr>, Dependent <chr>, Fixed <chr>

``` r
#We get an average accuracy of 56% 

# We take out the results metrics
CV %>% select(1:9) %>% kable()
```

|  Balanced Accuracy|         F1|  Sensitivity|  Specificity|  Pos Pred Value|  Neg Pred Value|        AUC|   Lower CI|  Upper CI|
|------------------:|----------:|------------:|------------:|---------------:|---------------:|----------:|----------:|---------:|
|          0.5628753|  0.4884158|    0.4318937|     0.693857|       0.5619597|       0.5732113|  0.5818598|  0.5561477|  0.607572|

``` r
# Confusion matrix
CV$`Confusion Matrix`[[1]] %>% kable()
```

| Fold Column | Prediction | Target | Pos\_0 | Pos\_1 |    N|
|:------------|:-----------|:-------|:-------|:-------|----:|
| .folds      | 0          | 0      | TP     | TN     |  689|
| .folds      | 1          | 0      | FN     | FP     |  304|
| .folds      | 0          | 1      | FP     | FN     |  513|
| .folds      | 1          | 1      | TN     | TP     |  390|

``` r
## We make a ROC curve
cv_plot(CV, type = "ROC") +
  theme_bw() + ggtitle("ROC curve")
```

![](A3_P2_DiagnosingSchizophrenia_instructions_files/figure-markdown_github/unnamed-chunk-5-1.png)
Link to the assignment report:
<a href="https://docs.google.com/document/d/11DQqMdC33JOjShAPvn6snAsEmwdoEiS3LJycVRIX0Oc/edit?usp=sharing" class="uri">https://docs.google.com/document/d/11DQqMdC33JOjShAPvn6snAsEmwdoEiS3LJycVRIX0Oc/edit?usp=sharing</a>
