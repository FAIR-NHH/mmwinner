Power analysis for replication
================
Erik
30 september, 2018

Aim
===

We want to estimate the power of the online replication experiment (n=4000) for the three main results reported on in Section 3 of the lab-only experiment in the working paper [Fairness in Winner-Take-All Markets](http://hdl.handle.net/11250/2496796).

The power calculations are meant to go into the pre-analysis-plan registered at the AEA RCT-registry.

Reading data and initial data transformations.
==============================================

I just repeat the transformation of data code from the estimation of the working paper.

Reading inn data from file.

``` r
df_data <- read.csv("data/mmwinner.csv",sep=",")
```

First, make the factor with treatments with names that correspond to those in the paper

``` r
df_data$T <- as.factor(df_data$T)
levels(df_data$T) <- c("WTA-No Choice",
                       "WTA",
                       "WTA-No Exp.",
                       "Base")
```

Some transformations that are useful for producing the graphs.

``` r
data_no_ties  <- df_data %>% mutate( shareWinner = 1 - shareX,
                                     allWinner = (e2==y2),
                                     difference = abs(x1-x2),
                                     performance_winner = x2,
                                     performance_loser = x1,
                                     normalized_income_winner = y2/0.15,
                                     normalized_income_loser  = y1/0.15,
                                     difference_trimmed = ifelse(difference>10,10,
                                                                 difference),
                                     cright = political>=4,
                                     female = (sex==2),
                                     gini = abs(y1-y2)/(y1+y2)) %>%
  filter(e1==0) 
```

Result 1
========

Result 1 said that **The majority of spectators accept extreme income inequality in a winner-take-all situation and do not redistribute at all, while the majority fully equalize incomes when the winner is randomly determined.**

I operationalize this as a significant difference in the amount if inequality between the WTA and the Base treatment.

``` r
data_no_ties %>% group_by(T) %>% summarize(mean_gini = mean(gini),
                                                   sd_gini = mean(gini))
```

    ## # A tibble: 4 x 3
    ##   T             mean_gini sd_gini
    ##   <fct>             <dbl>   <dbl>
    ## 1 WTA-No Choice     0.624   0.624
    ## 2 WTA               0.754   0.754
    ## 3 WTA-No Exp.       0.516   0.516
    ## 4 Base              0.247   0.247

``` r
sd_overall <- sd(data_no_ties$gini)
md1 <- mean(data_no_ties$gini[data_no_ties$T=="WTA"]) - 
    mean(data_no_ties$gini[data_no_ties$T=="Base"])
(delta1 <- md1 / sd_overall)
```

    ## [1] 1.314687

Since there is probably more noise in the survey data to be collected, let's be conservative and assume an effect size 50% of the lab size estimate.

``` r
power.t.test(n=1000, delta=0.5*delta1, type="two.sample")
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 1000
    ##           delta = 0.6573433
    ##              sd = 1
    ##       sig.level = 0.05
    ##           power = 1
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

For this we have power close to 1.

Result 2
========

Result 2 said that **Spectators are significantly less likely to accept extreme income inequality if the workers have not chosen to work in a winner-take-all situation and are not aware of the winner-take-all reward structure.**

We operationalize this as well with inequality, between the WTA No Expectations
and the WTA treatments.

``` r
md2 <- mean(data_no_ties$gini[data_no_ties$T=="WTA"]) - 
    mean(data_no_ties$gini[data_no_ties$T=="WTA-No Exp."])
(delta2 <- md2/sd_overall)
```

    ## [1] 0.6185683

We do the same downrating of treatment difference:

``` r
power.t.test(n=1000, delta=0.5*delta2, type="two.sample")
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 1000
    ##           delta = 0.3092842
    ##              sd = 1
    ##       sig.level = 0.05
    ##           power = 0.9999996
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

The same power approximately equal to one.

Result 3
========

Result 3 states that **Spectators largely endorse the "factual merit" fairness argument in a winner-take-all situation. The redistribution taking place appears to reflect that spectators give the loser a fixed "consolation prize," irrespective of the winning margin.**

We operationalize this as the two regression coefficients used for testing in Section 3.2 in the working paper. For these coefficients, it is not so clear what role noise would play, so we calculate power for the empirical values found in the lab study. We calculate power by translating into one-sided t-tests.

For the first part, we estimate 0.939 with standard error 0.066, from 265 participants. We can translate back into standard deviation and calculate power with 3000 participans (Base treatment not included).

``` r
sd3_1 <- 0.066 * sqrt(265)
delta3_1 <- (1-0.939)/sd3_1
power.t.test(n=3000, delta=delta3_1, type="one.sample")
```

    ## 
    ##      One-sample t test power calculation 
    ## 
    ##               n = 3000
    ##           delta = 0.05677576
    ##              sd = 1
    ##       sig.level = 0.05
    ##           power = 0.8746759
    ##     alternative = two.sided

We see that this gives us power to reject unity with this test, even with an effect size that is too small to be of much scientific value; certainly enough precision.

For the second part, we estimate 0.100 with standard error 0.052, and we want to test that it is one half. We translate to a t-test in the same way.

``` r
sd3_2 <- 0.052 * sqrt(265)
delta3_2 <- (0.5 - 0.100)/sd3_2
power.t.test(n=3000, delta=delta3_2, type="one.sample")
```

    ## 
    ##      One-sample t test power calculation 
    ## 
    ##               n = 3000
    ##           delta = 0.4725347
    ##              sd = 1
    ##       sig.level = 0.05
    ##           power = 1
    ##     alternative = two.sided

This is clearly sufficient power for any purpose, even if some of the rejections correspond to differences that are scientifically not very important (because of the high precision).
