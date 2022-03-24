P8131 Homework 5
================
Roxy Zhang
3/23/2022

## Question 1

``` r
crab = read.table("data/HW5-crab.txt", header = TRUE)
```

``` r
# Poisson model M1
m1 <- glm(
  Sa ~ W,
  family = poisson(link = log),
  data = crab)

summary(m1)
```

    ## 
    ## Call:
    ## glm(formula = Sa ~ W, family = poisson(link = log), data = crab)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8526  -1.9884  -0.4933   1.0970   4.9221  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.30476    0.54224  -6.095  1.1e-09 ***
    ## W            0.16405    0.01997   8.216  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 632.79  on 172  degrees of freedom
    ## Residual deviance: 567.88  on 171  degrees of freedom
    ## AIC: 927.18
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Goodness of fit
# Deviance
m1_D = sum(residuals(m1, type = "deviance") ^ 2)

# Pearson chi-square
m1_G = sum(residuals(m1, type = "pearson") ^ 2)

m1_df = 173 - 2

1 - pchisq(m1_D, m1_df)
```

    ## [1] 0

``` r
# Poisson model M2
m2 <- glm(
  Sa ~ W + Wt,
  family = poisson(link = log),
  data = crab)

summary(m2)
```

    ## 
    ## Call:
    ## glm(formula = Sa ~ W + Wt, family = poisson(link = log), data = crab)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9308  -1.9705  -0.5481   0.9700   4.9905  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -1.29168    0.89929  -1.436  0.15091   
    ## W            0.04590    0.04677   0.981  0.32640   
    ## Wt           0.44744    0.15864   2.820  0.00479 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 632.79  on 172  degrees of freedom
    ## Residual deviance: 559.89  on 170  degrees of freedom
    ## AIC: 921.18
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Goodness of fit
# Deviance
m2_D = sum(residuals(m2, type = "deviance") ^ 2)

# Pearson chi-square
m2_G = sum(residuals(m2, type = "pearson") ^ 2)

m2_df = 173 - 3

1 - pchisq(m2_D, m2_df)
```

    ## [1] 0

``` r
# check over-dispersion in M2

# residual of M2
m2_res = residuals(m2, type = "pearson")

# over-dispersion parameter
phi = m2_G / (173 - 3)

# half normal plot
plot(
  qnorm((173 + 1:173 + 0.5) / (2 * 173 + 1.125)),
  sort(abs(m2_res)),
  xlab='Expected Half-Normal Order Statistics', 
  ylab='Ordered Absolute Pearson Residuals')

abline(a = 0, b = 1)
abline(a = 0, b = sqrt(phi), lty = 2)
```

![](bm2_hw5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# fit model with constant over-dispersion
summary(m2, dispersion = phi)
```

    ## 
    ## Call:
    ## glm(formula = Sa ~ W + Wt, family = poisson(link = log), data = crab)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9308  -1.9705  -0.5481   0.9700   4.9905  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -1.29168    1.59771  -0.808    0.419
    ## W            0.04590    0.08309   0.552    0.581
    ## Wt           0.44744    0.28184   1.588    0.112
    ## 
    ## (Dispersion parameter for poisson family taken to be 3.156449)
    ## 
    ##     Null deviance: 632.79  on 172  degrees of freedom
    ## Residual deviance: 559.89  on 170  degrees of freedom
    ## AIC: 921.18
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# goodness of fit
p_value = 1 - pchisq(m2_G/phi, 170)
p_value # fit is good
```

    ## [1] 0.4855753

## Question 2

``` r
parasite = read.table("data/HW5-parasite.txt", header = T)

dim(parasite)
```

    ## [1] 1254   11

``` r
summary(parasite)
```

    ##      Sample         Intensity            omit             Year     
    ##  Min.   :   1.0   Min.   :  0.000   Min.   :0.0000   Min.   :1999  
    ##  1st Qu.: 314.2   1st Qu.:  0.000   1st Qu.:0.0000   1st Qu.:1999  
    ##  Median : 627.5   Median :  0.000   Median :0.0000   Median :2000  
    ##  Mean   : 627.5   Mean   :  6.183   Mean   :0.4785   Mean   :2000  
    ##  3rd Qu.: 940.8   3rd Qu.:  4.000   3rd Qu.:1.0000   3rd Qu.:2001  
    ##  Max.   :1254.0   Max.   :257.000   Max.   :1.0000   Max.   :2001  
    ##                   NA's   :57                                       
    ##      omit.1          omit.2           Length           omit.3     
    ##  Min.   : 50.0   Min.   :  34.0   Min.   : 17.00   Min.   :0.000  
    ##  1st Qu.:110.0   1st Qu.: 765.5   1st Qu.: 44.00   1st Qu.:1.000  
    ##  Median :180.0   Median :1432.0   Median : 54.00   Median :1.000  
    ##  Mean   :176.2   Mean   :1704.3   Mean   : 53.45   Mean   :1.411  
    ##  3rd Qu.:235.0   3rd Qu.:2222.5   3rd Qu.: 62.00   3rd Qu.:2.000  
    ##  Max.   :293.0   Max.   :9990.0   Max.   :101.00   Max.   :2.000  
    ##                  NA's   :6        NA's   :6                       
    ##      omit.4          omit.5           Area      
    ##  Min.   :0.000   Min.   : 0.00   Min.   :1.000  
    ##  1st Qu.:1.000   1st Qu.: 3.00   1st Qu.:2.000  
    ##  Median :1.000   Median : 4.00   Median :3.000  
    ##  Mean   :1.409   Mean   : 4.07   Mean   :2.612  
    ##  3rd Qu.:2.000   3rd Qu.: 5.00   3rd Qu.:3.000  
    ##  Max.   :4.000   Max.   :10.00   Max.   :4.000  
    ## 

``` r
# check for categorical variable levels
unique(parasite$Area)
```

    ## [1] 2 3 4 1

``` r
unique(parasite$Year)
```

    ## [1] 1999 2000 2001

``` r
m3 = glm(
  Intensity ~ factor(Area) + factor(Year) + Length,
  family = poisson(link = log), 
  data = parasite)

summary(m3)
```

    ## 
    ## Call:
    ## glm(formula = Intensity ~ factor(Area) + factor(Year) + Length, 
    ##     family = poisson(link = log), data = parasite)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -9.3632  -2.7158  -2.0142  -0.4731  30.2492  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       2.6431709  0.0542838  48.692  < 2e-16 ***
    ## factor(Area)2    -0.2119557  0.0491691  -4.311 1.63e-05 ***
    ## factor(Area)3    -0.1168602  0.0428296  -2.728  0.00636 ** 
    ## factor(Area)4     1.4049366  0.0356625  39.395  < 2e-16 ***
    ## factor(Year)2000  0.6702801  0.0279823  23.954  < 2e-16 ***
    ## factor(Year)2001 -0.2181393  0.0287535  -7.587 3.29e-14 ***
    ## Length           -0.0284228  0.0008809 -32.265  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 25797  on 1190  degrees of freedom
    ## Residual deviance: 19153  on 1184  degrees of freedom
    ##   (63 observations deleted due to missingness)
    ## AIC: 21089
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
exp(m3$coefficients)
```

    ##      (Intercept)    factor(Area)2    factor(Area)3    factor(Area)4 
    ##       14.0577088        0.8090006        0.8897096        4.0752685 
    ## factor(Year)2000 factor(Year)2001           Length 
    ##        1.9547848        0.8040134        0.9719773

``` r
# Goodness of fit
m3_D = sum(residuals(m3, type = "deviance") ^ 2)
deviance(m3) # the same
```

    ## [1] 19152.8

``` r
m3_G = sum(residuals(m3, type = "pearson") ^ 2)

pchisq(deviance(m3), df.residual(m3), lower.tail = FALSE)
```

    ## [1] 0

``` r
# zero-inflated model
m4 =  zeroinfl(
  Intensity ~ factor(Area) +  factor(Year) + Length, 
  data = parasite)

summary(m4)
```

    ## 
    ## Call:
    ## zeroinfl(formula = Intensity ~ factor(Area) + factor(Year) + Length, 
    ##     data = parasite)
    ## 
    ## Pearson residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1278 -0.8265 -0.5829 -0.1821 25.4837 
    ## 
    ## Count model coefficients (poisson with log link):
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       3.8431720  0.0583793  65.831  < 2e-16 ***
    ## factor(Area)2     0.2687838  0.0500467   5.371 7.84e-08 ***
    ## factor(Area)3     0.1463174  0.0439485   3.329 0.000871 ***
    ## factor(Area)4     0.9448070  0.0368342  25.650  < 2e-16 ***
    ## factor(Year)2000  0.3919828  0.0282952  13.853  < 2e-16 ***
    ## factor(Year)2001 -0.0448457  0.0296057  -1.515 0.129831    
    ## Length           -0.0368067  0.0009747 -37.762  < 2e-16 ***
    ## 
    ## Zero-inflation model coefficients (binomial with logit link):
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       0.552579   0.275762   2.004  0.04509 *  
    ## factor(Area)2     0.718680   0.189552   3.791  0.00015 ***
    ## factor(Area)3     0.657710   0.167402   3.929 8.53e-05 ***
    ## factor(Area)4    -1.022864   0.188201  -5.435 5.48e-08 ***
    ## factor(Year)2000 -0.752121   0.172965  -4.348 1.37e-05 ***
    ## factor(Year)2001  0.456533   0.143962   3.171  0.00152 ** 
    ## Length           -0.009889   0.004629  -2.136  0.03266 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Number of iterations in BFGS optimization: 17 
    ## Log-likelihood: -6950 on 14 Df

``` r
exp(m4$coefficients$count)
```

    ##      (Intercept)    factor(Area)2    factor(Area)3    factor(Area)4 
    ##       46.6732884        1.3083722        1.1575635        2.5723170 
    ## factor(Year)2000 factor(Year)2001           Length 
    ##        1.4799122        0.9561450        0.9638624

``` r
exp(m4$coefficients$zero)
```

    ##      (Intercept)    factor(Area)2    factor(Area)3    factor(Area)4 
    ##        1.7377285        2.0517222        1.9303664        0.3595635 
    ## factor(Year)2000 factor(Year)2001           Length 
    ##        0.4713656        1.5785910        0.9901599
