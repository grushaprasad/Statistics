Regressions tutorial
================
Grusha Prasad
July 15, 2018

What is linear regression?
==========================

Linear regression allows us to describe the relationship between a dependent variable (y) and one or more independent variables (x). Unlike with correlation where we are interested in the extent to which x and y are related to each other, with regression we are specifically interested in predicting y from x - i.e. how much of the variance in y can be explained by grouping the data by x. Given this goal, people often talk about regression in terms of causation. For example consider the following equation:

*y* = 3*x* + 4

One way to describe this model: The model predicts that one unit increase in x causes/ is associated with 3 units increase in y. Therefore the coefficient of x is the degree to which x can impact y. This description has a causal flavour because it describes it as a difference within an individual or a group (i.e. how would the response of an indvidual change if they were given the 'treatment'). However Gelman and Hill argue that we should be thinking of regressions as a difference between individuals/groups.

> "Linear regression is a method that summarizes how the average values of a numerical outcome variable vary over subpopulations defined by linear functions of predictors." (pg 31)

Another way to describe the model: The model predicts that two groups that have a one unit difference in x on average tend to have two units difference in y. The coefficient of x is predicted *average difference* in y for groups that vary in x. This description does not have the same causal flavour

Brief description of the dataset
--------------------------------

``` r
data(iris)
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

There are 4 continuous variables and one categorical variable. In the remainder of this document we will predict the Petal.Length from the other variables.

Categorical predictors
======================

One predictor
-------------

Let us start by looking at just two species.

``` r
two_species <- subset(iris, Species != 'virginica')
two_species$Species <- factor(two_species$Species)  #removes ghost levels
lm(two_species$Petal.Length ~ two_species$Species)
```

    ## 
    ## Call:
    ## lm(formula = two_species$Petal.Length ~ two_species$Species)
    ## 
    ## Coefficients:
    ##                   (Intercept)  two_species$Speciesversicolor  
    ##                         1.462                          2.798

In order to understand what the intercept and the coefficients mean, it is important to understand how the contrasts are coded for the variable

``` r
contrasts(two_species$Species)
```

    ##            versicolor
    ## setosa              0
    ## versicolor          1

As a default, R uses dummy coding - which means that it treats one of the levels as a "baseline" and compares all the other levels with this baseline. (It picked 'setosa' as the basline because the levels were organized alphabetically)

With dummy coding with one predictor, the intercept is the mean petal length of the baseline category (i.e. setosa)

``` r
mean(subset(two_species, Species == 'setosa')$Petal.Length)
```

    ## [1] 1.462

The coefficient is the mean difference in petal length between baseline cateogry and the category it is being compared to.

``` r
mean(subset(two_species, Species == 'versicolor')$Petal.Length - subset(two_species, Species == 'setosa')$Petal.Length)
```

    ## [1] 2.798

The same idea applies when there are more than two levels to a predictor. There are if a variable has k levels, there are k-1 coefficients to estimate

``` r
lm(iris$Petal.Length ~ iris$Species)
```

    ## 
    ## Call:
    ## lm(formula = iris$Petal.Length ~ iris$Species)
    ## 
    ## Coefficients:
    ##            (Intercept)  iris$Speciesversicolor   iris$Speciesvirginica  
    ##                  1.462                   2.798                   4.090

``` r
contrasts(iris$Species)
```

    ##            versicolor virginica
    ## setosa              0         0
    ## versicolor          1         0
    ## virginica           0         1

The intercept is again the mean of the baseline (setosa)

``` r
mean(subset(iris, Species == 'setosa')$Petal.Length)
```

    ## [1] 1.462

The coefficients are the mean difference between the baseline and the cateogory that is being compared to the baseline

``` r
mean(subset(iris, Species == 'setosa')$Petal.Length - subset(iris, Species == 'versicolor')$Petal.Length)
```

    ## [1] -2.798

``` r
mean(subset(iris, Species == 'setosa')$Petal.Length - subset(iris, Species == 'virginica')$Petal.Length)
```

    ## [1] -4.09

Note, the dummy contrasts assumes we have a baseline level that we can compare the other levels with. This might be useful when thinking about treatment groups and control groups. However this is not always useful for other kinds of categorical variables. Instead we can use summed contrasts - which will allow us to compare the means for groups with the grand mean.

``` r
two_species$Species_sc <- two_species$Species
contrasts(two_species$Species_sc) <- "contr.sum"
contrasts(two_species$Species_sc)
```

    ##            [,1]
    ## setosa        1
    ## versicolor   -1

``` r
lm(two_species$Petal.Length ~ two_species$Species_sc)
```

    ## 
    ## Call:
    ## lm(formula = two_species$Petal.Length ~ two_species$Species_sc)
    ## 
    ## Coefficients:
    ##             (Intercept)  two_species$Species_sc1  
    ##                   2.861                   -1.399

The intercept (or the baseline we are comparing the group means to) in this case is the grand mean of petal length across species

``` r
mean(two_species$Petal.Length)
```

    ## [1] 2.861

The coefficient in this case is how much the average petal length of each species varies from the grand mean.

``` r
mean(two_species$Petal.Length) - mean((subset(two_species, Species == 'versicolor')$Petal.Length))
```

    ## [1] -1.399

``` r
mean(two_species$Petal.Length) - mean((subset(two_species, Species == 'setosa')$Petal.Length))
```

    ## [1] 1.399

When there are two conditions this is just half of the distance between the conditions we get with the 0,1 dummy coding. So if we wanted to have the same effect size we could set the contrasts to be -0.5 and 0.5 instead of 1 and -1.

Similarly looking at summed contrasts for three levels.

``` r
iris$Species_sc <- iris$Species
contrasts(iris$Species_sc) <- "contr.sum"
contrasts(iris$Species_sc)
```

    ##            [,1] [,2]
    ## setosa        1    0
    ## versicolor    0    1
    ## virginica    -1   -1

``` r
lm(iris$Petal.Length ~ iris$Species_sc)
```

    ## 
    ## Call:
    ## lm(formula = iris$Petal.Length ~ iris$Species_sc)
    ## 
    ## Coefficients:
    ##      (Intercept)  iris$Species_sc1  iris$Species_sc2  
    ##            3.758            -2.296             0.502

``` r
mean(iris$Petal.Length)
```

    ## [1] 3.758

``` r
mean(iris$Petal.Length) - mean((subset(iris, Species == 'setosa')$Petal.Length))
```

    ## [1] 2.296

``` r
mean(iris$Petal.Length) - mean((subset(iris, Species == 'versicolor')$Petal.Length))
```

    ## [1] -0.502

Note though this doesn't directly tell us value for virgincia, this should be negative sum of the other two. -(-2.296 + 0.502) = 1.794

Look [here](http://atyre2.github.io/2016/09/03/sum-to-zero-contrasts.html) for more discussion.

Two predictors
--------------

### No interaction

Let us start by adding another categorical predictor (and using dummy coding).

``` r
two_species$Sepal.Length.cat <- factor(ifelse(two_species$Sepal.Length > mean(two_species$Sepal.Length), "long", "short"))

contrasts(two_species$Species)  
```

    ##            versicolor
    ## setosa              0
    ## versicolor          1

``` r
contrasts(two_species$Sepal.Length.cat)
```

    ##       short
    ## long      0
    ## short     1

``` r
lm(Petal.Length ~ Species + Sepal.Length.cat, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ Species + Sepal.Length.cat, data = two_species)
    ## 
    ## Coefficients:
    ##           (Intercept)      Speciesversicolor  Sepal.Length.catshort  
    ##                1.8163                 2.4909                -0.3937

The intercept should be the expected mean petal length for the baseline for both predictors (so setosa with long sepals). However if we look at the mean for long setosas, we see that this is not equal to the intercept.

``` r
mean(subset(two_species, Species == 'setosa' & Sepal.Length.cat == 'long')$Petal.Length)
```

    ## [1] 1.42

And we see that the means are not equal for the coefficients either. Let us briefly look at what they should be before delving into why the means are not equal to the estimates of the model.

The coefficient for Species/Sepal.Length should be the expected mean difference in petal length between the baseline and the category being compared to.

You can read more about this [here](https://stats.stackexchange.com/questions/120030/interpretation-of-betas-when-there-are-multiple-categorical-variables/120035#120035)

Note since the number of observations per group is not equal (i.e. there are dispropotionate number of long between setosa and versicolor), taking the mean of just long will weight the observations from versicolor as being more important than the observations from setosa. Hence we need to first compute the means of each group individually

``` r
means <- ddply(two_species, c('Sepal.Length.cat', 'Species'), summarise, Petal.Length = mean(Petal.Length, na.rm = T))
means
```

    ##   Sepal.Length.cat    Species Petal.Length
    ## 1             long     setosa     1.420000
    ## 2             long versicolor     4.352273
    ## 3            short     setosa     1.466667
    ## 4            short versicolor     3.583333

``` r
mean(subset(means, Species == 'versicolor')$Petal.Length) - mean(subset(means, Species == 'setosa')$Petal.Length)
```

    ## [1] 2.52447

``` r
mean(subset(means, Sepal.Length.cat == 'short')$Petal.Length) - mean(subset(means, Sepal.Length.cat == 'long')$Petal.Length)
```

    ## [1] -0.3611364

In order to address why the estimates are close to the difference of the means but not exactly equal, we need to think about how the model is trying to get the estimates.

In order to fit the model, R samples a space of all possible parameters and picks the ones that maximize the likelihood of the model predicting our observed data. This method is called Maximum Likelihood Estimation and you can read more about what it intuitively means [here]('https://stats.stackexchange.com/questions/112451/maximum-likelihood-estimation-mle-in-layman-terms'). However is most cases the maximum likelihood estimate for the intercept or the coefficient ends up being the value that minimizes the summed square difference between the predicted values and observed values (called [sum of square residuals]('https://en.wikipedia.org/wiki/Residual_sum_of_squares')).

When you are trying to find the intercept and coefficient for a model with one categorical predictor, the values that minimize error (and maximize likelihood) tend to be the empirical means in the case of intercept (i.e. the mean of observations in the data) or difference between empirical means in the case of coefficients. Intuitively this makes sense because picking the value in the middle gives you the least error. If you pick a value less than the mean, you will over estimate more often and if you pick a value less than the mean you will under estimate more often.

However why this is not true when we have two predictors? When we have a model with two predictors without an interaction term, we are implicitly assuming that the slopes of both these predictors are equal (i.e. the regression lines for the predictors are parallel). Or as Gelman and Hill put it:

> "We interpret the regression slopes as comparisons of individuals that differ in one predictor while being at the same levels of other predictors ... the slope of the regression was forced to be equal across subgroups"

So when we are estimating the coefficient for Species, by keeping Sepal.Length constant, we are assuming that

*m**e**a**n*(*l**o**n**g**s**e**t**o**s**a*)−*m**e**a**n*(*s**h**o**r**t**s**e**t**o**s**a*)=*m**e**a**n*(*l**o**n**g**v**e**r**s**i**c**o**l**o**r*)−*m**e**a**n*(*s**h**o**r**t**v**e**r**s**i**c**o**l**o**r*)

Similarly when we estimating the coefficient for Sepal.Length, by keeping Species constant, we are assuming that:

*m**e**a**n*(*l**o**n**g**s**e**t**o**s**a*)−*m**e**a**n*(*l**o**n**g**v**e**r**s**i**c**o**l**o**r*)=*m**e**a**n*(*s**h**o**r**t**s**e**t**o**s**a*)−*m**e**a**n*(*s**h**o**r**t**v**e**r**s**i**c**o**l**o**r*)

However neither of these are true. However since the model is trying to find the estimates that maximize a model that makes these assumptions, the empirical means or the difference between empirical means are not necessarily the values that minimze error and maximize likelihood of this model.

However if we do add an interaction term and abandon this assumption, the empricial means are again the estimates that will maximize likelihood.

### Adding interactions

#### With dummy coding

``` r
lm(Petal.Length ~ Sepal.Length.cat * Species, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ Sepal.Length.cat * Species, data = two_species)
    ## 
    ## Coefficients:
    ##                             (Intercept)  
    ##                                 1.42000  
    ##                   Sepal.Length.catshort  
    ##                                 0.04667  
    ##                       Speciesversicolor  
    ##                                 2.93227  
    ## Sepal.Length.catshort:Speciesversicolor  
    ##                                -0.81561

The intercept of the model with interaction is the mean petal length of both basline groups

``` r
mean(subset(two_species,Sepal.Length.cat == 'long' & Species == 'setosa')$Petal.Length)
```

    ## [1] 1.42

The coefficient for Sepal.Length is the difference between short and long for the baseline/ reference species group (setosa)

``` r
mean(subset(two_species, Sepal.Length.cat == 'short' & Species == 'setosa')$Petal.Length) - mean(subset(two_species, Sepal.Length.cat == 'long' & Species == 'setosa')$Petal.Length) 
```

    ## [1] 0.04666667

The coefficient for Species is the difference between versicolor and setosa for the baseline/ reference sepal.length group (long)

``` r
mean(subset(two_species, Sepal.Length.cat == 'long' & Species == 'versicolor')$Petal.Length) - mean(subset(two_species, Sepal.Length.cat == 'long' & Species == 'setosa')$Petal.Length)
```

    ## [1] 2.932273

Since the coefficients are in terms of the reference group of the other predictor, they can't be interepreted as main effects (since the main effect would be just the average difference between the categories)

The interaction is the difference between the intercept and "additive effect" of both the reference groups. You can read more about it [here](https://stats.stackexchange.com/questions/122246/interpretation-of-interaction-term/122251#122251)

In our case it is:

*s**h**o**r**t* *v**e**r**s**i**c**o**l**o**r* − (*l**o**n**g* *s**e**t**o**s**a* + (*s**h**o**r**t* *s**e**t**o**s**a* − *l**o**n**g* *s**e**t**o**s**a*)+(*l**o**n**g* *v**e**r**s**i**c**o**l**o**r* − *l**o**n**g* *s**e**t**o**s**a*))
=*s**h**o**r**t* *v**e**r**s**i**c**o**l**o**r* + *l**o**n**g* *s**e**t**o**s**a* − *s**h**o**r**t* *s**e**t**o**s**a* − *l**o**n**g* *v**e**r**s**i**c**o**l**o**r*

=(*s**h**o**r**t* *v**e**r**s**i**c**o**l**o**r* + *l**o**n**g* *s**e**t**o**s**a*)−(*l**o**n**g* *v**e**r**s**i**c**o**l**o**r* + *s**h**o**r**t* *s**e**t**o**s**a*)

``` r
mean(subset(two_species,Sepal.Length.cat == 'short' & Species == 'versicolor')$Petal.Length) + mean(subset(two_species, Sepal.Length.cat == 'long' & Species == 'setosa')$Petal.Length)   - mean(subset(two_species, Sepal.Length.cat == 'long' & Species == 'versicolor')$Petal.Length) - mean(subset(two_species, Sepal.Length.cat == 'short' & Species == 'setosa')$Petal.Length)
```

    ## [1] -0.8156061

Note, if there is no interaction, short versicolor will be equal to short setosa and hence they will cancel out. Similarly long setosa will be equal to long versicolor and will cancel out. Hence if there is 0 interaction the model with the interaction term will be the same as the model without the interaction term.

### With summed contrasts:

Gelman and Hill say:

> "Models with interaction can often be easily interpreted if we first pre-process the data by centering each input variable about its mean or some other convenient reference point"

For categorical variables, using summed contrasts is a way of centering the variables about a reference point - i.e. 0.

``` r
two_species$Sepal.Length.cat_sc <- two_species$Sepal.Length.cat

contrasts(two_species$Sepal.Length.cat_sc) <- "contr.sum"

lm(Petal.Length ~  Species_sc * Sepal.Length.cat_sc, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ Species_sc * Sepal.Length.cat_sc, 
    ##     data = two_species)
    ## 
    ## Coefficients:
    ##                      (Intercept)                       Species_sc1  
    ##                           2.7056                           -1.2622  
    ##             Sepal.Length.cat_sc1  Species_sc1:Sepal.Length.cat_sc1  
    ##                           0.1806                           -0.2039

``` r
contrasts(two_species$Sepal.Length.cat_sc)
```

    ##       [,1]
    ## long     1
    ## short   -1

``` r
contrasts(two_species$Species_sc)
```

    ##            [,1]
    ## setosa        1
    ## versicolor   -1

The intercept like earlier, is the average petal length across species and sepal lengths.

``` r
means <- ddply(two_species, c('Sepal.Length.cat_sc', 'Species_sc'), summarise, Petal.Length = mean(Petal.Length, na.rm = T))
means
```

    ##   Sepal.Length.cat_sc Species_sc Petal.Length
    ## 1                long     setosa     1.420000
    ## 2                long versicolor     4.352273
    ## 3               short     setosa     1.466667
    ## 4               short versicolor     3.583333

``` r
mean(means$Petal.Length)
```

    ## [1] 2.705568

Note here we can't just take the mean of Petal.Length because the number of observations in each group is not equal.

Unlike with dummy coding, the coefficients of Species and Sepal.Length are more directly interpretable as the main effect. The coefficient for Sepal.Length is how much the short sepals vary from the grand mean. Similarly the coefficient for Species is how much versicolor varies from the grand mean.

``` r
mean(means$Petal.Length) - mean(subset(means, Sepal.Length.cat_sc == 'short')$Petal.Length)
```

    ## [1] 0.1805682

``` r
mean(means$Petal.Length) - mean(subset(means, Species_sc == 'versicolor')$Petal.Length)
```

    ## [1] -1.262235

For the other coefficients we looked at the difference between the grand mean and the reference group of a given predictor. The coefficient for the interaction term is the difference between the grand mean and how the reference group of predictors interact with the baseline group of the other predictors. In other words it is the combination of Species\*Sepal.Length that gets coded as -1. We can get the coding for the combination by just multiplying both predictors

-   Short versicolor = 1 \* 1 = 1
-   Short setosa = 1 \* -1 = -1
-   Long versicolor = -1 \* 1 = -1
-   Long setosa = -1 \* -1 = 1

``` r
mean(means$Petal.Length) - mean(c(subset(means, Sepal.Length.cat_sc == 'long' & Species_sc == 'versicolor')$Petal.Length,subset(means, Sepal.Length.cat_sc == 'short' & Species_sc == 'setosa')$Petal.Length))
```

    ## [1] -0.2039015

Continuous predictor
====================

With one predictor
------------------

``` r
lm(Petal.Length ~ Sepal.Length, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ Sepal.Length, data = two_species)
    ## 
    ## Coefficients:
    ##  (Intercept)  Sepal.Length  
    ##       -7.180         1.835

The intercept in this case is not interpretable because it is the Petal.Length when Sepal.Length is 0 - which it can never be.

The coefficient says that when there is a one unit difference in the sepal length between two irises, on average the difference in petal length is going to be 1.84. Note since this is an expected difference, we can't actually get the number directly from our data by subtracting the mean petal length of two irises with one unit difference in petal length.

Let us look at the effect on centering

``` r
c. <- function (x) scale(x, scale = FALSE)

lm(Petal.Length ~ c.(Sepal.Length), data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length), data = two_species)
    ## 
    ## Coefficients:
    ##      (Intercept)  c.(Sepal.Length)  
    ##            2.861             1.835

While the slope does not change, the intercept becomes more interpretable. It should be the mean of the Petal.Length when Sepal.Length = mean(Sepal.Length) - in other words it is the average petal length of an iris with an average sepal length.

Two predictors and interaction with centering
---------------------------------------------

### Dummy coding of species

``` r
lm(Petal.Length ~ c.(Sepal.Length)*Species, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length) * Species, data = two_species)
    ## 
    ## Coefficients:
    ##                        (Intercept)                    c.(Sepal.Length)  
    ##                             1.5232                              0.1316  
    ##                  Speciesversicolor  c.(Sepal.Length):Speciesversicolor  
    ##                             2.4176                              0.5548

``` r
contrasts(two_species$Species)
```

    ##            versicolor
    ## setosa              0
    ## versicolor          1

The intercept is the average petal length of setosa whose sepal length is the average sepal length.

The coefficient of Sepal.Length is how much the mean petal length of setosa's vary with one unit difference in Sepal.Length

The coefficient of Species is mean(versicolor with average sepal length) - mean(setosa with average sepal length)

The interaction is the difference between between the slopes of Sepal length for setosa and versicolor. We can get the slopes for setosa and versicolor by running the model on subsets of the two\_species data.

``` r
lm(Petal.Length ~ c.(Sepal.Length), data = subset(two_species, Species == 'setosa'))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length), data = subset(two_species, 
    ##     Species == "setosa"))
    ## 
    ## Coefficients:
    ##      (Intercept)  c.(Sepal.Length)  
    ##           1.4620            0.1316

``` r
lm(Petal.Length ~ c.(Sepal.Length), data = subset(two_species, Species == 'versicolor'))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length), data = subset(two_species, 
    ##     Species == "versicolor"))
    ## 
    ## Coefficients:
    ##      (Intercept)  c.(Sepal.Length)  
    ##           4.2600            0.6865

So the slope of the interaction is 0.6865 - 0.1316 = 0.5549

In other words how much the mean petal length of setosas vary with one unit difference in Sepal.Length and how much the mean petal length of versicolors vary with one unit difference in Sepal.Length

### Contrast coding of species

``` r
lm(Petal.Length ~ c.(Sepal.Length)*Species_sc, data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length) * Species_sc, data = two_species)
    ## 
    ## Coefficients:
    ##                  (Intercept)              c.(Sepal.Length)  
    ##                       2.7320                        0.4091  
    ##                  Species_sc1  c.(Sepal.Length):Species_sc1  
    ##                      -1.2088                       -0.2774

``` r
contrasts(two_species$Species_sc)
```

    ##            [,1]
    ## setosa        1
    ## versicolor   -1

The intercept is the average petal length of irises (both setosas and versicolors) whose sepal length is the average sepal length.

The coefficient of Sepal.Length is how much the mean petal length of irises (both setosas and versicolors) vary with one unit difference in Sepal.Length. So this is more like the "main effect" of Sepal.Length

The coefficient of Species is mean(irises with average sepal length) - mean(setosa with average sepal length). In other words how much does a particular species vary from the average. So this is the main effect of Species.

The interaction might be the difference between between the slopes of Sepal length for irises and setosa. We can get the slopes for setosa and versicolor by running the model on subsets of the two\_species data.

``` r
lm(Petal.Length ~ c.(Sepal.Length), data = two_species)
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length), data = two_species)
    ## 
    ## Coefficients:
    ##      (Intercept)  c.(Sepal.Length)  
    ##            2.861             1.835

``` r
lm(Petal.Length ~ c.(Sepal.Length), data = subset(two_species, Species == 'setosa'))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length), data = subset(two_species, 
    ##     Species == "setosa"))
    ## 
    ## Coefficients:
    ##      (Intercept)  c.(Sepal.Length)  
    ##           1.4620            0.1316

But it does not seem to be the case.

Statistical inference
=====================

### Residual error

A regression model is a linear equation + an error term - which can be thought of as unexplained variance. The residual standard deviation gives us the average distance of the observed outcomes from the predicted values. Therefore it gives us an estimate of how much of the variance is unexplained. Note the degrees of freedom to estimate this is n-k where n is the number of observations and k is the number of parameters being estimated. The closer k is to n, the more likely it is for our model to overfit the data - because in the limit, if we had one parameter for one observation we can perfectly fit the data with 0 error.

### Standard error

While the residual error tells us how much of the variance in the data is explained by the model with our estimated coefficients, they can't give us an estimate of how likely it is that these coefficients are explaining our data by chance (i.e. the difference between the groups predicted by the coefficients is not statistically significant). In order to estimate that we need the standard error for the coefficients - which we can get from the summary of the model.

``` r
summary(lm(Petal.Length ~ c.(Sepal.Length)*Species_sc, data = two_species))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ c.(Sepal.Length) * Species_sc, data = two_species)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.68611 -0.12305 -0.01386  0.14040  0.79607 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   2.73200    0.03806  71.787  < 2e-16 ***
    ## c.(Sepal.Length)              0.40905    0.06155   6.646 1.83e-09 ***
    ## Species_sc1                  -1.20879    0.03806 -31.763  < 2e-16 ***
    ## c.(Sepal.Length):Species_sc1 -0.27742    0.06155  -4.507 1.85e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2508 on 96 degrees of freedom
    ## Multiple R-squared:  0.971,  Adjusted R-squared:  0.9701 
    ## F-statistic:  1070 on 3 and 96 DF,  p-value: < 2.2e-16

We can construct a 95% confidence interval from the standard errors in the following way

*C**I* = *e**s**t**i**m**a**t**e* ± 2 \* *S**E*
 This confidence interval tells us that all of the coefficients in this interval would be consistent with 95% of our data. The null hypothesis for a regression coefficent is that it is equal to 0 (i.e. that coefficient does not play a role in predicting the data). Therefore in order for a coeffcient to be statistically significant, the CI for the estimate of that coefficient should not contain 0.

Let us consider our model with one cateogrical predictor with dummy coding

``` r
summary(lm(Petal.Length ~ Species, data = two_species))
```

    ## 
    ## Call:
    ## lm(formula = Petal.Length ~ Species, data = two_species)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -1.260 -0.162  0.038  0.238  0.840 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.46200    0.05010   29.18   <2e-16 ***
    ## Speciesversicolor  2.79800    0.07085   39.49   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3542 on 98 degrees of freedom
    ## Multiple R-squared:  0.9409, Adjusted R-squared:  0.9403 
    ## F-statistic:  1560 on 1 and 98 DF,  p-value: < 2.2e-16

This model tells us that the mean difference in petal length between versicolor and setosa is 2.798. The CI tells us that any other coefficient in that interval will be equal to the mean difference in petal length between the species for 95% of our data. If this interval contains 0, then it means that if we assume that the mean difference in petal length between setosa and versicolor is 0, that assumption would be consistent with 95% of our data. Conversely if the interval does not contain 0, then 95% of our data is consistent with the fact that the mean difference is *not* 0. Hence if 0 does not fall in the CI, then we can be pretty confident in our estimate.

**Questions** Is this correct? What does 95% of our data mean exactly? Does it mean any subset of our data with 95% in it? Or does it mean 95% of the time??

Assumptions of regression models
================================

These assumptions are taken from Gelman and Hill (pg 46)

1.  The variable being predicted is a linear combination of specific predictors - i.e. you *add* different predictors together. If we think that y = abc and not y = a + b + c, we could transform the data such that our model is y = loga + logb + logc. ALternatively we could include interactions

2.  Errors from the prediction line are independent - if they are not independent is this when why we use mixed effect modeling?

3.  Variance of the regression estimates are equal.

4.  Regression errors are normally distributed.

According to Gelman and Hill, the last two are not that important
