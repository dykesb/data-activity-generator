activity_fun <- function(dataname, outformat, outname, yvar, xvar, ydesc, xdesc, yunits, popunit)
{
  rmarkdown::render("TAP4_2.Rmd", output_format = outformat, output_file = outname, 
                    params = list(yvar = yvar, xvar = xvar, ydesc = ydesc, xdesc = xdesc, yunits = yunits, popunit = popunit, dataname = dataname))
}

activity_fun("SandwichAnts.xlsx", "word_document", "myoutput2", "Ants", "Bread", "ant count", "type of bread", "number of ants", "bread slice")


stat_model <- function() 
{
  
  time_seed <- Sys.time()
  
  cat("The textbook uses the format *DATA = MODEL + ERROR* for a statistical model. For the observation shown above, what corresponds to the *DATA*?", "\n\n")
       
  set.seed(time_seed)
     
  randomized_answers <- sample(c("The student's `r params$ydesc` of `r first_complete$y`", 
                                 "The `r params$popdesc`'s `r params$xdesc` of `r first_complete$x`",
                                 "The connection between `r params$ydesc` and `r params$xdesc`",
                                 "The *RecordID* of 1447147"))
          
  cat("A.", randomized_answers[1], "\n")
          
  cat("B.", randomized_answers[2], "\n")
        
  cat("C.", randomized_answers[3], "\n")   
        
  cat("D.", randomized_answers[4], "\n") 
  
  return(time_seed)
}

what_is_mu <- function() 
{
  cat("What does $\mu$ represent?

          <!-- -->
          
          A.  The mean `r params$ydesc` of all of one `r params$xdesc` (for example, `r xvals[1]`) for `r params$popdesc`
          
          B.  The mean `r params$ydesc` of all `r params$popdesc`
          
          C.  The mean `r params$ydesc` of all the individuals included in the dataset
          
          D.  The mean `r params$ydesc` of all of one grade level (for example, 9^th^ graders) included in the data set")
}

what_is_alpha <- function() 
{
  cat("Let's call the level `r xvals[2]` of the *`r params$xvar`* variable Group 2. What does $\alpha_{2}$ correspond to?

          <!-- -->
          
          A.  The difference in the mean `r params$ydesc` of all `r params$popdesc` and the mean `r params$ydesc` of all `r xvals[2]` `r params$popdesc` 
          
          B.  The mean `r params$ydesc` of all `r xvals[2]` `r params$popdesc` in the U.S in 2015
          
          C.  The difference in the mean `r params$ydesc` of all the individuals in the data set and the mean `r params$ydesc` of all the `r xvals[2]` individuals in the data set
          
          D.  The mean `r params$ydesc` of all `r xvals[2]` individuals in the data set")
}

why_drop_alphas <- function() 
{
  cat("A main goal of One-way ANOVA is to determine if the categorical grouping variable has an effect on the response variable. If the categorical grouping variable has no effect (the null hypothesis is true), one can write the model from Questions 2 and 3 as: $Y = \mu + \varepsilon$. Why?

          <!-- -->
          
          A.  If *`r params$xvar`* has no effect, then each and the term drops from the model
          
          B.  If there is no effect of *`r params$xvar`* on *`r params$yvar`*, then we could ignore *`r params$xvar`* when discussing *`r params$yvar`* without losing any information on *`r params$yvar`*
          
          C.  If *`r params$xvar`* is not related to *`r params$yvar`*, then knowing *`r params$xvar`* does not change our prediction of *`r params$yvar`* so there is no need to include *`r params$xvar`* in the model
          
          D.  All of the above")
}

when_sum_zero <- function() 
{
  cat("Notice that the `r length(xvals)` group effect estimates do not add to 0. Under what condition would they have to add to 0?

          <!-- -->
          
          A.  Each level of *`r params$xvar`* has the same number of respondents in the data set
          
          B.  The percentage of each level of *`r params$xvar`* in the data set exactly matches its true percentage in the population
          
          C.  The distribution of `r params$ydesc` has a normal distribution in each level of *`r params$xvar`*
          
          D.  Each level of *`r params$xvar`* has the same number of individuals in the population")
}

hyp_test_residuals <- function() 
{
  cat("Comparing these two residuals (i.e., under the null and under the alternative) for *RecordID* 1447147, what do you notice?
  
          <!-- -->
          
          A.  The two residuals are essentially the same. The model taking *`r params$xvar`* into consideration is no different at predicting this individual's `r params$ydesc` than the model not considering *`r params$xvar`*.
        
          B.  The residual under the null has a larger absolute value. The model taking *`r params$xvar`* into consideration is better for predicting this individual's *`r params$yvar`*.
          
          C.  The residual under the alternative is negative. The model taking *`r params$xvar`* into consideration is no different at predicting this individual's `r params$ydesc` than the model not considering *`r params$xvar`*.
          
          D.  The two residuals are very large. Neither model is better for predicting this individual's *`r params$yvar`*.")
}


graph_comparison <- function() 
{
  cat("Which of the following is a reasonable conclusion from looking at the two graphs?

          <!-- -->
          
          A.  There is a statistically significant difference in mean *`r params$yvar`* between the `r length(xvals)` *`r params$xvar`* groups
          
          B.  There is not a statistically significant difference in mean *`r params$yvar`* between the `r length(xvals)` *`r params$xvar`* groups
          
          C.  *`r params$xvar`* explains very little of the difference in *`r params$yvar`* from individual to individual
          
          D.  *`r params$xvar`* explains a substantial amount of the difference in *`r params$yvar`* from individual to individual")
}

nul_res_plot <- function() 
{
  cat("What conclusion can one make by looking at the residual plot under the null model with respect to normality of errors?

          <!-- -->
          
          A.  `r params$ydesc`s of `r params$popdesc` are roughly normally distributed about a mean of `r ymean`
          
          B.  `r params$ydesc`s of `r params$popdesc` are roughly normally distributed for `r xvals[1]`. The same is true for `r xvals[-1]`
          
          C.  Neither A nor B
          
          D.  Both A and B")
}

model_error_df <- function() 
{
  cat("1.  What are the degrees of freedom values for Model and Error?

        <!-- -->
        
        A.  *df*~Model~ = `r length(xvals) - 1`; *df*~Error~ = `r sum(myanova[,1]) - length(xvals) + 1`
        
        B.  *df*~Model~ = `r length(xvals)`; *df*~Error~ = `r sum(myanova[,1]) - length(xvals)`
        
        C.  *df*~Model~ = `r length(xvals)`; *df*~Error~ = `r length(xvals)`
        
        D.  There is not enough information to answer this question")
}

sum_squares_tot <- function() 
{
  cat("1.  The Sums of Squares Total is found as: $\sum_{i = 1}^{n}\left( y_{i} - \overline{y} \right)^{2}$. What does the Sums of Squares Total correspond to?

        <!-- -->
        
        A.  Total of all `r length(myres)` `r params$ydesc`s squared
        
        B.  Total variation leftover in all `r length(myres)` `r params$ydesc`s after accounting for the person's `r params$xdesc`
        
        C.  Total variation in all `r length(myres)` `r params$ydesc`s explained by each person's `r params$xdesc`
        
        D.  Total variation in all `r length(myres)` `r params$ydesc`s about the mean grand mean `r params$ydesc` of `r ymean`")
}

sum_squares_value <- function() 
{
  cat("What is the value for Sums of Squares Model?

        <!-- -->
        
        A.  `r sum(myanova[,2]) - myanova[2, 2]`
        
        B.  `r sum(myanova[,1]) - length(xvals)`
        
        C.  `r (sum(myanova[,2]) - myanova[2, 2]) * 0.4`
        
        D.  There is not enough information to answer this question")
}


sum_squares_error <- function() 
{
  cat("What does the Sums of Squares Error correspond to?

        <!-- -->
        
        A.  Total of all `r length(myres)` `r params$ydesc`s squared
        
        B.  Total variation leftover in all `r length(myres)` `r params$ydesc`s after accounting for the individual's `r params$xdesc`
        
        C.  Total variation in all `r length(myres)` `r params$ydesc`s explained by each individual's `r params$xdesc`
        
        D.  Total variation in all `r length(myres)` `r params$ydesc`s about the mean grand mean `r params$ydesc` of `r ymean`")
}


ms_model_error <- function() 
{
  cat("What are the Mean Squares (MS) for Model and Error?

        <!-- -->
        
        A.  MS~Model~ = `r myanova[1,3] * 50`; MS~Error~ = `r myanova[2,3] * 25`
        
        B.  MS~Model~ = `r myanova[1,3] * 3`; MS~Error~ = `r myanova[2,3] * 7`
        
        C.  MS~Model~ = `r myanova[1,3] * 75`; MS~Error~ = `r myanova[2,3] * 12125`
        
        D.  MS~Model~ = `r myanova[1,3]`; MS~Error~ = `r myanova[2,3]`")
}


what_is_fstat <- function() 
{
  cat("What is the value of the *F*-Statistic?

        <!-- -->
        
        A.  `r myanova[1,4] / 8`
        
        B.  `r myanova[1,4] / 4`
        
        C.  `r myanova[1,4] / 2`
        
        D.  `r myanova[1,4]`")
}

pvalue_conclusion <- function() 
{
  cat("Which of the following is a reasonable conclusion based on the *p*-value (`r if (myanova[1,5] < 0.0001) {"< 0.0001"} else myanova[1,5]`)?

        <!-- -->
        
        A.  Each of the `r length(xvals)` `r params$xdesc`s has a statistically significantly difference in mean `r params$ydesc` from each of the other `r params$xdesc`s
        
        B.  There is a statistically significant difference in mean `r params$ydesc`samong the `r length(xvals)` `r params$xdesc`s
        
        C.  `r params$xdesc` plays a major role in determining an individual's `r params$ydesc`
        
        D.  From a practical point of view, knowing a person's `r params$xdesc` will help me to predict the person's `r params$ydesc` with much greater accuracy than if I did not know the individual's `r params$xdesc`")
}

rsquared_value <- function() 
{
  cat("What is the value of *R*^2^ for this situation? (***Remember***: Often people multiply the value by 100 to change *R*^2^ to a value between 0% and 100%.)

        <!-- -->
        
        A.  1.687
        
        B.  0.372
        
        C.  0.158
        
        D.  `r 1 - myanova[2,2] / (myanova[1,2] + myanova[2,2])`")
}

rsquared_meaning <- function() 
{
  cat("What does the *R*^2^ value tell us?

        <!-- -->
        
        A.  *`r params$xvar`* alone is not a very good predictor of *`r params$yvar`* in `r params$popdesc`
        
        B.  Only `r 100 * (1 - myanova[2,2] / (myanova[1,2] + myanova[2,2]))`% of the variation in *`r params$yvar`* in `r params$popdesc` is explained by the person's *`r params$xdesc`*
        
        C.  Both A and B
        
        D.  Neither A nor B")
}

pvalue_rsquared_comparison <- function() #Depends on pvalue_conclusion() and rsquared_value
{
  cat("In Question 16 we concluded that there is a statistically significant difference in mean *`r params$ydesc`s* among the *`r params$xdesc`s*; in Question 18 we concluded that only `r 100 * (1 - myanova[2,2] / (myanova[1,2] + myanova[2,2]))`% of the variation in *`r params$ydesc`s* is explained by *`r params$xvar`*. How can both these be true?

        <!-- -->
        
        A.  Practical significance and statistical significance are completely unrelated ideas
        
        B.  The sample size is very large so even a small `r params$xdesc` effect will be picked up by the ANOVA model
        
        C.  The Sum of Squares Model is very small relative to the Sum of Squares Error
        
        D.  The number of groups being compared is only `r length(xvals)` so there are not a lot of comparisons to be made")
}

zero_mean_condition <- function() 
{
  cat("**The errors must have Zero (0) Mean**. Basically, we need to determine if the method of data collection is likely to produce unbiased results. Which of the following best describes the sample and what we can conclude about the Zero Mean condition?

        <!-- -->
        
        A.  Because the respondents are a random sample from among all `r params$popdesc` it is likely that the data are unbiased and the Zero Mean condition is met.
        
        B.  Because the respondents are a representative sample from among all `r params$popdesc` it is likely that the data are unbiased and the Zero Mean condition is met.
        
        C.  Because the respondents are a random sample from among all `r params$popdesc` we know that any sample means and sample proportions calculated will be equal to the corresponding population mean or population proportion; thus, the Zero Mean condition is met.
        
        D.  Because the sample size is large it doesn't matter whether or not the sample is a representative sample because the data will be unbiased and the Zero Mean condition is met.")
}

same_std_condition <- function() 
{
  cat("1.  **The errors must have the Same Standard Deviation among each of the groups**. Which of the following provides the most convincing evidence about what we can conclude about the Same Standard Deviation condition?

        <!-- -->
        
        A.  The Residual vs. Fitted Plot shows roughly equal variation among the groups and the ratio of the largest sample standard deviation to the smallest sample standard deviation is less than 2; thus, we conclude the Same Standard Deviation condition is met.
        
        B.  The residuals on the Normal QQ Plot follow a 45-degree line which suggests the Same Standard Deviation condition is met.
        
        C.  The ratio of the largest sample mean to the smallest sample mean is less than 1.5; thus, we conclude the Same Standard Deviation condition is met.
        
        D.  The sample sizes of the `r length(xvals)` groups are not roughly equal; thus, we cannot conclude that the Same Standard Deviation condition is met.")
}

normal_condition <- function() 
{
  cat("**The errors must follow a Normal Distribution**. Which of the following provides the most convincing evidence about what we can conclude about the Normal Distribution condition?

        <!-- -->
        
        A.  The Residual vs. Fitted Plot shows roughly equal variation among the groups and the ratio of the largest sample standard deviation to the smallest sample standard deviation is less than 2; thus, we conclude the Normal Distribution condition is met.
        
        B.  The residuals on the Normal QQ Plot follows a 45-degree line which suggests the Normal Distribution condition is met.
        
        C.  The ratio of the largest sample mean to the smallest sample mean is less than 1.5; thus, we conclude the Normal Distribution condition is met.
        
        D.  The sample sizes of the `r length(xvals)` groups are all at least 30; thus, we can conclude that the Normal Distribution condition is met.")
}

independence_condition <- function() 
{
  cat("**The errors must be Independent**. Which of the following provides the most convincing evidence about what we can conclude about the Independence condition?

        <!-- -->
        
        A.  There is no reason to think that the error in `r params$ydesc` for one individual in the data set is influenced by any other individual in the data set; thus, we can conclude that the Independence condition is met.
        
        B.  The residuals on the Normal QQ Plot follows a 45-degree line which suggests the Independence condition is met.
        
        C.  Because this is a representative sample from among all `r params$popdesc`, we know that the Independence condition is met.
        
        D.  For most individuals in the data set the error in `r params$ydesc` will not be influenced by any other individual in the data set. The exception is that students that are siblings will probably have similar errors. But, since there are probably few such students, we can conclude that the Independence condition is met.")
}


#TAP 4.2 Questions


boxplot_relationship <- function()
{
cat("2.  Based on the comparative boxplot, choose the best statement to describe the relationship between *`r params$yvar`* and *`r params$xvar`*. ***NOTE***: The *`r params$xvar`* levels on the *X* axes are not in the expected order.

    <!-- -->
    
    A.  There is no trend between *`r params$xvar`* and *`r params$yvar`* in `r params$popdesc` because the person-to-person variability is very large compared to any `r params$xvar` level differences.
    
    B.  There is a very weak positive relationship between *`r params$xvar`* and *`r params$yvar`* in `r params$popdesc` the first three years of high school, but then in the fourth year the trend reverses.
    
    C.  There is a very weak relationship between *`r params$xvar`* and *`r params$yvar`* in `r params$popdesc`. As `r params$popdesc` get older their *`r params$yvar`* tends to increase, but the person-to-person variability is very large.
    
    D.  The plot shows us that we can track an individual `r params$popunit`’s *`r params$yvar`* from year-to-year through high school, and the trend is a slight increase in *`r params$yvar`* for the `r params$popunit`.")
}

qq_assumptions <- function()
{
  cat("2.  What does the Normal QQ Plot suggest about the assumptions of the ANOVA model?

      <!-- -->
      
      A.  The errors do not have mean 0
      
      B.  The errors do not have the same standard deviation in each group
      
      C.  The errors do not follow a normal distribution
      
      D.  The errors are not independent")
}

grand_mean <- function()
{
  cat("1.  In the model notation , *μ* is the grand mean over all `r params$popunit`s, *α*~k~ is the `r params$xvar` effect for `r params$xvar` *k* where 1 = 9^th^ `r params$xvar`, 2 = 10^th^ `r params$xvar`, 3 = 11^th^ `r params$xvar`, and 4 = 12^th^ `r params$xvar`, and *ε* is the error. What is *Y*?

      <!-- -->
      
      A.  *Log `r params$yvar`* for an individual
      
      B.  *`r params$yvar`* for an individual
      
      C.  Mean of *Log `r params$yvar`* for all individuals
      
      D.  *`r params$yvar`* for all individuals
      
      <!-- -->")
}

hypothesis_conclusion <- function()
{
  cat("1.  What would you conclude about the hypotheses *H*~0~: vs. *H*~a~: at least one μ is different?

      <!-- -->
      
      A.  Reject *H*~0~ and conclude that every *`r params$xvar`* level has a different mean *log `r params$yvar`*
      
      B.  Fail to reject *H*~0~ and conclude that there is not statistically significant evidence that at least one *`r params$xvar`* level has a different mean *log `r params$yvar`*
      
      C.  Reject *H*~0~ and conclude that at least one *`r params$xvar`* level has a different mean *log `r params$yvar`*
      
      D.  Fail to reject *H*~0~ and conclude that all *`r params$xvar`* levels have the same *log `r params$yvar`*")
}

qq_plot_log_yvar <- function()
{
  cat("1.  Which of the following best describes what can be decided from the Normal QQ Plot/Histogram?

<!-- -->

A.  Taking the logarithm of `r params$yvar` has made the distribution of the errors perfectly normal.

B.  Taking the logarithm of `r params$yvar` has made the distribution of the errors more normal than when we didn’t take the log.

C.  Taking the logarithm of `r params$yvar` has made the errors independent when they were not previously.")
}

resid_plot_log_yvar <- function()
{
  cat("1.  Which of the following best describes what can be decided from the Residual Plot?

      <!-- -->
      
      A.  The standard deviation in the errors in predicting log `r params$yvar` is roughly the same for each `r params$xvar`.
      
      B.  The errors in predicting log `r params$yvar` follow a normal distribution.
      
      C.  The errors in predicting log `r params$yvar` are independent from individual to individual.")
}

bonferroni_error_rate <- function()
{
  cat("1.  What does it mean for Bonferroni procedure where it states, “The test controls the Type 1 experiment-wise error rate”?

      <!-- -->
      
      A.  The chance of concluding that at least one pairwise comparison among `r params$xvar`s has different mean log `r params$yvar` if there really is not a difference is greater than 0.05.
      
      B.  For each pairwise comparison among `r params$xvar`s, the chance of concluding a difference in mean log `r params$yvar` if there really is not a difference is 0.05.
      
      C.  Both A and B
      
      D.  Neither A nor B")
}

lsd_error_rate <- function()
{
  cat("1.  What does it mean for LSD procedure where it states, “The test controls the Type 1 experiment-wise error rate”?
      
      <!-- -->
      
      A.  The chance of concluding that at least one pairwise comparison among `r params$xvar`s has different mean log `r params$yvar` if there really is not a difference is greater than 0.05.
      
      B.  For each pairwise comparison among `r params$xvar`s, the chance of concluding a difference in mean log `r params$yvar` if there really is not a difference is 0.05.
      
      C.  Both A and B
      
      D.  Neither A nor B")
}

tukey_error_rate <- function()
{
  cat("1.  What does it mean for Tukey's HSD procedure where it states, “The test controls the Type 1 experiment-wise error rate”?
      
      <!-- -->
      
      A.  The chance of concluding that at least one pairwise comparison among `r params$xvar`s has different mean log `r params$yvar` if there really is not a difference is greater than 0.05.
      
      B.  For each pairwise comparison among `r params$xvar`s, the chance of concluding a difference in mean log `r params$yvar` if there really is not a difference is 0.05.
      
      C.  Both A and B
      
      D.  Neither A nor B")
}