
rm(list = ls()) # cleans up whatever was open in R before

## load packages
library(foreign)
library(tidyverse)
library(ggdag)
library(dplyr)
library(haven)


# read in data file
VSL_Project <- read_dta("YOUR FOLDER PATH/ProjectVSL_Data.dta")

# generate dummy variable "old", where old is defined as all workers above the age of 45
    VSL_Project$old <- ifelse(VSL_Project$age > 45, 1, 0)

# summary stats of risk levels; see Table 1 of Gegax at al. (1991).
   # Examples:  
    sample_union_blue <- filter(VSL_Project, union == 1 & blue == 1)  # defines the subsample of blue-collar workers who are member of a labor union
    summary(sample_union_blue$risk) # calculates average risk unionized blue-collar workers face in their jobs

    meanrisk_union_blue <- round(mean(sample_union_blue$risk),digits = 3) # To be used later in Table to beconstructed for Task 4.
    summary(meanrisk_union_blue)
    
    sample_union_all <- filter(VSL_Project, union == 1)  # defines the subsample of all workers who are member of a labor union
    summary(sample_union_all$risk)
    meanrisk_union_all <- round(mean(sample_union_all$risk),digits = 3)
    summary(meanrisk_union_all)
    
    # Task 1A: The above code calculates the means, but not standard deviations. Adjust the code. 1B. Replicate the remainder of Table 1, preferably using the type of code to construct a table as presented for the VSL calculations; see lines 86-91.   

    
# Who selects into risky jobs?
      reg1_1 <- lm(risk ~ age + race + sex + disab + vet + live # The personal characteristics indicators
                  , data = VSL_Project)
      summary(reg1_1)
    
    # Task 2A. Do "older people" select in the safer jobs? Replace "age" by "old", and interpret the results. 2B. Using a similar regression but a different dependent variable, who selects into unionized jobs? 2C. And are unionized jobs more risky that non-unionized jobs? (Hint: for 2C, run a t-test).


# There are three sets of worker and job characteristics
  # The human capital indicators are: SCHOOL1 SCHOOL2 SCHOOL3 SCHOOL4 SCHOOL5 yrspo yrsft yrspe
    #SCHOOL6 is the omitted category; when running the regressions, this variable does NOT have to be included
  # The indicators of the work environment are: RQSCHL1 RQSCHL2 RQSCHL3 RQSCHL4 wkexp super govt union yrsqual miles number central service trans labor equip craft cleric sales manage
    #RQSCHL5 and PROF are the omitted categories; when running the regressions, these variables do NOT have to be included
  # The indicators of the personal characteristics are: age race sex disab vet live
    # For this type of characteristics there are no omitted categories
# If you use any of these worker or job characteristics indicators in a regression, always include ALL indicators that belong to the set 
      
      
# This regression replicates Table 2
      reg2 <- lm(lnrwage ~ risk + 
                   SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live # The personal characteristics indicators
                   , data = VSL_Project)
      summary(reg2)


# This replicates the first result of Table 3
      reg3_1 <- lm(lnrwage ~ risk + 
                   SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live # The personal characteristics indicators
                   , data = sample_union_blue)
      summary(reg3_1)
      
      riskcoef_union_blue <- round(coef(summary(reg3_1))["risk","Estimate"], digit = 4) # store coefficient with 4 decimals; needed to calculate VSL later on
      summary(riskcoef_union_blue)

  # Task 3: Replicate the remainder of Table 3, and interpret the results. Preferable present the results in a Table using a code similar to the code written to produce a table for Task 4 (lines 86-91).

  # Task 4: For all those subsamples for which the coefficient on risk was signifIcant, calculate the VSL in three steps:
        # Step 1:Calculate the mean ANNUAL income level (= wage rate x 2000, as wage rate is hourly, and a yr consists of 2000 working hours)
        # Step 2: Calculate the VSL 4000 X risk coefficient of the subsample x the subsample's mean annual income (as the risk is the number of deaths per 4000 workers)
      
      # Example:
          # VSL_union_blue
            summary(sample_union_blue$wagerate)    
            meanannualwagerate_union_blue <- 2000 * round(mean(sample_union_blue$wagerate),digits = 2) # Measures the mean wage rate of the subsample of interest
            summary(meanannualwagerate_union_blue)
            VSL_union_blue <- round(4000 * riskcoef_union_blue * meanannualwagerate_union_blue, digits = 0) #Calculates the VSL
            summary(VSL_union_blue) 

      # VSL result summarized in a Table:
          tab_VSL <- matrix(c(meanrisk_union_blue,VSL_union_blue), ncol=2, byrow=TRUE)
          colnames(tab_VSL) <- c('Perceived Risk','VSL')
          rownames(tab_VSL) <- c('Union Blue')
          tab_VSL <- as.table(tab_VSL)
          tab_VSL

# Are some worker types (age (or old), race, sex, disab, vet, live) more averse to risk (= have a higher MWTP) than others?
      reg5_1 <- lm(lnrwage ~ risk  + risk*old +
                   SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live # The personal characteristics indicators
                 , data = VSL_Project)
      summary(reg5_1)

    # Task 5: 5A. Do the same for the subset of blue-collar workers who are member of a labor union. 5B. Do the same analysis for the other worker characteristics as defined on line 47, using the subset of union members. 



# Task 6: 6A. What about elderly males -- do they have a lower VSL than elderly females, or than younger males? (Hint: use "old"!, and do not just calculate the MWTP, but also their VSL (see Task 4)).
        # 6B. Do public sector (government) employees tend to have a lower VSL than private sector (non-government) employees?


# Task 7: write up the results of all Tasks concisely in a short paper, where you try to interpret the results. When looking at all results, what have you learnt about the VSL? Can it be measured using this technique, does it vary with personal characteristics as expected, and do the persons with the highest tolerance to risk select into the more risky jobs? 