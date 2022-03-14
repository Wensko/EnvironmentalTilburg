
rm(list = ls()) # cleans up whatever was open in R before

## load packages
library(foreign)
library(tidyverse)
library(ggdag)
library(dplyr)
library(haven)
library(stargazer)


# read in data file
VSL_Project <- read_dta("ProjectVSL_Data.dta")

# generate dummy variable "old", where old is defined as all workers above the age of 45
    VSL_Project$old <- ifelse(VSL_Project$age > 45, 1, 0)

# summary stats of risk levels; see Table 1 of Gegax at al. (1991).
# 'union.blue', 'union.white', 'nunion.blue', 'nunion.white', 'all.union', 'all.nunion', 'white', 'blue', 'full')

    ##CREATING SUBSAMPLES
    union.blue <- filter(VSL_Project, union == 1 & blue == 1)
    union.white <- filter(VSL_Project, union == 1 & blue == 0)
    nunion.blue <- filter(VSL_Project, union == 0 & blue == 1)  
    nunion.white <- filter(VSL_Project, union == 0 & blue == 0)
    all.union <- filter(VSL_Project, union == 1)
    all.nunion <- filter(VSL_Project, union == 0)
    white <- filter(VSL_Project, blue == 0)
    blue <- filter(VSL_Project, blue == 1)
    full <- VSL_Project
    
    a <- c('union.blue', 'union.white', 'nunion.blue', 'nunion.white', 'all.union', 'all.nunion', 'white', 'blue', 'full')
    b <- c()
    c <- c()
    d <- c()
    e <- c()
    ## PUT ALL SAMPLS IN A LIST SO I CAN LOOP
    base <- list(union.blue, union.white, nunion.blue, nunion.white, all.union, all.nunion, white, blue, full)
    names(base) <- a
    
    for (i in 1:length(base)) {
      b <- c(b,nrow(base[[i]]))
      c <- c(c, round(mean(base[[i]]$risk), digits = 3))
      d <- c(d, round(sd(base[[i]]$risk), digits = 3))
      e <- c(e, round(mean(base[[i]]$fatal), digits = 3))
      
    }
    #TABLE FOR SUMMARY STATS
tabela1 <- cbind(a,b,c,d)
colnames(tabela1) <- c('subsample', 'N', 'Risk', 'SD')
stargazer(tabela1, type = 'text')

# Who selects into risky jobs?
reg1_1 <- lm(risk ~ age + race + sex + disab + vet + live # The personal characteristics indicators
                  , data = VSL_Project)
summary(reg1_1)

## FROM HERE, IT SEEMS MEN AND PEOPLE WHO DOESN'T LIVE IN CENTRAL CENTERS SELECT INTO RISKIER JOBS.
#  NEVERTHELESS, WE GOTTA TAKE CARE OF ENDOGENEITY AND OMITTED VARIABLE BIAS
# AS LIVIN IN THE SUBURBS MIGHT RESTRICT WHAT KINDS OF JOBS ARE AVAILABLE TO YOU, FOR EXAMPLE. OLDER
# PEOPLE ALSO SEEM TO AVOID RISKY. 

    
# Task 2A. Do "older people" select in the safer jobs? Replace "age" by "old", and interpret the results. 
# 2B. Using a similar regression but a different dependent variable, who selects into unionized jobs? 
# 2C. And are unionized jobs more risky that non-unionized jobs? (Hint: for 2C, run a t-test).

reg2_A <- lm(risk ~ old + race + sex + disab + vet + live # The personal characteristics indicators
             , data = VSL_Project)
summary(reg2_A)
## WE NOW HAVE SIMILAR RESULTS. THE ONLY THING WE GOTTA STRESS IS THE FACT THAT NOW, USING A DUMMY, WE BE 
## ESTIMATING CONDICIONAL MEANS TO BINARY VARIABLES (THERE ISN'T ANY CONTINUOUS VARIABLE)

reg2_B <- lm(union ~ risk + race + sex + disab + vet + live # The personal characteristics indicators
             , data = VSL_Project)
summary(reg2_B)

# IN THIS CASE, IT SEEMS THAT VERERANS AND THOSE WHO WORK INDO RISKIER OCCUPATIONS PARTICIPATE IN UNIONS
# MORE. AGAIN, WE GOTTA PAY ATTENTION TO ENDOGENEITY. IT MIGHT BE THAT UNIONS ARE MORE PRESENT IN RISKIER
# OCCUPATIONS TO FULFIL BIGGER DEMAND BY THE WORKERS. IT MIGHT ALSO BE THE CASE, AS STRESSED BY THE PAPER
# THAT UNIONS RAISE AWARENESS REGARDING RISKS, AND THEN THIS CAUSE A ELEVATED PERCEPTION OF RISK IN UNIONIZED
# WORKERS.

test2_c <- t.test(all.union$risk, all.nunion$risk)

# ACCORDING TO A SIMPLE T-TEST ON MEANS, WE CAN REJECT THE HYPOTHESIS THAT THE RISK IN THE TWO 
# STRATS ARE THE SAME. INDEED, THE RISK IS GREATER AT UNIONIZED JOBS (MEAN 3.442482)

      
# This regression replicates Table 2
      reg2 <- lm(lnrwage ~ risk + 
                   SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live # The personal characteristics indicators
                   , data = VSL_Project)
      summary(reg2)

      # Task 3: Replicate the remainder of Table 3, and interpret the results. Preferable present the results in a Table using a code similar to the code written to produce a table for Task 4 (lines 86-91).
      
      
      ## I CREATED 2 LISTS, SO I CAN LOOP FOR THE VARIABLE RISK AND FOR THE VARIABLE FATAL
      regs3_risk <- list()
      regs3_fatal <- list()
      # IN THIS LOOP, WE CALCULATE THE REGRESSIONS OF WAGE IN RISK/FATAL AND THE OTHER COVARIATES
      for (i in 1:length(base)) {
        regs3_risk[[i]] <- lm(lnrwage ~ risk + 
                      SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                      RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                      age + race + sex + disab + vet + live # The personal characteristics indicators
                      , data = base[[i]])
        regs3_fatal[[i]] <- lm(lnrwage ~ fatal + 
                                SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                                RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                                age + race + sex + disab + vet + live # The personal characteristics indicators
                              , data = base[[i]])
        
      }
      

      
      ## IN THIS SECTION, WE EXTRACT THE ESTIMATES AND T-VALUES FROM THE REGRESSIONS TO CREATE TABLE 3
      
      tabela.risk <- data.frame(NA)
      for (i in 1:9) {
        tabela.risk[i,1] <- a[i]
        tabela.risk[i,2] <- as.numeric(round(coef(summary(regs3_risk[[i]]))['risk','Estimate'], digits = 4))
        tabela.risk[i,3] <- as.numeric(round(coef(summary(regs3_risk[[i]]))['risk','t value'], digits = 4))
        
        # Task 4: For all those subsamples for which the coefficient on risk was signifIcant, calculate the VSL in three steps:
        # Step 1:Calculate the mean ANNUAL income level (= wage rate x 2000, as wage rate is hourly, and a yr consists of 2000 working hours)
        # Step 2: Calculate the VSL 4000 X risk coefficient of the subsample x the subsample's mean annual income (as the risk is the number of deaths per 4000 workers)
        
        tabela.risk[i,4] <- 2000*round(mean(base[[i]]$wagerate), digits = 2)
        tabela.risk[i,5] <- c[i]
        tabela.risk[i,6] <- 4000*tabela.risk[i,2]*tabela.risk[i,4]
      }
      colnames(tabela.risk) <- c('subsamples', 'coefficient', 't-stat', 'Mean Annual Wage Rate', 'Mean Perceived Risk', 'VSL')
      tabela.fatal <- data.frame(NA)
      for (i in 1:9) {
        tabela.fatal[i,1] <- a[i]
        tabela.fatal[i,2] <- as.numeric(round(coef(summary(regs3_fatal[[i]]))['fatal','Estimate'], digits = 4))
        tabela.fatal[i,3] <- as.numeric(round(coef(summary(regs3_fatal[[i]]))['fatal','t value'], digits = 4))
        
        # Task 4: For all those subsamples for which the coefficient on risk was signifIcant, calculate the VSL in three steps:
        # Step 1:Calculate the mean ANNUAL income level (= wage rate x 2000, as wage rate is hourly, and a yr consists of 2000 working hours)
        # Step 2: Calculate the VSL 4000 X risk coefficient of the subsample x the subsample's mean annual income (as the risk is the number of deaths per 4000 workers)
        
        tabela.fatal[i,4] <- 2000*round(mean(base[[i]]$wagerate), digits = 2)
        tabela.fatal[i,5] <- e[i]
        tabela.fatal[i,6] <- 1000*tabela.fatal[i,2]*tabela.fatal[i,4]
      }
      colnames(tabela.fatal) <- c('subsamples', 'coefficient', 't-stat', 'Mean Annual Wage Rate', 'Mean Fatal', 'VSL')
      
      ## THE OBJECTS 'tabela.x' ARE TABLES CONTAININ NOT ONLY THE TABLE 3 BUT ALSO SOME THINGS THAT WILL BE REQUIRED FOR TASK 4
      # HERE EXTRACT FROM 'tabela.x' the data for table 3
      table3 <- cbind(tabela.risk[,1:3], tabela.fatal[2:3])

      # Task 4: For all those subsamples for which the coefficient on risk was signifIcant, calculate the VSL in three steps:
      # Step 1:Calculate the mean ANNUAL income level (= wage rate x 2000, as wage rate is hourly, and a yr consists of 2000 working hours)
      # Step 2: Calculate the VSL 4000 X risk coefficient of the subsample x the subsample's mean annual income (as the risk is the number of deaths per 4000 workers)
      
      ## Subsamples who present significant coeficients in table 3
      # summary(regs3_risk[[1]]) #Union Blue
      # summary(regs3_risk[[5]])  #all union
      # summary(regs3_risk[[8]])  #blue
      # summary(regs3_fatal[[1]])
      # summary(regs3_fatal[[5]])
      # summary(regs3_fatal[[8]])
      
      # USING 'tabela.x' WE SOLVE TASK 4
      task4.table <- cbind(tabela.risk[c(1,5,8),c(1,4:6)], tabela.fatal[c(1,5,8),4:6]) 

# Are some worker types (age (or old), race, sex, disab, vet, live) more averse to risk (= have a higher MWTP) than others?

      # AS SUGGESTED BY THE PROFESSOR, WE USE THE INTERACTION BETWEEN TYPES AND RISK TO CALCULATE IF
      # ANY TYPE IS ABLE TO EXTRACT HETEROGENOUS PREMIUMS FROM RISKS
      # THESE LISTS ARE GONNA STORE THE REGRESSIONS FOR EACH SAMPLE (BLUE.UNION AND ALL.UNION)
      # AND EACH TYPE (old, race, sex, disab, vet, live)
      task5_union.blue.regs <- list()
      task5_all.union.regs <- list()
      
      #WE GONNA USE THIS INSIDE THE LOOP
      chars <- c('old', 'race', 'sex', 'disab', 'vet', 'live')
      
      # THESE DATAFRAMES WERE CREATED SO WE CAN STORE THE VALUES FOR ESTIMATES OF THE INTERACTION
      # TERMS AND THEIR Pr(>|t|). 
      
      task5_union.blue.estimates <- data.frame(Characteristics = chars, Estimate = NA, pvalue = NA)
      task5_all.union.estimates <- data.frame(Characteristics = chars, Estimate = NA, pvalue = NA)
      for (i in 1:6) {
        formula.reg <- paste('lnrwage ~ risk  + risk*', chars[i],'+ SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live')
        task5_union.blue.regs[[i]] <- lm(formula = formula.reg, data=union.blue)
        task5_all.union.regs[[i]] <- lm(formula = formula.reg, data = all.union)
        task5_union.blue.estimates[i,2] <- round(coef(summary(task5_union.blue.regs[[i]]))[paste('risk', chars[i], sep = ':'), 'Estimate'], digits = 4)
        task5_all.union.estimates[i,2] <- round(coef(summary(task5_all.union.regs[[i]]))[paste('risk', chars[i], sep = ':'), 'Estimate'], digits = 4)
        task5_union.blue.estimates[i,3] <- round(coef(summary(task5_union.blue.regs[[i]]))[paste('risk', chars[i], sep = ':'), 'Pr(>|t|)'], digits = 4)
        task5_all.union.estimates[i,3] <- round(coef(summary(task5_all.union.regs[[i]]))[paste('risk', chars[i], sep = ':'), 'Pr(>|t|)'], digits = 4)
      }
      stargazer(task5_all.union.estimates, task5_union.blue.estimates,summary = F, type = 'text')
      
      # NONE OF THE TERMS GOT ANY STATISTICAL SIFNIFICANCE
      {
      reg5_1 <- lm(lnrwage ~ risk  + risk*old +
                   SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                   RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                   age + race + sex + disab + vet + live # The personal characteristics indicators
                 , data = base[[9]])
      summary(reg5_1)

    # Task 5: 5A. Do the same for the subset of blue-collar workers who are member of a labor union. 5B. Do the same analysis for the other worker characteristics as defined on line 47, using the subset of union members. 

      reg5_1 <- lm(lnrwage ~ risk  + risk*old +
                     SCHOOL1 + SCHOOL2 + SCHOOL3 + SCHOOL4 + SCHOOL5 + yrspo + yrsft + yrspe + # The human capital indicators
                     RQSCHL1 + RQSCHL2 + RQSCHL3 + RQSCHL4 + wkexp + super + govt + union + yrsqual + miles + number + central + service + trans + labor + equip + craft + cleric + sales + manage + # The indicators of the work environment
                     age + race + sex + disab + vet + live # The personal characteristics indicators
                   , data = union.blue)
      summary(reg5_1)
      
}## Here is the professor examples using my datasets

      
      ### I STILL HAVEN'T DONE 6 AND 7. I ALSO INTEND TO PLOT SOME GRAPHS SO WE CAN PUT THEM IN THE PAPER
      
# Task 6: 6A. What about elderly males -- do they have a lower VSL than elderly females, or than younger males? (Hint: use "old"!, and do not just calculate the MWTP, but also their VSL (see Task 4)).
        # 6B. Do public sector (government) employees tend to have a lower VSL than private sector (non-government) employees?


# Task 7: write up the results of all Tasks concisely in a short paper, where you try to interpret the results. When looking at all results, what have you learnt about the VSL? Can it be measured using this technique, does it vary with personal characteristics as expected, and do the persons with the highest tolerance to risk select into the more risky jobs? 