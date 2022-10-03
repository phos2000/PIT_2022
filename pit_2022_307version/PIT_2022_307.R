library(tidyverse)
library(Hmisc)
library(kableExtra)
library(gridExtra)
library(psych)
library(gmodels)
library(vcd)
library(tinytex)
library(pollster)


# Set working directory to client folder, read in point in time (PIT) data

# NOTE- this "Disability_Coded" file was created offline 
# to specifically group all possible disabilities into a single variable
pit_data_2022_raw = read.csv("PITSurvey_0925_cleaned.csv", stringsAsFactors = FALSE)

# Type checking

pit_data_2022 = pit_data_2022_raw
pit_data_2022$id[pit_data_2022$id==""] = NA
pit_data_2022$init_hoh[pit_data_2022$init_hoh==""] = NA

# recode to NA

# pit_data_2022[c("gender", "race", "ethnicity")][pit_data_2022[c("gender", "race", "ethnicity")]>6] = NA

# pit_data_2022[c("hoh_tonight", "hoh_children_under18", 
#             "hoh_addfamily", "hoh_nonfamily", "hoh_pet")][
#               pit_data_2022[c("hoh_tonight", "hoh_children_under18", 
#             "hoh_addfamily", "hoh_nonfamily", "hoh_pet")]>6
#             ] = NA
# 
# pit_data_2022[c("first_time_homeless", "shelter", "covid")][
#               pit_data_2022[c("first_time_homeless", "shelter", "covid")]>6
#             ] =  NA
# 
# pit_data_2022[c("covid_result", "disability_reported", "disability_physical")][
#   pit_data_2022[c("covid_result", "disability_reported", "disability_physical")]>6] = NA
# 
# pit_data_2022[c("disability_mental","disability_abuse", "disability_HIV", "income", "violence", "housing_interest", "usmv", "usaf", "ngr","benefit_vamc")][
#   pit_data_2022[c("disability_mental","disability_abuse", "disability_HIV", "income", "violence", "housing_interest", "usmv", "usaf", "ngr", "benefit_vamc")]>6] = NA

# pit_data_2022$gender.labeled = factor(pit_data_2022$gender, 
#                                        levels = c(1:9), 
#                                        labels = c("Female", "Male", "non-binary", "Transgender", "Questioning", "Multigender", "Don't Know", "Refused", "Data Not Collected"))

pit_data_2022$GENDER = factor(pit_data_2022$GENDER)
pit_data_2022$RACE = factor(pit_data_2022$RACE)
pit_data_2022$ethnicity = factor(pit_data_2022$ethnicity)

pit_data_2022$male = ifelse(pit_data_2022$GENDER == 'Male', 1, 0)

# pit_data_2022$race.labeled = factor(pit_data_2022$race, levels = c(1:9), labels = c("Indian", "Asian", "Black", "Islander", "White", "Multiracial", "DK", "Refused", "Data Not Collected"))

#pit_data_2022$ethnicity.labeled = factor(pit_data_2022$ethnicity, levels = c(0,1,7:9), labels = c("Non-Hispanic/Non-Latin(a)(o)(x)", "Hispanic/Latin(a)(o)(x)","DK", "Refused", "Data Not Collected"))


pit_data_2022$white_nonhispanic = ifelse(pit_data_2022$RACE == "White" & pit_data_2022$ethnicity == "Non-Hispanic/Non-Latin(a)(o)(x)", 1, 0)

pit_data_2022$EPISODES = factor(pit_data_2022$EPISODES)
pit_data_2022$PERIOD = factor(pit_data_2022$PERIOD)
# pit_data_2022$nhomelessepisodes.group = cut(x = pit_data_2022$nhomelessepisodes, 
#                                breaks = c(-1, 0, 1, 2, 3, 1000), 
#                                labels = c("0", "1", "2", "3", "4+"))
# pit_data_2022$periodhomeless.group = cut(x=pit_data_2022$periodhomeless, 
#                                breaks = c(0, 12, 1000), 
#                                labels = c("less than a year", "more than a year"))

pit_data_2022$with_others = ifelse(pit_data_2022$hoh_pet == 1 | pit_data_2022$hoh_tonight == 1, 1, 0)

pit_data_2022$CONTINUOUS = factor(pit_data_2022$CONTINUOUS, levels = c(0,1), labels = c("NO", "CONTINUOUSLY HOMELESS FOR 1 YR OR MORE or 4 EPS OF HOMELESSNESS IN PAST 3 YRS"))
pit_data_2022$CHRONIC = factor(pit_data_2022$CHRONIC, levels = c(0,1), labels = c("No", "CHRONIC HOMELSSNESS"))

# pit_data_2022$history = ifelse(pit_data_2022$nhomelessepisodes.group == "4+" |  pit_data_2022$periodhomeless.group == "more than a year", 1, 0)
# pit_data_2022$chronic = ifelse(pit_data_2022$history == 1 & pit_data_2022$disability_any == 1, 1, 0)

# for (i in c("white_nonhispanic", "male", "hoh_tonight", "hoh_spouse", "hoh_children_under18", "hoh_children_1824", "hoh_addfamily", "hoh_nonfamily", "hoh_pet", "with_others", "shelter", "covid", "covid_result", 
#             "disability_reported", "disability_physical", "disability_mental", "disability_abuse", "disability_HIV", "disability_any", 
#             "income", "violence", "housing_interest", "usmv", "usaf","ngr", "benefit_vamc")){
#   pit_data_2022[i] = factor(pit_data_2022[i], 
#                              levels = c(0,1,7:9), 
#                              labels = c("No/None", "Yes/Have","DK", "Refused", "Data Not Collected"))
# }

# pit_data_2020_raw = read.csv("PITSurvey2020_DATA_2020-01-31_DisabilityCoded.csv", stringsAsFactors = FALSE)
# pit_data_2020 = pit_data_2020_raw[-1,]

# define a function for probability calcuations based on n months homeless
# input: original data. optional: condition for subgroups 
calculate_pit_probabilities = function(point_in_time, subsetVariable=NA, subsetCondition=NA) {
  
  # if a value has been provided for subset variable and condition,
  # get valid data percent only based on months of homelessness, not considering subgroup variable, 
  # then create the relevant data subset
  if (!is.na(subsetCondition) & !is.na(subsetVariable)){
    # create subgroup
    subsetCode = paste(subsetVariable,subsetCondition,sep="")
    point_in_time = subset(point_in_time,eval(str2expression(subsetCode)))
  }
  
  # if no subgroup condition was supplied, get percentage of valid data
  # based on duration of homelessness
  
  percent_valid = mean(!is.na(point_in_time$nmonthshomeless)) 
  
  # Either way- continue by subsetting the individuals
  # who did report their duration of homelessness
  months_all_complete = point_in_time[!is.na(point_in_time$nmonthshomeless),c("id","nmonthshomeless")]
  
  # Since 36 was total possible months, replace values larger than that with 36
  months_all_complete$nmonthshomeless[months_all_complete$nmonthshomeless > 36] = 36
  
  # Summarize count by number months
  months_frequency = months_all_complete %>% 
    group_by(nmonthshomeless) %>% 
    summarise(count = n())
  
  # adjust by propotion of missing data
  months_frequency$countWithMissing = months_frequency$count / percent_valid
  
  ## add in probability estimates, adjusting for 0 months
  months_frequency$probability = months_frequency$nmonthshomeless / 36
  months_frequency$probability[months_frequency$nmonthshomeless==0] = 0.01 
  months_frequency$weight = 1/(months_frequency$probability * percent_valid)
  months_frequency$adjusted_count = months_frequency$count * months_frequency$weight
  return(months_frequency)
}

# Second function: plot observed and adjusted distributions
# Optional- include sampling coverage adjustment other than 1
pit_distribution_plots = function(month_frequency, samplingRate=1) {
  
  # adjust probabilities by sampling coverage (no adjustment if 1)  
  month_frequency$adjusted_count2 = month_frequency$adjusted_count / samplingRate
  
  # add sampling rate to plot title
  samplingPercent = samplingRate * 100
  
  # observed plot
  plot1 = month_frequency %>% 
    ggplot(aes(nmonthshomeless, count)) + geom_col() + 
    ggtitle("Observed Distribution") + xlab("Number of Months Homeless") + ylab("Number of Individuals") +
    scale_x_continuous(breaks = seq(0, 36, by = 12)) +
    theme_minimal()
  # adjusted plot
  plot2 = month_frequency %>% ggplot(aes(nmonthshomeless, adjusted_count2)) + geom_col() + 
    ggtitle("Projected Distribution") + xlab("Number of Months Homeless") + ylab("Number of Individuals") +
    scale_x_continuous(breaks = seq(0, 36, by = 12)) +
    theme_minimal()
  grid.arrange(plot1, plot2, ncol=2)
  ggsave("observed_plot.jpeg", plot = plot1)
  ggsave("projected_plot.jpeg", plot = plot2)
}

# Third function: create table with adjusted estimates
pit_count_table = function(month_frequency,missing) {
  
  # convert relevant proportions to percentages for output
  percent_missing = missing * 100
  
  ## initial and adjusted sums: 
  total_nonMissing = sum(month_frequency$count)
  total_withMissing = sum(month_frequency$countWithMissing)
  adjusted_sum_value = sum(month_frequency$adjusted_count) 
  adjusted_sum = adjusted_sum_value / missing
  
  ## output table-insert data
  table_frame = data.frame("InitialTotalCount" = total_withMissing,
                           "ValidCount" = total_nonMissing,
                           "Projected Total" = adjusted_sum)
  
  # create formatted table output
  kable(table_frame, col.names = c("Observed Estimate", "Count with Complete Data", "Projected Total Estimate"), digits = 0)
}

# plot- of percent difference by months
pit_plot_difference=function(month_frequency1,month_frequency2,group1,group2){
  
  # convert counts into adjusted probabilities
  month_frequency1$adj_prob = month_frequency1$adjusted_count / sum(month_frequency1$adjusted_count)
  month_frequency2$adj_prob = month_frequency2$adjusted_count / sum(month_frequency2$adjusted_count)
  
  # probability difference
  months = 0:36
  percent_difference=data.frame(months=months,difference=NA)
  for (ii in months) {
    # compute difference in probabilities
    diff_ii = subset(month_frequency1,nmonthshomeless==ii)$adj_prob -
      subset(month_frequency2,nmonthshomeless==ii)$adj_prob
    # if this is a valid number, add it to dataset
    percent_difference[months==ii,"difference"]=
      ifelse(length(diff_ii)>0,100*diff_ii,NA)
  }
  
  # create labels
  plot_title = paste("Comparison:",group1,"vs.",group2,sep=" ")
  ylab_text = paste("Difference:",group1,"-",group2,"(%)",sep=" ")
  comparePlot = ggplot(percent_difference,aes(months, difference)) + geom_col() + ggtitle(plot_title) + xlab("Number of Months Homeless") + ylab(ylab_text) + ylim(-10,10)
  comparePlot
  
}
#who one is always than 0. 

wtd.describe <- function(x, weights=NULL, trim=.1){
  require(TAM)
  require(diagis)
  require(robsurvey)
  out <- NULL
  # Handling simple vectors
  x <- as.data.frame(x)
  # If no weights given, all weights = 1
  if(is.null(weights)) {weights <- seq(1, nrow(x))}
  i <- 1
  for(colname in colnames(x)){
    # Removing rows with missing data or weight
    d <- x[complete.cases(x[[colname]], weights), , drop=FALSE][[colname]]
    w <- weights[complete.cases(x[[colname]], weights)]
    wd <- data.frame(
      "vars"     = i,
      "n"        = length(d),
      "mean"     = TAM::weighted_mean(d, w = w),
      "sd"       = TAM::weighted_sd(d, w = w),
      "median"   = robsurvey::weighted_median(d, w = w, na.rm = TRUE),
      "trimmed"  = robsurvey::weighted_mean_trimmed(d, w = w, LB = trim, UB = (1 - trim), na.rm = TRUE),  
      "mad"      = robsurvey::weighted_mad(d, w = w, na.rm = TRUE, constant = 1.4826),
      "min"      = min(d),
      "max"      = max(d),
      "range"    = max(d) - min(d),
      "skew"     = TAM::weighted_skewness(d, w = w),
      "kurtosis" = TAM::weighted_kurtosis(d, w = w),
      "se"       = diagis::weighted_se(d, w = w, na.rm = TRUE),
      row.names  = colname
    )
    i <- i+1
    out <- rbind(out, wd)
  }
  return(out)
}

months_frequency_2022=calculate_pit_probabilities(pit_data_2022)

pit_data_2022_weighted = left_join(x = pit_data_2022, y = months_frequency_2022, by = 'nmonthshomeless')
pit_data_2022_weighted = pit_data_2022_weighted[!is.na(pit_data_2022_weighted$weight),]

mean_weight_2022 =  mean(pit_data_2022_weighted$weight)
pit_data_2022_weighted$relative_weight = pit_data_2022_weighted$weight / mean_weight_2022

# A comparison for all calculations
# wtd.describe works better for continuous variable. and the missing data will be set as just 1 value. That's the difference. 
# weights::wpct not so accurate and don't know why -- but will
# pollster will give the missing data the weight(as it have). 

# we will use valid percent

# projected group variables
projected_male = topline(df = pit_data_2022_weighted, variable = GENDER, weight = weight)[2, 4]
projected_race_african_american = topline(df = pit_data_2022_weighted, variable = RACE, weight = weight)[3, 4]
projected_race_white = topline(df = pit_data_2022_weighted, variable = RACE, weight = weight)[8, 4]
# should not include NA when summing up
projected_race_all_other = sum(topline(df = pit_data_2022_weighted, variable = RACE, weight = weight)[c(1,2,6), 4])
projected_ethnicity = topline(df = pit_data_2022_weighted, variable = ethnicity, weight = weight)[3, 4]
projected_hoh_tonight = topline(df = pit_data_2022_weighted, variable = hoh_tonight, weight = weight)[2, 4]
projected_pet = topline(df = pit_data_2022_weighted, variable = hoh_pet, weight = weight)[2, 4]
projected_with_others = topline(df = pit_data_2022_weighted, variable = with_others, weight = weight)[2, 4]

projected_first = topline(df = pit_data_2022_weighted, variable = first_time_homeless, weight = weight)[2, 4]
# projected_first = 0

projected_time_1 = topline(df = pit_data_2022_weighted, variable = EPISODES, weight = weight)[1, 4]
projected_time_2 = topline(df = pit_data_2022_weighted, variable = EPISODES, weight = weight)[2, 4]
projected_time_3 = topline(df = pit_data_2022_weighted, variable = EPISODES, weight = weight)[3, 4]
projected_time_4 = topline(df = pit_data_2022_weighted, variable = EPISODES, weight = weight)[4, 4]
projected_shelter = topline(df = pit_data_2022_weighted, variable = shelter, weight = weight)[2, 4]
projected_covid = topline(df = pit_data_2022_weighted, variable = covid, weight = weight)[2, 4]
projected_physical_disability = topline(df = pit_data_2022_weighted, variable = disability_physical, weight = weight)[2, 4]
projected_mental_disability = topline(df = pit_data_2022_weighted, variable = disability_mental, weight = weight)[2, 4]
projected_use_disorder = topline(df = pit_data_2022_weighted, variable = disability_abuse, weight = weight)[2, 4]
projected_HIV = topline(df = pit_data_2022_weighted, variable = disability_HIV, weight = weight)[2, 4]
projected_any_disability = topline(df = pit_data_2022_weighted, variable = disability_any, weight = weight)[1, 4]
projected_chronic = topline(df = pit_data_2022_weighted, variable = CHRONIC, weight = weight)[2, 4]
projected_violence = topline(df = pit_data_2022_weighted, variable = violence, weight = weight)[2, 4]
projected_income = topline(df = pit_data_2022_weighted, variable = income, weight = weight)[2, 4]
projected_housing_interest = topline(df = pit_data_2022_weighted, variable = housing_interest, weight = weight)[2, 4]
projected_military_veteran = topline(df = pit_data_2022_weighted, variable = usmv, weight = weight)[2, 4]

# the proportion is from the interviewees who have answered the question about homeless history. 

observed_male = 100 * prop.table(table(pit_data_2022_weighted$GENDER))["Male"]
observed_race_african_american = 100 * prop.table(table(pit_data_2022_weighted$RACE))["Black or African American"]
observed_race_white = 100 * prop.table(table(pit_data_2022_weighted$RACE))["White"]
observed_race_all_other = 100 *  sum(prop.table(table(pit_data_2022_weighted$RACE))[c("American Indian, Alaska Native", "Asian or Asian American", "Multiracial")])
observed_ethnicity = 100 * prop.table(table(pit_data_2022_weighted$ethnicity))["Hispanic/Latin(a)(o)(x)"]
observed_hoh_tonight = 100 * prop.table(table(pit_data_2022_weighted$hoh_tonight))["1"]
observed_pet = 100 * prop.table(table(pit_data_2022_weighted$hoh_pet))["1"]
observed_with_others = 100 * prop.table(table(pit_data_2022_weighted$with_others))["1"]

observed_first = 100 * prop.table(table(pit_data_2022_weighted$first_time_homeless))["1"]
# observed_first = 0

observed_time_1 = 100 * prop.table(table(pit_data_2022_weighted$EPISODES))["1"]
observed_time_2 = 100 * prop.table(table(pit_data_2022_weighted$EPISODES))["2"]
observed_time_3 = 100 * prop.table(table(pit_data_2022_weighted$EPISODES))["3"]
observed_time_4 = 100 * prop.table(table(pit_data_2022_weighted$EPISODES))["4+"]
observed_shelter = 100 * prop.table(table(pit_data_2022_weighted$shelter))["1"]
observed_covid = 100 * prop.table(table(pit_data_2022_weighted$covid))["1"]
observed_physical_disability = 100 * prop.table(table(pit_data_2022_weighted$disability_physical))["1"]
observed_mental_disability = 100 * prop.table(table(pit_data_2022_weighted$disability_mental))["1"]
observed_use_disorder = 100 * prop.table(table(pit_data_2022_weighted$disability_abuse))["1"]
observed_HIV = 100 * prop.table(table(pit_data_2022_weighted$disability_HIV))["1"]
observed_any_disability = 100 * prop.table(table(pit_data_2022_weighted$disability_any))["1"]
observed_chronic = 100 * prop.table(table(pit_data_2022_weighted$CHRONIC))["CHRONIC HOMELSSNESS"]
observed_violence = 100 * prop.table(table(pit_data_2022_weighted$violence))["1"]
observed_income = 100 * prop.table(table(pit_data_2022_weighted$income))["1"]
observed_housing_interest = 100 * prop.table(table(pit_data_2022_weighted$housing_interest))["1"]
observed_military_veteran = 100 * prop.table(table(pit_data_2022_weighted$usmv))["1"]


# Percentages for discrete varibles

projected_discrete = c(projected_male, projected_race_african_american, projected_race_white, projected_race_all_other, projected_ethnicity, projected_hoh_tonight, projected_pet, projected_with_others, projected_first, projected_time_1, projected_time_2, projected_time_3, projected_time_4, projected_shelter, projected_covid, projected_physical_disability, projected_mental_disability, projected_use_disorder, projected_HIV, projected_any_disability, projected_chronic, projected_violence, projected_income, projected_housing_interest, projected_military_veteran)

observed_discrete = c(observed_male, observed_race_african_american, observed_race_white, observed_race_all_other, observed_ethnicity, observed_hoh_tonight, observed_pet, observed_with_others, observed_first, observed_time_1, observed_time_2, observed_time_3, observed_time_4, observed_shelter, observed_covid, observed_physical_disability, observed_mental_disability, observed_use_disorder, observed_HIV, observed_any_disability, observed_chronic, observed_violence, observed_income, observed_housing_interest, observed_military_veteran)

discrete_rownames = c("Male", "Race:  African American", "Race:  White", "Race:  All other", "Ethnicity:  Hispanic/Latino", "With any family or non-family members staying with you", "With pet(s)", "With either someone else or pets (this is union of last two)", "First time homeless", "Times homeless in last three years = 1", "Times homeless = 2", "Times homeless = 3", "Times homeless = 4 or more", "Stayed at any shelter", "Had Covid", "Physical disability", "Serious mental illness", "Substance use disorder", "HIV/AIDS", "Any disability", "Meets chronic definition", "Homeless because of violence", "SSI/SSDI income", "Interested in housing if needs met", "Military veteran")

discrete_desc = data.frame(cbind(observed_discrete, projected_discrete), row.names = discrete_rownames)
colnames(discrete_desc) = c("Observed %", "Projected %")

discrete_desc


# Distributions and Graphs for continuous variables 

# age
projected_age = wtd.describe(pit_data_2022_weighted$age, weights = pit_data_2022_weighted$weight)
age_2022 = na.omit(pit_data_2022_weighted$age)
weight_age_2022 = pit_data_2022_weighted$weight[!is.na(pit_data_2022_weighted$age)]

observed_age = psych::describe(pit_data_2022$age)

# age first homeless

projected_age_first_homeless = wtd.describe(pit_data_2022_weighted$age_homeless, weights = pit_data_2022_weighted$weight)
age_homeless_2022 = na.omit(pit_data_2022_weighted$age_homeless)
weight_age_homeless_2022 = pit_data_2022_weighted$weight[!is.na(pit_data_2022_weighted$age_homeless)]

observed_age_first_homeless = psych::describe(pit_data_2022$age_homeless)

# months homeless

projected_months_homeless = wtd.describe(pit_data_2022_weighted$nmonthshomeless, weights = pit_data_2022_weighted$weight)

observed_months_homeless = psych::describe(pit_data_2022$nmonthshomeless)

age_desc = c(observed_age$mean, observed_age$sd, projected_age$mean, projected_age$sd)
age_first_homeless_desc = c(observed_age_first_homeless$mean, observed_age_first_homeless$sd, projected_age_first_homeless$mean, projected_age_first_homeless$sd)
months_homeless_desc = c(observed_months_homeless$mean, observed_months_homeless$sd, projected_months_homeless$mean, projected_months_homeless$sd)
continuous_desc = data.frame(rbind(age_desc, age_first_homeless_desc, months_homeless_desc), row.names = c("Age", "Age at first homelessness", "# months homeless (top code at 36)"))
colnames(continuous_desc) = c("mean", "sd", "mean", "sd")

continuous_desc

# plots
plotrix::weighted.hist(x =age_2022, w = weight_age_2022, breaks = 10, main = "Projected Distribution: Age")
hist(x=pit_data_2022$age, main = "Observed Distribution: Age")
plotrix::weighted.hist(x = age_homeless_2022, w = weight_age_homeless_2022, breaks = 10, main = "Projected Age at first homelessness")
hist(x=pit_data_2022$age_homeless, breaks = c(7, 13.1, 19.2, 25.3, 31.4, 37.5, 43.6, 49.7, 55.8, 61.9, 68), main = "Observed Age at first homelessness")

# the months for homelessness could use originial weight. Because the weights are obtained from the months of homelessness. 

observed_everyone = nrow(pit_data_2022)
observed_valid = nrow(pit_data_2022_weighted)
pit_distribution_plots(months_frequency_2022,1.0)


# Number of people observed and projected

pit_count_table(months_frequency_2022,1.0)

pit_data_2022$shelter = factor(pit_data_2022$shelter, levels = c(0,1,9), labels = c("No Use", "Use", "Missing"))
pit_data_2022$hoh_tonight = factor(pit_data_2022$hoh_tonight, levels = c(0,1,9), labels = c("Self", "Other", "Missing"))
pit_data_2022$with_others = factor(pit_data_2022$with_others, levels = c(0,1), labels = c("Self", "With someone/pet(s)"))
pit_data_2022$white_nonhispanic = factor(pit_data_2022$white_nonhispanic, levels = c(0,1), labels = c("No", "White and Non-His/Lat"))
pit_data_2022$violence = factor(pit_data_2022$violence, levels = c(0,1,7,9), labels = c("No", "Yes", "DK", "Missing"))
pit_data_2022$male = factor(pit_data_2022$male, levels = c(0,1), labels = c("Male", "Not Male"))

# Crosstables for the observed data

## Stayed at any shelter by with someone

CrossTable(pit_data_2022$shelter, pit_data_2022$hoh_tonight, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE)

## Stayed at any shelter by with someone or with a pet

CrossTable(pit_data_2022$shelter, pit_data_2022$with_others, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE)

## Stayed at any shelter by meets chronic definition

CrossTable(pit_data_2022$shelter, pit_data_2022$CHRONIC)

## Stayed at any shelter by race (white non-hispanic vs. all other)

CrossTable(pit_data_2022$shelter, pit_data_2022$white_nonhispanic, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE)

## Stayed at any shelter by homeless due to violence

CrossTable(pit_data_2022$shelter, pit_data_2022$violence, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE)

## Homeless due to violence by gender (use all the categories)

CrossTable(pit_data_2022$GENDER, pit_data_2022$violence, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE)

## Homeless due to violence by gender dichotomy (male vs. all others)

CrossTable(pit_data_2022$violence, pit_data_2022$male, prop.r=FALSE, prop.c=FALSE,
           prop.t=TRUE, prop.chisq=FALSE, chisq = TRUE)