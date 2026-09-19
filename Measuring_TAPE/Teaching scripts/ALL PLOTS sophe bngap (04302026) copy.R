# PLOTS for 2026 Conferences!


# Install and load required libraries
# install.packages("usmap")
# install.packages("ggplot2")
# install.pack
install.packages("ggstats")
library(usmap)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(magrittr)
library(likert)
library(scales)
library(ggstats)
library(labelled)




# establishing color palettes
my_colors <- c("#ac00e5", "#FFa488", "#cdfcfc")
alt_colors <- c("#dee23b", "#cdfcfc","#FFa488", "#ac00e5")
all_colors <- c("#cdfcfc", "#FDEFFF", "#FFB8C7", "#dee23b", "#FFa488", "#B03060", "#ac00e5", "#54235C")
rev_all_colors <- c("#54235C", "#ac00e5","#B03060", "#FFa488","#dee23b", "#FFB8C7", "#FDEFFF","#cdfcfc")
nine_colors <- c( c("#54235C", "#ac00e5","#B03060", "#FFa488","#dee23b", "#FFB8C7", "#FDEFFF","#FFFFE0", "#cdfcfc"))


#########
# SOPHE #
#########


# Loading study data
df <- read.csv('/Users/natajahroberts/Desktop/TAPE_GIPT_clean_merge(122)_V2.csv', 
               stringsAsFactors = TRUE)
ref <- colnames(df)
view(ref)




### SAMPLE ###
#dev.off()

#### Gender Pie & Bar Charts ####
# start by fixing bad factors
df$Gender <- fct_recode(df$Gender, "Transman / Genderqueer" = "transman, genderqueer")

# now reordering the levels
df$Gender <- fct_relevel(df$Gender, "Woman or feminine person", "Nonbinary", "Genderqueer", 
                         "Man or masculine person", "Agender")

# Pie chart
pie(table(df$Gender), , main = "Gender", col = c("#54235c","#FFa488", "#cdfcfc", "#ac00e5"))

# Barplot using base R
gender_bar <- barplot(table(df$Gender), las = 2, main = "Gender", 
                      col = c("#54235c","#FFa488", "#cdfcfc", "#ac00e5"))


# FINAL VERSION
# Barchart using ggplot because I need coord_flip()
gender_bar2 <- df  %>% ggplot(aes(x=Gender, fill=Gender)) + geom_bar() +    # makes barplot
  coord_flip() + scale_fill_manual(values = rev_all_colors) + theme_bw() +   # flips coord, manual colors, sets theme
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),         # removes x axis title, label, ticks
                                 axis.title = element_blank()) + 
  theme(axis.text.y = element_text(face = "bold"))                          # makes y labels bold

gender_bar2

# get percentages to add later
gender_summary <- df %>% group_by(Gender) %>%
  summarise(Percentage = n() / nrow(df) * 100)
gender_summary









#### Sexuality Pie & Bar Charts ####
# fixing factors again
levels(df$Sexuality)
table(df$Sexuality)
df$Sexuality <- fct_recode(df$Sexuality, "Queer" = "queer", "Queer" = "Queer, why on earth isn’t this an option?")
df$Sexuality <- fct_relevel(df$Sexuality, "Bisexual / Bi+ / Pansexual", "Heterosexual / Straight", "Queer", 
                            "Gay / Lesbian / Homosexual", "Demisexual", "Asexual")

barplot(table(df$Sexuality), las = 2,
        main = "Sexuality", col = c("#54235c","#FFa488", "#cdfcfc", "#ac00e5"))

pie(table(df$Sexuality), main = "Sexuality", col = c("#54235c","#FFa488", "#cdfcfc", "#ac00e5"))




# FINAL VERSION
# Barchart using ggplot because I need coordflip
sexuality_bar2 <- df  %>% ggplot(aes(x=Sexuality, fill=Sexuality)) + geom_bar() + 
  coord_flip() + scale_fill_manual(values = rev_all_colors) + theme_bw() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank()) + theme(axis.text.y = element_text(face = "bold"))
sexuality_bar2


# get percentages to add later
sexuality_summary <- df %>% group_by(Sexuality) %>%
  summarise(Percentage = n() / nrow(df) * 100)
sexuality_summary



  
  
  


#### Race Bar chart ####

# extracting and transforming race variables
race <- df[50:57]

# summarising data like it's a table so I get accurate counts before I pivot
race_sums <- race  %>% summarise(across(everything(), sum))
str(race_sums)

# pivot longer to get two columns, races and counts
long_race <- race_sums %>% pivot_longer(everything(), names_to = 'races', values_to = 'count')

# fixing names in races
str(long_race) # checking that it's a tibble

# making races a factor for ease later
long_race$races <- as.factor(long_race$races)

# using mutate and recode to change individual values in the races column
long_race <- long_race %>% 
  mutate(races = fct_recode(races, "American Indian/Alaska Native" = "Amer_Native"))

# I think it only takes one argument so I have to do it a second time for the other
long_race <- long_race %>% 
  mutate(races = fct_recode(races,"Native Hawaiian/Pacific Islander" = "Pacific_Native"))

#checking 
long_race

  
# Plotting race
# note: to achieve this with two columns instead of one
# you need to assign x to the category and y to the value
# AND you need to keep stat = "identity" in geom_bar()
# I also built in the reorder to fix the bar orders since I can't refactor long_race
# using -count so it goes in descending order (confusing cause biggest is on bottom)

race_bar <- ggplot(long_race, aes(x = reorder(races, -count), y = count, fill = races)) +
  geom_bar(stat = "identity") + 
  coord_flip() + scale_fill_manual(values = rev_all_colors) + theme_bw() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank()) + theme(axis.text.y = element_text(face = "bold")) + labs(fill = "Racial / Ethnic Identity")
  
race_bar


# get percentages to add later
# doing it manually cause long_df is weird

white <- 111
asian <- 5
native <- 4
black <- 5
latine <- 5
mena <- 1

white_p <- white / nrow(df) *100
asian_p <- asian / nrow(df) *100
native_p <- native / nrow(df) *100
black_p <- black / nrow(df) *100
latine_p <- latine / nrow(df) *100
mena_p <- mena / nrow(df) *100
# other and native hawaiian have 0

percentages <- c(white_p, asian_p, native_p, black_p, latine_p, mena_p, 0, 0)
RACE <- c("white", "asian", "native", "black", "latine", "mena", "pacific", "other")

race_summary <- tibble(RACE, percentages)

# percentages for later
race_summary









### Age boxplot ~ provider type ###

# first let's create a new var cause these values are too big
df$ProviderType <- df$Occupation

levels(df$ProviderType)

df$ProviderType <- fct_recode(df$ProviderType, 
                              "Behavioral Health" = "I am a behavioral health provider (e.g., a social worker, therapist, counselor, etc.)",
                              "Medical" = "I am a medical provider (e.g., MD, DO, NP, PA, etc.)",
                              "Both" = "I am both a medical and behavioral health provider")




### Age boxplot and facet by provider type
age_type <- df  %>% ggplot(aes(x=Age, fill=ProviderType)) + 
  geom_boxplot(color = "#54235c") + coord_flip() + 
  labs(title = "Sample Age by Provider Type", x = "Age") +     
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Provider Type", 
                      labels = c("Client", "Personal", "Work")) 

# BASE PLOT
age_type



# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
age_final <- age_type + theme(axis.text.x = element_blank(),axis.ticks = element_blank()) +
  theme_classic() +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.x = element_text(color = "#54235c"), axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.x = element_text(color = "#54235c"), axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank()) + theme(axis.text.y = element_text(face = "bold"))

age_final  






### EXPERIENCES ###
# will just copy in a excel table to show risk ratios in a matrix view











### ATTITUDES ###
# Doing stacked 100% barcharts for each question
# would like to have one big plot, but that may mean aggregating the answers
# and adding a category for faceting
# look back at burnout trio for that

# let's isolate the attitudes data
attitudes <- tibble(df$ClimateAfraid, df$ClimateMotivated, df$ClimateSafety, 
                    df$ClimateJobWorry,  df$ClimateRemoteSafer, 
                     df$ClimateStopProviding, df$ClimateMoreDetermined)
# checking
attitudes

# rename columns
colnames(attitudes) <- c("Afraid", "Motivated", "Safety", "JobWorry", "RemoteSafer", "StopProviding", "MoreDetermined")
attitudes

# updating factors so the likert is in order
attitudes$Afraid <- factor(attitudes$Afraid, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$Motivated <- factor(attitudes$Motivated, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$Safety <- factor(attitudes$Safety, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$JobWorry <- factor(attitudes$JobWorry, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$RemoteSafer <- factor(attitudes$RemoteSafer, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$StopProviding <- factor(attitudes$StopProviding, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
attitudes$MoreDetermined <- factor(attitudes$MoreDetermined, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))




# do I need to pivot longer? 
# Yes, but there are other packages that can help!


# Manual ggplot method
# note how they pivot longer from attitudes with everything
# results in one row for every single respons with the question as a category

test1 <- attitudes %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")

ggplot(test1, aes(x = Question, fill = Response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  coord_flip() # Often better for long question text


# also found gglikert() whichi is the "modern" approach
first_attempt <- gglikert(attitudes) + scale_fill_manual(values = all_colors)

# wow, what an easy solution! 
# since it's based around ggplot I think you can just add on the themes
# switching to classic theme and seeing if I can add a title

first_attempt + theme_classic() + labs(title = "Attitudes", y = "Questions")
# kinda wack with labels
# also whoa, I understand the percents on the bottom,
# it's a short cut for determining above or below neutral I think!

# last version with bold question labels
first_attempt + theme(axis.text.y = element_text(face = "bold"))




# example code (position fill makes it 100%, black fill= makes it stacked?)
ggplot(attitudes, aes(fill=, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")













### SCALE MEANS AND DISTRO ###

# Setting up 3 x 1 plot
#dev.off()
par(mfrow = c(1,3))

### CBI Scores
# Personal burnout (highest)
boxplot(df$CBI_personal,
        ylim = c(0,100),
        xlab = "Personal", 
        cex.lab = 1.5)

# Work burnout
boxplot(df$CBI_work,
        ylim = c(0,100),
        main = "Burnout by Type",
        xlab = "Work",
        cex.lab = 1.5,
        cex.main = 1.5)

# Client burnout
boxplot(df$CBI_clients,
        ylim = c(0,100),
        xlab = "Clients",
        cex.lab = 1.5)





### LGBTQ+ Resilience & Inequities scale
# Reset plot window with new layout
dev.off()
par(mfrow = c(1,2))


# Resilience
df$LGBTQ_resilience

# Filtering out zeros from non-LGBTQ participants
resilience_nozero <- c()
resilience_nozero <- df$LGBTQ_resilience[df$LGBTQ_resilience != 0]

boxplot(resilience_nozero,
        ylim = c(0,7),
        main = "LGBTQ+ Resilience")


# Inequities
inequities_nozero <- c()
inequities_nozero <- df$LGBTQ_inequities[df$LGBTQ_inequities != 0]

boxplot(inequities_nozero,
        ylim = c(0,7),
        main = "LGBTQ+ Inequities")




### TO PUT SUBSCALES IN THE SAME PLOT ###


### LGBTQ inequities/resilience duo boxplot 

# to get them in one plot, they need to be one column
# export 2 columns to new_df ->
# pivot_longer and take names from colnames(new_df) ->
# boxplot the new column and group (easier in ggplot)

new_ineq_resil <- tibble(df$LGBTQ_inequities, df$LGBTQ_resilience)

new_ineq_resil <- new_ineq_resil %>% rename("LGBTQ Inequities" = 'df$LGBTQ_inequities',
                            "LGBTQ Resilience" = 'df$LGBTQ_resilience')

long_df2 <- new_ineq_resil %>%  pivot_longer(cols = c("LGBTQ Inequities","LGBTQ Resilience"),
                                    names_to = 'Subscale',
                                    values_to = 'Scale score') 
long_df2
# Great! now long_df contains all the burnout scores and a way to group 

# wait, we have to remove all of the non-queer participations

long_df2 <- long_df2 %>% mutate(selct(!))


# Removes rows containing AT LEAST ONE zero
df_clean <- long_df2[apply(long_df2 != 0, 1, all), ]



### PLOTTING: LGBTQ subscale boxplot duo
LGBTQ_duo <- df_clean  %>% ggplot(aes(x=`Scale score`, fill = Subscale)) + 
  geom_boxplot(color = "#54235c") + coord_flip() + 
  labs(title = "LGBTQ Inequities", x = "Subscale Score") +     
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Subscale:", 
                      labels = c("LGBTQ inequities", "LGBTQ resilience")) 

# BASE PLOT
LGBTQ_duo

LGBTQ_duo

# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
LGBTQ_final <- LGBTQ_duo +
  theme_classic() +  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 

LGBTQ_final + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 





















### LGBTQ inequities/resilience duo boxplot 

# to get them in one plot, they need to be one column
# export 2 columns to new_df ->
# pivot_longer and take names from colnames(new_df) ->
# boxplot the new column and group (easier in ggplot)

new_ineq_resil <- tibble(df$LGBTQ_inequities, df$LGBTQ_resilience)

new_ineq_resil <- new_ineq_resil %>% rename("LGBTQ Inequities" = 'df$LGBTQ_inequities',
                                            "LGBTQ Resilience" = 'df$LGBTQ_resilience')

long_df2 <- new_ineq_resil %>%  pivot_longer(cols = c("LGBTQ Inequities","LGBTQ Resilience"),
                                             names_to = 'Subscale',
                                             values_to = 'Scale score') 
long_df2
# Great! now long_df contains all the burnout scores and a way to group 

# wait, we have to remove all of the non-queer participations
# Removes rows containing AT LEAST ONE zero
df_clean <- long_df2[apply(long_df2 != 0, 1, all), ]



### PLOTTING: LGBTQ subscale boxplot duo
LGBTQ_duo <- df_clean  %>% ggplot(aes(x=`Scale score`, fill = Subscale)) + 
  geom_boxplot(color = "#54235c") + coord_flip() + 
  labs(title = "LGBTQ Community Resilience & Inequities", x = "Subscale Score") +     
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Subscale:", 
                      labels = c("LGBTQ inequities", "LGBTQ resilience")) 

# BASE PLOT
LGBTQ_duo

LGBTQ_duo

# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
LGBTQ_final <- LGBTQ_duo +
  theme_classic() +  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 

LGBTQ_final + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 












### MSPSS Scores
# Reset plot window with new layout
#dev.off()
# trying more complex layout
#layout(matrix(c(1,1,1,2,3,4), nrow = 2, byrow = T))
#layout.show(4)


# NOTE: in cleaning code I averaged the total so it felt comparable to subscales
#   also found this range given to work with the mean score
# Low support = 1 - 2.9
# Med support =  3 - 5
# High support = 5.1 - 7

# But we can also just multiply the column by 12 to get a recognizeable range for presentation
df$MSPSS_real_total <- c()
df$MSPSS_real_total <- df$MSPSS_total*12


 dev.off()
# Full total plot
boxplot(df$MSPSS_real_total,
        ylim = c(0,90),
        xlab = "Overall Total",
        main = "Perceived Social Support")



# now reset plot window to get 2x2 with averaged total
dev.off()
par(mfrow = c(2,2))

# Mean total plot
boxplot(df$MSPSS_total,
        ylim = c(0,8),
        xlab = "Overall",
        cex.lab = 1.5)
df$MSPSS_total


# Friends
boxplot(df$MSPSS_friends,
        ylim = c(0,8),
        xlab = "Friends",
        cex.lab = 1.5,
        cex.main = 1.5)

# SO
boxplot(df$MSPSS_SO,
        ylim = c(0,8),
        xlab = "Significant Other",
        cex.lab = 1.5)

# Family
boxplot(df$MSPSS_family,
        ylim = c(0,8),
        xlab = "Family",
        cex.lab = 1.5)



### MSPSS quad boxplot ###

# to get them in one plot, they need to be one column
# export 4 columns to new_df ->
# pivot_longer and take names from colnames(new_df) ->
# boxplot the new column and group (easier in ggplot)

new_support <- tibble(df$MSPSS_family, df$MSPSS_friends, df$MSPSS_SO, df$MSPSS_total)

new_support <- new_support %>% rename("Family" = 'df$MSPSS_family',
                                            "Friends" = 'df$MSPSS_friends',
                                            "Significant Other" = "df$MSPSS_SO", 
                                            "Overall" = "df$MSPSS_total")

long_support <- new_support %>%  pivot_longer(cols = c("Family","Friends","Significant Other", "Overall"),
                                             names_to = 'Subscale',
                                             values_to = 'Scale score') 
long_support


# Great! now long_support contains all the MSPSS scores and a way to group 


### PLOTTING: LGBTQ subscale boxplot duo
MSPSS_quad <- long_support  %>% ggplot(aes(x=`Scale score`, fill = Subscale)) + 
  geom_boxplot(color = "#54235c") + coord_flip() + 
  labs(title = "Social Support", x = "Subscale Score") +     
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Subscale:", 
                      labels = c("Family", "Friends", "Significant Other", "Overall")) 

# BASE PLOT
MSPSS_quad



# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
MSPSS_final <- MSPSS_quad +
  theme_classic() +  scale_fill_manual(values = alt_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 

MSPSS_final + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 











### PLOTTING: BRS boxplot solo


BRS_plot <- df %>% ggplot(aes(x=BRS_total, fill ='#ac00e5')) + 
  geom_boxplot(color = "#54235c") + coord_flip() +
  labs(title = "Resilience", x = "Score") 

# BASE PLOT
BRS_plot


# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
BRS_final <- BRS_plot +
  theme_classic()  + scale_fill_manual(values = "#ac00e5") +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.y = element_text(color = "#54235c")) + theme(legend.position = "none") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

BRS_final 





























### CORRELATIONS ###

# Establishing the data for the corrplot
final_scores <- df[,65:74]                    
cor_matrix <- cor(final_scores)

# view(cor_matrix) will show you the matrix of values


# Look up color pallete and use 
# col = brewer.pal(n=, name = "")
RColorBrewer::display.brewer.all()


### Correlation heat map of scale scores ###
corrplot(cor_matrix,
         diag = FALSE,
         type = "lower",
         order = "hclust",
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         col = brewer.pal(n = 8, name = "Spectral"))

# SAVE! #
##########




### Correlation heat map of experiences by burnout ###

# Setting up new mini df
# First need to make sure all the experience columns are numeric
typeof(df$WorkplacePushback)

# Now extracting those variables and 3 burnouts
burnout_exps <- df[,c(18:42, 66:68)]                    

miniref <- colnames(burnout_exps)
view(miniref)

burnout_exps_2 <- burnout_exps %>% 
  select(-c("ThreatsHome", "LicensingImpact", "ChildAbuse", "Criminalization", 
            "Jail", "FinedYN", "FinedCount", "FinedHighest", "Moved", "DelayLength"))

# confirm it worked
colnames(burnout_exps_2)


new_cor_matrix <- cor(burnout_exps_2)

corrplot(new_cor_matrix,
         diag = FALSE,
         type = "lower",
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         number.cex = .6,
         addCoef.col = "black",
         col = brewer.pal(n = 8, name = "Spectral"))

# SAVE! #





# Plotting survey date and client burnout 
p6 <- df %>% ggplot(mapping = aes(Date, CBI_personal)) +
  geom_point()
p6













##########










#### MODEL PLOTS ####

# For now I'm running models in analysis RMD and just plotting here



# Model 1 - Personal Burnout 
ggplot(tapedata, aes(x = total_LO, y = CBI_personal)) +
  geom_point(col = "#54235c") +                          # Add data points
  geom_smooth(method = "lm", se = TRUE, col = "#FFa488") +# Add linear regression line
  xlab("Lowest GIPT score") + ylab("Personal Burnout") +
  theme_minimal()  


# Model 2 - Personal Burnout 
ggplot(tapedata, aes(x = total_HI, y = CBI_personal)) +
  geom_point(col = "#54235c") +                          # Add data points
  geom_smooth(method = "lm", se = TRUE, col = "#FFa488") +# Add linear regression line
  xlab("Highest GIPT score") + ylab("Personal Burnout") +
  theme_minimal()  


# Model 3 - Personal Burnout 
ggplot(tapedata, aes(x = c(total_LO + total_DIFF), y = CBI_personal, factor = multiL)) +
  geom_point(col = "#54235c") +                          # Add data points
  geom_smooth(method = "lm", se = TRUE, col = "#FFa488") +# Add linear regression line
  xlab("Lowest GIPT + GIPT Diff + Multiple Licenses") + ylab("Personal Burnout") +
  theme_minimal()  


plot(m3_personal)



















##### POST HOC PLOTS #####


### Correlation table to justify the harassment index! ###


# also adding back in other vars that aren't explicitly harassment?

harass_vars <- tapedata[, c(
  "CBI_personal",
  "ThreatsWork",
  "ThreatsEmail",
  "ThreatsPhone",
  "HarassmentWorkplace",
  "HarassmentPublic",
  "Violence"
)]

full_vars <- tapedata[, c(
  "CBI_personal",
  "ThreatsWork",
  "ThreatsEmail",
  "ThreatsPhone",
  "HarassmentWorkplace",
  "HarassmentPublic",
  "Violence",
  "WorkplacePushback", 
  "InsuranceImpact",
  "Sued",
  "MeasuresInternet",
  "MeasuresLegal",
  "MeasuresAffirming",
  "DelayCareYN",
  "ReferDeclineYN",
  "ConsideredMoving"
)]


harass_vars <- data.frame(lapply(harass_vars, as.numeric))

full_vars <- data.frame(lapply(full_vars, as.numeric))


harass_burnout <- cor(vars_to_check, use = "pairwise.complete.obs")

# using details from ALL PLOTS to make consistent
corrplot(harass_burnout,
         diag = FALSE,
         type = "lower",
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 1,
         addCoef.col = "black",
         col = brewer.pal(n = 8, name = "Spectral"))


# now checking correlations with all experiences that aren't 0s

allexp_burnout <- cor(full_vars, use = "pairwise.complete.obs")

# using details from ALL PLOTS to make consistent
corrplot(allexp_burnout,
         diag = FALSE,
         type = "lower",
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         number.cex = .8,
         addCoef.col = "black",
         col = brewer.pal(n = 8, name = "Spectral"))








### Histogram of Harassment Index ###
# NOTE: just calling plot here for export, vars live in analysis RMD


#   quick histogram of index

hist(tapedata$harassment_index, breaks = 4, 
     ylim = c(0,120),
     col = my_colors,
     xaxt = "n",
     main = "Histogram of Harassment Index")
axis(side = 1, at = seq(min(tapedata$harassment_index), max(tapedata$harassment_index), length.out = 5))

# LOL I need a barplot
# need table or it plots them as unique values (must still be chars and not ints)
ggplot(
  
  (table(tapedata$harassment_index),
     ylim = c(0,80),
     col = my_colors,
     main = "Barplot of Harassment Index") + theme_classic()

table(tapedata$harassment_index)

### and now to do it in ggplot so it's consistent -_-

# Barplot using base R
harass_bar <- barplot(table(tapedata$harassment_index), main = "Harassment Index", col = my_colors)

# FINAL VERSION
#
harass_bar2 <- tapedata  %>% ggplot(aes(x=harassment_index, y = , fill=harassment_index)) + geom_bar(stat = "identity") +    # makes barplot
  scale_fill_manual(values = alt_colors)                        # makes y labels bold

harass_bar2

## UGH not working, letting it go and useing the base R plot instead

























#############
### BNGAP ###
#############

# Main plots will be TF-IDF and word cloud
# Would like to include heat map of burnout (with number of licenses?)
# Also doing 4x4 sample (burnout trio boxplot, and barcharts for
          # TransYN, Queer/Hetero, Med/Behav)


### HEAT MAP of BURNOUT ###

# STEPS #
# Use revised map guide
# Load expanded version of the data (regression ready v2?)
# Create a total burnout score
# Create a color palette for scaling or pick two colors
# Update title and subtitle
# Create and edit legend
# Anything else?
# export



### Plot a basic US map with territories
plot_usmap() +
  labs(title = "Burnout by State",
       subtitle = "Mean Burnout Score based on Provider State of Licensure")

### Loading study data
mapdf <- read.csv('/Users/natajahroberts/Desktop/TAPE_GIPT_regression_ready_v2.csv')
mapref <- colnames(mapdf)
view(mapref)


# Confirming data format
str(mapdf)
# 159 obs of 200 vars, with GIPT scores included



# Creating overall burnout score
mapdf$burnout_total <- c()

mapdf$burnout_total <- (mapdf$CBI_personal + mapdf$CBI_clients + mapdf$CBI_work)

# checking
test <- mapdf %>% select(CBI_personal, CBI_work, CBI_clients, burnout_total)
view(test)

# Plotting map with burnout_total
new2 <- data.frame(mapdf$STATE, mapdf$burnout_total)

plot_usmap() +
  labs(title = "Burnout by State",
       subtitle = "Mean Burnout Score based on Provider State of Licensure",
       data = new, 
       values = "burnout_total", 
       labels = TRUE,
       color="red", 
       fill="blue", 
       linewidth=0.4)


new2 <- new2 %>% rename(burnout = mapdf.burnout_total, state =  mapdf.STATE)


# Plot with custom fills (e.g., using built-in state population data)
basic <- plot_usmap(data = new2, values = "burnout") +
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  labs(title = "Burnout by State",
       subtitle = "Mean Burnout Score based on Provider State of Licensure") +
  theme_void()

# Need to add title and fix legend in illustrator
basic




#############################
##### 4 x 4 sample plot #####
#############################
#dev.off 
# set up box


# establishing color palette
my_colors <- c("#ac00e5", "#FFa488", "#cdfcfc")
alt_colors <- c("#FFa488", "#cdfcfc", "#ac00e5", "#dee23b")



### 1. Burnout trio boxplot 

# to get them in one plot, they need to be one column
# export 3 columns to new_df ->
# pivot_longer and take names from colnames(new_df) ->
# boxplot the new column and group (easier in ggplot)

new_df <- tibble(df$CBI_clients, df$CBI_work, df$CBI_personal)

new_df <- new_df %>% rename(Client = 'df$CBI_clients',
                            Work = 'df$CBI_work',
                            Personal = 'df$CBI_personal'
  
)

long_df <- new_df %>%  pivot_longer(cols = c('Client', 'Work', 'Personal'),
                                    names_to = 'Subtype',
                                    values_to = 'burnout_score') 
long_df
# Great! now long_df contains all the burnout scores and a way to group 

### Another transformation
# to make sure the boxes are organized highest to lowest, 
# we need to reorder this factor we've made in long_df
long_df$Subtype <- factor(long_df$Subtype)

long_df$Subtype <- fct_reorder(long_df$Subtype, long_df$burnout_score, .fun = mean, .desc = TRUE)
str(long_df$Subtype)



### PLOTTING: Burnout boxplot trio
burnout_trio <- long_df  %>% ggplot(aes(x=burnout_score, fill = Subtype)) + 
  geom_boxplot(color = "#54235c") + coord_flip() + 
  labs(title = "Personal, Work, and Client Burnout Scores", x = "Burnout Score") +     
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Subtype:", 
                      labels = c("Client", "Personal", "Work")) 

# BASE PLOT
#burnout_trio

# FINAL PLOT w/ edits
# removing x ticks and labels, but keeping y labels
# classic theme, bold title
# implementing color palette & making all text dark purple
p1 <- burnout_trio + theme(axis.text.x = element_blank(),axis.ticks = element_blank()) +
  theme_classic() +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.x = element_text(color = "#54235c"), axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.x = element_text(color = "#54235c"), axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 

p1  
  






### 2. Occupation barplot

#fixing Occupation levels
df$Occupation <- fct_recode(df$Occupation, "Behavioral" = "I am a behavioral health provider (e.g., a social worker, therapist, counselor, etc.)", 
                            "Medical" = "I am a medical provider (e.g., MD, DO, NP, PA, etc.)" ,
                            "Both" = "I am both a medical and behavioral health provider")

# Plotting
occupation_bar <- df %>% ggplot(aes(x = Occupation, fill = Occupation)) +
  geom_bar(color = "#54235c") +
  labs(title = "Provider Type", x = "Provider Type", y = "Count") +     
  theme(legend.position = "bottom") 

# Base plot
#occupation_bar

### FINAL Plot
# theme classic, no legend, bars with custom colors, all text dark purple
p2 <- occupation_bar + 
  theme_classic() + theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.x = element_text(color = "#54235c"), axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.x = element_text(color = "#54235c"), axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 







### 3. TransYN barplot

#fixing TransYN levels, needs to be reordered
levels(df$TransYN)
df$TransYN <- fct_relevel(df$TransYN, "I don't know", after = Inf)
# nope! just need a clear x axis label

# Plotting
transYN_bar <- df %>% ggplot(aes(x = TransYN, fill = TransYN)) +
  geom_bar(color = "#54235c") +
  labs(title = "Trans Providers", x = "Do you identify as trans or as having trans lived experience?", y = "Count") +     
  theme(legend.position = "bottom") 

# Base plot
#occupation_bar

### FINAL Plot
# theme classic, no legend, bars with custom colors, all text dark purple
p3 <- transYN_bar + 
  theme_classic() + theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.x = element_text(color = "#54235c"), axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.x = element_text(color = "#54235c"), axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 





### 4. sexuality_collapse barplot

# fixing sexuality collapse levels
df$sexuality_collapse <- fct_recode(df$sexuality_collapse, "Queer" = "not_straight", "Straight" = "straight")

# Plotting
sexuality_bar <- df %>% ggplot(aes(x = sexuality_collapse, fill = sexuality_collapse)) +
  geom_bar(color = "#54235c") +
  labs(title = "Provider Sexuality", x = "Sexuality", y = "Count") +     
  theme(legend.position = "bottom") 
sexuality_bar


### FINAL Plot
# theme classic, no legend,  bars with custom colors, all text dark purple
p4 <- sexuality_bar + 
  theme_classic() + theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(color = "#54235c", face = "bold")) +
  theme(axis.title.x = element_text(color = "#54235c"), axis.title.y = element_text(color = "#54235c")) +
  theme(axis.text.x = element_text(color = "#54235c"), axis.text.y = element_text(color = "#54235c")) +
  theme(legend.text = element_text(color = "#54235c")) 



### MAKING 4x4

# can't use par() with ggplot

grid.arrange(p1,p2,p3,p4, nrow = 2)
