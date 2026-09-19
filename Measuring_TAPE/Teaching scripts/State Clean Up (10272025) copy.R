##################
# State Clean Up!#
##################

library(tidyverse)


# Goal: Create a separate tib with the following columns:
### - participant ID, 
### - submission date, 
### - licensure (state)
### - a column for each state (in our data set), 
### - region


### Step 1 - Create empty tib and make column of all state abbreviations
x <- state.abb

tib <- tibble(x)

tib$example <- numeric(50)



# Add column for DC
r <- c('DC', 0)
tib <- rbind(tib, r)

view(tib)


### Step 2 - Pivot wider so each abbreviation is a column

new <- tib %>%
  pivot_wider(names_from = x, values_from = example)

view(new)

# Created another filler row
z <- numeric(51)



### Step 3 - Create filler rows so we can join provider study data 
# (needs to have the same number of rows to join)

fill_rows <- function(df, vec) {
  count = 0
  while (count < 124) {
    count = count + 1
    df <- rbind(df, vec)
  }
  return(df)
}


# Running the function to create a new data frame with the right number of rows
new2 <- fill_rows(new, z)
view(new2)


# Create ids for the join, name that column X
new2$X <- as.integer(rownames(new2))
view(new2)





### Step 4 - Pull in participant ID, submission date, licensure; join

# load cleaned data
df <- read.csv('/Users/natajahroberts/Desktop/TAPE_CLEANED_3.0.csv')
ref <- colnames(df)
view(df)


                            


# merge provider study data and existing dataframe of survey data

state_df <- full_join(df, new2, by = join_by(X))

view(state_df)




### Step 5 - Make state column lowercase and replace non-abbreviations

# lower case the column 
state_df$States <- tolower(state_df$States)

# Using nested gsub to replace multiple patterns
### Source and more efficient alternative here: https://www.statology.org/r-gsub-multiple-patterns/

# Correct of remove weird abbreviations like cnm and dv


state_df$States <- gsub("virginia", "va", 
                        gsub("mass", "ma",
                             gsub("texas", "tx",
                                  gsub("california", "ca",
                                       gsub("ohio", 'oh',
                                            gsub("washington", "wa",
                                                 gsub("illinois", "il",
                                                      gsub("indiana", "in",
                                                           gsub("idaho", "id",
                                                                gsub("tennessee", "tn",
                                                                     gsub("maine", "me",
                                                                          gsub("and", "",
                                                                               gsub("dv", "dc",
                                                                                    gsub("cnm", "",
                                                                                         gsub("not licensed. in ", "", 
                                                                                    
                                    state_df$States)))))))))))))))

view(state_df)






# Step 6 - Create a function with case logic for each state in licensure column


# Detecting states for hot encoding
state_chart <- state_df %>% mutate(AL=paste(case_when(str_detect(States, regex("al", ignore_case=TRUE)) ~ "1", .default = "0")),
                            AK=paste(case_when(str_detect(States, regex("ak", ignore_case=TRUE)) ~ "1", .default = "0")),
                            AZ=paste(case_when(str_detect(States, regex("az", ignore_case=TRUE)) ~ "1", .default = "0")),
                            AR=paste(case_when(str_detect(States, regex("ar", ignore_case=TRUE)) ~ "1", .default = "0")),
                            AS=paste(case_when(str_detect(States, regex("as", ignore_case=TRUE)) ~ "1", .default = "0")),
                            CA=paste(case_when(str_detect(States, regex("ca", ignore_case=TRUE)) ~ "1", .default = "0")),
                            CO=paste(case_when(str_detect(States, regex("co", ignore_case=TRUE)) ~ "1", .default = "0")),
                            CT=paste(case_when(str_detect(States, regex("ct", ignore_case=TRUE)) ~ "1", .default = "0")),
                            DE=paste(case_when(str_detect(States, regex("de", ignore_case=TRUE)) ~ "1", .default = "0")),
                            DC=paste(case_when(str_detect(States, regex("dc", ignore_case=TRUE)) ~ "1", .default = "0")),
                            FL=paste(case_when(str_detect(States, regex("fl", ignore_case=TRUE)) ~ "1", .default = "0")),
                            GA=paste(case_when(str_detect(States, regex("ga", ignore_case=TRUE)) ~ "1", .default = "0")),
                            GU=paste(case_when(str_detect(States, regex("gu", ignore_case=TRUE)) ~ "1", .default = "0")),
                            HI=paste(case_when(str_detect(States, regex("hi", ignore_case=TRUE)) ~ "1", .default = "0")),
                            ID=paste(case_when(str_detect(States, regex("id", ignore_case=TRUE)) ~ "1", .default = "0")),
                            IL=paste(case_when(str_detect(States, regex("il", ignore_case=TRUE)) ~ "1", .default = "0")),
                            IN=paste(case_when(str_detect(States, regex("in", ignore_case=TRUE)) ~ "1", .default = "0")),
                            IA=paste(case_when(str_detect(States, regex("ia", ignore_case=TRUE)) ~ "1", .default = "0")),
                            KS=paste(case_when(str_detect(States, regex("ks", ignore_case=TRUE)) ~ "1", .default = "0")),
                            KY=paste(case_when(str_detect(States, regex("ky", ignore_case=TRUE)) ~ "1", .default = "0")),
                            LA=paste(case_when(str_detect(States, regex("la", ignore_case=TRUE)) ~ "1", .default = "0")),
                            ME=paste(case_when(str_detect(States, regex("me", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MD=paste(case_when(str_detect(States, regex("md", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MA=paste(case_when(str_detect(States, regex("ma", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MI=paste(case_when(str_detect(States, regex("mi", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MN=paste(case_when(str_detect(States, regex("mn", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MS=paste(case_when(str_detect(States, regex("ms", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MO=paste(case_when(str_detect(States, regex("mo", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MT=paste(case_when(str_detect(States, regex("mt", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NE=paste(case_when(str_detect(States, regex("ne", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NV=paste(case_when(str_detect(States, regex("nv", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NH=paste(case_when(str_detect(States, regex("nh", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NJ=paste(case_when(str_detect(States, regex("nj", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NM=paste(case_when(str_detect(States, regex("nm", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NY=paste(case_when(str_detect(States, regex("ny", ignore_case=TRUE)) ~ "1", .default = "0")),
                            NC=paste(case_when(str_detect(States, regex("nc", ignore_case=TRUE)) ~ "1", .default = "0")),
                            ND=paste(case_when(str_detect(States, regex("nd", ignore_case=TRUE)) ~ "1", .default = "0")),
                            MP=paste(case_when(str_detect(States, regex("mp", ignore_case=TRUE)) ~ "1", .default = "0")),
                            OH=paste(case_when(str_detect(States, regex("oh", ignore_case=TRUE)) ~ "1", .default = "0")),
                            OK=paste(case_when(str_detect(States, regex("ok", ignore_case=TRUE)) ~ "1", .default = "0")),
                            OR=paste(case_when(str_detect(States, regex("or", ignore_case=TRUE)) ~ "1", .default = "0")),
                            PA=paste(case_when(str_detect(States, regex("pa", ignore_case=TRUE)) ~ "1", .default = "0")),
                            PR=paste(case_when(str_detect(States, regex("pr", ignore_case=TRUE)) ~ "1", .default = "0")),
                            RI=paste(case_when(str_detect(States, regex("ri", ignore_case=TRUE)) ~ "1", .default = "0")),
                            SC=paste(case_when(str_detect(States, regex("sc", ignore_case=TRUE)) ~ "1", .default = "0")),
                            SD=paste(case_when(str_detect(States, regex("sd", ignore_case=TRUE)) ~ "1", .default = "0")),
                            TN=paste(case_when(str_detect(States, regex("tn", ignore_case=TRUE)) ~ "1", .default = "0")),
                            TX=paste(case_when(str_detect(States, regex("tx", ignore_case=TRUE)) ~ "1", .default = "0")),
                            TT=paste(case_when(str_detect(States, regex("tt", ignore_case=TRUE)) ~ "1", .default = "0")),
                            UT=paste(case_when(str_detect(States, regex("ut", ignore_case=TRUE)) ~ "1", .default = "0")),
                            VT=paste(case_when(str_detect(States, regex("vt", ignore_case=TRUE)) ~ "1", .default = "0")),
                            VA=paste(case_when(str_detect(States, regex("va", ignore_case=TRUE)) ~ "1", .default = "0")),
                            VI=paste(case_when(str_detect(States, regex("vi", ignore_case=TRUE)) ~ "1", .default = "0")),
                            WA=paste(case_when(str_detect(States, regex("wa", ignore_case=TRUE)) ~ "1", .default = "0")),
                            WV=paste(case_when(str_detect(States, regex("wv", ignore_case=TRUE)) ~ "1", .default = "0")),
                            WI=paste(case_when(str_detect(States, regex("wi", ignore_case=TRUE)) ~ "1", .default = "0")),
                            WY=paste(case_when(str_detect(States, regex("wy", ignore_case=TRUE)) ~ "1", .default = "0"))
)
view(state_chart)





# Step 7 -- Decide our region list and create region columns

# Using 4 regions - Northeast, midwest, south, and west

# List of states and regions as reference for code below
NE <- c("PA", "NY", "NJ", "CT", "MA", "VT", "NH", "ME", "RI")
S <- c('TX', 'OK', 'AR', 'LA', 'MS', 'AL', 'TN', 'KY', 'WV', 'MD', 'DE', 'DC',' VA', 'NC', 'SC', 'GA', 'FL')
M <- c('ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO', 'WI',' IL', 'IN', 'MI', 'OH')
W <- c("WA", "OR", "CA", "NV", "AZ", "AK", "HI", "NM", "CO", "UT", "WY", "MT", "ID")



state_chart <- state_chart %>% mutate(Northeast=paste(case_when(str_detect(States, regex("PA|NY|NJ|CT|MA|VT|NH|ME|RI", 
                                                                              ignore_case=TRUE)) ~ "1", .default = "0")),
                                South=paste(case_when(str_detect(States, regex("TX|OK|AR|LA|MS|AL|TN|KY|WV|MD|DE|DC|VA|NC|SC|GA|FL", 
                                                                              ignore_case=TRUE)) ~ "1", .default = "0")),
                                West=paste(case_when(str_detect(States, regex("WA|OR|CA|NV|AZ|AK|HI|NM|CO|UT|WY|MT|ID", 
                                                                              ignore_case=TRUE)) ~ "1", .default = "0")),
                                Midwest=paste(case_when(str_detect(States, regex("ND|SD|NE|KS|MN|IA|MO|WI|IL|IN|MI|OH", 
                                                                                 ignore_case=TRUE)) ~ "1", .default = "0"))
                                )
                                         

view(state_chart)



### EXPORT ###

# Export this version of the data before doing Erin Reed charts
write.csv(state_chart, "/Users/natajahroberts/Desktop/TAPE_CLEAN_with_STATES.csv")

########














# Step 8 -- Determine our legislation list and create new columns 


### Erin version
# Adult - Do Not Travel, Worst Laws Passed, High Risk in 2yr, Mod Risk in 2yr, Low Risk in 2yr, Safest State w Strong Protections

# Sept 2024 state catagories via Erin
DoNotTravel_Sept <- ('FL')
WorstStates_Sept <- ("AL, ID, KS, LA, MS, MT, OK, ND, TN, TX, UT")
ModerateRisk_Sept <- ("AK, GA, KY, NC, SD, WY")
HighRiskStates_Sept <- ('AR, IA, IN, MO, NE, NH, OH, SC, WV')
LowRiskStates_Sept <- ('AZ, DE, MI, NV, PA, VA, WI')
MostProtective_Sept <- ("CA, CO, CT, DC, HI, IL, MA, MD, ME, MN, NJ, NM, NY, OR, RI, VT, WA")

#creating Regex ready patterns
DNT_Sept <- DoNotTravel_Sept
WS_Sept <- gsub(", ", "|", WorstStates_Sept)
MR_Sept <- gsub(", ", "|", ModerateRisk_Sept)
HRS_Sept <- gsub(", ", "|", HighRiskStates_Sept)
LRS_Sept <- gsub(", ", "|", LowRiskStates_Sept)
MP_Sept <- gsub(", ", "|", MostProtective_Sept)


# using case logic to OHE
Erin_Sept <- state_chart %>% mutate(DoNotTravel_Sept=paste(case_when(str_detect(States, regex(DNT_Sept,
                                                                                              ignore_case=TRUE)) ~ "1", .default = "0")),
                                    HighRiskStates_Sept=paste(case_when(str_detect(States, regex(HRS_Sept,
                                                                                                 ignore_case=TRUE)) ~ "1", .default = "0")),
                                    WorstStates_Sept=paste(case_when(str_detect(States, regex(WS_Sept,
                                                                                              ignore_case=TRUE)) ~ "1", .default = "0")),
                                    ModerateRisk_Sept=paste(case_when(str_detect(States, regex(MR_Sept,
                                                                                               ignore_case=TRUE)) ~ "1", .default = "0")),
                                    LowRiskStates_Sept=paste(case_when(str_detect(States, regex(LRS_Sept,
                                                                                                ignore_case=TRUE)) ~ "1", .default = "0"))
)

view(Erin_Sept)






# Dec 2024 state categories via Erin

DoNotTravel_Dec <- ('FL, TX')
WorstStates_Dec <- ("AL, ID, KS, LA, MS, MT, OH, OK, ND, TN, UT")
HighRiskStates_Dec <- ('AR, GA, IA, IN, MO, NE, NH, SC, WV, WY')
ModerateRisk_Dec <- ("AK, KY, NC, SD")
LowRiskStates_Dec <- ('AZ, DE, MI, NV, PA, VA, WI, DC')
MostProtective_Dec <- ("CA, CO, CT, HI, IL, MA, MD, ME, MN, NJ, NM, NY, OR, RI, VT, WA")

#creating Regex ready patterns
DNT_Dec <- gsub(", ", "|", DoNotTravel_Dec)
WS_Dec <- gsub(", ", "|", WorstStates_Dec)
MR_Dec <- gsub(", ", "|", ModerateRisk_Dec)
HRS_Dec <- gsub(", ", "|", HighRiskStates_Dec)
LRS_Dec <- gsub(", ", "|", LowRiskStates_Dec)
MP_Dec <- gsub(", ", "|", MostProtective_Dec)


# using case logic to OHE
Erin_Dec <- state_chart %>% mutate(DoNotTravel_Dec=paste(case_when(str_detect(States, regex(DNT_Dec,
                                                                                            ignore_case=TRUE)) ~ "1", .default = "0")),
                                   HighRiskStates_Dec=paste(case_when(str_detect(States, regex(HRS_Dec,
                                                                                               ignore_case=TRUE)) ~ "1", .default = "0")),
                                   WorstStates_Dec=paste(case_when(str_detect(States, regex(WS_Dec,
                                                                                            ignore_case=TRUE)) ~ "1", .default = "0")),
                                   ModerateRisk_Dec=paste(case_when(str_detect(States, regex(MR_Dec,
                                                                                             ignore_case=TRUE)) ~ "1", .default = "0")),
                                   LowRiskStates_Dec=paste(case_when(str_detect(States, regex(LRS_Dec,
                                                                                              ignore_case=TRUE)) ~ "1", .default = "0"))
)        

view(Erin_Dec)









# March 2025 state categories via Erin
DoNotTravel_March <- ("FL, TX")
WorstStates_March <- ("AL, IA, ID, KS, LA, MS, MT, OH, OK, ND, SD, TN, UT, WV, WY")
HighRiskStates_March <- ('AR, GA, IN, MO, NE, NH, SC')
ModerateRisk_March <- ("AK, KY, NC, VA")
LowRiskStates_March <- ('AZ, DE, MI, NV, PA, WI, DC')
MostProtective_March <- ("CA, CO, CT, HI, IL, MA, MD, ME, MN, NJ, NM, NY, OR, RI, VT, WA")

#creating Regex ready patterns
DNT_March <- gsub(", ", "|", DoNotTravel_March)
WS_March <- gsub(", ", "|", WorstStates_March)
HRS_March <- gsub(", ", "|", HighRiskStates_March)
MR_March <- gsub(", ", "|", ModerateRisk_March)
LRS_March <- gsub(", ", "|", LowRiskStates_March)
MP_March <- gsub(", ", "|", MostProtective_March)


# using case logic to OHE
Erin_March <- state_chart %>% mutate(DoNotTravel_March=paste(case_when(str_detect(States, regex(DNT_March,
                                                                                                ignore_case=TRUE)) ~ "1", .default = "0")),
                                     WorstStates_March=paste(case_when(str_detect(States, regex(WS_March,
                                                                                                ignore_case=TRUE)) ~ "1", .default = "0")),
                                     HighRiskStates_March=paste(case_when(str_detect(States, regex(HRS_March,
                                                                                                   ignore_case=TRUE)) ~ "1", .default = "0")),
                                     ModerateRisk_March=paste(case_when(str_detect(States, regex(MR_March,
                                                                                                 ignore_case=TRUE)) ~ "1", .default = "0")),
                                     LowRiskStates_March=paste(case_when(str_detect(States, regex(LRS_March,
                                                                                                  ignore_case=TRUE)) ~ "1", .default = "0"))
)

view(Erin_March)





### Trans leg tracker
# Tracks bills considered and bills passed
# Waiting for email response with dated data




### Step 9 ###
# OUTPUT FILES 

# note: these include all surveys; will need to filter later based on submission date


# Erin coded data
write.csv(Erin_March, "/Users/natajahroberts/Desktop/StatesCleanedErinMarch.csv")

write.csv(Erin_Sept, "/Users/natajahroberts/Desktop/StatesCleanedErinSept.csv")

write.csv(Erin_Dec, "/Users/natajahroberts/Desktop/StatesCleanedErinDec.csv")




