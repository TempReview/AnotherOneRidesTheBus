###############################################
### Step 0: Setup
###############################################
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")
library(stringr)

library(haven)
library(readr)
library(dplyr)
library(sdcR)
library(data.table)



###############################################
## Step 1: Load all datasets from Stata and csv files
###############################################

plz_georef<- read_dta("C:/Users/ra000016/ownCloud/Input/GESIS/ZA6294_plz_georef_v1-0-0.dta")
full_data <- read_dta("C:/Users/ra000016/ownCloud/Input/GESIS/ZA6294_v1-0-0.dta")


import_gesis <- read_csv("C:/Users/ra000016/ownCloud/Home/Input_User/import_gesis.csv", 
                         col_types = cols(plz = col_character())) 
                           col_types = cols(plz = col_character()))
                           col_types = cols(plz = col_character()))


###### IMPORTANT: Change 'plz' in zip_statistics to fix an enconding error
condition <- str_length(zip_statistics$plz)==4
zip_statistics$plz[condition] <- paste0("0", zip_statistics$plz[condition])


###############################################
## Step 2: Calculate different reductions of Mesaras Dataset
###############################################

#Only students that are freshman.
fresh <- full_data[ (full_data$d301_302_303__I_begstud< 5),]

# And are either at their prefered uni OR preferred subject
fresh_pref <- fresh[ fresh$q30800__econpref_lb==2 | fresh$q30900__unipref==2, ]

# ... and are at their favourite Uni
fresh_pref_uni  <- fresh[fresh$q30900__unipref==2,]

# ... Only the economic students as robustness for later
fresh_pref_econ <- fresh[fresh$q30800__econpref_lb==2, ]

# ... At prefered university and prefereed subject
fresh_pref_both <- fresh[ fresh$q30800__econpref_lb==2 & fresh$q30900__unipref==2, ]

# ... At prefered university XOR prefereed subject
fresh_pref_xor <- fresh[ xor(fresh$q30800__econpref_lb==2, fresh$q30900__unipref==2), ]


###############################################
## Step 3: Create Dependent variable: moves
###############################################


data <- fresh

data$pref_uni <- FALSE
data$pref_uni[data$q30900__unipref==2] <- TRUE

data_check <- data[,c("s003__unizip","q31300__resloc","q10400__bplace",
                      "q20101__elschoolloc", "q20201__res1school", 
                      "q20301__res1postschool", "q20302__res2postschool")]

data$uni_place <- data$s003__unizip
data$current_place <- data$q31300__resloc
data <- data[!is.na(data$current_place),]

#Remove Students that do not have a current place 
condition <- (data$current_place=="")
data <- data[!condition,] 
count(data)

#Remove students that did not find their final accomodation yet (Robustness Check for later)
#data <- data[data$q31200__accomfound_lb==2 & !is.na(data$q31200__accomfound_lb),]
#count(data)

#We have to reconstruct the last residency value depending on the first known residency
data$last_place <- data$q10400__bplace 

# Replace birthplace with newer information if available
condition <- is.na(data$last_place) |  data$q20101__elschoolloc !="" | str_detect(data$last_place,"[a-z]")
data$last_place[condition] <- data$q20101__elschoolloc[condition]  
count(data)

# Replace faulty, empty data with moves at school-time for missing values (if)
condition <- is.na(data$last_place) | data$q20201__res1school!="" | str_detect(data$last_place,"[a-z]") 
data$last_place[condition] <- data$q20201__res1school[condition]
count(data)


# If the last residential change is to or from foreign country (for example for working abroad), we cannot be sure where the people moved back to and we have to drop the data.
data<-data[!str_detect(data$q20301__res1postschool, "[a-z]") | is.na(data$q20301__res1postschool) ,] 
count(data)

# If there are residential changes after school, but no address is reported.
data<-data[!(data$q20301__res1postschool == "" & data$q20300__respostschool_lb>0), ] 
count(data)

#If the last residential change after school EXISTS and DOES NOT match the current place of living -> Drop (strict)
condition <- (data$q20301__res1postschool!=data$current_place & str_detect(data$q20301__res1postschool, "[0-9]")) 
data <- data[!condition,]
count(data)

#If the last residential change after school DOES NOT EXISTS and the CURRENT Place of living does not match the last one-> Drop (strict)
condition <- (data$q20301__res1postschool=="" & data$current_place != data$last_place)
data <- data[!condition,]
count(data)

#If the person never moved and the current place is not really close to his birth place or the place of elementary <- Drop
condition1 <- ((data$q20200__resschool + data$q20300__respostschool_lb)==0 | is.na(data$q20200__resschool) | is.na( data$q20300__respostschool_lb)) 
condition2 <- ((substr(data$current_place, 1,5) != substr(data$q10400__bplace, 1,5)) & (substr(data$current_place,1,5) != substr(data$q20101__elschoolloc,1,5)))
data_test <- data[(condition1 & condition2),c("s003__unizip","current_place","last_place", "q10400__bplace","q20100__elschool", "q20101__elschoolloc", "q20200__resschool", "q20201__res1school",  "q20300__respostschool_lb", "q20301__res1postschool", "q20302__res2postschool")] 
data <- data[!(condition1&condition2),]
count(data)

# Remove unkown last places
data <- data[str_detect(data$last_place, "[0-9]") & !is.na(data$last_place), ]
table(data$s003__unizip)
count(data)


###############################################
# Step 4: Merge with commuting distances
###############################################
# Generate Keys for Merging
data$key_uni_last <- paste0(data$uni_place,data$last_place)
data$key_uni_current <- paste0((data$uni_place),(data$current_place))
import_gesis$key <- paste0((import_gesis$uni_plz),(import_gesis$plz))


#data$key_uni_last[!data$key_uni_last %in% import_gesis$key] # For Diagnostics

# Merge on basis of current place und former place 
dist_data <-merge(data,import_gesis, by.x = "key_uni_last", by.y = "key")

# now plz.x means the zip of the former place and plz.y means the zip of the current place
dist_data <-merge(dist_data,import_gesis, by.x = "key_uni_current", by.y = "key")
dist_data <- dist_data[!is.infinite(dist_data$travel_time_pop_centroid.x),]
dist_data$move <- TRUE
dist_data$move[dist_data$last_place == dist_data$current_place] <- FALSE
count(dist_data)

#Drop students that did not decrease the commuting time while moving 
#dist_data <- dist_data[dist_data$travel_time_pop_centroid.y - (dist_data$travel_time_pop_centroid.x)<= 0, ] 
#count(dist_data)


###############################################
# Step 5: Merge with regional Indicators
###############################################
plz_encoding <- merge( plz_georef[,c(4,8)], krs_statistics, by.x="district_code_force", by.y="Kennziffer")
zip_statistics <- merge(zip_statistics,plz_encoding,by="plz")                  


# Merge with zip_statistics
dist_data <- merge(dist_data,zip_statistics[,c(1,6,8,9,11,15,16,17)], by.x = "last_place", by.y = "plz")
dist_data <- merge(dist_data,zip_statistics[,c(1,6,8,9,11,15,16,17)], by.x = "uni_place", by.y = "plz")

# Create Binary Variables for indicating better performing Regions
dist_data$better_migration <- dist_data$Gesamtwanderungssaldo.x < dist_data$Gesamtwanderungssaldo.y
dist_data$better_income <- dist_data$`Haushalte mit hohem Einkommen.x` < dist_data$`Haushalte mit hohem Einkommen.y`
dist_data$better_pop <- dist_data$`Bev<U+663C><U+3E36>lkerung gesamt.x` < dist_data$`Bev<U+663C><U+3E36>lkerung gesamt.y`
dist_data$better_tourism <- dist_data$`G<U+653C><U+3E34>ste<U+663C><U+3E63>bernachtungen in Beherbergungsbetrieben 2013.x` < dist_data$`G<U+653C><U+3E34>ste<U+663C><U+3E63>bernachtungen in Beherbergungsbetrieben 2013.y`
dist_data$better_cinemas <- dist_data$`Kinos 2017.x` < dist_data$`Kinos 2017.y`
dist_data$better_swimming <- dist_data$`<U+643C><U+3E36>ffentliche B<U+653C><U+3E34>der 2017.x`< dist_data$`<U+643C><U+3E36>ffentliche B<U+653C><U+3E34>der 2017.y`
dist_data$better_density <- dist_data$`Siedlungsdichte in km<U+623C><U+3E32>.x` < dist_data$`Siedlungsdichte in km<U+623C><U+3E32>.y`
#Create 

# Merge University Names to Zip Codes
name_key <- unique(import_gesis[,c(2:3)])
colnames(name_key) <- c("uni","uni_place")
dist_data <- merge(dist_data,name_key, by="uni_place")


###############################################
# Step 6: Prepare FINAL Data (FIN) for Analysis
###############################################

fin <- dist_data[,c(3:4,8)]
colnames(fin) #Just for overview of the 3:4,8

#Add important Variables and recode them

# Moves/Distances/Locations
fin$uni <- (dist_data$uni)
fin$uni_place <- dist_data$uni_place
fin$current_place <- dist_data$current_place
fin$last_place <- dist_data$last_place
fin$birth_place <- dist_data$q10400__bplace
fin$move <- as.factor(dist_data$move)
fin$commuting_time <- dist_data$travel_time_pop_centroid.x 
fin$commuting_time_after_move  <- dist_data$travel_time_pop_centroid.y 
fin$commuting_distance <- dist_data$distance.x/1000 
fin$commuting_distance[(fin$uni_place == fin$last_place)] <- 0
fin$ticket_available <- as.factor(dist_data$ticket_covered.x)

# Personal 
fin$siblings <- as.factor(dist_data$q10300__siblings_lb >=1)
fin$female   <- as.factor((dist_data$q10100__gender==0))
fin$partner  <- as.factor(as.logical(dist_data$q11100__partner_lb-1))
fin$academic_house <- as.factor(as.logical(dist_data$d109_110__academichouse1*1))
fin$risk <- dist_data$q40102__movestaterisk_lb
fin$pref_uni <- as.factor(dist_data$pref_uni)

#Finances
fin$bafoeg_share <- dist_data$q31804__finbafoeg_lb
fin$accomodation_cost <- dist_data$q31600__accomcost_lb 
fin$single_child <- as.factor(!(fin$siblings==TRUE))
fin$bafoeg_indicator <- as.factor(fin$bafoeg_share>0)
fin$migration_trend_home <- dist_data$Gesamtwanderungssaldo
fin$density_uni <- dist_data$`Siedlungsdichte in km<U+623C><U+3E32>.y`/1000
fin$density_home <- dist_data$`Siedlungsdichte in km<U+623C><U+3E32>.x`/1000
fin$total_budget <- (dist_data$d316_317__tot_budget_lb)/100
fin$bafoeg_amount <- fin$total_budget * fin$bafoeg_share
fin$budget_without_bafoeg <- fin$total_budget - fin$bafoeg_amount

#Regional Controls
fin$better_density <- as.factor(dist_data$better_density)
fin$better_migration  <- as.factor(dist_data$better_migration)
fin$better_income <- as.factor(dist_data$better_income )
fin$better_pop <- as.factor(dist_data$better_pop)
fin$better_tourism  <- as.factor(dist_data$better_tourism)
fin$better_cinemas <- as.factor(dist_data$better_cinemas )
fin$better_swimming <- as.factor(dist_data$better_swimming)

fin$migration <- dist_data$Gesamtwanderungssaldo.x
fin$income <- dist_data$`Haushalte mit hohem Einkommen.x`
fin$pop <- dist_data$`Bev<U+663C><U+3E36>lkerung gesamt.x`
fin$tourism <- dist_data$`G<U+653C><U+3E34>ste<U+663C><U+3E63>bernachtungen in Beherbergungsbetrieben 2013.x`
fin$cinema <- dist_data$`Kinos 2017.x`
fin$swimming <- dist_data$`<U+643C><U+3E36>ffentliche B<U+653C><U+3E34>der 2017.x`

#Fix non-zero commiting times for same ZIP Code
fin$commuting_time[(fin$uni_place == fin$last_place)] <- 0
fin$commuting_time_after_move[(fin$uni_place == fin$current_place)] <- 0

###############################################
# Step 7: Save important Data for later
###############################################
fin <- na.omit(data.table(fin), cols=c("commuting_time","total_budget","ticket_available","risk","pref_uni","single_child","female","partner","academic_house","better_density","better_income","better_cinemas"))
fin_bafoeg <- na.omit(fin, col=c("bafoeg_share"))

#Save important datasets... (remove everything and reload in next script)
save(fresh, fresh_pref, fresh_pref_uni, fresh_pref_econ, fresh_pref_both, fresh_pref_xor, fin, fin_bafoeg, dist_data, file="./DataForAnalysis.Rdata")


#Short test
#produce_log_file("./Data_Prepare.R", log_file = "./Output/Logs/Data_Prepare.log")


