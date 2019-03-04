#THIS SCRIPT PEFORMES AGGREGATION OF KI SELECT ONE ANSWERS 
#THROUGH  CONSENSUS METHODOLOGY
# ARNO 10/09/2018- FOR DETAILS CONTACT ZACKARNO@GMAIL.COM


#CLEAR ANY VARIABLES
rm(list=ls())
#LOAD NECESSARY LIBRARIES
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(rgdal)
library(sp)



########LOAD AND BIND DATA SETS--YOU WILL HAVE TO REDUE THIS PART WITH NEW CLEAN DATA SETS############
#enter folder containing data as working directory
setwd("C:/Users/Zack/Documents/Impact_GIS/02_MSNA_2018/06_H2R/02_H2R_data_raw/H2R_Data_Raw/Week_13August2018")
#There were 2 versions of the kobo tool-- so load the forms submitted on te new tool
df_new<-read.csv( "NGA_H2R_17_August_2018.csv"  , header=TRUE,na.strings=c("","NA"),stringsAsFactors = FALSE)
#load the forms submitted on the old tool.
df_old<-read.csv("NGA_H2R_OldForm_18August2018.csv", header=TRUE,na.strings=c("","NA"),stringsAsFactors = FALSE)
#get an even older file which I like the headers for better
setwd("C:/Users/Zack/Documents/Impact_GIS/02_MSNA_2018/06_H2R/02_H2R_data_raw/H2R_Data_Raw")
getheaders<-read.csv("H2R_Data_New_Form_07Aug2018_xmlheaders.csv", header=TRUE,na.strings=c("","NA"),stringsAsFactors = FALSE)
#switch the headers for both old and new data
colnames(df_new)<-colnames(getheaders)
colnames(df_old)<-colnames(getheaders)
#add a column to identify which form they are coming from
df_new$form<-"new_form"
df_old$form<-"old_form"
#combine them
mydat<-rbind(df_old,df_new)
#IF YOU WANT TO WRITE OUT FINAL DATA SET RUN LINE BELOW
# write.csv(mydat,"C:/Users/Milan/Documents/Impact_GIS/02_MSNA_2018/06_H2R/02_H2R_data_raw/h2r_forms_merged_30aug2018.csv")

#THIS SECTION LOADS THE KOBO DATA AND ALLOWS YOU TO SUBSET JUST SELECT ONE QUESTIONS
library(XLConnect)
#load kobo survey
kobo_workbook<-loadWorkbook ("C:/Users/Zack/Documents/Impact_GIS/02_MSNA_2018/06_H2R/Kobo_H2R/H2R_Kobo_Tool/01_Final_Kobo_H2R_August2018/NGA_Kobo_H2R_03August_2018.xlsx" , create = TRUE )
#get the survey 
survey<-readWorksheet (kobo_workbook , "survey",header = TRUE )
choices<-readWorksheet(kobo_workbook,"choices",header=TRUE)


#ADD TWO NEW COLUMNS
mydat$populated<-ifelse(mydat$living_now=="Yes",1,2)
mydat$sett_id<-paste0(mydat$h2r_state,mydat$h2r_lga,mydat$h2r_ward,mydat$h2r_settlement)
 
#LOOK AT DATA WITHOUT OTHER ANSWERS FOR H2R
#WITHOUT NA VALUES FOR THOSE-- SHOULD ONLY BE FULLY ANSWERED INTEVIEWS
mydat$timely_response<-factor(mydat$h2r_lastinfo)
levels(mydat$timely_response) <-c(3,2,4,4,4,1)
mydat$timely_response<-mydat$timely_response %>% as.numeric()
#GET GROUP BY UNIQUE SETTLEMENT- IF ONE PERSON SAID LIVING THE OTHER DIDNT- USE THE POPULATED KI ANSWER

mydat2<-mydat %>% group_by(h2r_state, h2r_lga, h2r_ward, h2r_settlement) %>% 
  filter(populated==min(populated)) %>% #IF CONTRADICTORY ANSWERS ON LIVING REMOVE ABANDONED ANSWER
  filter(living_now=="Yes") %>%#ONLY SELECT LIVING
  filter(!is.na(resp_age)==TRUE) %>% #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(loc_gps)==TRUE) %>%  #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(h2r_settlement)) %>% #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(h2r_settlement != "other")# REMOVE  RECORDS WITH H2R SETTLEMENTS LISTED AS "OTHER"- SHOULD BE LESS AFTER FEMIS CLEANING

#FROM KOBO FORM -FIND ALL QUESTIONS LABELED AS SELECT ONE
s1_index<-which(str_detect(survey$type, "select_one")==TRUE)
#GET COLUMN NAMES FOR ALL SELECT ONE QUESTIONS
sel_one<-survey[s1_index,2]
# GET FULL QUESTIONS FOR ALL SELECT ONE- this will be used later for creating a column to display as title
#in data driven pages tool
full_q<-survey[s1_index,3]

#THIS IS THE 2ND MODE FUNCTION SETH SENT ME- IT RETURNS NC IF THERE IS NO CONSESNUS 
Mode2 <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  if(length(modes) > 1) {
    return("NC")
  }
  
  else {
    return(modes)
  }
}



###MAKE UNIQUE SETTLEMENT ID SO THAT WE CAN SPLIT THE DATAFRAME BY IT
mydat2$unique_set<-paste0(mydat2$h2r_lga,mydat2$h2r_ward,mydat2$h2r_settlement)
mydat_ones<-mydat2[,sel_one]
#SPLIT DATA FRAME INTO LIST OF DATA FRAMES
lmdf<-split(mydat_ones %>% data.frame(), mydat2$unique_set)

#FOR LOOP THROUGH EACH DATA FRAME IN LIST AND RUN MODAL FUNCTION ON EACH COLUMN
#WHERE THERE IS NO CONSENSUS, PUT NC
s<-list()
for (i in 1:length(lmdf)){
 a <- sapply(lmdf[[i]],function(x) Mode2(x)) %>% data.frame
 s[[i]]<-t(a)
}

#BIND DATA FRAMES BACK TOGETHER INTO ONE
cen1<-do.call(rbind,s) 
rownames(cen1)<-1:nrow(cen1)
cen1df<-cen1 %>% data.frame()

#CEN1DF HAS ALL SELECT ONE QUESTIONS TOGETHER WITH MODAL ANSWER. WHERE THERE IS NO
# CONSENCUS IT SAYS NC

# IN ORDER TO GET RID OF SOME OF THE NCs-- FIRST RUN SAME OPERATION, BUT
# FILTER GROUPS TO CHOOSE THE KIS THAT HAVE THE MOST RECENT KNOWLEDGE AND TRY TO 
# GET A MODAL CONSENSUS AGAIN

mydat3<-mydat %>% group_by(h2r_lga, h2r_ward, h2r_settlement) %>% 
  filter(populated==min(populated)) %>% #IF CONTRADICTORY ANSWERS ON LIVING REMOVE ABANDONED ANSWER
  filter(living_now=="Yes") %>%#ONLY SELECT LIVING
  filter(!is.na(resp_age)==TRUE) %>% #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(loc_gps)==TRUE) %>%  #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(h2r_settlement)) %>% #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(h2r_settlement != "other") %>% # REMOVE  RECORDS WITH H2R SETTLEMENTS LISTED AS "OTHER"- SHOULD BE LESS AFTER FEMIS CLEANING
  filter(timely_response==min(timely_response)) #NOW FOR DUPLICATE SETTLEMENTS CHOOSE THE KI RESPONSE WITH MOST RECENT INFO

#SUBSET TO JUST SELECT ONE QUESTIONS
mydat_selone<-mydat3[,sel_one]
#AGAIN ADD UNIQUE COLUMN TO SPLIT DATA FRAME INTO LIST OF DATA FRAMES (YES I KNOW THIS IS REPETITIVE)
mydat_selone$unique_set<-paste0(mydat_selone$h2r_lga,mydat_selone$h2r_ward,mydat_selone$h2r_settlement)
lmdf2<-split(mydat_selone %>% data.frame(), mydat_selone$unique_set)

#FOR LOOP THROUGH EACH DATA FRAME IN LIST AND RUN MODAL FUNCTION ON EACH COLUMN
#WHERE THERE IS NO CONSENSUS, PUT NC
s2<-list()
for (i in 1:length(lmdf2)){
  a <- sapply(lmdf2[[i]],function(x) Mode2(x)) %>% data.frame
  s2[[i]]<-t(a)}

cen2<-do.call(rbind,s2) 
rownames(cen2)<-1:nrow(cen2)
cen2df<-cen2 %>% data.frame()
colnames(cen2df)


#GO THROUGH EACH VALUE AND REPLACE NC BY CONSENUS DATA FRAME VALUE
#TO BE SAFE JUST MAKE ALL OF THE SUBSET DF CHARACTER VALUES
cen1dfa<-cen1df;cen2dfa<-cen2df
cen1dfa[] <- lapply(cen1df, as.character)
cen2dfa[]<-lapply(cen2df, as.character) 
#CREATE EMPTY DATA FRAME TO FILL WITH FOR LOOP
round_1_output<-data.frame()

# i WILL LOOP THROUGH COLUMNS WHILE j WILL LOOP THROUGH ROWS IN EACH COLUMN
for (i in 1: ncol(cen1dfa)){
  for (j in 1: nrow(cen1dfa)){
    round_1_output[j,i]<-ifelse(cen1dfa[j,i]=="NC",cen2dfa[j,i], cen1dfa[j,i] )
                        }
}
#COLUMN NAMES WERE REMOVED- PUT THEM BACK IN
colnames(round_1_output)<-colnames(cen1df)

#THIS MIGHT ACTUALLY BE REDUNDANT NOW THAT I AM LOOKING OVER THE SCRIPT
#LAST STEP: IF THERE IS STILL CONTRADICTION GO WITH
#THE RESPONDENT WITH THE MOST RECENT INFORMATION, IF THAT DOESNT WORK- LEAVE "NC"
#SO CREATE A DATA FRAME THAT DOES THIS FIRST
mydat4<-mydat %>% group_by(h2r_lga, h2r_ward, h2r_settlement) %>% 
  filter(populated==min(populated)) %>% #IF CONTRADICTORY ANSWERS ON LIVING REMOVE ABANDONED ANSWER
  filter(living_now=="Yes") %>%#ONLY SELECT LIVING
  filter(!is.na(resp_age)==TRUE) %>% #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(loc_gps)==TRUE) %>%  #REMOVE ALL ROWS WITH NA VAL HERE- INTERVIEW INCOMPLETE
  filter(!is.na(h2r_settlement)) %>% 
  filter(h2r_settlement != "other") %>% 
  filter(timely_response==min(timely_response)) 

#SUBSET ONLY SELECT ONES
dat4_selone<-mydat4[,sel_one]

#GET THE NUMBER OF KIS PER SETTLEMENT IN NEW DATASET
d5<-dat4_selone %>% group_by (h2r_lga,h2r_ward, h2r_settlement) %>% 
  count()
#ONLY LOOK AT RECORDS WHERE GROUPING LEAVES ONLY 1 KI PER SETTLEMENT
d6<-filter(d5, n==1)

#MERGE ANSWERS ON
d7b<-left_join(dat4_selone, d6)
dim(d7b)
d7b$n
#ONLY WANT TO LOOK AT ANSWERS WHERE THERE IS ONE KI PER SETTLEMENT
d8<-filter(d7b, n==1)
#JOIN LAST OUTPUT AND THOSE VALUES
d9<-left_join(round_1_output, d8, by=c("h2r_lga", "h2r_ward", "h2r_settlement"))
#NO REMOVE ALL THE LEFT SIDE OF THE MERGE
d10<-d9 %>% select(-ends_with(".x"))
# JUST REARRANGE COLUMNS FOR FOR LOOP LATER TO BE THE SAME AS
# ROUND 1 OUTPUT
d11<-d10[,c(4:12,1:3,13:ncol(d10))]
#CHECK TO MAKE SURE COLUMN NAMES ARE THE SAME?
cbind(colnames(d11), colnames(round_1_output)) # YES THEY ARE EXCEPT FOR LAST COLUMN N WHIH WAS ADDED
dim(round_1_output)
#FOR LOOP
#MAKE SURE EVERYTHING IS CHARACTER
round_1_output[]<-lapply(round_1_output, as.character)
d11[]<-lapply(d11, as.character)
#MAKE NEW DF TO FILL
round_2_output<-data.frame()
#ONLY WANT TO LOOK AT ROWS PRESENT IN ROUND 1 OUTPUT
# IF ROUND1 IS "NC" AND D11 == 1 REPLACE WITH D11, IF NOT LEAVE "NC"
#LOOKING AT THIS A SECOND TIME- I THINK THIS DOESNT ACTUALLY DO ANYTHING AND IS REPETITVE- BUT HEY
# IT DOESNT CAUSE AN ERROR SO II AM LEAVNIG IT FOR NOW
for (i in 1: ncol(round_1_output)){
  for (j in 1: nrow(round_1_output)){
    round_2_output[j,i]<-ifelse(round_1_output[j,i]=="NC" & d11[j,"n"]=="1", d11[j,i],round_1_output[j,i] )
  }
}
colnames(round_2_output)<-colnames(cen1df)
round_2_output %>% head()

#ROUND 2 OUTPUT IS  NOW THE SELECT ONE DATA AGGREGATED BY CONSENSUS AT THE SETTLEMENT LEVEL PART ONE- DONE

# PART 2- REFORMAT DATA FOR DD PAGES TOOL
settlement_answers<-round_2_output %>% 
  filter(living_now=="Yes") %>% #REPETITIVE
  group_by(h2r_state,h2r_lga,h2r_ward,h2r_settlement)

#REORDER COLUMNS TO MELT DATA 
settlement_answers2<-settlement_answers %>% 
  select(c(h2r_state,h2r_lga,h2r_ward, h2r_settlement),everything())

#MELTH/GATHER DATA TO LONG FORMAT
data_long_settlements<-gather(settlement_answers2, question, answer, kmz_yn:h2r_additional, factor_key = TRUE)

#IN ORDER TO GET THE ACTUAL QUESTION NAMES NEED TO REMELT DATA WITH COLUMNS NAMES REPLACED BY 
#ACTUAKL QUESTIONS
s1_svy_fullq<-round_2_output %>% ungroup() #UNGROUP SO THAT I CAN REGROUP LATER ACCORDING TO QUESTION NAMES
colnames(s1_svy_fullq)<-full_q # RENAME COLUMNS
s1_svy_fullq %>% nrow()#CHECK LENGTH

#REDO WHAT I DID TO GET "SETTLEMENT_ANSWERS" BUT JUST WITH DATA SET WITH QUESTIONS IN COLUMNS
s1_svy_fullq2<-s1_svy_fullq  %>% 
  filter(`Do people currently live in ${h2r_settlement}?`=="Yes") %>% 
  group_by(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`,
           `What is the name of the settlement that you have outside information about?`)

#MOVE THE COLUMNS THAT WONT BE MELTED TO BEGINNING AGAIN
s1_svy_fullq3<-s1_svy_fullq2 %>% 
  select(c(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`,
           `What is the name of the settlement that you have outside information about?`), everything())

#MELTH/GATHER DATA SET AGAIN         
set_fullq_long<-gather(s1_svy_fullq3,question, answer, `did you use the kmz file provided by REACH do find this respondent?`:`Do you have information on any other hard to reach settlements?`,factor_key=TRUE)
#BIND THE TWO DATA SETS (QUESTIONS AND SHORTENED NAMES)
set_long<-data.frame(h2r_state=data_long_settlements$h2r_state,h2r_lga= data_long_settlements$h2r_lga,
                     h2r_ward=data_long_settlements$h2r_ward,h2r_settlements=data_long_settlements$h2r_settlement,
                     question_id=data_long_settlements$question,question=set_fullq_long$question,answer=data_long_settlements$answer)

#THIS IS ANNOYING BECAUSE I HAVE DONE IT SO MANY TIMES IN SO MANY SCRIPTS
#BUT I JUWST NEED TO GET THE POPULATION DATA, CORRECT IT TO SPATIALLY MATCH OCHA ADMIN BOUNDARIES
#CLEAN COLUMN NAMES AND THEN BIND OUR NEW LONG DATA TO THE SETTLEMENT DATA TO GET GPS LOCATIONS
#######UGLY SPATIAL JOINS AND SPATIAL CLEANING===================================
fgdb_h2r<-"C:/Users/Zack/Documents/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/03_GDBs_H2R/H2R_Settlements.gdb"
fgdb_msna<-"C:/Users/Zack/Documents/Impact_GIS/02_MSNA_2018/03_MSNA_GIS/01_MSNA_Sampling/02_GDB_MSNA/01_MSNA_planning.gdb"
fgdb2<-"C:/Users/Zack/Documents/Impact_GIS/15_Administrative_Mapping/13_GDB/REACH_NGA_original.gdb"
fc_list2 <- ogrListLayers(fgdb2)
print(fc_list2)
#load ocha shapefile
ocha3sp<-readOGR(dsn=fgdb2,layer="OCHA_NGA_adm3")
ocha2sp<-readOGR(dsn=fgdb2,layer="OCHA_NGA_adm2")
#READ DATA SETS (YOBE, ADAMWAWA, BORNO)- BIND EM
yobe_all<-readOGR(dsn=fgdb_h2r,layer="polio_yobeo_clipped_yobe")
ya<-data.frame(state=yobe_all$State, lga= yobe_all$LGA,ward=yobe_all$Ward, settlement= yobe_all$Settlement,lat= yobe_all$Latitude,lon=yobe_all$Longitude)
adam_all<-readOGR(dsn=fgdb_h2r,layer="VTS_Adamawa_Clipped_Adamawa")
aa<-data.frame(state=adam_all$State, lga= adam_all$Lga,ward=adam_all$Ward, settlement= adam_all$Settlement_Name,lat= adam_all$Lat_,lon=adam_all$Long_)
borno_all<-readOGR(dsn=fgdb_msna,layer="polio_access_data")
ba<-data.frame(state="Borno", lga= borno_all$LGA_NAME,ward=borno_all$WARD_NAME, settlement= borno_all$SETTLEMENT_NAME,lat= borno_all$LATITUDE,lon=borno_all$LONGITUDE)
# combine the three data sets
state3<-rbind(ya, ba, aa)
allpop<-state3
coordinates(allpop)<-~lon+lat
#make it same CRS as ocha
proj4string(allpop)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#spatial join the too and keep ocah state,lga, ward columns
ps<-over(allpop,ocha3sp, returnList = FALSE)

correct_admin<-data.frame(state=ps$ADM1_EN, lga= ps$ADM2_EN,ward= ps$ADM3_EN, settlement=allpop$settlement,lon=allpop$lon,lat=allpop$lat)
#rename so preexisting code will work
allpop2<-correct_admin
#make cooridnates numeric
allpop2[,5:6]<-sapply(allpop2[,5:6],function(x) as.numeric(x)) %>% data.frame 
#number index may be useful later maybe not
allpop2$number_index<-seq(1:nrow(allpop2))
#clean data set 
allpop3<-data.frame(sapply(allpop2, function(x)trimws(x)))
allpop4<- data.frame(sapply(allpop3, function(x) gsub("[[:punct:]]", "_",x)))
allpop5<-data.frame(sapply(allpop4, function(x) tolower(x)))
allpop5<-data.frame(sapply(allpop5, function(x) gsub(" ","_",x)))
# allpop5<-data.frame(sapply(allpop4, function(x) tolower(x)))
#have to put "." back in since it was juwst replaced by "_"
allpop5[,5:6]<-sapply(allpop5[,5:6],function(x) gsub("_",".",x) %>%  as.numeric(x)) %>% data.frame 
#make exta settlement column because original geets erased in merege?
allpop5$settlement2<-allpop5$settlement
allpop5 %>% head()
#clean the kobo info the same way--- might be unnecessart

class(set_long)
set_long[,1:4]<-data.frame(sapply(set_long[,1:4], function(x) trimws(x)))
set_long[,1:4]<-data.frame(sapply(set_long[,1:4], function(x) gsub("[[:punct:]]", "_",x)))
set_long[,1:4]<-data.frame(sapply(set_long[,1:4], function(x) gsub(" ","_",x))) 
set_long[,1:4]<-data.frame(sapply(set_long[,1:4], function(x) tolower(x))) 
#merge all data by appropriate columns keeping only kobo recoreds
set_long %>% head()

#######MERGE DATA SETS AND WRITE SHAPEFILE========================
dfsp1<-merge(set_long,allpop5, by.x= c("h2r_lga","h2r_ward", "h2r_settlements"), by.y= c("lga","ward", "settlement2"), all.x=TRUE)
dfsp1<-na.omit(data.table(dfsp1), c("lon","lat"))

coordinates(dfsp1)<-~lon+lat
proj4string(dfsp1)<-proj4string(ocha2sp)
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data"
 # writeOGR(dsn=folder,obj= dfsp1,layer="settlement_answ_long_format_04September2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)
### ALL THAT IS LEFT IS TO SANITIZE THE ADMINISTRATIVE BOUDARY NAMES, AND MERGE IT TO THE POLIO/ALL DATA SETS.

#NEXT IT IS NECESSARY TO MAKE SHAPE
victim<-dfsp1@data[,"question"] %>% unique() %>% data.frame()
# victim$fid<-1:nrow(victim)
victim$lat<-rep(0,nrow(victim))
victim$lon<-rep(0,nrow(victim))

coordinates(victim)<-~lon+lat
proj4string(victim)<-proj4string(ocha2sp)
# dir.create("F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/victim_files")
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/victim_files"
# writeOGR(dsn=folder,obj= victim,layer="victim_settlements", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#######NEXT AGGREGATE WARDS======================================================
fgdb2<-"C:/Users/Zack/Documents/Impact_GIS/15_Administrative_Mapping/13_GDB/REACH_NGA_original.gdb"
fc_list2 <- ogrListLayers(fgdb2)
print(fc_list2)
#load ocha shapefile
ocha3sp<-readOGR(dsn=fgdb2,layer="OCHA_NGA_adm3")
ocha2sp<-readOGR(dsn=fgdb2,layer="OCHA_NGA_adm2")


#USE ROUND 2 OUTPUT TO AGGREGATE WARD SELECT ONE ANSWERS 
agg_input<-round_2_output
agg_input$ward_id<-paste0(agg_input$h2r_lga,agg_input$h2r_ward)

#FOR SOME REASON WHEN I US DPLYRS SUMMARIZE FUNCTION IN CONJUNCTION WIH MODE 2 WHEN THERE IS
#NC IT JUST GIVE FIRST ANSWER-- THEREFORE USING THIS METHOD
ward_list<-split.data.frame(agg_input, agg_input$ward_id)
s2<-list()
for (i in 1:length(ward_list)){
  a <- sapply(ward_list[[i]],function(x) Mode2(x)) %>% data.frame
  s2[[i]]<-t(a)}
length(s2)
ward_mc<-do.call(rbind,s2) 
rownames(ward_mc)<-1:nrow(ward_mc)
ward_mc_df<-ward_mc %>% data.frame()

#ADD COLUMN GIVING SETTLEMENTS/WARD
set_per_ward<- round_2_output %>% 
  filter(living_now=="Yes") %>% 
  count(h2r_state,h2r_lga,h2r_ward) 
set_per_ward

#JOIN TWO DATA FRAMES
ward_mc2<-left_join(ward_mc_df,set_per_ward)


#NOW GET DATA IN FORMAT TO MELT FOR SHAPE FILES FOR DD PAGES
number1_choice_ward2<-ward_mc2 %>% 
  select(c(h2r_state,h2r_lga,h2r_ward,n),everything())
colnames(number1_choice_ward2)
#GET RID OF LAST COLUMN
number1_choice_ward2<-number1_choice_ward2[,-c(118)]
#MELT DATA TO LONG FORMAT
data_long_wards <- gather(number1_choice_ward2, question, answer, kmz_yn:h2r_additional, factor_key=TRUE)
data_long_wards %>% head()
s1_svy_fullq<-round_2_output %>% ungroup() #UNGROUP SO THAT I CAN REGROUP LATER ACCORDINT TO QUESTION NAMES
colnames(s1_svy_fullq)<-full_q # RENAME COLUMNS
s1_svy_fullq %>% nrow()#CHECK LENGTH

#NOW REDO GROUPING AND MELTING WITH COLUMN NAMES AS QUESTIONS RATHER THAN COLUMN NAMES
number1_choice_ward_fullq<-s1_svy_fullq  %>% 
  filter(`Do people currently live in ${h2r_settlement}?`=="Yes") %>% 
  group_by(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`) %>% 
  summarize_all(funs(m=Mode2))
number1_choice_ward_fullq$set_p_ward<-set_per_ward$n

#REARRANGE COLUMNS FOR MELT
number1_choice_ward_fullq2<-number1_choice_ward_fullq %>% 
  select(c(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`,`set_p_ward`), everything())

data_long_wards_fullq <- gather(number1_choice_ward_fullq2, question, answer,`did you use the kmz file provided by REACH do find this respondent?_m`:`Do you have information on any other hard to reach settlements?_m`, factor_key=TRUE)

wards_long_full<-data.frame(h2r_state=data_long_wards$h2r_state,h2r_lga= data_long_wards$h2r_lga,
                            h2r_ward=data_long_wards$h2r_ward,set_p_ward=data_long_wards$n ,question_id=data_long_wards$question,
                            question=data_long_wards_fullq$question %>% as.character(),answer=data_long_wards$answer)
wards_long_full %>% head()
#WITH WARDS_LONG_FULL WE NOW HAVE COLUMNS FOR # SET/WARD, QUESTION COLUMN NAMES,  AND FULL QUESTIONS AND ANSWERS

#need to "sanitize ocha3sp ward names so that the merge works correctly
ocha3sp$wardlink<-ocha3sp$ADM3_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha3sp$lgalink<-ocha3sp$ADM2_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha2sp$lgalink<-ocha2sp$admin2Name %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

## MAKE EVERYTHING A CHARACTER TO BE SAFE
number1_choice_ward_perfect<-sapply(wards_long_full, as.character) %>% data.frame
#GET RID OF SPECIAL CHARACTERS
number1_choice_ward_perfect$h2r_lga2<-number1_choice_ward_perfect$h2r_lga %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
number1_choice_ward_perfect$h2r_ward2<-number1_choice_ward_perfect$h2r_ward %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

### MERGE LONG DATA TO OCHA ADMIN SHAPE FILE-- ALLOW DUPLICATE GEOMETRIES
d<-sp::merge(x=ocha3sp,y=number1_choice_ward_perfect,by.x=c("lgalink","wardlink"), by.y=c("h2r_lga2","h2r_ward2"),all.x=FALSE,all.y=TRUE,duplicateGeoms = TRUE)

#WRITE DATA TO SHAPEFILE
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/select_one"
# writeOGR(dsn=folder,obj= d,layer="most_common_populated_ward_level_04Aug2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#NEXT IT IS NECESSARY TO MAKE VICTIM SHAPE
victim<-d@data[,"question"] %>% unique() %>% data.frame()
victim$fid<-1:nrow(victim)
victim$lat<-rep(0,nrow(victim))
victim$lon<-rep(0,nrow(victim))

coordinates(victim)<-~lon+lat
proj4string(victim)<-proj4string(ocha2sp)
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/victim_files"
# writeOGR(dsn=folder,obj= victim,layer="victim_select1_ward", driver = "ESRI Shapefile", overwrite_layer = TRUE)


########NEXT LGA MOST COMMON ANSWER =====================================================
#SAME THING REDONE BUT FOR LGA LEVEL AGGREGATIOND

agg_input<-round_2_output

agg_input$h2r_lga
lga_list<-split.data.frame(agg_input, agg_input$h2r_lga)
s2<-list()
for (i in 1:length(lga_list)){
  a <- sapply(lga_list[[i]],function(x) Mode2(x)) %>% data.frame
  s2[[i]]<-t(a)}
length(s2)
lga_mc<-do.call(rbind,s2) 
rownames(lga_mc)<-1:nrow(lga_mc)
lga_mc_df<-ward_mc %>% data.frame()


set_per_lga<- round_2_output %>% 
  filter(living_now=="Yes") %>% 
  count(h2r_state,h2r_lga) 
set_per_lga

lga_mc2<-left_join(ward_mc_df,set_per_lga)

number1_choice_lga2<-lga_mc2 %>% 
  select(c(h2r_state,h2r_lga,h2r_ward,n),everything())
colnames(number1_choice_lga2)
number1_choice_lga2<-number1_choice_lga2[,-c(118:119)]

data_long_lga <- gather(number1_choice_lga2, question, answer, kmz_yn:h2r_additional, factor_key=TRUE)
data_long_lga %>% head()
s1_svy_fullq<-round_2_output %>% ungroup() #UNGROUP SO THAT I CAN REGROUP LATER ACCORDINT TO QUESTION NAMES
colnames(s1_svy_fullq)<-full_q # RENAME COLUMNS
s1_svy_fullq %>% nrow()#CHECK LENGTH

number1_choice_lga_fullq<-s1_svy_fullq  %>% 
  filter(`Do people currently live in ${h2r_settlement}?`=="Yes") %>% 
  group_by(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`) %>% 
  summarize_all(funs(m=Mode))
number1_choice_lga_fullq$set_p_ward<-set_per_lga$n

number1_choice_lga_fullq2<-number1_choice_lga_fullq %>% 
  select(c(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`,`set_p_lga`), everything())

data_long_lga_fullq <- gather(number1_choice_lga_fullq2, question, answer,`did you use the kmz file provided by REACH do find this respondent?_m`:`Do you have information on any other hard to reach settlements?_m`, factor_key=TRUE)

lga_long_full<-data.frame(h2r_state=data_long_lga$h2r_state,h2r_lga= data_long_lga$h2r_lga,
                            h2r_ward=data_long_lga$h2r_ward,set_p_lga=data_long_lga$n ,question_id=data_long_lga$question,
                            question=data_long_lga_fullq$question %>% as.character(),answer=data_long_lga$answer)
lga_long_full %>% head()
wards_long_full %>% head()
#need to "sanitize ocha3sp ward names so that the merge works correctly
ocha3sp$wardlink<-ocha3sp$ADM3_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha3sp$lgalink<-ocha3sp$ADM2_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha2sp$lgalink<-ocha2sp$admin2Name %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

## MAKE EVERYTHING A CHARACTER TO BE SAFE
number1_choice_lga_perfect<-sapply(lga_long_full, as.character) %>% data.frame

number1_choice_lga_perfect$h2r_lga2<-number1_choice_lga_perfect$h2r_lga %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
# number1_choice_ward_perfect$h2r_ward2<-number1_choice_ward_perfect$h2r_ward %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

### MERGE LONG DATA TO OCHA ADMIN SHAPE FILE-- ALLOW DUPLICATE GEOMETRIES
d<-sp::merge(x=ocha2sp,y=number1_choice_lga_perfect,by.x=c("lgalink"), by.y=c("h2r_lga2"),all.x=FALSE,all.y=TRUE,duplicateGeoms = TRUE)

#WRITE DATA TO SHAPEFILE
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/select_one"
# writeOGR(dsn=folder,obj= d,layer="most_common_populated_lga_level_04sept2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#NEXT IT IS NECESSARY TO MAKE SHAPE
victim<-d@data[,"question"] %>% unique() %>% data.frame()
victim$fid<-1:nrow(victim)
victim$lat<-rep(0,nrow(victim))
victim$lon<-rep(0,nrow(victim))

coordinates(victim)<-~lon+lat
proj4string(victim)<-proj4string(ocha2sp)
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/victim_files"
 # writeOGR(dsn=folder,obj= victim,layer="victim_select1_lga", driver = "ESRI Shapefile", overwrite_layer = TRUE)

number1_choice_ward<-round_2_output%>% 
  filter(living_now=="Yes") %>% # only consider populated settlements 
  group_by(h2r_state,h2r_lga,h2r_ward) %>%
  summarize_all(funs(m=Mode2))

ward_list<-split.data.frame(agg_input, agg_input$ward_id)
s2<-list()
for (i in 1:length(ward_list)){
  a <- sapply(ward_list[[i]],function(x) Mode2(x)) %>% data.frame
  s2[[i]]<-t(a)}
length(s2)
ward_mc<-do.call(rbind,s2) 
rownames(ward_mc)<-1:nrow(ward_mc)
ward_mc_df<-ward_mc %>% data.frame()
ward_mc_df$h2r_ward


set_per_ward<- round_2_output %>% 
  filter(living_now=="Yes") %>% 
  count(h2r_state,h2r_lga,h2r_ward) 
set_per_ward

ward_mc2<-left_join(ward_mc_df,set_per_ward)

number1_choice_ward2<-ward_mc2 %>% 
  select(c(h2r_state,h2r_lga,h2r_ward,n),everything())
colnames(number1_choice_ward2)
number1_choice_ward2<-number1_choice_ward2[,-c(118:119)]

data_long_wards <- gather(number1_choice_ward2, question, answer, kmz_yn:h2r_additional, factor_key=TRUE)
data_long_wards %>% head()
s1_svy_fullq<-round_2_output %>% ungroup() #UNGROUP SO THAT I CAN REGROUP LATER ACCORDINT TO QUESTION NAMES
colnames(s1_svy_fullq)<-full_q # RENAME COLUMNS
s1_svy_fullq %>% nrow()#CHECK LENGTH

number1_choice_ward_fullq<-s1_svy_fullq  %>% 
  filter(`Do people currently live in ${h2r_settlement}?`=="Yes") %>% 
  group_by(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`) %>% 
  summarize_all(funs(m=Mode))
number1_choice_ward_fullq$set_p_ward<-set_per_ward$n

number1_choice_ward_fullq2<-number1_choice_ward_fullq %>% 
  select(c(`In which State is the settlement that you have outside information about located?`,
           `In which LGA is the settlement that you have outside information about located?`,
           `In which ward is the settlement  that you have outside information about located?`,`set_p_ward`), everything())

data_long_wards_fullq <- gather(number1_choice_ward_fullq2, question, answer,`did you use the kmz file provided by REACH do find this respondent?_m`:`Do you have information on any other hard to reach settlements?_m`, factor_key=TRUE)

wards_long_full<-data.frame(h2r_state=data_long_wards$h2r_state,h2r_lga= data_long_wards$h2r_lga,
                            h2r_ward=data_long_wards$h2r_ward,set_p_ward=data_long_wards$n ,question_id=data_long_wards$question,
                            question=data_long_wards_fullq$question %>% as.character(),answer=data_long_wards$answer)
wards_long_full %>% head()

#need to "sanitize ocha3sp ward names so that the merge works correctly
ocha3sp$wardlink<-ocha3sp$ADM3_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha3sp$lgalink<-ocha3sp$ADM2_EN %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
ocha2sp$lgalink<-ocha2sp$admin2Name %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

## MAKE EVERYTHING A CHARACTER TO BE SAFE
number1_choice_ward_perfect<-sapply(wards_long_full, as.character) %>% data.frame

number1_choice_ward_perfect$h2r_lga2<-number1_choice_ward_perfect$h2r_lga %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()
number1_choice_ward_perfect$h2r_ward2<-number1_choice_ward_perfect$h2r_ward %>% trimws() %>%  gsub("[[:punct:]]", "_",.) %>% gsub(" ","_",.) %>% tolower()

### MERGE LONG DATA TO OCHA ADMIN SHAPE FILE-- ALLOW DUPLICATE GEOMETRIES
d<-sp::merge(x=ocha3sp,y=number1_choice_ward_perfect,by.x=c("lgalink","wardlink"), by.y=c("h2r_lga2","h2r_ward2"),all.x=FALSE,all.y=TRUE,duplicateGeoms = TRUE)

#WRITE DATA TO SHAPEFILE
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/select_one"
writeOGR(dsn=folder,obj= d,layer="most_common_populated_ward_level_04Aug2018", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#NEXT IT IS NECESSARY TO MAKE SHAPE
victim<-d@data[,"question"] %>% unique() %>% data.frame()
victim$fid<-1:nrow(victim)
victim$lat<-rep(0,nrow(victim))
victim$lon<-rep(0,nrow(victim))

coordinates(victim)<-~lon+lat
proj4string(victim)<-proj4string(ocha2sp)
#NEED TO CHANGE PATH
folder="F:/REACH_Work_Backup_23August2018/Impact_GIS/02_MSNA_2018/06_H2R/01_GIS_H2R/04_SHP_H2R/long_format_data/victim_files"
#UNCOMMENT TO WRITE FILE
# writeOGR(dsn=folder,obj= victim,layer="victim_select1_ward", driver = "ESRI Shapefile", overwrite_layer = TRUE)
















