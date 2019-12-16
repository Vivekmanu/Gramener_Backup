
# Load Everthing to one directory:
setwd("D:/Rolled_Up_View")

#libraries:
require(tidyverse)
require(readxl)
require(janitor)
require(dplyr)
require(reshape2)
library(rlang)
require(xlsx)
library(openxlsx)
library(stringr)

#Data Gather################################################################################################################################
#ITSM:
itsm<- read_excel('itsm.xlsx', na = "NA") %>% clean_names()

#Manfacturer_mapping:
manu_mapping <- read_excel('Manufacturer_Std_Mapping V1_new.xlsx')  %>% clean_names()

#Contracts:
contracts <- read_excel('Contracts June Snapshot -Active and ND.xlsx', na = 'NA' )
contracts$contract_line_number <- paste(contracts$ContractNumber,contracts$ContractItemNumber,sep ="-")
contracts$NetValueUSD <- as.numeric(contracts$NetValueUSD)

#Selecting only VBR and Support Services:
contract_fil <- contracts %>% filter(Portfolio == "VBR" | Portfolio == "Support Services")
contracts <- contract_fil

#PO:
po <- read.csv('PO.csv', na.strings = "", stringsAsFactors = F)
po$sales_document_line_number <- paste(po$SalesDocumentNumber,po$SalesDocumentItemNumber,sep = "-")

#Calculating missing NetValueUSD for non-zero ACVBUDGETUSD:
contracts_filter<- contracts %>% filter(ACVUSDBudget != 0, NetValueUSD == 0)
contracts_filter$date_diff_days <- as.Date(contracts_filter$`End Date`)- as.Date(contracts_filter$`Start Date`)
contracts_filter$ACV_per_day <- as.numeric(contracts_filter$ACVUSDBudget/365)
contracts_filter$NetValueUSD <- as.numeric(contracts_filter$ACV_per_day * as.numeric(contracts_filter$date_diff_days))
contracts_filter<- contracts_filter[,c(67,45)]

contracts_merged_final <- merge(contracts, contracts_filter, by = "contract_line_number", all.x = T)
contracts_merged_final$NetValueUSD.y[which(is.na(contracts_merged_final$NetValueUSD.y))]<- contracts_merged_final$NetValueUSD.x[which(is.na(contracts_merged_final$NetValueUSD.y))]


contracts_filter_2<- filter( contracts_merged_final, ACVUSDBudget != 0 & NetValueUSD.y != 0)
#removing NetValueUSD.x
contracts_filter_2 <- contracts_filter_2[,-46]
#renaming NetValueUSD.y
names(contracts_filter_2)[names(contracts_filter_2) == "NetValueUSD.y"] <- "NetValueUSD.x"

#Taking contracts_filter_2 as Contracts:
contracts <- contracts_filter_2

################ VENDOR, ITSM_MANUFACTURER, MANUFACTURER_STANDARDISATION: ################################################################################################################

####1: Imputing DD-AM,AP,EU, GLOBAL, MEA:

po$Manufacturer <- gsub("DD-GLOBAL", "DD", po$Manufacturer)
po$Manufacturer <- gsub("DD-AM", "DD", po$Manufacturer)
po$Manufacturer <- gsub("DD-AP", "DD", po$Manufacturer)
po$Manufacturer <- gsub("DD-EU", "DD", po$Manufacturer)
po$Manufacturer <- gsub("DD-MEA", "DD", po$Manufacturer)
contracts$ManufacturerCode <- gsub("DD-GLOBAL", "DD",contracts$ManufacturerCode)
contracts$ManufacturerCode <- gsub("DD-AM", "DD",contracts$ManufacturerCode)
contracts$ManufacturerCode <- gsub("DD-AP", "DD", contracts$ManufacturerCode)
contracts$ManufacturerCode <- gsub("DD-EU", "DD", contracts$ManufacturerCode)
contracts$ManufacturerCode <- gsub("DD-MEA", "DD", contracts$ManufacturerCode)


####2: Mapping standard manufactureres names to ITSM from Mapping File:################################################################################################################

#renaming itsm_manufacturer to manufacturer in manu_mapping
names(manu_mapping)[names(manu_mapping) == "itsm_manufacturer"] <- "manufacturer"
#names(manu_mapping)[names(manu_mapping) == "manufacturer"] <- "itsm_manufacturer"

#getting manufacturers standard names in ITSM:
itsm_mapping_merged <-  merge(itsm, manu_mapping, by= "manufacturer" , all.x = T)

#Rearranging:
itsm_mapping_merged <- itsm_mapping_merged[,c(2,1,3)]

sum(is.na(itsm_mapping_merged$standard_mapping))
#[1] 4761 not mapped
sum(!is.na(itsm_mapping_merged$standard_mapping))
#[1] 114312 mapped

#Replacing NAs in Standard Mapping with manufacturer as given in manu_mapping file.
itsm_mapping_merged$standard_mapping[which(is.na(itsm_mapping_merged$standard_mapping))] <- itsm_mapping_merged$manufacturer[which(is.na(itsm_mapping_merged$standard_mapping))]

itsm <- itsm_mapping_merged %>% clean_names()

#Merging Contracts, PO and ITSM:
contracts_copy <- as.data.frame(contracts)

#contract_PO:
contracts_merged_po <- merge(contracts,po,by.x = "contract_line_number",by.y = "sales_document_line_number",all.x = T)
contracts_merged_po$merged_po <- ifelse(is.na(contracts_merged_po$SalesDocumentNumber),0,1)

#contracts_po_itsm:
contract_po_itsm_merged <- merge(contracts_merged_po, itsm[,c(1,3)], by= "contract_line_number", all.x = T)
contract_po_itsm_merged$merged_itsm <- ifelse(is.na(contract_po_itsm_merged$standard_mapping),0,1)

#missing itsm manufacturers in contracts_po_itsm:
sum(is.na(contract_po_itsm_merged$standard_mapping))
# 173798


# here on taking contracts as a single source of all data.
contracts<- contract_po_itsm_merged %>% clean_names()


####3:################ Vendor standardisation: ################################################################################################################################

contracts_manufac_filter <- contracts %>% select(contract_line_number, manufacturer, manufacturer_code, vendor, standard_mapping)
vendor_mapper <-  manu_mapping
names(vendor_mapper)[names(vendor_mapper) == "manufacturer"] <- "vendor"
names(vendor_mapper)[names(vendor_mapper) == "standard_mapping"] <- "standard_mapping_vendor"
contracts_manufac_filter_1 <- merge(contracts_manufac_filter[,c(1,4)], vendor_mapper, by="vendor", all.x = T)

#Replacing NA in standardised_vendor with existing vendor missing in vendor_mapper:
contracts_manufac_filter_1$standard_mapping_vendor[which(is.na(contracts_manufac_filter_1$standard_mapping_vendor))] <- contracts_manufac_filter_1$vendor[which(is.na(contracts_manufac_filter_1$standard_mapping_vendor))]

#tail(contracts_manufac_filter_1)
contracts_manufac_filter_1 <- contracts_manufac_filter_1[-c(390767:390798),]

#Arranging Contract_line_number and Vendor for cbind:
contracts <- arrange(contracts, contract_line_number, vendor)
contracts_manufac_filter_1 <- arrange(contracts_manufac_filter_1, contract_line_number, vendor)

#cbinding standardised vendor to contracts:
contracts <- contracts[,-73] #removing_vendor
contracts_manufac_filter_1 <- contracts_manufac_filter_1[,-c(1)] #removing_vendor
contracts <- cbind(contracts, contracts_manufac_filter_1)
contracts <- contracts[,-85] # removing extra contract_line_number
names(contracts)[names(contracts) == "standard_mapping_vendor"] <- "vendor" #standardized_vendor

# Removing unwanted DFs:
rm(contract_fil)
rm(contract_po_itsm_merged)
rm(contracts_copy)
rm(contracts_filter)
rm(contracts_filter_2)
rm(contracts_manufac_filter)
rm(contracts_manufac_filter_1)
rm(contracts_merged_final)
rm(itsm_mapping_merged)
rm(vendor_mapper)

################ Manufacturer-Mapping: ################################################################################################################ 

contracts$manufacturer_po <- contracts$manufacturer
contracts$manufacturer <- NULL
contracts$manufacturer <- ifelse(contracts$portfolio == "VBR",as.character(contracts$manufacturer_code),as.character(contracts$manufacturer_po))
contracts$manufacturer <- ifelse(contracts$portfolio == "VBR" & is.na(contracts$manufacturer), as.character(contracts$vendor),as.character(contracts$manufacturer))

contracts$manufacturer <- ifelse(contracts$portfolio == "Support Services" & contracts$merged_po ==0 ,as.character(contracts$standard_mapping),as.character(contracts$manufacturer))
contracts$manufacturer <- ifelse(contracts$portfolio == "Support Services" & contracts$merged_po ==1 & is.na(contracts$manufacturer),as.character(contracts$vendor),as.character(contracts$manufacturer))
contracts$manufacturer <- ifelse(contracts$portfolio == "Support Services" & contracts$merged_po ==1 & is.na(contracts$manufacturer),as.character("DD-UPTIME"),as.character(contracts$manufacturer))

contracts$manufacturer[which(is.na(contracts$manufacturer))] <- as.character("DD-UPTIME")

#merged_itsm:
contracts$merged_itsm <- ifelse(contracts$manufacturer == contracts$standard_mapping,1,0)

# removing manufacturer_po:
contracts <- contracts[,-c(85)]

#########################################################################################################################################################################
# After merge NetValue_x is skewed :
# Overall:
Merged_TCV <-  contracts %>% summarise(sum(net_value_usd_x))
# sum(net_value_usd_x)
#     53144981950

#VBR:
vf <- contracts %>% filter(portfolio =="VBR") %>%summarise(sum(net_value_usd_x))
# sum(net_value_usd_x)
#     2880448485

#Support Services:
sf <- contracts %>% filter(portfolio =="Support Services") %>%summarise(sum(net_value_usd_x))
# sum(net_value_usd_x)
#     50264533465
################################################ Adjusting NetValueUSD_x on Final PO and ISM Merged Contracts  ##########################################################

#client_country & client_country_manufacturer combinations.
contracts$client_country <- paste(contracts$adjusted_client_domain_description,contracts$country,sep = "-")
contracts$client_country_manufacturer <- paste(contracts$client_country,contracts$manufacturer ,sep = "-")

# Separate DF for vbr and ss :
df_vbr <- contracts %>% filter(portfolio =="VBR")
df_ss <- contracts %>% filter(portfolio =="Support Services")

#Creating TCV split up: only for Support Services.
total_spend_value <- df_ss  %>% group_by(contract_line_number) %>% summarise("total_spend_value" = sum(spend_value_usd,na.rm =T),count=n_distinct(client_country_manufacturer))
total_spend_value <- as.data.frame(total_spend_value)
df_ss <- merge(df_ss ,total_spend_value,by="contract_line_number",all.x = T)
df_ss$net_value_usd_x_1 <- df_ss$net_value_usd_x/df_ss$count

df_ss$proportion <-  df_ss$spend_value_usd/ifelse(df_ss$spend_value_usd == 0|df_ss$total_spend_value == 0,NA,df_ss$total_spend_value)
pos_na <- which(is.na(df_ss$proportion))
df_na <- df_ss[which(is.na(df_ss$proportion)),]

df_na1 <- df_na %>% group_by(contract_line_number) %>% summarise(count = n()) %>% mutate(proportion_new = 1/count)
df_na2 <- merge(df_na,df_na1[,c(1,3)],by = "contract_line_number")
df_ss$proportion[pos_na] <- df_na2$proportion_new

#Creating TCV split up: only for VBR.
vbr_cli_count <- df_vbr %>% group_by(contract_line_number) %>% summarise(count = n()) %>% mutate(proportion = 1/count)
vbr_cli_count <- as.data.frame(vbr_cli_count)
df_vbr <- merge(df_vbr ,vbr_cli_count, by="contract_line_number",all.x = T)
df_vbr$net_value_usd_x_1 <- df_vbr$net_value_usd_x

# removing total_spend_value from df_ss:
df_ss<- df_ss[,-88]

#rbinding df_ss & df_vbr
contracts <- rbind(df_ss,df_vbr)

# net_value_usd_x_calc :
contracts$net_value_usd_x_calc <- contracts$net_value_usd_x_1 * contracts$proportion

############################################################################################################################################
# After fix net_value_usd_x_calc check :
# Overall:
Merged_TCV <-  contracts %>% summarise(sum(net_value_usd_x_calc))
# sum(net_value_usd_x)
#     2830058965

#VBR:
vf <- contracts %>% filter(portfolio =="VBR") %>%summarise(sum(net_value_usd_x_calc))
# sum(net_value_usd_x)
#     1201307327

#Support Services:
sf <- contracts %>% filter(portfolio =="Support Services") %>%summarise(sum(net_value_usd_x_calc))
# sum(net_value_usd_x)
#     1628751638

################# CLIENT_COUNTRY_MANUFACTURER_VIEW ###############################################################################

# Removing contract_line_number and existing count:

contracts <- contracts[,-c(1,2,3,88)]
# names(contracts)
# when on doubt use names(df) to get column number

################ Creating New Proportions for ccm view ############################################################################

# Separate DF for vbr and ss :
df_vbr <- contracts %>% filter(portfolio =="VBR")
df_ss <- contracts %>% filter(portfolio =="Support Services")

#Creating TCV split up: only for Support Services.
total_net_value_ss <- df_ss  %>% group_by(client_country_manufacturer) %>% summarise("total_net_value" = sum(net_value_usd_x_calc,na.rm =T),count=n())
total_net_value_ss <- as.data.frame(total_net_value_ss)
df_ss <- merge(df_ss ,total_net_value_ss,by="client_country_manufacturer",all.x = T)

# since no NA:
#sum(which(is.na(df_ss$net_value_usd_x ==0)))
#[1] 0 =>
df_ss$proportion_1 <- 1/df_ss$count

# Creating TCV split up: only for  VBR.
total_net_value_vbr <- df_vbr  %>% group_by(client_country_manufacturer) %>% summarise("total_net_value" = sum(net_value_usd_x_calc,na.rm =T),count=n())
total_net_value_vbr <- as.data.frame(total_net_value_vbr)
df_vbr <- merge(df_vbr ,total_net_value_vbr,by="client_country_manufacturer",all.x = T)

df_vbr$proportion_1 <- 1/df_vbr$count

#rbinding df_ss & df_vbr
contracts <- rbind(df_ss,df_vbr)

# Net_Value_USD SPLIT :
contracts$net_value_usd_x_calc_1 <- contracts$total_net_value * contracts$proportion_1

######################################################################## #####################################################################
################ CHECK NET_VALUE_SPLIT_CALC_1  across client_country_manufacturer groups #####################################################

vf<- contracts %>% filter(portfolio=="VBR") 
sum(vf$net_value_usd_x_calc_1)
# 1201307327

sf<- contracts %>% filter(portfolio=="Support Services") 
sum(sf$net_value_usd_x_calc_1)
# 1628751640

#ccm_f <- contracts %>% select(contract_line_number,client_country_manufacturer,count,net_value_usd_x,net_value_usd_x_1,spend_value_usd,proportion,net_value_usd_x_calc)

Merged_TCV_Con_PO_ITSM <- sum(contracts$net_value_usd_x_calc_1)
Merged_TCV_Con_PO_ITSM
#[1] 2830058967

#Removing unwanted Dfs:
rm(df_na)
rm(df_na1)
rm(df_na2)
rm(df_ss)
rm(df_vbr)
rm(sf)
rm(vf)
rm(total_net_value_ss)
rm(total_net_value_vbr)
rm(vbr_cli_count)
rm(Merged_TCV)
rm(total_spend_value)
rm(pos_na)

####### selecting columns required for ROLL-UP #####################################################################################################

rolled_up_filter <- contracts %>% select(client_country,country,client_country_manufacturer,adjusted_client_domain_description,manufacturer,proportion_1,portfolio,net_value_usd_x_calc_1)
rolled_up_data <- as.data.frame(rolled_up_filter)

####### GROUPING- MAIN ROLL-UP ####################################################################################################

roll_test <- rolled_up_data %>% group_by(client_country,country,adjusted_client_domain_description,manufacturer,portfolio) %>% summarise(grand_total = sum(net_value_usd_x_calc_1))

roll_test <- as.data.frame(roll_test)

rolled_up_view <- spread(roll_test, portfolio,grand_total)

rolled_up_view$VBR <- ifelse(rolled_up_view$VBR == 0,NA,rolled_up_view$VBR)

rolled_up_view$`Support Services` <- ifelse(rolled_up_view$`Support Services` == 0,NA,rolled_up_view$`Support Services`)

#names(rolled_up_view):
rolled_up_view$grand_total <- rowSums(rolled_up_view[,c(5,6)], na.rm=TRUE)

rolled_up_view$client_country_manufacturer <- paste(rolled_up_view$client_country, rolled_up_view$manufacturer, sep = '-')

rolled_up_view <- as.data.frame(rolled_up_view)

#ru_backup <- as.data.frame(rolled_up_view)
#rolled_up_view <- as.data.frame(ru_backup)

# Rearranging:
#names(rolled_up_view)
rolled_up_view <- rolled_up_view[c(3,2,1,4,8,5,6,7)]

#-----------------------------------------*** DERIVED - METRICS ***---------------------------------------------------------------------------------------------------------
#Creating intelligent columns:

#client_country_manufacturer_both_1_uptime_2_vbr_3:
client_country_manufacturer_both_1_uptime_2_vbr_3 <- rolled_up_view  %>% group_by(client_country_manufacturer) %>% summarise(total_vbr=sum(VBR,na.rm = T), total_ss =sum(`Support Services`,na.rm =T))
client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr <- ifelse(client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr == 0,NA,client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr)
client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss  <- ifelse(client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss == 0,NA,client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss)
client_country_manufacturer_both_1_uptime_2_vbr_3 <- as.data.frame(client_country_manufacturer_both_1_uptime_2_vbr_3)
client_country_manufacturer_both_1_uptime_2_vbr_3$client_country_manufacturer_both_1_uptime_2_vbr_3 <- ifelse((!is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr) &  !is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss)),1,ifelse((is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr) &  !is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss)),2,ifelse((!is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_vbr) &  is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$total_ss)),3,4)))
client_country_manufacturer_both_1_uptime_2_vbr_3 <- client_country_manufacturer_both_1_uptime_2_vbr_3[,c(1,4)]

rolled_up_view <- merge(rolled_up_view,client_country_manufacturer_both_1_uptime_2_vbr_3,by = "client_country_manufacturer",all.x = T)
rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3 <- ifelse(is.na(rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3),4,rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3)
rm(client_country_manufacturer_both_1_uptime_2_vbr_3)

#multiple_overall
multiple_overall <- rolled_up_view %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
multiple_overall <- as.data.frame(multiple_overall)
multiple_overall$multiple_overall <- 1
multiple_overall <- multiple_overall[,c(1,3)]
rolled_up_view <- merge(rolled_up_view,multiple_overall,by = "client_country",all.x = T)
rolled_up_view$multiple_overall  <- ifelse(!is.na(rolled_up_view$multiple_overall),1,0)
sum(rolled_up_view$multiple_overall)
# [1] 32944
rm(multiple_overall)

#client_uptime
client_uptime <- rolled_up_view %>% filter(!is.na(`Support Services`)) %>% group_by(adjusted_client_domain_description) %>% summarise(count = n())
client_uptime <- as.data.frame(client_uptime)
client_uptime$client_uptime <- 1
client_uptime <- client_uptime[,c(1,3)]

rolled_up_view <- merge(rolled_up_view,client_uptime,by = "adjusted_client_domain_description",all.x = T)
rolled_up_view$client_uptime  <- ifelse(!is.na(rolled_up_view$client_uptime),1,0)
sum(rolled_up_view$client_uptime)
# [1] 30270
rm(client_uptime)

#client_country_uptime
client_country_uptime <- rolled_up_view %>% filter(!is.na(`Support Services`)) %>% group_by(client_country) %>% summarise(count = n())
client_country_uptime <- as.data.frame(client_country_uptime)
client_country_uptime$client_country_uptime <- 1
client_country_uptime <- client_country_uptime[,c(1,3)]

rolled_up_view <- merge(rolled_up_view,client_country_uptime,by = "client_country",all.x = T)
rolled_up_view$client_country_uptime  <- ifelse(!is.na(rolled_up_view$client_country_uptime),1,0)
sum(rolled_up_view$client_country_uptime)
# [1] 28181
rm(client_country_uptime)

#rolled_up_bckp<- as.data.frame(rolled_up_view)

#success_play_3 discussed logic:
#creating client-manufacturer:
rolled_up_view$client_manufacturer <- paste(rolled_up_view$adjusted_client_domain_description, rolled_up_view$manufacturer, sep = "-")
rp_data <- rolled_up_view %>% select(client_manufacturer,VBR,`Support Services`,grand_total) %>% group_by(client_manufacturer) %>% summarise(total_vbr = sum(VBR,na.rm=T),total_ss = sum(`Support Services`,na.rm=T) )
distinct_client_country <- rp_data %>% filter(total_vbr!=0 & total_ss!=0)  %>% mutate(flag =1)
distinct_client_country <- distinct_client_country[,c(1,4)]
rolled_up_view <- merge(rolled_up_view,distinct_client_country, by="client_manufacturer",all.x = T)
rolled_up_view$flag <- ifelse(is.na(rolled_up_view$flag),0,rolled_up_view$flag)

s3_ss  <- rolled_up_view %>% filter(!is.na(`Support Services`))  %>%  distinct(client_country_manufacturer)
s3_vbr <- rolled_up_view %>% filter(!is.na(VBR)) %>%  distinct(client_country_manufacturer)

s3_ss$f<- "ss"
s3_vbr$f<- "vbr"

vbr_left <- merge(s3_vbr , s3_ss, by ='client_country_manufacturer', all.x = T)
vbr_left$check <- ifelse(is.na(vbr_left$f.y),1,0)
vbr_left<-vbr_left[c(1,4)]

rolled_up_view <- merge(rolled_up_view,vbr_left, by="client_country_manufacturer",all.x = T)
rolled_up_view$check <- ifelse(is.na(rolled_up_view$check),0,rolled_up_view$check)
rolled_up_view$check_final<- ifelse(rolled_up_view$flag ==1 & rolled_up_view$check ==1 ,1,0)

##### Creating Success_Play columns:  #####################################################################################################

rolled_up_view$success_play_1 <- ifelse(rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3 == 1,1,0)

rolled_up_view$success_play_2 <- ifelse(rolled_up_view$multiple_overall == 1 & rolled_up_view$success_play_1 == 0 & rolled_up_view$client_country_uptime==1 & rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3==3,1,0)

rolled_up_view$success_play_3 <- ifelse(rolled_up_view$client_country_manufacturer_both_1_uptime_2_vbr_3==3 & rolled_up_view$check_final==1,1,0)

rolled_up_view$success_play_4 <- ifelse(rolled_up_view$client_uptime == 1,1,0)

rolled_up_view$success_play <- ifelse(rolled_up_view$success_play_1 == 1 ,1,ifelse(rolled_up_view$success_play_1==0 & rolled_up_view$success_play_2 ==1,2,ifelse(rolled_up_view$success_play_1==0 & rolled_up_view$success_play_2==0 & rolled_up_view$success_play_3==1,3,ifelse(rolled_up_view$success_play_1==0 & rolled_up_view$success_play_2==0 & rolled_up_view$success_play_3==0 & rolled_up_view$success_play_4==1,4,"No Success Play"))))

# Creating TPIC_FLAG:client_country
Tpic <- rolled_up_view %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1) %>% mutate(TPIC_FLAG = 1)
Tpic <- Tpic[,c(1,3)] 
rolled_up_view <- merge(rolled_up_view,Tpic,by = "client_country",all.x = T)
rolled_up_view$TPIC_FLAG <- ifelse(is.na(rolled_up_view$TPIC_FLAG),0,rolled_up_view$TPIC_FLAG)


# Validation_Check:
VBR_netval <- rolled_up_view  %>% summarise(total=sum(VBR,na.rm = T))
VBR_netval <- VBR_netval$total

SS_netval <- rolled_up_view %>% summarise(total=sum(`Support Services`,na.rm = T))
SS_netval <- SS_netval$total

SS_1_netval <- rolled_up_view %>% filter(success_play_1==1) %>% summarise(total=sum(grand_total,na.rm = T))
SS_1_netval <- SS_1_netval$total

SS_2_netval <- rolled_up_view %>% filter(success_play_2==1) %>% summarise(total=sum(grand_total,na.rm = T))
SS_2_netval <- SS_2_netval$total

SS_3_netval <- rolled_up_view %>% filter(success_play_3==1) %>% summarise(total=sum(grand_total,na.rm = T))
SS_3_netval <- SS_3_netval$total

SS_4_netval <- rolled_up_view %>% filter(success_play_4==1) %>% summarise(total=sum(grand_total,na.rm = T))
SS_4_netval <- SS_4_netval$total

# re-arranging:
# names(rolled_up_view)

rm(distinct_client_country)
rm(contracts_merged_po)
rm(roll_test)
rm(rolled_up_data)
rm(rolled_up_filter)
rm(rp_data)
rm(Tpic)
rm(s3_ss)
rm(s3_vbr)
rm(vbr_left)

# Rearranging:
rolled_up_view_1 <- rolled_up_view[c(1,4,6,7,8,9,17,18,19,20,21,22)]
 
# OUTPUT:
write.xlsx(rolled_up_view_1,"Rolled_up_final_view.xlsx")



