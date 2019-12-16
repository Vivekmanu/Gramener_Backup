
setwd("D:/Manufacturer_Overall_Standardisation")

require(tidyverse)
require(readxl)
require(janitor)
require(dplyr)
require(reshape2)
library(rlang)
require(xlsx)
library(openxlsx)

#ITSM:
itsm<- read_excel('itsm.xlsx', na = "NA") %>% clean_names()

#Manfacturer_mapping:
manu_mapping <- read_excel('Manufacturer_Std_Mapping V1_new.xlsx')  %>% clean_names()

#Contracts:
contracts <- read_excel('Contracts June Snapshot -Active and ND.xlsx', na = 'NA' )
contracts$contract_line_number <- paste(contracts$ContractNumber,contracts$ContractItemNumber,sep ="-")
contracts$NetValueUSD <- as.numeric(contracts$NetValueUSD)

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

################ VENDOR, ITSM_MANUFACTURER, MANUFACTURER_STANDARDISATION: ################

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


####2: Mapping standard manufactureres names to ITSM from Mapping File:

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

# Final ITSM:
itsm <- itsm_mapping_merged %>% clean_names()

################# Merging Contracts, PO and ITSM ################ 

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


####3:################ Vendor standardisation: ################

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

# Removing excess DFs:
rm(contract_fil)
rm(contract_po_itsm_merged)
rm(contracts_filter_2)
rm(contracts_filter)
rm(contracts_manufac_filter)
rm(contracts_manufac_filter_1)
rm(contracts_merged_po)
rm(vendor_mapper)
rm(itsm_mapping_merged)
rm(contracts_merged_final)

################ Manufacturer-Mapping: ################ 
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

################# Creating basic column combinations:################ 

#client_country & client_country_manufacturer combinations.
contracts$client_country <- paste(contracts$adjusted_client_domain_description,contracts$country,sep = "-")
contracts$client_country_manufacturer <- paste(contracts$client_country,contracts$manufacturer ,sep = "-")

# Separate DF for vbr and ss :
df_vbr <- contracts %>% filter(portfolio =="VBR")
df_ss <- contracts %>% filter(portfolio =="Support Services")

################ CALCULATING "Net_Value_USD SPLIT ":################ 

#Creating TCV split up: only for Merged PO == Support Services
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

#Creating TCV split up: only for Merged PO == VBR.
vbr_cli_count <- df_vbr %>% group_by(contract_line_number) %>% summarise(count = n()) %>% mutate(proportion = 1/count)
vbr_cli_count <- as.data.frame(vbr_cli_count)
df_vbr <- merge(df_vbr ,vbr_cli_count, by="contract_line_number",all.x = T)
df_vbr$net_value_usd_x_1 <- df_vbr$net_value_usd_x

# removing total_spend_value from df_ss:
df_ss<- df_ss[,-88]

#rbinding df_ss & df_vbr
contracts <- rbind(df_ss,df_vbr)

# Net_Value_USD SPLIT :
contracts$net_value_usd_x_calc <- contracts$net_value_usd_x_1 * contracts$proportion

# ######### CHECK Portfolio = SS, Spend value across client_country_manufacturer group == 0 #################

vf<- contracts %>% filter(portfolio=="VBR") 
sum(vf$net_value_usd_x_calc)
# 1201307327

sf<- contracts %>% filter(portfolio=="Support Services") 
sum(sf$net_value_usd_x_calc)
# 1628751638

#ccm_f <- contracts %>% select(contract_line_number,client_country_manufacturer,count,net_value_usd_x,net_value_usd_x_1,spend_value_usd,proportion,net_value_usd_x_calc)

Merged_TCV_Con_PO_ITSM <- sum(contracts$net_value_usd_x_calc)
Merged_TCV_Con_PO_ITSM
#[1] 2830058965

rm(total_spend_value)
rm(pos_na)
rm(df_na)
rm(df_na1)
rm(df_na2)
rm(vbr_cli_count)
rm(df_ss)
rm(df_vbr)
rm(sf)
rm(vf)

############################################################################################################

#-----------------------------------------*** DERIVED - METRICS ***---------------------------------------------------------------------------------------------------------
#Creating intelligent columns:

#client_manufacturer
contracts$client_manufacturer <- paste(contracts$adjusted_client_domain_description,contracts$manufacturer ,sep = "-")

#client_country_uptime
client_country_uptime <- contracts %>% filter(portfolio != "VBR") %>% group_by(client_country) %>% summarise(count = n())
client_country_uptime <- as.data.frame(client_country_uptime)
client_country_uptime$client_country_uptime <- 1
client_country_uptime <- client_country_uptime[,c(1,3)]

contracts <- merge(contracts,client_country_uptime,by = "client_country",all.x = T)
contracts$client_country_uptime  <- ifelse(!is.na(contracts$client_country_uptime),1,0)
sum(contracts$client_country_uptime)
# [1]  343695
rm(client_country_uptime)

#client_uptime
client_uptime <- contracts %>% filter(portfolio != "VBR") %>% group_by(adjusted_client_domain_description) %>% summarise(count = n())
client_uptime <- as.data.frame(client_uptime)
client_uptime$client_uptime <- 1
client_uptime <- client_uptime[,c(1,3)]

contracts <- merge(contracts,client_uptime,by = "adjusted_client_domain_description",all.x = T)
contracts$client_uptime  <- ifelse(!is.na(contracts$client_uptime),1,0)
sum(contracts$client_uptime)
# [1] 362416
rm(client_uptime)

#multiple_overall
multiple_overall <- contracts %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
multiple_overall <- as.data.frame(multiple_overall)
multiple_overall$multiple_overall <- 1
multiple_overall <- multiple_overall[,c(1,3)]
contracts <- merge(contracts,multiple_overall,by = "client_country",all.x = T)
contracts$multiple_overall  <- ifelse(!is.na(contracts$multiple_overall),1,0)
sum(contracts$multiple_overall)
# [1] 350983
rm(multiple_overall)

#client_country_manufacturer_both_1_uptime_2_vbr_3 
client_country_manufacturer_both_1_uptime_2_vbr_3 <- contracts %>% filter(!is.na(manufacturer)) %>% group_by(client_country_manufacturer,portfolio) %>% summarise(tcv = sum(net_value_usd_x_calc,na.rm = T))
client_country_manufacturer_both_1_uptime_2_vbr_3 <- as.data.frame(client_country_manufacturer_both_1_uptime_2_vbr_3)
client_country_manufacturer_both_1_uptime_2_vbr_3 <- dcast(client_country_manufacturer_both_1_uptime_2_vbr_3,client_country_manufacturer~portfolio)
client_country_manufacturer_both_1_uptime_2_vbr_3$total <- rowSums(client_country_manufacturer_both_1_uptime_2_vbr_3[,c(2,3)], na.rm=TRUE)
client_country_manufacturer_both_1_uptime_2_vbr_3$VBR <- ifelse(client_country_manufacturer_both_1_uptime_2_vbr_3$VBR == 0,NA,client_country_manufacturer_both_1_uptime_2_vbr_3$VBR)
client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services` <- ifelse(client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services` == 0,NA,client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)
client_country_manufacturer_both_1_uptime_2_vbr_3$client_country_manufacturer_both_1_uptime_2_vbr_3 <- ifelse((!is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),1,ifelse((is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),2,ifelse((!is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$VBR) &  is.na(client_country_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),3,4)))
client_country_manufacturer_both_1_uptime_2_vbr_3 <- client_country_manufacturer_both_1_uptime_2_vbr_3[,c(1,5)]
contracts <- merge(contracts,client_country_manufacturer_both_1_uptime_2_vbr_3,by = "client_country_manufacturer",all.x = T)
contracts$client_country_manufacturer_both_1_uptime_2_vbr_3 <- ifelse(is.na(contracts$client_country_manufacturer_both_1_uptime_2_vbr_3),4,contracts$client_country_manufacturer_both_1_uptime_2_vbr_3)
rm(client_country_manufacturer_both_1_uptime_2_vbr_3)

## --------------------------------------------------------------------------------------------------------------------------------------

#Renaming columns as standard names:
names(contracts)[names(contracts) == "manufacturer_part_number_x"] <- "manufacturer_part_number"
contracts$tpic <- NA

#removing standard mapping:
contracts <- contracts[,-c(84)]

# Success_play 3 new logic:
sp3_data <- contracts %>% group_by(client_manufacturer,portfolio) %>% summarise(tcv = sum(net_value_usd_x_calc,na.rm = T))
sp3_data <- as.data.frame(sp3_data)
sp3_data <- dcast(sp3_data, client_manufacturer~portfolio)
sp3_data$grand_total <- rowSums(sp3_data[,c(2,3)],na.rm = T)
sp3_data$VBR <- ifelse(sp3_data$VBR == 0,NA,sp3_data$VBR)
sp3_data$`Support Services` <- ifelse(sp3_data$`Support Services` == 0,NA,sp3_data$`Support Services`)

sp3_data <- sp3_data  %>% group_by(client_manufacturer) %>% summarise(total_vbr = sum(VBR,na.rm=T),total_ss = sum(`Support Services`,na.rm=T))
distinct_client_country <- sp3_data %>% filter(total_vbr!=0 & total_ss!=0)  %>% mutate(flag =1)
distinct_client_country <- distinct_client_country[,c(1,4)]
contracts <- merge(contracts,distinct_client_country, by="client_manufacturer",all.x = T)
contracts$flag <- ifelse(is.na(contracts$flag),0,contracts$flag)

s3_ss  <- contracts %>% filter(portfolio == "Support Services")  %>%  distinct(client_country_manufacturer)
s3_vbr <- contracts %>% filter(portfolio == "VBR") %>%  distinct(client_country_manufacturer)

s3_ss$f<- "ss"
s3_vbr$f<- "vbr"

vbr_left <- merge(s3_vbr , s3_ss, by ='client_country_manufacturer', all.x = T)
vbr_left$check <- ifelse(is.na(vbr_left$f.y),1,0)
vbr_left<-vbr_left[c(1,4)]

contracts <- merge(contracts,vbr_left, by="client_country_manufacturer",all.x = T)
contracts$check <- ifelse(is.na(contracts$check),0,contracts$check)
contracts$check_final<- ifelse(contracts$flag ==1 & contracts$check ==1 ,1,0)

# Calculating acvusd_budget_calc:
contracts$acvusd_budget_calc <- contracts$acvusd_budget * contracts$proportion

sum(contracts$acvusd_budget)
#[1] 29567559014
sum(contracts$acvusd_budget_calc)
#[1] 1750740508


# CALCULATING SUCCESS_PLAYS:
contracts$success_play_1 <- ifelse(contracts$client_country_manufacturer_both_1_uptime_2_vbr_3 == 1,1,0)

contracts$success_play_2 <- ifelse(contracts$multiple_overall ==1 & contracts$success_play_1 == 0 & contracts$client_country_uptime==1 & contracts$client_country_manufacturer_both_1_uptime_2_vbr_3==3,1,0)

contracts$success_play_3 <- ifelse(contracts$client_country_manufacturer_both_1_uptime_2_vbr_3==3 & contracts$check_final==1,1,0)

contracts$success_play_4 <- ifelse(contracts$client_uptime == 1,1,0)


rm(distinct_client_country)
rm(s3_ss)
rm(s3_vbr)
rm(sp3_data)
rm(vbr_left)

# Validation_Check:
VBR_netval <- contracts %>% filter(portfolio =="VBR") %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
VBR_netval <- VBR_netval$total

SS_netval <- contracts %>% filter(portfolio =="Support Services") %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
SS_netval <- SS_netval$total

SS_1_netval <- contracts %>% filter(success_play_1==1) %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
SS_1_netval <- SS_1_netval$total

SS_2_netval <- contracts %>% filter(success_play_2==1) %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
SS_2_netval <- SS_2_netval$total

SS_3_netval <- contracts %>% filter(success_play_3==1) %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
SS_3_netval <- SS_3_netval$total

SS_4_netval <- contracts %>% filter(success_play_4==1) %>% summarise(total=sum(net_value_usd_x_calc,na.rm = T))
SS_4_netval <- SS_4_netval$total

# OUTPUT:
write.csv(contracts,'contract_line_item_view_final.csv')
write.xlsx(contracts, 'contract_line_item_view_final.xlsx')







