require(tidyverse)
require(readxl)
require(janitor)
require(dplyr)
require(reshape2)

#Reading the contracts file
contracts <- read.csv('C:\\Users\\gramener\\Desktop\\VBR to Uptime dashboard\\Contracts.csv')
contracts$contract_line_number <- paste(contracts$ContractNumber,contracts$ContractItemNumber,sep ="-")

#Reading in the PO file
po <- read.csv('C:\\Users\\gramener\\Desktop\\VBR to Uptime dashboard\\PO.csv')
po$sales_document_line_number <- paste(po$SalesDocumentNumber,po$SalesDocumentItemNumber,sep = "-")

#Contracts merged with PO
contracts_merged_po <- merge(contracts,po,by.x = "contract_line_number",by.y = "sales_document_line_number",all.x = T)
contracts_merged_po$merged_po <- ifelse(is.na(contracts_merged_po$SalesDocumentNumber),0,1)

#Create a single view of Contracts merged with PO in Contracts
contracts <- contracts_merged_po %>% clean_names()


#Creating TCV split up
total_spend_value <- contracts %>% group_by(contract_line_number) %>% summarise("total_spend_value" = sum(spend_value_usd,na.rm =T))
total_spend_value <- as.data.frame(total_spend_value)
contracts <- merge(contracts,total_spend_value,by="contract_line_number",all.x = T)

contracts$proportion <-  contracts$spend_value_usd/ifelse(contracts$total_spend_value == 0,NA,contracts$total_spend_value)
pos_na <- which(is.na(contracts$proportion))
df_na <- contracts[which(is.na(contracts$proportion)),]
df_na1 <- df_na %>% group_by(contract_line_number) %>% summarise(count = n()) %>% mutate(proportion_new = 1/count)
df_na2 <- merge(df_na,df_na1[,c(1,3)],by = "contract_line_number")
contracts$proportion[pos_na] <- df_na2$proportion_new
contracts$net_value_usd_x_calc <- contracts$net_value_usd_x * contracts$proportion
rm(total_spend_value)
rm(pos_na)
rm(df_na)
rm(df_na1)
rm(df_na2)

#Creating basic column combinations
#client_country
contracts$client_country <- paste(contracts$adjusted_client_domain_description,contracts$country,sep = "-")

#manufacturer #if VBR take manufacturer_code else take from PO
contracts$manufacturer_po <- contracts$manufacturer
contracts$manufacturer <- NULL
contracts$manufacturer <- ifelse(contracts$portfolio == "VBR",as.character(contracts$manufacturer_code),as.character(contracts$manufacturer_po))

#client_manufacturer
contracts$client_manufacturer <- paste(contracts$adjusted_client_domain_description,contracts$manufacturer1 ,sep = "-")

#client_country_manufacturer
contracts$client_country_manufacturer <- paste(contracts$client_country,contracts$manufacturer ,sep = "-")

#Creating intelligent columns
#client_country_uptime
client_country_uptime <- contracts %>% filter(portfolio != "VBR") %>% group_by(client_country) %>% summarise(count = n())
client_country_uptime <- as.data.frame(client_country_uptime)
client_country_uptime$client_country_uptime <- 1
client_country_uptime <- client_country_uptime[,c(1,3)]

contracts <- merge(contracts,client_country_uptime,by = "client_country",all.x = T)
contracts$client_country_uptime  <- ifelse(!is.na(contracts$client_country_uptime),1,0)
sum(contracts$client_country_uptime)
rm(client_country_uptime)

#client_country_vbr
client_country_vbr <- contracts %>% filter(portfolio == "VBR") %>% group_by(client_country) %>% summarise(count = n())
client_country_vbr <- as.data.frame(client_country_vbr)
client_country_vbr$client_country_vbr <- 1
client_country_vbr <- client_country_vbr[,c(1,3)]

contracts <- merge(contracts,client_country_vbr,by = "client_country",all.x = T)
contracts$client_country_vbr  <- ifelse(!is.na(contracts$client_country_vbr),1,0)
sum(contracts$client_country_vbr)
rm(client_country_vbr)

#client_uptime
client_uptime <- contracts %>% filter(portfolio != "VBR") %>% group_by(adjusted_client_domain_description) %>% summarise(count = n())
client_uptime <- as.data.frame(client_uptime)
client_uptime$client_uptime <- 1
client_uptime <- client_uptime[,c(1,3)]

contracts <- merge(contracts,client_uptime,by = "adjusted_client_domain_description",all.x = T)
contracts$client_uptime  <- ifelse(!is.na(contracts$client_uptime),1,0)
sum(contracts$client_uptime)
rm(client_uptime)


#client_vbr
client_vbr <- contracts %>% filter(portfolio == "VBR") %>% group_by(adjusted_client_domain_description) %>% summarise(count = n())
client_vbr <- as.data.frame(client_vbr)
client_vbr$client_vbr <- 1
client_vbr <- client_vbr[,c(1,3)]

contracts <- merge(contracts,client_vbr,by = "adjusted_client_domain_description",all.x = T)
contracts$client_vbr  <- ifelse(!is.na(contracts$client_vbr),1,0)
rm(client_vbr)


#multiple_uptime 
multiple_uptime <- contracts %>% filter(portfolio != "VBR") %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
multiple_uptime <- as.data.frame(multiple_uptime)
multiple_uptime$multiple_uptime <- 1
multiple_uptime <- multiple_uptime[,c(1,3)]
contracts <- merge(contracts,multiple_uptime,by = "client_country",all.x = T)
contracts$multiple_uptime  <- ifelse(!is.na(contracts$multiple_uptime),1,0)
sum(contracts$multiple_uptime)
rm(multiple_uptime)


#multiple_vbr
multiple_vbr <- contracts %>% filter(portfolio == "VBR") %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
multiple_vbr <- as.data.frame(multiple_vbr)
multiple_vbr$multiple_vbr <- 1
multiple_vbr <- multiple_vbr[,c(1,3)]
contracts <- merge(contracts,multiple_vbr,by = "client_country",all.x = T)
contracts$multiple_vbr  <- ifelse(!is.na(contracts$multiple_vbr),1,0)
sum(contracts$multiple_vbr)
rm(multiple_vbr)

#multiple_overall
multiple_overall <- contracts %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
multiple_overall <- as.data.frame(multiple_overall)
multiple_overall$multiple_overall <- 1
multiple_overall <- multiple_overall[,c(1,3)]
contracts <- merge(contracts,multiple_overall,by = "client_country",all.x = T)
contracts$multiple_overall  <- ifelse(!is.na(contracts$multiple_overall),1,0)
sum(contracts$multiple_overall)
rm(multiple_overall)

#4 scoring intelligent columns
#same_client_country_manufacturer
#same_client_country_different_manufacturer
#same_client_manufacturer_different_country
#same_client_different_country_manufacture



scoring_columns <- function(data){
  start <- Sys.time()
  vbr <- data  %>% filter(portfolio == "VBR") %>% group_by(adjusted_client_domain_description,country,manufacturer_code,client_country_manufacturer) %>% summarise(count = n())
  vbr <- as.data.frame(vbr)
  vbr <- vbr[,1:4]
  data_uptime <- data %>% filter(portfolio != "VBR",merged_po == 1)
  data_uptime <- as.data.frame(data_uptime)
  same_client_country_manufacturer <- rep(0,nrow(vbr))
  same_client_country_different_manufacturer <- rep(0,nrow(vbr))
  same_client_manufacturer_different_country <- rep(0,nrow(vbr))
  same_client_different_country_manufacturer <- rep(0,nrow(vbr))
  for(i in 1:nrow(vbr)){
    print(i)
    country_vbr <- vbr[i,"country"]
    manufacturer_vbr <- vbr[i,"manufacturer_code"]
    data_uptime_client <- subset(data_uptime,adjusted_client_domain_description == vbr[i,"adjusted_client_domain_description"])
    data_uptime_client <- as.data.frame(data_uptime_client)
    if(nrow(data_uptime_client) > 0){
      for(j in 1:nrow(data_uptime_client)){
        country_uptime <- data_uptime_client[j,"country"]
        manufacturer_uptime <- data_uptime_client[j,"manufacturer"]
        if(country_vbr == country_uptime & manufacturer_vbr == manufacturer_uptime){
          same_client_country_manufacturer[i] <- 1
        }
        if(country_vbr == country_uptime & manufacturer_vbr != manufacturer_uptime){
          same_client_country_different_manufacturer[i] <- 1
        }
        if(country_vbr != country_uptime & manufacturer_vbr == manufacturer_uptime){
          same_client_manufacturer_different_country[i] <- 1
        }
        if(country_vbr != country_uptime & manufacturer_vbr != manufacturer_uptime){
          same_client_different_country_manufacturer[i] <- 1
        }
      }
    }
  }
  vbr$same_client_country_manufacturer <- same_client_country_manufacturer
  vbr$same_client_country_different_manufacturer <- same_client_country_different_manufacturer
  vbr$same_client_manufacturer_different_country <- same_client_manufacturer_different_country
  vbr$same_client_different_country_manufacturer <- same_client_different_country_manufacturer
  vbr <- vbr[,4:8]
  end <- Sys.time()
  time <- end-start
  print(time)
  return(vbr)
}


vbr <- scoring_columns(contracts)
contracts_vbr <- contracts %>% filter(portfolio == "VBR")
contracts_vbr <- merge(contracts_vbr,vbr,by = "client_country_manufacturer",all.x = T)
contracts_uptime <- contracts %>% filter(portfolio != "VBR")
contracts_uptime$same_client_country_manufacturer <- 0
contracts_uptime$same_client_country_different_manufacturer <- 0
contracts_uptime$same_client_manufacturer_different_country <- 0
contracts_uptime$same_client_different_country_manufacturer <- 0
contracts <- rbind(contracts_vbr,contracts_uptime)
rm(scoring_columns)
rm(vbr)
rm(contracts_uptime)
rm(contracts_vbr)

#client_country_both_1_uptime_2_vbr_3
client_country_both_1_uptime_2_vbr_3 <- contracts %>% group_by(client_country,portfolio) %>% summarise(tcv = sum(net_value_usd_x_calc,na.rm = T))
client_country_both_1_uptime_2_vbr_3 <- as.data.frame(client_country_both_1_uptime_2_vbr_3)
client_country_both_1_uptime_2_vbr_3 <- dcast(client_country_both_1_uptime_2_vbr_3,client_country~portfolio)
client_country_both_1_uptime_2_vbr_3$total <- rowSums(client_country_both_1_uptime_2_vbr_3[,c(2,3)], na.rm=TRUE)
client_country_both_1_uptime_2_vbr_3$VBR <- ifelse(client_country_both_1_uptime_2_vbr_3$VBR == 0,NA,client_country_both_1_uptime_2_vbr_3$VBR)
client_country_both_1_uptime_2_vbr_3$`Support Services` <- ifelse(client_country_both_1_uptime_2_vbr_3$`Support Services` == 0,NA,client_country_both_1_uptime_2_vbr_3$`Support Services`)
client_country_both_1_uptime_2_vbr_3$client_country_both_1_uptime_2_vbr_3 <- ifelse((!is.na(client_country_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_country_both_1_uptime_2_vbr_3$`Support Services`)),1,ifelse((is.na(client_country_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_country_both_1_uptime_2_vbr_3$`Support Services`)),2,ifelse((!is.na(client_country_both_1_uptime_2_vbr_3$VBR) &  is.na(client_country_both_1_uptime_2_vbr_3$`Support Services`)),3,4)))
client_country_both_1_uptime_2_vbr_3 <- client_country_both_1_uptime_2_vbr_3[,c(1,5)]
contracts <- merge(contracts,client_country_both_1_uptime_2_vbr_3,by = "client_country",all.x = T)
rm(client_country_both_1_uptime_2_vbr_3)

#client_both_1_uptime_2_vbr_3
client_both_1_uptime_2_vbr_3 <- contracts %>% group_by(adjusted_client_domain_description,portfolio) %>% summarise(tcv = sum(net_value_usd_x_calc,na.rm = T))
client_both_1_uptime_2_vbr_3 <- as.data.frame(client_both_1_uptime_2_vbr_3)
client_both_1_uptime_2_vbr_3 <- dcast(client_both_1_uptime_2_vbr_3,adjusted_client_domain_description~portfolio)
client_both_1_uptime_2_vbr_3$total <- rowSums(client_both_1_uptime_2_vbr_3[,c(2,3)], na.rm=TRUE)
client_both_1_uptime_2_vbr_3$VBR <- ifelse(client_both_1_uptime_2_vbr_3$VBR == 0,NA,client_both_1_uptime_2_vbr_3$VBR)
client_both_1_uptime_2_vbr_3$`Support Services` <- ifelse(client_both_1_uptime_2_vbr_3$`Support Services` == 0,NA,client_both_1_uptime_2_vbr_3$`Support Services`)
client_both_1_uptime_2_vbr_3$VBR <- ifelse(client_both_1_uptime_2_vbr_3$VBR == 0 ,NA,client_both_1_uptime_2_vbr_3$VBR)
client_both_1_uptime_2_vbr_3$`Support Services` <- ifelse(client_both_1_uptime_2_vbr_3$`Support Services` == 0 ,NA,client_both_1_uptime_2_vbr_3$`Support Services`)
client_both_1_uptime_2_vbr_3$client_both_1_uptime_2_vbr_3 <- ifelse((!is.na(client_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_both_1_uptime_2_vbr_3$`Support Services`)),1,ifelse((is.na(client_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_both_1_uptime_2_vbr_3$`Support Services`)),2,ifelse((!is.na(client_both_1_uptime_2_vbr_3$VBR) &  is.na(client_both_1_uptime_2_vbr_3$`Support Services`)),3,4)))
client_both_1_uptime_2_vbr_3 <- client_both_1_uptime_2_vbr_3[,c(1,5)]
contracts <- merge(contracts,client_both_1_uptime_2_vbr_3,by = "adjusted_client_domain_description",all.x = T)
rm(client_both_1_uptime_2_vbr_3)

#client_manufacturer_both_1_uptime_2_vbr_3 #blank to na
client_manufacturer_both_1_uptime_2_vbr_3 <- contracts %>% filter(!is.na(manufacturer)) %>% group_by(client_manufacturer,portfolio) %>% summarise(tcv = sum(net_value_usd_x_calc,na.rm = T))
client_manufacturer_both_1_uptime_2_vbr_3 <- as.data.frame(client_manufacturer_both_1_uptime_2_vbr_3)
client_manufacturer_both_1_uptime_2_vbr_3 <- dcast(client_manufacturer_both_1_uptime_2_vbr_3,client_manufacturer~portfolio)
client_manufacturer_both_1_uptime_2_vbr_3$total <- rowSums(client_manufacturer_both_1_uptime_2_vbr_3[,c(2,3)], na.rm=TRUE)
client_manufacturer_both_1_uptime_2_vbr_3$VBR <- ifelse(client_manufacturer_both_1_uptime_2_vbr_3$VBR == 0,NA,client_manufacturer_both_1_uptime_2_vbr_3$VBR)
client_manufacturer_both_1_uptime_2_vbr_3$`Support Services` <- ifelse(client_manufacturer_both_1_uptime_2_vbr_3$`Support Services` == 0,NA,client_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)
client_manufacturer_both_1_uptime_2_vbr_3$client_manufacturer_both_1_uptime_2_vbr_3 <- ifelse((!is.na(client_manufacturer_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),1,ifelse((is.na(client_manufacturer_both_1_uptime_2_vbr_3$VBR) &  !is.na(client_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),2,ifelse((!is.na(client_manufacturer_both_1_uptime_2_vbr_3$VBR) &  is.na(client_manufacturer_both_1_uptime_2_vbr_3$`Support Services`)),3,4)))
client_manufacturer_both_1_uptime_2_vbr_3 <- client_manufacturer_both_1_uptime_2_vbr_3[,c(1,5)]
contracts <- merge(contracts,client_manufacturer_both_1_uptime_2_vbr_3,by = "client_manufacturer",all.x = T)
contracts$client_manufacturer_both_1_uptime_2_vbr_3 <- ifelse(is.na(contracts$client_manufacturer_both_1_uptime_2_vbr_3),4,contracts$client_manufacturer_both_1_uptime_2_vbr_3)
rm(client_manufacturer_both_1_uptime_2_vbr_3)

#client_country_manufacturer_both_1_uptime_2_vbr_3 #blanks to na
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

#multiple_countries_only_vbr_both
multiple_countries <- contracts %>% group_by(adjusted_client_domain_description) %>% summarise(count = n_distinct(country)) %>% filter(count >1)
multiple_countries_only_vbr_both <- contracts %>% filter(adjusted_client_domain_description %in% multiple_countries$adjusted_client_domain_description) %>% group_by(adjusted_client_domain_description,client_country_both_1_uptime_2_vbr_3) %>% summarise(count = n_distinct(country))
multiple_countries_only_vbr_both <- as.data.frame(multiple_countries_only_vbr_both)
multiple_countries_only_vbr_both <- dcast(multiple_countries_only_vbr_both,adjusted_client_domain_description~client_country_both_1_uptime_2_vbr_3)
multiple_countries_only_vbr_both$multiple_countries_only_vbr_both <- ifelse((!is.na(multiple_countries_only_vbr_both$`1`) & !is.na(multiple_countries_only_vbr_both$`3`)),1,0)
multiple_countries_only_vbr_both <- multiple_countries_only_vbr_both[,c(1,6)]
contracts <- merge(contracts,multiple_countries_only_vbr_both,by = "adjusted_client_domain_description",all.x = T)
contracts$multiple_countries_only_vbr_both <- ifelse(is.na(contracts$multiple_countries_only_vbr_both),0,contracts$multiple_countries_only_vbr_both)
rm(multiple_countries)
rm(multiple_countries_only_vbr_both)

#client_country_multiple_manufacturer_both_vbr #blanks to na
multiple_manufacturers <- contracts %>% filter(!is.na(manufacturer)) %>% group_by(client_country) %>% summarise(count = n_distinct(manufacturer)) %>% filter(count >1)
client_country_multiple_manufacturer_both_vbr <- contracts %>% filter(client_country %in% multiple_manufacturers$client_country) %>% group_by(client_country,client_country_manufacturer_both_1_uptime_2_vbr_3) %>% summarise(count = n_distinct(manufacturer))
client_country_multiple_manufacturer_both_vbr <- as.data.frame(client_country_multiple_manufacturer_both_vbr)
client_country_multiple_manufacturer_both_vbr <- dcast(client_country_multiple_manufacturer_both_vbr,client_country~client_country_manufacturer_both_1_uptime_2_vbr_3)
client_country_multiple_manufacturer_both_vbr$client_country_multiple_manufacturer_both_vbr <- ifelse((!is.na(client_country_multiple_manufacturer_both_vbr$`1`) & !is.na(client_country_multiple_manufacturer_both_vbr$`3`)),1,0)
client_country_multiple_manufacturer_both_vbr <- client_country_multiple_manufacturer_both_vbr[,c(1,6)]
contracts <- merge(contracts,client_country_multiple_manufacturer_both_vbr,by = "client_country",all.x = T)
contracts$client_country_multiple_manufacturer_both_vbr <- ifelse(is.na(contracts$client_country_multiple_manufacturer_both_vbr),0,contracts$client_country_multiple_manufacturer_both_vbr)
rm(multiple_manufacturers)
rm(client_country_multiple_manufacturer_both_vbr)

#client_manufacturer_multiple_countries_both_vbr #blanks to na
multiple_countries <- contracts %>% filter(!is.na(manufacturer)) %>% group_by(client_manufacturer) %>% summarise(count = n_distinct(country)) %>% filter(count >1)
client_manufacturer_multiple_countries_both_vbr <- contracts %>% filter(client_manufacturer %in% multiple_countries$client_manufacturer) %>% group_by(client_manufacturer,client_country_manufacturer_both_1_uptime_2_vbr_3) %>% summarise(count = n_distinct(country))
client_manufacturer_multiple_countries_both_vbr <- as.data.frame(client_manufacturer_multiple_countries_both_vbr)
client_manufacturer_multiple_countries_both_vbr <- dcast(client_manufacturer_multiple_countries_both_vbr,client_manufacturer~client_country_manufacturer_both_1_uptime_2_vbr_3)
client_manufacturer_multiple_countries_both_vbr$client_manufacturer_multiple_countries_both_vbr <- ifelse((!is.na(client_manufacturer_multiple_countries_both_vbr$`1`) & !is.na(client_manufacturer_multiple_countries_both_vbr$`3`)),1,0)
client_manufacturer_multiple_countries_both_vbr <- client_manufacturer_multiple_countries_both_vbr[,c(1,6)]
contracts <- merge(contracts,client_manufacturer_multiple_countries_both_vbr,by = "client_manufacturer",all.x = T)
contracts$client_manufacturer_multiple_countries_both_vbr <- ifelse(is.na(contracts$client_manufacturer_multiple_countries_both_vbr),0,contracts$client_manufacturer_multiple_countries_both_vbr)
rm(multiple_countries)
rm(client_manufacturer_multiple_countries_both_vbr)

write.csv(contracts,'C:\\Users\\gramener\\Desktop\\VBR to Uptime dashboard\\Contracts_merged_PO.csv')