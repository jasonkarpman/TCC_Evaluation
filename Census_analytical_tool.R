##################################################
#Title: Census analytical tool
#Project: TCC Evaluation
#Author: Bo Liu 
#Organization: UCLA Luskin Center for Innovation
#Website: innovation.luskin.ucla.edu
#Purpose: The purpose of this code is to pull American Community Survey (ACS) 5-year data 
         #using the United States Census Bureau's Application Programming Interface (API), 
         #and to analyze that data for statistically significant trends over time 
         #in four different geographies: (1) aggregated TCC census tracts; 
         #(2) aggregated control census tracts; (3) counties with TCC sites; 
         #(4) California. More information about variables included in this code 
         #can be found in the annual TCC progress reports posted here: https://innovation.luskin.ucla.edu/tracking-groundbreaking-climate-action/
##################################################

#Load packages
library(tidyverse)
library(tidycensus)
library(reshape2)

#Set working directory
setwd("...")

#Load your API Key
##For first time use, request a key via: https://api.census.gov/data/key_signup.html 
#Run the next line with your key in "..."
census_api_key("...", install = TRUE)

#Define TCC and control tracts, counties, and the time frame
Fresno_TCC <- c("06019000200",
                "06019000100",
                "06019000400",
                "06019000700",
                "06019000600",
                "06019001000",
                "06019001100",
                "06019000902",
                "06019000901",
                "06019000300")

Fresno_Control <- c("06019001202",
                    "06019004704",
                    "06019005100",
                    "06019003202",
                    "06019002800",
                    "06019005403",
                    "06019001407",
                    "06019001304",
                    "06019004802",
                    "06019003807")

Ontario_TCC <- c("06071001600", 
                 "06071001702", 
                 "06071001400", 
                 "06071001501", 
                 "06071001503", 
                 "06071001504", 
                 "06071001707", 
                 "06071001706", 
                 "06071001813", 
                 "06071001812")

Ontario_Control <- c("06071002204",
                     "06071004700",
                     "06071006302",
                     "06071006700",
                     "06071007000",
                     "06071004604",
                     "06071002804",
                     "06071002602",
                     "06071000201",
                     "06071000303",
                     "06071003401",
                     "06071002402",
                     "06071002401",
                     "06071002501",
                     "06071002902",
                     "06071003200",
                     "06071000904",
                     "06071001104",
                     "06071001001",
                     "06071000207",
                     "06071000603",
                     "06071001305",
                     "06071003509",
                     "06071003102",
                     "06071003301",
                     "06071003101",
                     "06071003302",
                     "06071003607",
                     "06071003803",
                     "06071006604")

Watts_TCC <- c("06037242600",
               "06037242700",
               "06037243000",
               "06037242100",
               "06037242200",
               "06037243100",
               "06037242000",
               "06037242300",
               "06037240800",
               "06037240900",
               "06037241001")

Watts_Control <- c("06037239601",
                   "06037219901",
                   "06037232120",
                   "06037541604",
                   "06037535102",
                   "06037540901",
                   "06037221500",
                   "06037237720",
                   "06037238310",
                   "06037238320",
                   "06037237710",
                   "06037241120",
                   "06037241400",
                   "06037231100",
                   "06037231210",
                   "06037231300",
                   "06037231600",
                   "06037231710",
                   "06037240010",
                   "06037240500",
                   "06037237500",
                   "06037232500",
                   "06037232700",
                   "06037240600",
                   "06037237101",
                   "06037237202",
                   "06037237401",
                   "06037239202",
                   "06037239501",
                   "06037239602",
                   "06037600304",
                   "06037241202",
                   "06037239802",
                   "06037239801",
                   "06037228500",
                   "06037231720",
                   "06037237102",
                   "06037240401")

Pacoima_TCC <- c("06037104500",
                 "06037104310",
                 "06037104320",
                 "06037104701",
                 "06037104810",
                 "06037104610",
                 "06037104620",
                 "06037104404",
                 "06037104703",
                 "06037104821",
                 "06037121210",
                 "06037104704",
                 "06037121222",
                 "06037104401")

Pacoima_Control <- c("06037201504",
                     "06037122121",
                     "06037123206",
                     "06037120010",
                     "06037201602",
                     "06037120105",
                     "06037204120",
                     "06037187200",
                     "06037185320",
                     "06037199202",
                     "06037121801",
                     "06037185310",
                     "06037203710",
                     "06037203900",
                     "06037221210",
                     "06037204910",
                     "06037185203",
                     "06037122200",
                     "06037134001",
                     "06037218210",
                     "06037201301",
                     "06037115302",
                     "06037122420",
                     "06037122120",
                     "06037120108",
                     "06037119320",
                     "06037117201",
                     "06037204300",
                     "06037106604",
                     "06037203200",
                     "06037201501",
                     "06037204810",
                     "06037204700",
                     "06037106114",
                     "06037127400")

Counties <- c("Fresno County", "Los Angeles County", "San Bernardino County")

years <- 2013:2018

#create a dataframe that include all selected tracts
n <- max(length(Fresno_TCC), length(Fresno_Control),
         length(Ontario_TCC), length(Ontario_Control),
         length(Watts_TCC), length(Watts_Control),
         length(Pacoima_TCC), length(Pacoima_Control))

length(Fresno_TCC) <- n                      
length(Fresno_Control) <- n
length(Ontario_TCC) <- n                      
length(Ontario_Control) <- n
length(Watts_TCC) <- n                      
length(Watts_Control) <- n
length(Pacoima_TCC) <- n 
length(Pacoima_Control) <- n

Tracts_wide <- do.call("cbind", list(data.frame(Fresno_TCC), data.frame(Fresno_Control),
                                     data.frame(Ontario_TCC), data.frame(Ontario_Control),
                                     data.frame(Watts_TCC), data.frame(Watts_Control),
                                     data.frame(Pacoima_TCC), data.frame(Pacoima_Control)))

Tracts <- na.omit(melt(Tracts_wide, 
                       measure.vars = c("Fresno_TCC", "Fresno_Control", 
                                        "Ontario_TCC", "Ontario_Control",
                                        "Watts_TCC", "Watts_Control",
                                        "Pacoima_TCC", "Pacoima_Control"),
                       variable.name="group",
                       value.name="GEOID"))

###
#Variable Group 1 - demograpphic indicators
###

#define variables needed and rename variables 
Vars1 <- c("B03002_001E", "B03002_001M",
           "B03002_012E", "B03002_012M",
           "B03002_003E", "B03002_003M",
           "B03002_004E", "B03002_004M",
           "B03002_006E", "B03002_006M",
           "B03002_007E", "B03002_007M",
           "B03002_005E", "B03002_005M",
           "B03002_008E", "B03002_008M",
           "B03002_009E", "B03002_009M",
           "B05006_001E", "B05006_001M",
           "B05006_047E", "B05006_047M",
           "B05006_091E", "B05006_091M",
           "B05006_124E", "B05006_124M")

#Extract data for selected variables
#create a loop for multiple-year data
demlist <- list()
demlist1 <- list()
demlist2 <- list()

for (i in years){

#census tract level 
Pop_i <- get_acs(geography = "tract", 
                 variables = Vars1, 
                 state = "CA", 
                 year = i)

#subset dataframe for selected tracts
Pop_i_input <- subset(Pop_i, Pop_i$GEOID %in% Tracts$GEOID)
Pop_i_input <- merge(Pop_i_input, Tracts)
colnames(Pop_i_input)[3] <- "var"
Pop_i_input1 <- subset(Pop_i_input, Pop_i_input$var %in% c("B03002_004","B03002_005",
                                                           "B03002_006","B03002_007",
                                                           "B03002_008","B03002_009")) %>%
                    mutate(var = gsub("B03002_004", "NonHisp_color_all", var)) %>%
                    mutate(var = gsub("B03002_005", "NonHisp_color_all", var)) %>%
                    mutate(var = gsub("B03002_006", "NonHisp_color_all", var)) %>%
                    mutate(var = gsub("B03002_007", "NonHisp_color_all", var)) %>%
                    mutate(var = gsub("B03002_008", "NonHisp_color_all", var)) %>%
                    mutate(var = gsub("B03002_009", "NonHisp_color_all", var)) 
Pop_i_input2 <- subset(Pop_i_input, Pop_i_input$var %in% c("B03002_005","B03002_007",
                                                           "B03002_008","B03002_009")) %>%
                    mutate(var = gsub("B03002_005", "NonHisp_color_other", var)) %>%
                    mutate(var = gsub("B03002_007", "NonHisp_color_other", var)) %>%
                    mutate(var = gsub("B03002_008", "NonHisp_color_other", var)) %>%
                    mutate(var = gsub("B03002_009", "NonHisp_color_other", var)) 
Pop_i_input3 <- rbind(Pop_i_input, Pop_i_input1, Pop_i_input2)

#estimate aggregate results
Pop_i_output1 <- Pop_i_input3 %>%
                   group_by(group,var) %>%
                   summarize(sumest = sum(estimate), 
                   summoe = moe_sum(moe, estimate))
Pop_i_output2 <- dcast(melt(Pop_i_output1, id.vars=1:2), 
                       group ~ var + variable, fun.aggregate = sum)
Pop_i_output3 <- Pop_i_output2
Pop_i_output3$Hisp_per <- Pop_i_output3$B03002_012_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$Hisp_per_moe <- moe_prop(Pop_i_output3$B03002_012_sumest, 
                                       Pop_i_output3$B03002_001_sumest,
                                       Pop_i_output3$B03002_012_summoe, 
                                       Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_whit_per <- Pop_i_output3$B03002_003_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_whit_per_moe <- moe_prop(Pop_i_output3$B03002_003_sumest, 
                                               Pop_i_output3$B03002_001_sumest,
                                               Pop_i_output3$B03002_003_summoe, 
                                               Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_color_all_per <- Pop_i_output3$NonHisp_color_all_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_color_all_per_moe <- moe_prop(Pop_i_output3$NonHisp_color_all_sumest, 
                                                Pop_i_output3$B03002_001_sumest,
                                                Pop_i_output3$NonHisp_color_all_summoe, 
                                                Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_color_other_per <- Pop_i_output3$NonHisp_color_other_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_color_other_per_moe <- moe_prop(Pop_i_output3$NonHisp_color_other_sumest, 
                                                    Pop_i_output3$B03002_001_sumest,
                                                    Pop_i_output3$NonHisp_color_other_summoe, 
                                                    Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_blac_per <- Pop_i_output3$B03002_004_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_blac_per_moe <- moe_prop(Pop_i_output3$B03002_004_sumest, 
                                               Pop_i_output3$B03002_001_sumest,
                                               Pop_i_output3$B03002_004_summoe, 
                                               Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_asia_per <- Pop_i_output3$B03002_006_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_asia_per_moe <- moe_prop(Pop_i_output3$B03002_006_sumest, 
                                               Pop_i_output3$B03002_001_sumest,
                                               Pop_i_output3$B03002_006_summoe, 
                                               Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_PI_per <- Pop_i_output3$B03002_007_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_PI_per_moe <- moe_prop(Pop_i_output3$B03002_007_sumest, 
                                             Pop_i_output3$B03002_001_sumest,
                                             Pop_i_output3$B03002_007_summoe, 
                                             Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_AI_per <- Pop_i_output3$B03002_005_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_AI_per_moe <- moe_prop(Pop_i_output3$B03002_005_sumest, 
                                             Pop_i_output3$B03002_001_sumest,
                                             Pop_i_output3$B03002_005_summoe, 
                                             Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_two_per <- Pop_i_output3$B03002_009_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_two_per_moe <- moe_prop(Pop_i_output3$B03002_009_sumest, 
                                                Pop_i_output3$B03002_001_sumest,
                                                Pop_i_output3$B03002_009_summoe, 
                                                Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$NonHisp_other_per <- Pop_i_output3$B03002_008_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$NonHisp_other_per_moe <- moe_prop(Pop_i_output3$B03002_008_sumest, 
                                                Pop_i_output3$B03002_001_sumest,
                                                Pop_i_output3$B03002_008_summoe, 
                                                Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$bornFor_per <- Pop_i_output3$B05006_001_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$bornFor_per_moe <- moe_prop(Pop_i_output3$B05006_001_sumest, 
                                         Pop_i_output3$B03002_001_sumest,
                                         Pop_i_output3$B05006_001_summoe, 
                                         Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$bornAs_per <- Pop_i_output3$B05006_047_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$bornAs_per_moe <- moe_prop(Pop_i_output3$B05006_047_sumest, 
                                         Pop_i_output3$B03002_001_sumest,
                                         Pop_i_output3$B05006_047_summoe, 
                                         Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$bornAf_per <- Pop_i_output3$B05006_091_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$bornAf_per_moe <- moe_prop(Pop_i_output3$B05006_091_sumest, 
                                         Pop_i_output3$B03002_001_sumest,
                                         Pop_i_output3$B05006_091_summoe, 
                                         Pop_i_output3$B03002_001_summoe)*100
Pop_i_output3$bornLA_per <- Pop_i_output3$B05006_124_sumest / Pop_i_output3$B03002_001_sumest * 100
Pop_i_output3$bornLA_per_moe <- moe_prop(Pop_i_output3$B05006_124_sumest, 
                                         Pop_i_output3$B03002_001_sumest,
                                         Pop_i_output3$B05006_124_summoe, 
                                         Pop_i_output3$B03002_001_summoe)*100
Pop_i_output4 <- Pop_i_output3[c("group",
                                 "B03002_001_sumest", "B03002_001_summoe",
                                 "Hisp_per", "Hisp_per_moe",
                                 "NonHisp_whit_per", "NonHisp_whit_per_moe",
                                 "NonHisp_color_all_per", "NonHisp_color_all_per_moe",
                                 "NonHisp_color_other_per", "NonHisp_color_other_per_moe",
                                 "NonHisp_blac_per", "NonHisp_blac_per_moe",
                                 "NonHisp_asia_per", "NonHisp_asia_per_moe",
                                 "NonHisp_PI_per", "NonHisp_PI_per_moe",
                                 "NonHisp_AI_per", "NonHisp_AI_per_moe",
                                 "NonHisp_two_per", "NonHisp_two_per_moe",
                                 "NonHisp_other_per", "NonHisp_other_per_moe",
                                 "bornFor_per", "bornFor_per_moe",
                                 "bornAs_per", "bornAs_per_moe",
                                 "bornAf_per", "bornAf_per_moe",
                                 "bornLA_per", "bornLA_per_moe")]
names(Pop_i_output4)[names(Pop_i_output4) %in%  c("B03002_001_sumest", "B03002_001_summoe")] <- c("Pop_tot", "Pop_tot_moe")
Pop_i_output5 <- melt(Pop_i_output4, id.vars="group")
Pop_i_output5$year <- i
demlist[[i]] <- Pop_i_output5

#county level
Pop_i_county <- get_acs(geography = "county", 
                        variables = Vars1, 
                        state = "CA", 
                        year = i)
Pop_i_county_input <- Pop_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Pop_i_county_input)[1:2] <- c("group", "var")
Pop_i_county_input1 <- subset(Pop_i_county_input, 
                               Pop_i_county_input$group %in% Counties)

#missing data means no sampling error, should be converted to 0
#see Page 11 in: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofDatai.pdf? 
Pop_i_county_input1$moe[is.na(Pop_i_county_input1$moe)] <- 0
Pop_i_county_input2 <- subset(Pop_i_county_input1, Pop_i_county_input1$var %in% c("B03002_004","B03002_005",
                                                                                  "B03002_006","B03002_007",
                                                                                  "B03002_008","B03002_009")) %>%
                            mutate(var = gsub("B03002_004", "NonHisp_color_all", var)) %>%
                            mutate(var = gsub("B03002_005", "NonHisp_color_all", var)) %>%
                            mutate(var = gsub("B03002_006", "NonHisp_color_all", var)) %>%
                            mutate(var = gsub("B03002_007", "NonHisp_color_all", var)) %>%
                            mutate(var = gsub("B03002_008", "NonHisp_color_all", var)) %>%
                            mutate(var = gsub("B03002_009", "NonHisp_color_all", var)) 
Pop_i_county_input3 <- subset(Pop_i_county_input1, Pop_i_county_input1$var %in% c("B03002_005","B03002_007",
                                                                                  "B03002_008","B03002_009")) %>%
                            mutate(var = gsub("B03002_005", "NonHisp_color_other", var)) %>%
                            mutate(var = gsub("B03002_007", "NonHisp_color_other", var)) %>%
                            mutate(var = gsub("B03002_008", "NonHisp_color_other", var)) %>%
                            mutate(var = gsub("B03002_009", "NonHisp_color_other", var)) 

Pop_i_county_input4 <- rbind(Pop_i_county_input1, Pop_i_county_input2, Pop_i_county_input3)
Pop_i_county_output1 <- Pop_i_county_input4 %>%
                          group_by(group,var) %>%
                          summarize(sumest = sum(estimate), 
                          summoe = moe_sum(moe, estimate))
Pop_i_county_output2 <- dcast(melt(Pop_i_county_output1, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Pop_i_county_output3 <- Pop_i_county_output2
Pop_i_county_output3$Hisp_per <- Pop_i_county_output3$B03002_012_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$Hisp_per_moe <- moe_prop(Pop_i_county_output3$B03002_012_sumest, 
                                              Pop_i_county_output3$B03002_001_sumest,
                                              Pop_i_county_output3$B03002_012_summoe, 
                                              Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_whit_per <- Pop_i_county_output3$B03002_003_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_whit_per_moe <- moe_prop(Pop_i_county_output3$B03002_003_sumest, 
                                                      Pop_i_county_output3$B03002_001_sumest,
                                                      Pop_i_county_output3$B03002_003_summoe, 
                                                      Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_color_all_per <- Pop_i_county_output3$NonHisp_color_all_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_color_all_per_moe <- moe_prop(Pop_i_county_output3$NonHisp_color_all_sumest, 
                                                       Pop_i_county_output3$B03002_001_sumest,
                                                       Pop_i_county_output3$NonHisp_color_all_summoe, 
                                                       Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_color_other_per <- Pop_i_county_output3$NonHisp_color_other_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_color_other_per_moe <- moe_prop(Pop_i_county_output3$NonHisp_color_other_sumest, 
                                                           Pop_i_county_output3$B03002_001_sumest,
                                                           Pop_i_county_output3$NonHisp_color_other_summoe, 
                                                           Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_blac_per <- Pop_i_county_output3$B03002_004_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_blac_per_moe <- moe_prop(Pop_i_county_output3$B03002_004_sumest, 
                                                      Pop_i_county_output3$B03002_001_sumest,
                                                      Pop_i_county_output3$B03002_004_summoe, 
                                                      Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_asia_per <- Pop_i_county_output3$B03002_006_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_asia_per_moe <- moe_prop(Pop_i_county_output3$B03002_006_sumest, 
                                                      Pop_i_county_output3$B03002_001_sumest,
                                                      Pop_i_county_output3$B03002_006_summoe, 
                                                      Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_PI_per <- Pop_i_county_output3$B03002_007_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_PI_per_moe <- moe_prop(Pop_i_county_output3$B03002_007_sumest, 
                                                    Pop_i_county_output3$B03002_001_sumest,
                                                    Pop_i_county_output3$B03002_007_summoe, 
                                                    Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_AI_per <- Pop_i_county_output3$B03002_005_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_AI_per_moe <- moe_prop(Pop_i_county_output3$B03002_005_sumest, 
                                                    Pop_i_county_output3$B03002_001_sumest,
                                                    Pop_i_county_output3$B03002_005_summoe, 
                                                    Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_two_per <- Pop_i_county_output3$B03002_009_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_two_per_moe <- moe_prop(Pop_i_county_output3$B03002_009_sumest, 
                                                       Pop_i_county_output3$B03002_001_sumest,
                                                       Pop_i_county_output3$B03002_009_summoe, 
                                                       Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$NonHisp_other_per <- Pop_i_county_output3$B03002_008_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$NonHisp_other_per_moe <- moe_prop(Pop_i_county_output3$B03002_008_sumest, 
                                                       Pop_i_county_output3$B03002_001_sumest,
                                                       Pop_i_county_output3$B03002_008_summoe, 
                                                       Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$bornFor_per <- Pop_i_county_output3$B05006_001_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$bornFor_per_moe <- moe_prop(Pop_i_county_output3$B05006_001_sumest, 
                                                Pop_i_county_output3$B03002_001_sumest,
                                                Pop_i_county_output3$B05006_001_summoe, 
                                                Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$bornAs_per <- Pop_i_county_output3$B05006_047_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$bornAs_per_moe <- moe_prop(Pop_i_county_output3$B05006_047_sumest, 
                                                Pop_i_county_output3$B03002_001_sumest,
                                                Pop_i_county_output3$B05006_047_summoe, 
                                                Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$bornAf_per <- Pop_i_county_output3$B05006_091_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$bornAf_per_moe <- moe_prop(Pop_i_county_output3$B05006_091_sumest, 
                                                Pop_i_county_output3$B03002_001_sumest,
                                                Pop_i_county_output3$B05006_091_summoe, 
                                                Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output3$bornLA_per <- Pop_i_county_output3$B05006_124_sumest / Pop_i_county_output3$B03002_001_sumest * 100
Pop_i_county_output3$bornLA_per_moe <- moe_prop(Pop_i_county_output3$B05006_124_sumest, 
                                                Pop_i_county_output3$B03002_001_sumest,
                                                Pop_i_county_output3$B05006_124_summoe, 
                                                Pop_i_county_output3$B03002_001_summoe)*100
Pop_i_county_output4 <- Pop_i_county_output3[c("group",
                                               "B03002_001_sumest", "B03002_001_summoe",
                                               "Hisp_per", "Hisp_per_moe",
                                               "NonHisp_whit_per", "NonHisp_whit_per_moe",
                                               "NonHisp_color_all_per", "NonHisp_color_all_per_moe",
                                               "NonHisp_color_other_per", "NonHisp_color_other_per_moe",
                                               "NonHisp_blac_per", "NonHisp_blac_per_moe",
                                               "NonHisp_asia_per", "NonHisp_asia_per_moe",
                                               "NonHisp_PI_per", "NonHisp_PI_per_moe",
                                               "NonHisp_AI_per", "NonHisp_AI_per_moe",
                                               "NonHisp_two_per", "NonHisp_two_per_moe",
                                               "NonHisp_other_per", "NonHisp_other_per_moe",
                                               "bornFor_per", "bornFor_per_moe",
                                               "bornAs_per", "bornAs_per_moe",
                                               "bornAf_per", "bornAf_per_moe",
                                               "bornLA_per", "bornLA_per_moe")]
names(Pop_i_county_output4)[names(Pop_i_county_output4) %in%  c("B03002_001_sumest", "B03002_001_summoe")] <- c("Pop_tot", "Pop_tot_moe")
Pop_i_county_output5 <- melt(Pop_i_county_output4, id.vars="group")
Pop_i_county_output5$year <- i
demlist1[[i]] <- Pop_i_county_output5

#state level
Pop_i_state <- get_acs(geography = "state", 
                       variables = Vars1, 
                       state = "CA", 
                       year = i)
Pop_i_state_input1 <- Pop_i_state[-1]
colnames(Pop_i_state_input1)[1:2] <- c("group", "var")

#missing data means no sampling error, should be converted to 0
#see Page 11 in: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofDatai.pdf? 
Pop_i_state_input1$moe[is.na(Pop_i_state_input1$moe)] <- 0
Pop_i_state_input2 <- subset(Pop_i_state_input1, Pop_i_state_input1$var %in% c("B03002_004","B03002_005",
                                                                                  "B03002_006","B03002_007",
                                                                                  "B03002_008","B03002_009")) %>%
                          mutate(var = gsub("B03002_004", "NonHisp_color_all", var)) %>%
                          mutate(var = gsub("B03002_005", "NonHisp_color_all", var)) %>%
                          mutate(var = gsub("B03002_006", "NonHisp_color_all", var)) %>%
                          mutate(var = gsub("B03002_007", "NonHisp_color_all", var)) %>%
                          mutate(var = gsub("B03002_008", "NonHisp_color_all", var)) %>%
                          mutate(var = gsub("B03002_009", "NonHisp_color_all", var)) 
Pop_i_state_input3 <- subset(Pop_i_state_input1, Pop_i_state_input1$var %in% c("B03002_005","B03002_007",
                                                                                  "B03002_008","B03002_009")) %>%
                          mutate(var = gsub("B03002_005", "NonHisp_color_other", var)) %>%
                          mutate(var = gsub("B03002_007", "NonHisp_color_other", var)) %>%
                          mutate(var = gsub("B03002_008", "NonHisp_color_other", var)) %>%
                          mutate(var = gsub("B03002_009", "NonHisp_color_other", var)) 
Pop_i_state_input4 <- rbind(Pop_i_state_input1, Pop_i_state_input2, Pop_i_state_input3)
Pop_i_state_output1 <- Pop_i_state_input4 %>%
                           group_by(group,var) %>%
                               summarize(sumest = sum(estimate), 
                                         summoe = moe_sum(moe, estimate))
Pop_i_state_output2 <- dcast(melt(Pop_i_state_output1, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Pop_i_state_output3 <- Pop_i_state_output2
Pop_i_state_output3$Hisp_per <- Pop_i_state_output3$B03002_012_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$Hisp_per_moe <- moe_prop(Pop_i_state_output3$B03002_012_sumest, 
                                              Pop_i_state_output3$B03002_001_sumest,
                                              Pop_i_state_output3$B03002_012_summoe, 
                                              Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_whit_per <- Pop_i_state_output3$B03002_003_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_whit_per_moe <- moe_prop(Pop_i_state_output3$B03002_003_sumest, 
                                                      Pop_i_state_output3$B03002_001_sumest,
                                                      Pop_i_state_output3$B03002_003_summoe, 
                                                      Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_color_all_per <- Pop_i_state_output3$NonHisp_color_all_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_color_all_per_moe <- moe_prop(Pop_i_state_output3$NonHisp_color_all_sumest, 
                                                           Pop_i_state_output3$B03002_001_sumest,
                                                           Pop_i_state_output3$NonHisp_color_all_summoe, 
                                                           Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_color_other_per <- Pop_i_state_output3$NonHisp_color_other_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_color_other_per_moe <- moe_prop(Pop_i_state_output3$NonHisp_color_other_sumest, 
                                                             Pop_i_state_output3$B03002_001_sumest,
                                                             Pop_i_state_output3$NonHisp_color_other_summoe, 
                                                             Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_blac_per <- Pop_i_state_output3$B03002_004_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_blac_per_moe <- moe_prop(Pop_i_state_output3$B03002_004_sumest, 
                                                      Pop_i_state_output3$B03002_001_sumest,
                                                      Pop_i_state_output3$B03002_004_summoe, 
                                                      Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_asia_per <- Pop_i_state_output3$B03002_006_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_asia_per_moe <- moe_prop(Pop_i_state_output3$B03002_006_sumest, 
                                                      Pop_i_state_output3$B03002_001_sumest,
                                                      Pop_i_state_output3$B03002_006_summoe, 
                                                      Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_PI_per <- Pop_i_state_output3$B03002_007_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_PI_per_moe <- moe_prop(Pop_i_state_output3$B03002_007_sumest, 
                                                    Pop_i_state_output3$B03002_001_sumest,
                                                    Pop_i_state_output3$B03002_007_summoe, 
                                                    Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_AI_per <- Pop_i_state_output3$B03002_005_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_AI_per_moe <- moe_prop(Pop_i_state_output3$B03002_005_sumest, 
                                                    Pop_i_state_output3$B03002_001_sumest,
                                                    Pop_i_state_output3$B03002_005_summoe, 
                                                    Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_two_per <- Pop_i_state_output3$B03002_009_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_two_per_moe <- moe_prop(Pop_i_state_output3$B03002_009_sumest, 
                                                     Pop_i_state_output3$B03002_001_sumest,
                                                     Pop_i_state_output3$B03002_009_summoe, 
                                                     Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$NonHisp_other_per <- Pop_i_state_output3$B03002_008_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$NonHisp_other_per_moe <- moe_prop(Pop_i_state_output3$B03002_008_sumest, 
                                                       Pop_i_state_output3$B03002_001_sumest,
                                                       Pop_i_state_output3$B03002_008_summoe, 
                                                       Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$bornFor_per <- Pop_i_state_output3$B05006_001_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$bornFor_per_moe <- moe_prop(Pop_i_state_output3$B05006_001_sumest, 
                                                 Pop_i_state_output3$B03002_001_sumest,
                                                 Pop_i_state_output3$B05006_001_summoe, 
                                                 Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$bornAs_per <- Pop_i_state_output3$B05006_047_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$bornAs_per_moe <- moe_prop(Pop_i_state_output3$B05006_047_sumest, 
                                                Pop_i_state_output3$B03002_001_sumest,
                                                Pop_i_state_output3$B05006_047_summoe, 
                                                Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$bornAf_per <- Pop_i_state_output3$B05006_091_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$bornAf_per_moe <- moe_prop(Pop_i_state_output3$B05006_091_sumest, 
                                                Pop_i_state_output3$B03002_001_sumest,
                                                Pop_i_state_output3$B05006_091_summoe, 
                                                Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output3$bornLA_per <- Pop_i_state_output3$B05006_124_sumest / Pop_i_state_output3$B03002_001_sumest * 100
Pop_i_state_output3$bornLA_per_moe <- moe_prop(Pop_i_state_output3$B05006_124_sumest, 
                                                Pop_i_state_output3$B03002_001_sumest,
                                                Pop_i_state_output3$B05006_124_summoe, 
                                                Pop_i_state_output3$B03002_001_summoe)*100
Pop_i_state_output4 <- Pop_i_state_output3[c("group",
                                               "B03002_001_sumest", "B03002_001_summoe",
                                               "Hisp_per", "Hisp_per_moe",
                                               "NonHisp_whit_per", "NonHisp_whit_per_moe",
                                               "NonHisp_color_all_per", "NonHisp_color_all_per_moe",
                                               "NonHisp_color_other_per", "NonHisp_color_other_per_moe",
                                               "NonHisp_blac_per", "NonHisp_blac_per_moe",
                                               "NonHisp_asia_per", "NonHisp_asia_per_moe",
                                               "NonHisp_PI_per", "NonHisp_PI_per_moe",
                                               "NonHisp_AI_per", "NonHisp_AI_per_moe",
                                               "NonHisp_two_per", "NonHisp_two_per_moe",
                                               "NonHisp_other_per", "NonHisp_other_per_moe",
                                               "bornFor_per", "bornFor_per_moe",
                                               "bornAs_per", "bornAs_per_moe",
                                               "bornAf_per", "bornAf_per_moe",
                                               "bornLA_per", "bornLA_per_moe")]
names(Pop_i_state_output4)[names(Pop_i_state_output4) %in%  c("B03002_001_sumest", "B03002_001_summoe")] <- c("Pop_tot", "Pop_tot_moe")
Pop_i_state_output5 <- melt(Pop_i_state_output4, id.vars="group")
Pop_i_state_output5$year <- i
demlist2[[i]] <- Pop_i_state_output5

}

#Combine results from all three geographical scales
Dem_results <- rbind(do.call(rbind, demlist), do.call(rbind, demlist1), do.call(rbind, demlist2))

###
#Variable Group 2 - economic indicators
###

#Subgroup 1: median household income & high-income share
#define variables needed
Vars2_1 <- c("B19001_001E", "B19001_001M",
             "B19001_002E", "B19001_002M",
             "B19001_003E", "B19001_003M",
             "B19001_004E", "B19001_004M",
             "B19001_005E", "B19001_005M",
             "B19001_006E", "B19001_006M",
             "B19001_007E", "B19001_007M",
             "B19001_008E", "B19001_008M",
             "B19001_009E", "B19001_009M",
             "B19001_010E", "B19001_010M",
             "B19001_011E", "B19001_011M",
             "B19001_012E", "B19001_012M",
             "B19001_013E", "B19001_013M",
             "B19001_014E", "B19001_014M",
             "B19001_015E", "B19001_015M",
             "B19001_016E", "B19001_016M",
             "B19001_017E", "B19001_017M")

#defining income brackets
Inc_brac <- data.frame(c("B19001_002", "B19001_003", "B19001_004", "B19001_005", 
                         "B19001_006", "B19001_007", "B19001_008", "B19001_009", 
                         "B19001_010", "B19001_011", "B19001_012", "B19001_013",
                         "B19001_014", "B19001_015", "B19001_016", "B19001_017"),
                       c(0, 10000, 15000, 20000,
                         25000, 30000, 35000, 40000, 
                         45000, 50000, 60000, 75000,
                         100000, 125000, 150000, 200000),
                       c(9999, 14999, 19999, 24999,
                         29999, 34999, 39999, 44999,
                         49999, 59999, 74999, 99999,
                         124999, 149999, 199999, 9999999))
colnames(Inc_brac) <- c("var", "lower", "upper")

#Extract data 
#create a loop for multiple-year data
inclist <- list()
inclist1 <- list()
inclist2 <- list()

incplist <- list()
incplist1 <- list()
incplist2 <- list()

for (i in years){

#census tract level 
Econ_inc_i <- get_acs(geography = "tract", 
                         variables = Vars2_1, 
                         state = "CA", 
                         year = i)

#subset dataframe for selected tracts
Econ_inc_i_input <- subset(Econ_inc_i, Econ_inc_i$GEOID %in% Tracts$GEOID)
Econ_inc_i_input <- merge(Econ_inc_i_input, Tracts)
colnames(Econ_inc_i_input)[3] <- "var"
Econ_inc_i_input1 <- subset(Econ_inc_i_input, Econ_inc_i_input$var != "B19001_001")
Econ_inc_i_input2 <- subset(Econ_inc_i_input, Econ_inc_i_input$var %in% c("B19001_001", "B19001_015", "B19001_016", "B19001_017"))
Econ_inc_i_input2 <- Econ_inc_i_input2 %>% 
                         mutate(var = gsub("B19001_001", "households_total", var)) %>%
                         mutate(var = gsub("B19001_015", "households_125kplus", var)) %>%
                         mutate(var = gsub("B19001_016", "households_125kplus", var)) %>%
                         mutate(var = gsub("B19001_017", "households_125kplus", var)) 
  
#estimate aggregate results - median household income
Econ_inc_i_output1 <- Econ_inc_i_input1 %>%
                          group_by(group,var) %>%
                               summarize(sumest = sum(estimate), 
                                         summoe = moe_sum(moe, estimate))
Econ_inc_i_output2 <- merge(Econ_inc_i_output1, Inc_brac)
Econ_inc_i_output2$accsum <- ave(Econ_inc_i_output2$sumest, Econ_inc_i_output2$group, FUN = cumsum)
Econ_inc_i_output2 <- Econ_inc_i_output2 %>%
                            group_by(group) %>%
                              mutate(share = accsum / max(accsum) * 100)
Econ_inc_i_output3_1 <- Econ_inc_i_output2 %>%
                              group_by(group) %>%
                                 filter(share <= 50) %>%
                                    filter(accsum == max(accsum))
Econ_inc_i_output3_2 <- Econ_inc_i_output2 %>%
                              group_by(group) %>%
                                 filter(share >= 50) %>%
                                    filter(accsum == min(accsum))
Econ_inc_i_output3_2$interval <- Econ_inc_i_output3_2$upper - Econ_inc_i_output3_2$lower
colnames(Econ_inc_i_output3_2)[which(names(Econ_inc_i_output3_2) == "share")] <- "share2"
Econ_inc_i_output3 <- merge(Econ_inc_i_output3_1, Econ_inc_i_output3_2[c("group", "interval", "share2")])
Econ_inc_i_output3$value <- round(Econ_inc_i_output3$upper + 
                                            (50-Econ_inc_i_output3$share)/(Econ_inc_i_output3$share2 - Econ_inc_i_output3$share)*Econ_inc_i_output3$interval, 
                                            digits = 0)

#Placeholder for moe estimation
#Manually assigning the values to be 1 in order to run the significance test 
Econ_inc_i_output3$income_median_moe <- 1
Econ_inc_i_output4 <- gather(Econ_inc_i_output3[c("group", "value", "income_median_moe")], variable, value, value:income_median_moe, factor_key=TRUE)
Econ_inc_i_output4$variable <- as.factor(gsub("value","income_median", Econ_inc_i_output4$variable))
Econ_inc_i_output4$year <- i
inclist[[i]] <- Econ_inc_i_output4

#estimate aggregate results - high income share
Econ_incp_i_output1 <- Econ_inc_i_input2 %>%
                              group_by(group,var) %>%
                                  summarize(sumest = sum(estimate), 
                                      summoe = moe_sum(moe, estimate))
Econ_incp_i_output2 <- dcast(melt(Econ_incp_i_output1, id.vars=1:2), group ~ var + variable, fun.aggregate = sum)
Econ_incp_i_output2$hhs_125kplus_per <- Econ_incp_i_output2$households_125kplus_sumest / Econ_incp_i_output2$households_total_sumest *100
Econ_incp_i_output2$hhs_125kplus_per_moe <- moe_prop(Econ_incp_i_output2$households_125kplus_sumest, 
                                                      Econ_incp_i_output2$households_total_sumest,
                                                      Econ_incp_i_output2$households_125kplus_summoe, 
                                                      Econ_incp_i_output2$households_total_summoe)*100
Econ_incp_i_output3 <- Econ_incp_i_output2[c("group", "hhs_125kplus_per", "hhs_125kplus_per_moe")]
Econ_incp_i_output4 <- melt(Econ_incp_i_output3, id.vars="group")
Econ_incp_i_output4$year <- i
incplist[[i]] <- Econ_incp_i_output4

#county level
#median household income
Econ_inc_i_county <- get_acs(geography = "county",
                                variables = "DP03_0062",
                                state = "CA",
                                year = i)
Econ_inc_i_county_output <- Econ_inc_i_county[-c(1,3)] %>% mutate(NAME = gsub(", California", "", NAME))
Econ_inc_i_county_output1 <- subset(Econ_inc_i_county_output, Econ_inc_i_county_output$NAME %in% Counties)
colnames(Econ_inc_i_county_output1) <- c("group", "income_median", "income_median_moe")
Econ_inc_i_county_output2 <- melt(Econ_inc_i_county_output1, id.vars="group")
Econ_inc_i_county_output2$year <- i
inclist1[[i]] <- Econ_inc_i_county_output2
#high income share
Econ_incp_i_county <- get_acs(geography = "county",
                              variables = Vars2_1,
                              state = "CA",
                              year = i)
Econ_incp_i_county <- Econ_incp_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Econ_incp_i_county)[1:2] <- c("group", "var")
Econ_incp_i_county1 <- subset(Econ_incp_i_county, Econ_incp_i_county$group %in% Counties)
Econ_incp_i_county_input1 <- subset(Econ_incp_i_county1, Econ_incp_i_county1$var %in% c("B19001_001", "B19001_015", "B19001_016", "B19001_017"))
Econ_incp_i_county_input2 <- Econ_incp_i_county_input1 %>%
                             mutate(var = gsub("B19001_001", "households_total", var)) %>%
                             mutate(var = gsub("B19001_015", "households_125kplus", var)) %>%
                             mutate(var = gsub("B19001_016", "households_125kplus", var)) %>%
                             mutate(var = gsub("B19001_017", "households_125kplus", var))
Econ_incp_i_county_output1 <- Econ_incp_i_county_input2 %>%
                                 group_by(group,var) %>%
                                    summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate))
Econ_incp_i_county_output2 <- dcast(melt(Econ_incp_i_county_output1, id.vars=1:2), group ~ var + variable, fun.aggregate = sum)
Econ_incp_i_county_output2$hhs_125kplus_per <- Econ_incp_i_county_output2$households_125kplus_sumest / Econ_incp_i_county_output2$households_total_sumest *100
Econ_incp_i_county_output2$hhs_125kplus_per_moe <- moe_prop(Econ_incp_i_county_output2$households_125kplus_sumest,
                                                            Econ_incp_i_county_output2$households_total_sumest,
                                                            Econ_incp_i_county_output2$households_125kplus_summoe,
                                                            Econ_incp_i_county_output2$households_total_summoe)*100
Econ_incp_i_county_output3 <- Econ_incp_i_county_output2[c("group", "hhs_125kplus_per", "hhs_125kplus_per_moe")]
Econ_incp_i_county_output4 <- melt(Econ_incp_i_county_output3, id.vars="group")
Econ_incp_i_county_output4$year <- i
incplist1[[i]] <- Econ_incp_i_county_output4

#state level
#median household income
Econ_inc_i_state <- get_acs(geography = "state",
                            variables = "DP03_0062",
                            state = "CA",
                            year = i)
Econ_inc_i_state_output <- Econ_inc_i_state[-c(1,3)]
colnames(Econ_inc_i_state_output) <- c("group", "income_median", "income_median_moe")
Econ_inc_i_state_output1 <- melt(Econ_inc_i_state_output, id.vars="group")
Econ_inc_i_state_output1$year <- i
inclist2[[i]] <- Econ_inc_i_state_output1
#high income share
Econ_incp_i_state <- get_acs(geography = "state",
                             variables = Vars2_1,
                             state = "CA",
                             year = i)
Econ_incp_i_state <- Econ_incp_i_state[-1]
colnames(Econ_incp_i_state)[1:2] <- c("group", "var")
Econ_incp_i_state_input1 <- subset(Econ_incp_i_state, Econ_incp_i_state$var %in% c("B19001_001", "B19001_015", "B19001_016", "B19001_017"))
Econ_incp_i_state_input2 <- Econ_incp_i_state_input1 %>%
                               mutate(var = gsub("B19001_001", "households_total", var)) %>%
                               mutate(var = gsub("B19001_015", "households_125kplus", var)) %>%
                               mutate(var = gsub("B19001_016", "households_125kplus", var)) %>%
                               mutate(var = gsub("B19001_017", "households_125kplus", var))
Econ_incp_i_state_output1 <- Econ_incp_i_state_input2 %>%
                                group_by(group,var) %>%
                                   summarize(sumest = sum(estimate), summoe = moe_sum(moe, estimate))
Econ_incp_i_state_output2 <- dcast(melt(Econ_incp_i_state_output1, id.vars=1:2), group ~ var + variable, fun.aggregate = sum)
Econ_incp_i_state_output2$hhs_125kplus_per <- Econ_incp_i_state_output2$households_125kplus_sumest / Econ_incp_i_state_output2$households_total_sumest *100
Econ_incp_i_state_output2$hhs_125kplus_per_moe <- moe_prop(Econ_incp_i_state_output2$households_125kplus_sumest,
                                                           Econ_incp_i_state_output2$households_total_sumest,
                                                           Econ_incp_i_state_output2$households_125kplus_summoe,
                                                           Econ_incp_i_state_output2$households_total_summoe)*100
Econ_incp_i_state_output3 <- Econ_incp_i_state_output2[c("group", "hhs_125kplus_per", "hhs_125kplus_per_moe")]
Econ_incp_i_state_output4 <- melt(Econ_incp_i_state_output3, id.vars="group")
Econ_incp_i_state_output4$year <- i
incplist2[[i]] <- Econ_incp_i_state_output4

}

#Subgroup 2: below poverty share
#define variables needed
Vars2_2 <- c("B17001_001E", "B17001_001M",
             "B17001_002E", "B17001_002M")

#Extract data 
#create a loop for multiple-year data
povlist <- list()
povlist1 <- list()
povlist2 <- list()

for (i in years){
  
#census tract level 
Econ_pov_i <- get_acs(geography = "tract", 
                         variables = Vars2_2, 
                         state = "CA", 
                         year = i)
  
#subset dataframe for selected tracts
Econ_pov_i_input <- subset(Econ_pov_i, Econ_pov_i$GEOID %in% Tracts$GEOID)
Econ_pov_i_input <- merge(Econ_pov_i_input, Tracts)
colnames(Econ_pov_i_input)[3] <- "var"

#estimate aggregate results
Econ_pov_i_output <- Econ_pov_i_input %>%
                         group_by(group,var) %>%
                            summarize(sumest = sum(estimate), 
                                      summoe = moe_sum(moe, estimate))
Econ_pov_i_output1 <- dcast(melt(Econ_pov_i_output, id.vars=1:2), 
                            group ~ var + variable, fun.aggregate = sum)
Econ_pov_i_output1$poverty_per <- Econ_pov_i_output1$B17001_002_sumest / Econ_pov_i_output1$B17001_001_sumest * 100
Econ_pov_i_output1$poverty_per_moe <- moe_prop(Econ_pov_i_output1$B17001_002_sumest,
                                               Econ_pov_i_output1$B17001_001_sumest,
                                               Econ_pov_i_output1$B17001_002_summoe,
                                               Econ_pov_i_output1$B17001_001_summoe)*100
Econ_pov_i_output2 <- Econ_pov_i_output1[c("group","poverty_per", "poverty_per_moe")]
Econ_pov_i_output3 <- melt(Econ_pov_i_output2, id.vars="group")
Econ_pov_i_output3$year <- i
povlist[[i]] <- Econ_pov_i_output3

#county level 
Econ_pov_i_county <- get_acs(geography = "county", 
                             variables = Vars2_2, 
                             state = "CA", 
                             year = i)

#subset dataframe for selected tracts
Econ_pov_i_county_output <- Econ_pov_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Econ_pov_i_county_output)[1:2] <- c("group", "var")
Econ_pov_i_county_output1 <- subset(Econ_pov_i_county_output, Econ_pov_i_county_output$group %in% Counties)

#estimate aggregate results
Econ_pov_i_county_output1 <- dcast(melt(Econ_pov_i_county_output1, id.vars=1:2), 
                                   group ~ var + variable, fun.aggregate = sum)
Econ_pov_i_county_output1$poverty_per <- Econ_pov_i_county_output1$B17001_002_estimate / Econ_pov_i_county_output1$B17001_001_estimate * 100
Econ_pov_i_county_output1$poverty_per_moe <- moe_prop(Econ_pov_i_county_output1$B17001_002_estimate,
                                                      Econ_pov_i_county_output1$B17001_001_estimate,
                                                      Econ_pov_i_county_output1$B17001_002_moe,
                                                      Econ_pov_i_county_output1$B17001_001_moe)*100
Econ_pov_i_county_output2 <- Econ_pov_i_county_output1[c("group","poverty_per", "poverty_per_moe")]
Econ_pov_i_county_output3 <- melt(Econ_pov_i_county_output2, id.vars="group")
Econ_pov_i_county_output3$year <- i
povlist1[[i]] <- Econ_pov_i_county_output3

#state level 
Econ_pov_i_state <- get_acs(geography = "state", 
                             variables = Vars2_2, 
                             state = "CA", 
                             year = i)

#subset dataframe for selected tracts
Econ_pov_i_state_output <- Econ_pov_i_state[-1]
colnames(Econ_pov_i_state_output)[1:2] <- c("group", "var")

#estimate aggregate results
Econ_pov_i_state_output1 <- dcast(melt(Econ_pov_i_state_output, id.vars=1:2), 
                                   group ~ var + variable, fun.aggregate = sum)
Econ_pov_i_state_output1$poverty_per <- Econ_pov_i_state_output1$B17001_002_estimate / Econ_pov_i_state_output1$B17001_001_estimate * 100
Econ_pov_i_state_output1$poverty_per_moe <- moe_prop(Econ_pov_i_state_output1$B17001_002_estimate,
                                                      Econ_pov_i_state_output1$B17001_001_estimate,
                                                      Econ_pov_i_state_output1$B17001_002_moe,
                                                      Econ_pov_i_state_output1$B17001_001_moe)*100
Econ_pov_i_state_output2 <- Econ_pov_i_state_output1[c("group","poverty_per", "poverty_per_moe")]
Econ_pov_i_state_output3 <- melt(Econ_pov_i_state_output2, id.vars="group")
Econ_pov_i_state_output3$year <- i
povlist2[[i]] <- Econ_pov_i_state_output3

}

#Subgroup 3: Education
#define variables needed
Vars2_3 <- c("DP02_0058E", "DP02_0058M",
             "DP02_0059E", "DP02_0059M",
             "DP02_0060E", "DP02_0060M",
             "DP02_0064E", "DP02_0064M",
             "DP02_0065E", "DP02_0065M")

#Extract data 
#create a loop for multiple-year data
edlist <- list()
edlist1 <- list()
edlist2 <- list()

for (i in years){
  
#census tract level 
Econ_ed_i <- get_acs(geography = "tract", 
                        variables = Vars2_3, 
                        state = "CA", 
                        year = i)
  
#subset dataframe for selected tracts
Econ_ed_i_input <- subset(Econ_ed_i, Econ_ed_i$GEOID %in% Tracts$GEOID)
Econ_ed_i_input <- merge(Econ_ed_i_input, Tracts)
colnames(Econ_ed_i_input)[3] <- "var"
Econ_ed_i_input1 <- Econ_ed_i_input %>%
                          mutate(var = gsub("DP02_0059", "ed_belowhs", var)) %>%
                          mutate(var = gsub("DP02_0060", "ed_belowhs", var)) %>%
                          mutate(var = gsub("DP02_0064", "ed_aboveba", var)) %>%
                          mutate(var = gsub("DP02_0065", "ed_aboveba", var)) 
  
#estimate aggregate results
Econ_ed_i_output <- Econ_ed_i_input1 %>%
                        group_by(group,var) %>%
                        summarize(sumest = sum(estimate), 
                        summoe = moe_sum(moe, estimate))
Econ_ed_i_output1 <- dcast(melt(Econ_ed_i_output, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Econ_ed_i_output1$ed_belowhs_per <- Econ_ed_i_output1$ed_belowhs_sumest / Econ_ed_i_output1$DP02_0058_sumest * 100
Econ_ed_i_output1$ed_belowhs_per_moe <- moe_prop(Econ_ed_i_output1$ed_belowhs_sumest,
                                               Econ_ed_i_output1$DP02_0058_sumest,
                                               Econ_ed_i_output1$ed_belowhs_summoe,
                                               Econ_ed_i_output1$DP02_0058_summoe)*100
Econ_ed_i_output1$ed_aboveba_per <- Econ_ed_i_output1$ed_aboveba_sumest / Econ_ed_i_output1$DP02_0058_sumest * 100
Econ_ed_i_output1$ed_aboveba_per_moe <- moe_prop(Econ_ed_i_output1$ed_aboveba_sumest,
                                                 Econ_ed_i_output1$DP02_0058_sumest,
                                                 Econ_ed_i_output1$ed_aboveba_summoe,
                                                 Econ_ed_i_output1$DP02_0058_summoe)*100
Econ_ed_i_output2 <- Econ_ed_i_output1[c("group","ed_belowhs_per", "ed_belowhs_per_moe",
                                               "ed_aboveba_per", "ed_aboveba_per_moe")]
Econ_ed_i_output3 <- melt(Econ_ed_i_output2, id.vars="group")
Econ_ed_i_output3$year <- i
edlist[[i]] <- Econ_ed_i_output3
  
#county level 
Econ_ed_i_county <- get_acs(geography = "county", 
                               variables = Vars2_3, 
                               state = "CA", 
                               year = i)
  
#subset dataframe for selected tracts
Econ_ed_i_county_input <- Econ_ed_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Econ_ed_i_county_input)[1:2] <- c("group", "var")
Econ_ed_i_county_input1 <- subset(Econ_ed_i_county_input, Econ_ed_i_county_input$group %in% Counties)

Econ_ed_i_county_input2 <- Econ_ed_i_county_input1 %>%
                               mutate(var = gsub("DP02_0059", "ed_belowhs", var)) %>%
                               mutate(var = gsub("DP02_0060", "ed_belowhs", var)) %>%
                               mutate(var = gsub("DP02_0064", "ed_aboveba", var)) %>%
                               mutate(var = gsub("DP02_0065", "ed_aboveba", var)) 

#missing data means no sampling error, should be converted to 0
#see Page 11 in: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofDatai.pdf? 
Econ_ed_i_county_input2$moe[is.na(Econ_ed_i_county_input2$moe)] <- 0

#estimate aggregate results
Econ_ed_i_county_output <- Econ_ed_i_county_input2 %>%
                             group_by(group,var) %>%
                                summarize(sumest = sum(estimate), 
                                summoe = moe_sum(moe, estimate))
Econ_ed_i_county_output1 <- dcast(melt(Econ_ed_i_county_output, id.vars=1:2), 
                                  group ~ var + variable, fun.aggregate = sum)
Econ_ed_i_county_output1$ed_belowhs_per <- Econ_ed_i_county_output1$ed_belowhs_sumest / Econ_ed_i_county_output1$DP02_0058_sumest * 100
Econ_ed_i_county_output1$ed_belowhs_per_moe <- moe_prop(Econ_ed_i_county_output1$ed_belowhs_sumest,
                                                        Econ_ed_i_county_output1$DP02_0058_sumest,
                                                        Econ_ed_i_county_output1$ed_belowhs_summoe,
                                                        Econ_ed_i_county_output1$DP02_0058_summoe)*100
Econ_ed_i_county_output1$ed_aboveba_per <- Econ_ed_i_county_output1$ed_aboveba_sumest / Econ_ed_i_county_output1$DP02_0058_sumest * 100
Econ_ed_i_county_output1$ed_aboveba_per_moe <- moe_prop(Econ_ed_i_county_output1$ed_aboveba_sumest,
                                                        Econ_ed_i_county_output1$DP02_0058_sumest,
                                                        Econ_ed_i_county_output1$ed_aboveba_summoe,
                                                        Econ_ed_i_county_output1$DP02_0058_summoe)*100
Econ_ed_i_county_output2 <- Econ_ed_i_county_output1[c("group","ed_belowhs_per", "ed_belowhs_per_moe",
                                               "ed_aboveba_per", "ed_aboveba_per_moe")]
Econ_ed_i_county_output3 <- melt(Econ_ed_i_county_output2, id.vars="group")
Econ_ed_i_county_output3$year <- i
edlist1[[i]] <- Econ_ed_i_county_output3
 
#state level 
Econ_ed_i_state <- get_acs(geography = "state", 
                              variables = Vars2_3, 
                              state = "CA", 
                              year = i)
  
#subset dataframe for selected tracts
Econ_ed_i_state_input1 <- Econ_ed_i_state[-1]
colnames(Econ_ed_i_state_input1)[1:2] <- c("group", "var")
Econ_ed_i_state_input2 <- Econ_ed_i_state_input1 %>%
                               mutate(var = gsub("DP02_0059", "ed_belowhs", var)) %>%
                               mutate(var = gsub("DP02_0060", "ed_belowhs", var)) %>%
                               mutate(var = gsub("DP02_0064", "ed_aboveba", var)) %>%
                               mutate(var = gsub("DP02_0065", "ed_aboveba", var))
  
#estimate aggregate results
Econ_ed_i_state_output <- Econ_ed_i_state_input2 %>%
                              group_by(group,var) %>%
                                  summarize(sumest = sum(estimate), 
                                            summoe = moe_sum(moe, estimate))
Econ_ed_i_state_output1 <- dcast(melt(Econ_ed_i_state_output, id.vars=1:2), 
                                 group ~ var + variable, fun.aggregate = sum)
Econ_ed_i_state_output1$ed_belowhs_per <- Econ_ed_i_state_output1$ed_belowhs_sumest / Econ_ed_i_state_output1$DP02_0058_sumest * 100
Econ_ed_i_state_output1$ed_belowhs_per_moe <- moe_prop(Econ_ed_i_state_output1$ed_belowhs_sumest,
                                                       Econ_ed_i_state_output1$DP02_0058_sumest,
                                                       Econ_ed_i_state_output1$ed_belowhs_summoe,
                                                       Econ_ed_i_state_output1$DP02_0058_summoe)*100
Econ_ed_i_state_output1$ed_aboveba_per <- Econ_ed_i_state_output1$ed_aboveba_sumest / Econ_ed_i_state_output1$DP02_0058_sumest * 100
Econ_ed_i_state_output1$ed_aboveba_per_moe <- moe_prop(Econ_ed_i_state_output1$ed_aboveba_sumest,
                                                       Econ_ed_i_state_output1$DP02_0058_sumest,
                                                       Econ_ed_i_state_output1$ed_aboveba_summoe,
                                                       Econ_ed_i_state_output1$DP02_0058_summoe)*100
Econ_ed_i_state_output2 <- Econ_ed_i_state_output1[c("group","ed_belowhs_per", "ed_belowhs_per_moe",
                                                       "ed_aboveba_per", "ed_aboveba_per_moe")]
Econ_ed_i_state_output3 <- melt(Econ_ed_i_state_output2, id.vars="group")
Econ_ed_i_state_output3$year <- i
edlist2[[i]] <- Econ_ed_i_state_output3
  
}



#Subgroup 4: Employment
#define variables needed
Vars2_4 <- c("B23025_001E", "B23025_001M",
             "B23025_004E", "B23025_004M")

#Extract data 
#create a loop for multiple-year data
emplist <- list()
emplist1 <- list()
emplist2 <- list()

for (i in years){
  
#census tract level 
Econ_emp_i <- get_acs(geography = "tract", 
                       variables = Vars2_4, 
                       state = "CA", 
                       year = i)

#subset dataframe for selectemp tracts
Econ_emp_i_input <- subset(Econ_emp_i, Econ_emp_i$GEOID %in% Tracts$GEOID)
Econ_emp_i_input <- merge(Econ_emp_i_input, Tracts)
colnames(Econ_emp_i_input)[3] <- "var"
  
#estimate aggregate results
Econ_emp_i_output <- Econ_emp_i_input %>%
                        group_by(group,var) %>%
                           summarize(sumest = sum(estimate), 
                           summoe = moe_sum(moe, estimate))
Econ_emp_i_output1 <- dcast(melt(Econ_emp_i_output, id.vars=1:2), 
                            group ~ var + variable, fun.aggregate = sum)
Econ_emp_i_output1$emp_16plus_per <- Econ_emp_i_output1$B23025_004_sumest / Econ_emp_i_output1$B23025_001_sumest * 100
Econ_emp_i_output1$emp_16plus_per_moe <- moe_prop(Econ_emp_i_output1$B23025_004_sumest,
                                                  Econ_emp_i_output1$B23025_001_sumest,
                                                  Econ_emp_i_output1$B23025_004_summoe,
                                                  Econ_emp_i_output1$B23025_001_summoe)*100
Econ_emp_i_output2 <- Econ_emp_i_output1[c("group","emp_16plus_per", "emp_16plus_per_moe")]
Econ_emp_i_output3 <- melt(Econ_emp_i_output2, id.vars="group")
Econ_emp_i_output3$year <- i
emplist[[i]] <- Econ_emp_i_output3
  
#county level 
Econ_emp_i_county <- get_acs(geography = "county", 
                             variables = Vars2_4, 
                             state = "CA", 
                             year = i)
  
#subset dataframe for selectemp tracts
Econ_emp_i_county_output <- Econ_emp_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Econ_emp_i_county_output)[1:2] <- c("group", "var")
Econ_emp_i_county_output1 <- subset(Econ_emp_i_county_output, Econ_emp_i_county_output$group %in% Counties)

#estimate aggregate results
Econ_emp_i_county_output1 <- dcast(melt(Econ_emp_i_county_output1, id.vars=1:2), 
                                    group ~ var + variable, fun.aggregate = sum)
Econ_emp_i_county_output1$emp_16plus_per <- Econ_emp_i_county_output1$B23025_004_estimate / Econ_emp_i_county_output1$B23025_001_estimate * 100
Econ_emp_i_county_output1$emp_16plus_per_moe <- moe_prop(Econ_emp_i_county_output1$B23025_004_estimate,
                                                          Econ_emp_i_county_output1$B23025_001_estimate,
                                                          Econ_emp_i_county_output1$B23025_004_moe,
                                                          Econ_emp_i_county_output1$B23025_001_moe)*100
Econ_emp_i_county_output2 <- Econ_emp_i_county_output1[c("group","emp_16plus_per", "emp_16plus_per_moe")]
Econ_emp_i_county_output3 <- melt(Econ_emp_i_county_output2, id.vars="group")
Econ_emp_i_county_output3$year <- i
emplist1[[i]] <- Econ_emp_i_county_output3
  
#state level 
Econ_emp_i_state <- get_acs(geography = "state", 
                            variables = Vars2_4, 
                            state = "CA", 
                            year = i)
  
#subset dataframe for selectemp tracts
Econ_emp_i_state_output <- Econ_emp_i_state[-1]
colnames(Econ_emp_i_state_output)[1:2] <- c("group", "var")
  
#estimate aggregate results
Econ_emp_i_state_output1 <- dcast(melt(Econ_emp_i_state_output, id.vars=1:2), 
                                   group ~ var + variable, fun.aggregate = sum)
Econ_emp_i_state_output1$emp_16plus_per <- Econ_emp_i_state_output1$B23025_004_estimate / Econ_emp_i_state_output1$B23025_001_estimate * 100
Econ_emp_i_state_output1$emp_16plus_per_moe <- moe_prop(Econ_emp_i_state_output1$B23025_004_estimate,
                                                         Econ_emp_i_state_output1$B23025_001_estimate,
                                                         Econ_emp_i_state_output1$B23025_004_moe,
                                                         Econ_emp_i_state_output1$B23025_001_moe)*100
Econ_emp_i_state_output2 <- Econ_emp_i_state_output1[c("group","emp_16plus_per", "emp_16plus_per_moe")]
Econ_emp_i_state_output3 <- melt(Econ_emp_i_state_output2, id.vars="group")
Econ_emp_i_state_output3$year <- i
emplist2[[i]] <- Econ_emp_i_state_output3
  
}

#Combine results from all three geographical scales
Econ_results <- rbind(do.call(rbind, inclist), do.call(rbind, inclist1), do.call(rbind, inclist2),
                      do.call(rbind, incplist), do.call(rbind, incplist1), do.call(rbind, incplist2),
                      do.call(rbind, povlist), do.call(rbind, povlist1), do.call(rbind, povlist2),
                      do.call(rbind, edlist), do.call(rbind, edlist1), do.call(rbind, edlist2),
                      do.call(rbind, emplist), do.call(rbind, emplist1), do.call(rbind, emplist2))

###
#Variable Group 3 - energy indicators
###

#define variables needed
Vars3 <- c("B25040_001E", "B25040_001M",
           "B25040_004E", "B25040_004M",
           "B25040_002E", "B25040_002M",
           "B25040_010E", "B25040_010M",
           "B25040_003E", "B25040_003M",
           "B25040_005E", "B25040_005M",
           "B25040_006E", "B25040_006M",
           "B25040_007E", "B25040_007M",
           "B25040_008E", "B25040_008M")

#Extract data 
#create a loop for multiple-year data
enlist <- list()
enlist1 <- list()
enlist2 <- list()

for (i in years){

#census tract level 
En_i <- get_acs(geography = "tract", 
                variables = Vars3, 
                state = "CA", 
                year = i)

#subset dataframe for selectemp tracts
En_i_input <- subset(En_i, En_i$GEOID %in% Tracts$GEOID)
En_i_input <- merge(En_i_input, Tracts)
colnames(En_i_input)[3] <- "var"
En_i_input1 <- En_i_input %>%
                   mutate(var = gsub("B25040_003", "heating_other_fossil", var)) %>%
                   mutate(var = gsub("B25040_005", "heating_other_fossil", var)) %>%
                   mutate(var = gsub("B25040_006", "heating_other_fossil", var)) %>%
                   mutate(var = gsub("B25040_007", "heating_other_nonfossil", var)) %>%
                   mutate(var = gsub("B25040_008", "heating_other_nonfossil", var)) 

#estimate aggregate results
En_i_output <- En_i_input1 %>%
                   group_by(group,var) %>%
                       summarize(sumest = sum(estimate), 
                                 summoe = moe_sum(moe, estimate))
En_i_output1 <- dcast(melt(En_i_output, id.vars=1:2), 
                           group ~ var + variable, fun.aggregate = sum)
En_i_output1$heating_elec_per <- En_i_output1$B25040_004_sumest / En_i_output1$B25040_001_sumest * 100
En_i_output1$heating_elec_per_moe <- moe_prop(En_i_output1$B25040_004_sumest,
                                              En_i_output1$B25040_001_sumest,
                                              En_i_output1$B25040_004_summoe,
                                              En_i_output1$B25040_001_summoe)*100
En_i_output1$heating_utilitygas_per <- En_i_output1$B25040_002_sumest / En_i_output1$B25040_001_sumest * 100
En_i_output1$heating_utilitygas_per_moe <- moe_prop(En_i_output1$B25040_002_sumest,
                                             En_i_output1$B25040_001_sumest,
                                             En_i_output1$B25040_002_summoe,
                                             En_i_output1$B25040_001_summoe)*100
En_i_output1$heating_no_per <- En_i_output1$B25040_010_sumest / En_i_output1$B25040_001_sumest * 100
En_i_output1$heating_no_per_moe <- moe_prop(En_i_output1$B25040_010_sumest,
                                            En_i_output1$B25040_001_sumest,
                                            En_i_output1$B25040_010_summoe,
                                            En_i_output1$B25040_001_summoe)*100
En_i_output1$heating_other_fossil_per <- En_i_output1$heating_other_fossil_sumest / En_i_output1$B25040_001_sumest * 100
En_i_output1$heating_other_fossil_per_moe <- moe_prop(En_i_output1$heating_other_fossil_sumest,
                                                      En_i_output1$B25040_001_sumest,
                                                      En_i_output1$heating_other_fossil_summoe,
                                                      En_i_output1$B25040_001_summoe)*100
En_i_output1$heating_other_nonfossil_per <- En_i_output1$heating_other_nonfossil_sumest / En_i_output1$B25040_001_sumest * 100
En_i_output1$heating_other_nonfossil_per_moe <- moe_prop(En_i_output1$heating_other_nonfossil_sumest,
                                                         En_i_output1$B25040_001_sumest,
                                                         En_i_output1$heating_other_nonfossil_summoe,
                                                        En_i_output1$B25040_001_summoe)*100
En_i_output2 <- En_i_output1[c("group","heating_elec_per", "heating_elec_per_moe",
                               "heating_utilitygas_per", "heating_utilitygas_per_moe",
                               "heating_other_fossil_per", "heating_other_fossil_per_moe",
                               "heating_other_nonfossil_per", "heating_other_nonfossil_per_moe",
                               "heating_no_per", "heating_no_per_moe")]
En_i_output3 <- melt(En_i_output2, id.vars="group")
En_i_output3$year <- i
enlist[[i]] <- En_i_output3

#county level 
En_i_county <- get_acs(geography = "county", 
                       variables = Vars3, 
                       state = "CA", 
                       year = i)
#subset dataframe for selectemp tracts
En_i_county_input <- En_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(En_i_county_input)[1:2] <- c("group", "var")
En_i_county_input1 <- subset(En_i_county_input, En_i_county_input$group %in% Counties)
En_i_county_input2  <- En_i_county_input1  %>%
                           mutate(var = gsub("B25040_003", "heating_other_fossil", var)) %>%
                           mutate(var = gsub("B25040_005", "heating_other_fossil", var)) %>%
                           mutate(var = gsub("B25040_006", "heating_other_fossil", var)) %>%
                           mutate(var = gsub("B25040_007", "heating_other_nonfossil", var)) %>%
                           mutate(var = gsub("B25040_008", "heating_other_nonfossil", var)) 

#estimate aggregate results
En_i_county_output <- En_i_county_input2 %>%
                 group_by(group,var) %>%
                 summarize(sumest = sum(estimate), 
                 summoe = moe_sum(moe, estimate))
En_i_county_output1 <- dcast(melt(En_i_county_output, id.vars=1:2), 
                                  group ~ var + variable, fun.aggregate = sum)
En_i_county_output1$heating_elec_per <- En_i_county_output1$B25040_004_sumest / En_i_county_output1$B25040_001_sumest * 100
En_i_county_output1$heating_elec_per_moe <- moe_prop(En_i_county_output1$B25040_004_sumest,
                                                     En_i_county_output1$B25040_001_sumest,
                                                     En_i_county_output1$B25040_004_summoe,
                                                     En_i_county_output1$B25040_001_summoe)*100
En_i_county_output1$heating_utilitygas_per <- En_i_county_output1$B25040_002_sumest / En_i_county_output1$B25040_001_sumest * 100
En_i_county_output1$heating_utilitygas_per_moe <- moe_prop(En_i_county_output1$B25040_002_sumest,
                                                           En_i_county_output1$B25040_001_sumest,
                                                           En_i_county_output1$B25040_002_summoe,
                                                           En_i_county_output1$B25040_001_summoe)*100
En_i_county_output1$heating_no_per <- En_i_county_output1$B25040_010_sumest / En_i_county_output1$B25040_001_sumest * 100
En_i_county_output1$heating_no_per_moe <- moe_prop(En_i_county_output1$B25040_010_sumest,
                                                   En_i_county_output1$B25040_001_sumest,
                                                   En_i_county_output1$B25040_010_summoe,
                                                   En_i_county_output1$B25040_001_summoe)*100
En_i_county_output1$heating_other_fossil_per <- En_i_county_output1$heating_other_fossil_sumest / En_i_county_output1$B25040_001_sumest * 100
En_i_county_output1$heating_other_fossil_per_moe <- moe_prop(En_i_county_output1$heating_other_fossil_sumest,
                                                             En_i_county_output1$B25040_001_sumest,
                                                             En_i_county_output1$heating_other_fossil_summoe,
                                                             En_i_county_output1$B25040_001_summoe)*100
En_i_county_output1$heating_other_nonfossil_per <- En_i_county_output1$heating_other_nonfossil_sumest / En_i_county_output1$B25040_001_sumest * 100
En_i_county_output1$heating_other_nonfossil_per_moe <- moe_prop(En_i_county_output1$heating_other_nonfossil_sumest,
                                                         En_i_county_output1$B25040_001_sumest,
                                                         En_i_county_output1$heating_other_nonfossil_summoe,
                                                         En_i_county_output1$B25040_001_summoe)*100
En_i_county_output2 <- En_i_county_output1[c("group","heating_elec_per", "heating_elec_per_moe",
                               "heating_utilitygas_per", "heating_utilitygas_per_moe",
                               "heating_other_fossil_per", "heating_other_fossil_per_moe",
                               "heating_other_nonfossil_per", "heating_other_nonfossil_per_moe",
                               "heating_no_per", "heating_no_per_moe")]
En_i_county_output3 <- melt(En_i_county_output2, id.vars="group")
En_i_county_output3$year <- i
enlist1[[i]] <- En_i_county_output3

#state level 
En_i_state <- get_acs(geography = "state", 
                            variables = Vars3, 
                            state = "CA", 
                            year = i)

#subset dataframe for selectemp tracts
En_i_state_input1 <- En_i_state[-1]
colnames(En_i_state_input1)[1:2] <- c("group", "var")
En_i_state_input2  <- En_i_state_input1  %>%
                         mutate(var = gsub("B25040_003", "heating_other_fossil", var)) %>%
                         mutate(var = gsub("B25040_005", "heating_other_fossil", var)) %>%
                         mutate(var = gsub("B25040_006", "heating_other_fossil", var)) %>%
                         mutate(var = gsub("B25040_007", "heating_other_nonfossil", var)) %>%
                         mutate(var = gsub("B25040_008", "heating_other_nonfossil", var)) 

#estimate aggregate results
En_i_state_output <- En_i_state_input2 %>%
                         group_by(group,var) %>%
                             summarize(sumest = sum(estimate), 
                                       summoe = moe_sum(moe, estimate))
En_i_state_output1 <- dcast(melt(En_i_state_output, id.vars=1:2), 
                             group ~ var + variable, fun.aggregate = sum)
En_i_state_output1$heating_elec_per <- En_i_state_output1$B25040_004_sumest / En_i_state_output1$B25040_001_sumest * 100
En_i_state_output1$heating_elec_per_moe <- moe_prop(En_i_state_output1$B25040_004_sumest,
                                                     En_i_state_output1$B25040_001_sumest,
                                                     En_i_state_output1$B25040_004_summoe,
                                                     En_i_state_output1$B25040_001_summoe)*100
En_i_state_output1$heating_utilitygas_per <- En_i_state_output1$B25040_002_sumest / En_i_state_output1$B25040_001_sumest * 100
En_i_state_output1$heating_utilitygas_per_moe <- moe_prop(En_i_state_output1$B25040_002_sumest,
                                                           En_i_state_output1$B25040_001_sumest,
                                                           En_i_state_output1$B25040_002_summoe,
                                                           En_i_state_output1$B25040_001_summoe)*100
En_i_state_output1$heating_no_per <- En_i_state_output1$B25040_010_sumest / En_i_state_output1$B25040_001_sumest * 100
En_i_state_output1$heating_no_per_moe <- moe_prop(En_i_state_output1$B25040_010_sumest,
                                                   En_i_state_output1$B25040_001_sumest,
                                                   En_i_state_output1$B25040_010_summoe,
                                                   En_i_state_output1$B25040_001_summoe)*100
En_i_state_output1$heating_other_fossil_per <- En_i_state_output1$heating_other_fossil_sumest / En_i_state_output1$B25040_001_sumest * 100
En_i_state_output1$heating_other_fossil_per_moe <- moe_prop(En_i_state_output1$heating_other_fossil_sumest,
                                                             En_i_state_output1$B25040_001_sumest,
                                                             En_i_state_output1$heating_other_fossil_summoe,
                                                             En_i_state_output1$B25040_001_summoe)*100
En_i_state_output1$heating_other_nonfossil_per <- En_i_state_output1$heating_other_nonfossil_sumest / En_i_state_output1$B25040_001_sumest * 100
En_i_state_output1$heating_other_nonfossil_per_moe <- moe_prop(En_i_state_output1$heating_other_nonfossil_sumest,
                                                                En_i_state_output1$B25040_001_sumest,
                                                                En_i_state_output1$heating_other_nonfossil_summoe,
                                                                En_i_state_output1$B25040_001_summoe)*100
En_i_state_output2 <- En_i_state_output1[c("group","heating_elec_per", "heating_elec_per_moe",
                                      "heating_utilitygas_per", "heating_utilitygas_per_moe",
                                      "heating_other_fossil_per", "heating_other_fossil_per_moe",
                                      "heating_other_nonfossil_per", "heating_other_nonfossil_per_moe",
                                      "heating_no_per", "heating_no_per_moe")]
En_i_state_output3 <- melt(En_i_state_output2, id.vars="group")
En_i_state_output3$year <- i
enlist2[[i]] <- En_i_state_output3

}

#Combine results from all three geographical scales
En_results <- rbind(do.call(rbind, enlist), do.call(rbind, enlist1), do.call(rbind, enlist2))

###
#Variable Group 4 - health indicators
###

#define variables needed
#2013-2014 variables are different from 2015-2018, data structure has changed overtime. 
Vars4_1 <- c("S2701_C01_001E", "S2701_C01_001M",
             "S2701_C04_001E", "S2701_C04_001M",
             "S2701_C04_046E", "S2701_C04_046M",
             "S2701_C04_054E", "S2701_C04_054M")
Vars4_2 <- c("S2701_C01_001E", "S2701_C01_001M",
             "S2701_C02_001E", "S2701_C02_001M",
             "S2703_C02_001E", "S2703_C02_001M",
             "S2704_C02_001E", "S2704_C02_001M")

#Extract data 
#create a loop for multiple-year data
hlthlist <- list()
hlthlist1 <- list()
hlthlist2 <- list()
hlthlist3 <- list()
hlthlist4 <- list()
hlthlist5 <- list()

for (i in 2013:2014){
  
#census tract level 
Hlth_i <- get_acs(geography = "tract", 
                  variables = Vars4_1, 
                  state = "CA", 
                  year = i)
  
#subset dataframe for selecthlth tracts
Hlth_i_input <- subset(Hlth_i, Hlth_i$GEOID %in% Tracts$GEOID)
Hlth_i_input <- merge(Hlth_i_input, Tracts)
colnames(Hlth_i_input)[3] <- "var"
  
#estimate aggregate results
Hlth_i_output <- Hlth_i_input %>%
                     group_by(group,var) %>%
                         summarize(sumest = sum(estimate), 
                                   summoe = moe_sum(moe, estimate))
Hlth_i_output1 <- dcast(melt(Hlth_i_output, id.vars=1:2), 
                        group ~ var + variable, fun.aggregate = sum)
Hlth_i_output1$hlth_ins_per <- Hlth_i_output1$S2701_C04_001_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_output1$S2701_C04_001_sumest,
                                            Hlth_i_output1$S2701_C01_001_sumest,
                                            Hlth_i_output1$S2701_C04_001_summoe,
                                            Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output1$hlth_ins_prv_per <- Hlth_i_output1$S2701_C04_046_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_output1$S2701_C04_046_sumest,
                                                  Hlth_i_output1$S2701_C01_001_sumest,
                                                  Hlth_i_output1$S2701_C04_046_summoe,
                                                  Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output1$hlth_ins_pub_per <- Hlth_i_output1$S2701_C04_054_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_output1$S2701_C04_054_sumest,
                                                Hlth_i_output1$S2701_C01_001_sumest,
                                                Hlth_i_output1$S2701_C04_054_summoe,
                                                Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output2 <- Hlth_i_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                     "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                     "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_output3 <- melt(Hlth_i_output2, id.vars="group")
Hlth_i_output3$year <- i
hlthlist[[i]] <- Hlth_i_output3
  
#county level 
Hlth_i_county <- get_acs(geography = "county", 
                         variables = Vars4_1, 
                         state = "CA", 
                         year = i)
  
#subset dataframe for selecthlth tracts
Hlth_i_county_output <- Hlth_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Hlth_i_county_output)[1:2] <- c("group", "var")
Hlth_i_county_output1 <- subset(Hlth_i_county_output, Hlth_i_county_output$group %in% Counties)
  
#estimate aggregate results
Hlth_i_county_output1 <- dcast(melt(Hlth_i_county_output1, id.vars=1:2), 
                                 group ~ var + variable, fun.aggregate = sum)
Hlth_i_county_output1$hlth_ins_per <- Hlth_i_county_output1$S2701_C04_001_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_county_output1$S2701_C04_001_estimate,
                                                     Hlth_i_county_output1$S2701_C01_001_estimate,
                                                     Hlth_i_county_output1$S2701_C04_001_moe,
                                                     Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output1$hlth_ins_prv_per <- Hlth_i_county_output1$S2701_C04_046_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_county_output1$S2701_C04_046_estimate,
                                                         Hlth_i_county_output1$S2701_C01_001_estimate,
                                                         Hlth_i_county_output1$S2701_C04_046_moe,
                                                         Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output1$hlth_ins_pub_per <- Hlth_i_county_output1$S2701_C04_054_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_county_output1$S2701_C04_054_estimate,
                                                         Hlth_i_county_output1$S2701_C01_001_estimate,
                                                         Hlth_i_county_output1$S2701_C04_054_moe,
                                                         Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output2 <- Hlth_i_county_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                                   "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                                   "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_county_output3 <- melt(Hlth_i_county_output2, id.vars="group")
Hlth_i_county_output3$year <- i
hlthlist1[[i]] <- Hlth_i_county_output3
  
#state level 
Hlth_i_state <- get_acs(geography = "state", 
                          variables = Vars4_1, 
                          state = "CA", 
                          year = i)
  
#subset dataframe for selecthlth tracts
Hlth_i_state_output <- Hlth_i_state[-1]
colnames(Hlth_i_state_output)[1:2] <- c("group", "var")
  
#estimate aggregate results
Hlth_i_state_output1 <- dcast(melt(Hlth_i_state_output, id.vars=1:2), 
                                group ~ var + variable, fun.aggregate = sum)
Hlth_i_state_output1$hlth_ins_per <- Hlth_i_state_output1$S2701_C04_001_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_state_output1$S2701_C04_001_estimate,
                                                    Hlth_i_state_output1$S2701_C01_001_estimate,
                                                    Hlth_i_state_output1$S2701_C04_001_moe,
                                                    Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output1$hlth_ins_prv_per <- Hlth_i_state_output1$S2701_C04_046_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_state_output1$S2701_C04_046_estimate,
                                                        Hlth_i_state_output1$S2701_C01_001_estimate,
                                                        Hlth_i_state_output1$S2701_C04_046_moe,
                                                        Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output1$hlth_ins_pub_per <- Hlth_i_state_output1$S2701_C04_054_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_state_output1$S2701_C04_054_estimate,
                                                        Hlth_i_state_output1$S2701_C01_001_estimate,
                                                        Hlth_i_state_output1$S2701_C04_054_moe,
                                                        Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output2 <- Hlth_i_state_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                                 "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                                 "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_state_output3 <- melt(Hlth_i_state_output2, id.vars="group")
Hlth_i_state_output3$year <- i
hlthlist2[[i]] <- Hlth_i_state_output3
  
}

for (i in 2015:2018){
  
#census tract level 
Hlth_i <- get_acs(geography = "tract", 
                       variables = Vars4_2, 
                       state = "CA", 
                       year = i)

#subset dataframe for selecthlth tracts
Hlth_i_input <- subset(Hlth_i, Hlth_i$GEOID %in% Tracts$GEOID)
Hlth_i_input <- merge(Hlth_i_input, Tracts)
colnames(Hlth_i_input)[3] <- "var"
  
#estimate aggregate results
Hlth_i_output <- Hlth_i_input %>%
                     group_by(group,var) %>%
                         summarize(sumest = sum(estimate), 
                                   summoe = moe_sum(moe, estimate))
Hlth_i_output1 <- dcast(melt(Hlth_i_output, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Hlth_i_output1$hlth_ins_per <- Hlth_i_output1$S2701_C02_001_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_output1$S2701_C02_001_sumest,
                                            Hlth_i_output1$S2701_C01_001_sumest,
                                            Hlth_i_output1$S2701_C02_001_summoe,
                                            Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output1$hlth_ins_prv_per <- Hlth_i_output1$S2703_C02_001_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_output1$S2703_C02_001_sumest,
                                                Hlth_i_output1$S2701_C01_001_sumest,
                                                Hlth_i_output1$S2703_C02_001_summoe,
                                                Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output1$hlth_ins_pub_per <- Hlth_i_output1$S2704_C02_001_sumest / Hlth_i_output1$S2701_C01_001_sumest * 100
Hlth_i_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_output1$S2704_C02_001_sumest,
                                                Hlth_i_output1$S2701_C01_001_sumest,
                                                Hlth_i_output1$S2704_C02_001_summoe,
                                                Hlth_i_output1$S2701_C01_001_summoe)*100
Hlth_i_output2 <- Hlth_i_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                             "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                             "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_output3 <- melt(Hlth_i_output2, id.vars="group")
Hlth_i_output3$year <- i
hlthlist3[[i]] <- Hlth_i_output3
  
#county level 
Hlth_i_county <- get_acs(geography = "county", 
                         variables = Vars4_2, 
                         state = "CA", 
                         year = i)
  
#subset dataframe for selecthlth tracts
Hlth_i_county_output <- Hlth_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Hlth_i_county_output)[1:2] <- c("group", "var")
Hlth_i_county_output1 <- subset(Hlth_i_county_output, Hlth_i_county_output$group %in% Counties)
  
#estimate aggregate results
Hlth_i_county_output1 <- dcast(melt(Hlth_i_county_output1, id.vars=1:2), 
                                    group ~ var + variable, fun.aggregate = sum)
Hlth_i_county_output1$hlth_ins_per <- Hlth_i_county_output1$S2701_C02_001_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_county_output1$S2701_C02_001_estimate,
                                                   Hlth_i_county_output1$S2701_C01_001_estimate,
                                                   Hlth_i_county_output1$S2701_C02_001_moe,
                                                   Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output1$hlth_ins_prv_per <- Hlth_i_county_output1$S2703_C02_001_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_county_output1$S2703_C02_001_estimate,
                                                         Hlth_i_county_output1$S2701_C01_001_estimate,
                                                         Hlth_i_county_output1$S2703_C02_001_moe,
                                                         Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output1$hlth_ins_pub_per <- Hlth_i_county_output1$S2704_C02_001_estimate / Hlth_i_county_output1$S2701_C01_001_estimate * 100
Hlth_i_county_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_county_output1$S2704_C02_001_estimate,
                                                         Hlth_i_county_output1$S2701_C01_001_estimate,
                                                         Hlth_i_county_output1$S2704_C02_001_moe,
                                                         Hlth_i_county_output1$S2701_C01_001_moe)*100
Hlth_i_county_output2 <- Hlth_i_county_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                                "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                                "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_county_output3 <- melt(Hlth_i_county_output2, id.vars="group")
Hlth_i_county_output3$year <- i
hlthlist4[[i]] <- Hlth_i_county_output3
  
#state level 
Hlth_i_state <- get_acs(geography = "state", 
                             variables = Vars4_2, 
                             state = "CA", 
                             year = i)
  
#subset dataframe for selecthlth tracts
Hlth_i_state_output <- Hlth_i_state[-1]
colnames(Hlth_i_state_output)[1:2] <- c("group", "var")
  
#estimate aggregate results
Hlth_i_state_output1 <- dcast(melt(Hlth_i_state_output, id.vars=1:2), 
                                   group ~ var + variable, fun.aggregate = sum)
Hlth_i_state_output1$hlth_ins_per <- Hlth_i_state_output1$S2701_C02_001_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_per_moe <- moe_prop(Hlth_i_state_output1$S2701_C02_001_estimate,
                                                  Hlth_i_state_output1$S2701_C01_001_estimate,
                                                  Hlth_i_state_output1$S2701_C02_001_moe,
                                                  Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output1$hlth_ins_prv_per <- Hlth_i_state_output1$S2703_C02_001_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_prv_per_moe <- moe_prop(Hlth_i_state_output1$S2703_C02_001_estimate,
                                                        Hlth_i_state_output1$S2701_C01_001_estimate,
                                                        Hlth_i_state_output1$S2703_C02_001_moe,
                                                        Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output1$hlth_ins_pub_per <- Hlth_i_state_output1$S2704_C02_001_estimate / Hlth_i_state_output1$S2701_C01_001_estimate * 100
Hlth_i_state_output1$hlth_ins_pub_per_moe <- moe_prop(Hlth_i_state_output1$S2704_C02_001_estimate,
                                                        Hlth_i_state_output1$S2701_C01_001_estimate,
                                                        Hlth_i_state_output1$S2704_C02_001_moe,
                                                        Hlth_i_state_output1$S2701_C01_001_moe)*100
Hlth_i_state_output2 <- Hlth_i_state_output1[c("group","hlth_ins_per", "hlth_ins_per_moe",
                                                           "hlth_ins_prv_per", "hlth_ins_prv_per_moe",
                                                           "hlth_ins_pub_per", "hlth_ins_pub_per_moe")]
Hlth_i_state_output3 <- melt(Hlth_i_state_output2, id.vars="group")
Hlth_i_state_output3$year <- i
hlthlist5[[i]] <- Hlth_i_state_output3
  
}

#Combine results from all three geographical scales
Hlth_results <- rbind(do.call(rbind, hlthlist), do.call(rbind, hlthlist1), do.call(rbind, hlthlist2),
                      do.call(rbind, hlthlist3), do.call(rbind, hlthlist4), do.call(rbind, hlthlist5))

###
#Variable Group 5 - housing indicators
###

#define variables needed
Vars5 <- c("B25003_001E", "B25003_001M",
           "B25003_002E", "B25003_002M",
           "B25003_003E", "B25003_003M",
           "B25070_001E", "B25070_001M",
           "B25070_007E", "B25070_007M",
           "B25070_008E", "B25070_008M",
           "B25070_009E", "B25070_009M",
           "B25070_010E", "B25070_010M",
           "B25091_001E", "B25091_001M",
           "B25091_007E", "B25091_007M",
           "B25091_008E", "B25091_008M",
           "B25091_009E", "B25091_009M",
           "B25091_010E", "B25091_010M",
           "B25014_001E", "B25014_001M",
           "B25014_005E", "B25014_005M",
           "B25014_006E", "B25014_006M",
           "B25014_007E", "B25014_007M",
           "B25014_011E", "B25014_011M",
           "B25014_012E", "B25014_012M",
           "B25014_013E", "B25014_013M",
           "B07013_001E", "B07013_001M",
           "B07013_005E", "B07013_005M",
           "B07013_006E", "B07013_006M",
           "B07010_001E", "B07010_001M",
           "B07010_013E", "B07010_013M",
           "B07010_015E", "B07010_015M",
           "B07010_016E", "B07010_016M",
           "B07010_017E", "B07010_017M",
           "B07010_018E", "B07010_018M",
           "B07010_019E", "B07010_019M",
           "B07010_020E", "B07010_020M",
           "B07010_021E", "B07010_021M",
           "B07010_022E", "B07010_022M",
           "B25002_001E", "B25002_001M",
           "B25004_002E", "B25004_002M",
           "B25004_004E", "B25004_004M")

#Extract data 
#create a loop for multiple-year data
Houslist <- list()
Houslist1 <- list()
Houslist2 <- list()

for (i in years){
  
#cHoussus tract level 
Hous_i <- get_acs(geography = "tract", 
                  variables = Vars5, 
                  state = "CA", 
                  year = i)
  
#subset dataframe for selectemp tracts
Hous_i_input <- subset(Hous_i, Hous_i$GEOID %in% Tracts$GEOID)
Hous_i_input <- merge(Hous_i_input, Tracts)
colnames(Hous_i_input)[3] <- "var"
Hous_i_input1 <- Hous_i_input %>%
                     mutate(var = gsub("B25070_007", "hhs_rent30plus", var)) %>%
                     mutate(var = gsub("B25070_008", "hhs_rent30plus", var)) %>%
                     mutate(var = gsub("B25070_009", "hhs_rent30plus", var)) %>%
                     mutate(var = gsub("B25070_010", "hhs_rent30plus", var)) %>%
                     mutate(var = gsub("B25091_007", "hhs_mort30plus", var)) %>%
                     mutate(var = gsub("B25091_008", "hhs_mort30plus", var)) %>%
                     mutate(var = gsub("B25091_009", "hhs_mort30plus", var)) %>%
                     mutate(var = gsub("B25091_010", "hhs_mort30plus", var)) %>%
                     mutate(var = gsub("B25014_005", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B25014_006", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B25014_007", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B25014_011", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B25014_012", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B25014_013", "hhs_occup_1plus", var)) %>%
                     mutate(var = gsub("B07010_013", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_015", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_016", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_017", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_018", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_019", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_020", "hhs_same1yr_75kminus", var)) %>%
                     mutate(var = gsub("B07010_021", "hhs_same1yr_75kminus", var))
Hous_i_input2 <- subset(Hous_i_input, Hous_i_input$var %in% c("B25070_010","B25091_010"))
Hous_i_input3 <- subset(Hous_i_input, Hous_i_input$var %in% c("B25014_005","B25014_006",
                                                              "B25014_007","B25014_011",
                                                              "B25014_012","B25014_013")) %>%
                     mutate(var = gsub("B25014_005", "hhs_occup_1plus_owner", var)) %>%
                     mutate(var = gsub("B25014_006", "hhs_occup_1plus_owner", var)) %>%
                     mutate(var = gsub("B25014_007", "hhs_occup_1plus_owner", var)) %>%
                     mutate(var = gsub("B25014_011", "hhs_occup_1plus_renter", var)) %>%
                     mutate(var = gsub("B25014_012", "hhs_occup_1plus_renter", var)) %>%
                     mutate(var = gsub("B25014_013", "hhs_occup_1plus_renter", var))
Hous_i_input4 <- rbind(Hous_i_input1, Hous_i_input2, Hous_i_input3)

#estimate aggregate results
Hous_i_output <- Hous_i_input4 %>%
                     group_by(group,var) %>%
                         summarize(sumest = sum(estimate), 
                                   summoe = moe_sum(moe, estimate))
Hous_i_output1 <- dcast(melt(Hous_i_output, id.vars=1:2), 
                        group ~ var + variable, fun.aggregate = sum)
Hous_i_output1$tenure_renter_per <- Hous_i_output1$B25003_003_sumest / Hous_i_output1$B25003_001_sumest * 100
Hous_i_output1$tenure_renter_per_moe <- moe_prop(Hous_i_output1$B25003_003_sumest,
                                                 Hous_i_output1$B25003_001_sumest,
                                                 Hous_i_output1$B25003_003_summoe,
                                                 Hous_i_output1$B25003_001_summoe)*100
Hous_i_output1$tenure_owner_per <- Hous_i_output1$B25003_002_sumest / Hous_i_output1$B25003_001_sumest * 100
Hous_i_output1$tenure_owner_per_moe <- moe_prop(Hous_i_output1$B25003_002_sumest,
                                                 Hous_i_output1$B25003_001_sumest,
                                                 Hous_i_output1$B25003_002_summoe,
                                                 Hous_i_output1$B25003_001_summoe)*100
Hous_i_output1$hhs_rent30plus_per <- Hous_i_output1$hhs_rent30plus_sumest / Hous_i_output1$B25070_001_sumest * 100
Hous_i_output1$hhs_rent30plus_per_moe <- moe_prop(Hous_i_output1$hhs_rent30plus_sumest,
                                                  Hous_i_output1$B25070_001_sumest,
                                                  Hous_i_output1$hhs_rent30plus_summoe,
                                                  Hous_i_output1$B25070_001_summoe)*100
Hous_i_output1$hhs_rent50plus_per <- Hous_i_output1$B25070_010_sumest / Hous_i_output1$B25070_001_sumest * 100
Hous_i_output1$hhs_rent50plus_per_moe <- moe_prop(Hous_i_output1$B25070_010_sumest,
                                                  Hous_i_output1$B25070_001_sumest,
                                                  Hous_i_output1$B25070_010_summoe,
                                                  Hous_i_output1$B25070_001_summoe)*100
Hous_i_output1$hhs_mort30plus_per <- Hous_i_output1$hhs_mort30plus_sumest / Hous_i_output1$B25091_001_sumest * 100
Hous_i_output1$hhs_mort30plus_per_moe <- moe_prop(Hous_i_output1$hhs_mort30plus_sumest,
                                                  Hous_i_output1$B25091_001_sumest,
                                                  Hous_i_output1$hhs_mort30plus_summoe,
                                                  Hous_i_output1$B25091_001_summoe)*100
Hous_i_output1$hhs_mort50plus_per <- Hous_i_output1$B25091_010_sumest / Hous_i_output1$B25091_001_sumest * 100
Hous_i_output1$hhs_mort50plus_per_moe <- moe_prop(Hous_i_output1$B25091_010_sumest,
                                                  Hous_i_output1$B25091_001_sumest,
                                                  Hous_i_output1$B25091_010_summoe,
                                                  Hous_i_output1$B25091_001_summoe)*100
Hous_i_output1$hhs_occup_1plus_per <- Hous_i_output1$hhs_occup_1plus_sumest / Hous_i_output1$B25014_001_sumest * 100
Hous_i_output1$hhs_occup_1plus_per_moe <- moe_prop(Hous_i_output1$hhs_occup_1plus_sumest,
                                                   Hous_i_output1$B25014_001_sumest,
                                                   Hous_i_output1$hhs_occup_1plus_summoe,
                                                   Hous_i_output1$B25014_001_summoe)*100
Hous_i_output1$hhs_occup_1plus_owner_per <- Hous_i_output1$hhs_occup_1plus_owner_sumest / Hous_i_output1$B25014_001_sumest * 100
Hous_i_output1$hhs_occup_1plus_owner_per_moe <- moe_prop(Hous_i_output1$hhs_occup_1plus_owner_sumest,
                                                         Hous_i_output1$B25014_001_sumest,
                                                         Hous_i_output1$hhs_occup_1plus_owner_summoe,
                                                         Hous_i_output1$B25014_001_summoe)*100
Hous_i_output1$hhs_occup_1plus_renter_per <- Hous_i_output1$hhs_occup_1plus_renter_sumest / Hous_i_output1$B25014_001_sumest * 100
Hous_i_output1$hhs_occup_1plus_renter_per_moe <- moe_prop(Hous_i_output1$hhs_occup_1plus_renter_sumest,
                                                          Hous_i_output1$B25014_001_sumest,
                                                          Hous_i_output1$hhs_occup_1plus_renter_summoe,
                                                          Hous_i_output1$B25014_001_summoe)*100
Hous_i_output1$hhs_same1yr_owner_per <- Hous_i_output1$B07013_005_sumest / Hous_i_output1$B07013_001_sumest * 100
Hous_i_output1$hhs_same1yr_owner_per_moe <- moe_prop(Hous_i_output1$B07013_005_sumest,
                                                     Hous_i_output1$B07013_001_sumest,
                                                     Hous_i_output1$B07013_005_summoe,
                                                     Hous_i_output1$B07013_001_summoe)*100
Hous_i_output1$hhs_same1yr_renter_per <- Hous_i_output1$B07013_006_sumest / Hous_i_output1$B07013_001_sumest * 100
Hous_i_output1$hhs_same1yr_renter_per_moe <- moe_prop(Hous_i_output1$B07013_006_sumest,
                                                      Hous_i_output1$B07013_001_sumest,
                                                      Hous_i_output1$B07013_006_summoe,
                                                      Hous_i_output1$B07013_001_summoe)*100
Hous_i_output1$hhs_same1yr_75kminus_per <- Hous_i_output1$hhs_same1yr_75kminus_sumest / Hous_i_output1$B07010_001_sumest * 100
Hous_i_output1$hhs_same1yr_75kminus_per_moe <- moe_prop(Hous_i_output1$hhs_same1yr_75kminus_sumest,
                                                        Hous_i_output1$B07010_001_sumest,
                                                        Hous_i_output1$hhs_same1yr_75kminus_summoe,
                                                        Hous_i_output1$B07010_001_summoe)*100
Hous_i_output1$hhs_same1yr_75kplus_per <- Hous_i_output1$B07010_022_sumest / Hous_i_output1$B07010_001_sumest * 100
Hous_i_output1$hhs_same1yr_75kplus_per_moe <- moe_prop(Hous_i_output1$B07010_022_sumest,
                                                       Hous_i_output1$B07010_001_sumest,
                                                       Hous_i_output1$B07010_022_summoe,
                                                       Hous_i_output1$B07010_001_summoe)*100
Hous_i_output1$tenure_vacant_rent_per <- Hous_i_output1$B25004_002_sumest / Hous_i_output1$B25002_001_sumest * 100
Hous_i_output1$tenure_vacant_rent_per_moe <- moe_prop(Hous_i_output1$B25004_002_sumest,
                                                      Hous_i_output1$B25002_001_sumest,
                                                      Hous_i_output1$B25004_002_summoe,
                                                      Hous_i_output1$B25002_001_summoe)*100

Hous_i_output1$tenure_vacant_sale_per <- Hous_i_output1$B25004_004_sumest / Hous_i_output1$B25002_001_sumest * 100
Hous_i_output1$tenure_vacant_sale_per_moe <- moe_prop(Hous_i_output1$B25004_004_sumest,
                                                      Hous_i_output1$B25002_001_sumest,
                                                      Hous_i_output1$B25004_004_summoe,
                                                      Hous_i_output1$B25002_001_summoe)*100
Hous_i_output2 <- Hous_i_output1[c("group","tenure_renter_per", "tenure_renter_per_moe",
                                   "tenure_owner_per", "tenure_owner_per_moe",
                                 "hhs_rent30plus_per", "hhs_rent30plus_per_moe",
                                 "hhs_rent50plus_per", "hhs_rent50plus_per_moe",
                                 "hhs_mort30plus_per", "hhs_mort30plus_per_moe",
                                 "hhs_mort50plus_per", "hhs_mort50plus_per_moe",
                                 "hhs_occup_1plus_per", "hhs_occup_1plus_per_moe",
                                 "hhs_occup_1plus_owner_per", "hhs_occup_1plus_owner_per_moe",
                                 "hhs_occup_1plus_renter_per", "hhs_occup_1plus_renter_per_moe",
                                 "hhs_same1yr_owner_per", "hhs_same1yr_owner_per_moe",
                                 "hhs_same1yr_renter_per", "hhs_same1yr_renter_per_moe",
                                 "hhs_same1yr_75kminus_per", "hhs_same1yr_75kminus_per_moe",
                                 "hhs_same1yr_75kplus_per", "hhs_same1yr_75kplus_per_moe",
                                 "tenure_vacant_rent_per", "tenure_vacant_rent_per_moe",
                                 "tenure_vacant_sale_per", "tenure_vacant_sale_per_moe")]
Hous_i_output3 <- melt(Hous_i_output2, id.vars="group")
Hous_i_output3$year <- i
Houslist[[i]] <- Hous_i_output3

#county level 
Hous_i_county <- get_acs(geography = "county", 
                         variables = Vars5, 
                         state = "CA", 
                         year = i)

#subset dataframe for selectemp tracts
Hous_i_county_input <- Hous_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Hous_i_county_input)[1:2] <- c("group", "var")
Hous_i_county_input <- subset(Hous_i_county_input, Hous_i_county_input$group %in% Counties)
Hous_i_county_input1 <- Hous_i_county_input %>%
                            mutate(var = gsub("B25070_007", "hhs_rent30plus", var)) %>%
                            mutate(var = gsub("B25070_008", "hhs_rent30plus", var)) %>%
                            mutate(var = gsub("B25070_009", "hhs_rent30plus", var)) %>%
                            mutate(var = gsub("B25070_010", "hhs_rent30plus", var)) %>%
                            mutate(var = gsub("B25091_007", "hhs_mort30plus", var)) %>%
                            mutate(var = gsub("B25091_008", "hhs_mort30plus", var)) %>%
                            mutate(var = gsub("B25091_009", "hhs_mort30plus", var)) %>%
                            mutate(var = gsub("B25091_010", "hhs_mort30plus", var)) %>%
                            mutate(var = gsub("B25014_005", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B25014_006", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B25014_007", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B25014_011", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B25014_012", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B25014_013", "hhs_occup_1plus", var)) %>%
                            mutate(var = gsub("B07010_013", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_015", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_016", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_017", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_018", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_019", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_020", "hhs_same1yr_75kminus", var)) %>%
                            mutate(var = gsub("B07010_021", "hhs_same1yr_75kminus", var))
Hous_i_county_input2 <- subset(Hous_i_county_input, Hous_i_county_input$var %in% c("B25070_010","B25091_010"))
Hous_i_county_input3 <- subset(Hous_i_county_input, Hous_i_county_input$var %in% c("B25014_005","B25014_006",
                                                              "B25014_007","B25014_011",
                                                              "B25014_012","B25014_013")) %>%
                                mutate(var = gsub("B25014_005", "hhs_occup_1plus_owner", var)) %>%
                                mutate(var = gsub("B25014_006", "hhs_occup_1plus_owner", var)) %>%
                                mutate(var = gsub("B25014_007", "hhs_occup_1plus_owner", var)) %>%
                                mutate(var = gsub("B25014_011", "hhs_occup_1plus_renter", var)) %>%
                                mutate(var = gsub("B25014_012", "hhs_occup_1plus_renter", var)) %>%
                                mutate(var = gsub("B25014_013", "hhs_occup_1plus_renter", var))
Hous_i_county_input4 <- rbind(Hous_i_county_input1, Hous_i_county_input2, Hous_i_county_input3)

#estimate aggregate results
Hous_i_county_output <- Hous_i_county_input4 %>%
                            group_by(group,var) %>%
                                summarize(sumest = sum(estimate), 
                                          summoe = moe_sum(moe, estimate))
Hous_i_county_output1 <- dcast(melt(Hous_i_county_output, id.vars=1:2), 
                               group ~ var + variable, fun.aggregate = sum)
Hous_i_county_output1$tenure_renter_per <- Hous_i_county_output1$B25003_003_sumest / Hous_i_county_output1$B25003_001_sumest * 100
Hous_i_county_output1$tenure_renter_per_moe <- moe_prop(Hous_i_county_output1$B25003_003_sumest,
                                                        Hous_i_county_output1$B25003_001_sumest,
                                                        Hous_i_county_output1$B25003_003_summoe,
                                                        Hous_i_county_output1$B25003_001_summoe)*100
Hous_i_county_output1$tenure_owner_per <- Hous_i_county_output1$B25003_002_sumest / Hous_i_county_output1$B25003_001_sumest * 100
Hous_i_county_output1$tenure_owner_per_moe <- moe_prop(Hous_i_county_output1$B25003_002_sumest,
                                                       Hous_i_county_output1$B25003_001_sumest,
                                                       Hous_i_county_output1$B25003_002_summoe,
                                                       Hous_i_county_output1$B25003_001_summoe)*100
Hous_i_county_output1$hhs_rent30plus_per <- Hous_i_county_output1$hhs_rent30plus_sumest / Hous_i_county_output1$B25070_001_sumest * 100
Hous_i_county_output1$hhs_rent30plus_per_moe <- moe_prop(Hous_i_county_output1$hhs_rent30plus_sumest,
                                                         Hous_i_county_output1$B25070_001_sumest,
                                                         Hous_i_county_output1$hhs_rent30plus_summoe,
                                                         Hous_i_county_output1$B25070_001_summoe)*100
Hous_i_county_output1$hhs_rent50plus_per <- Hous_i_county_output1$B25070_010_sumest / Hous_i_county_output1$B25070_001_sumest * 100
Hous_i_county_output1$hhs_rent50plus_per_moe <- moe_prop(Hous_i_county_output1$B25070_010_sumest,
                                                         Hous_i_county_output1$B25070_001_sumest,
                                                         Hous_i_county_output1$B25070_010_summoe,
                                                         Hous_i_county_output1$B25070_001_summoe)*100
Hous_i_county_output1$hhs_mort30plus_per <- Hous_i_county_output1$hhs_mort30plus_sumest / Hous_i_county_output1$B25091_001_sumest * 100
Hous_i_county_output1$hhs_mort30plus_per_moe <- moe_prop(Hous_i_county_output1$hhs_mort30plus_sumest,
                                                         Hous_i_county_output1$B25091_001_sumest,
                                                         Hous_i_county_output1$hhs_mort30plus_summoe,
                                                         Hous_i_county_output1$B25091_001_summoe)*100
Hous_i_county_output1$hhs_mort50plus_per <- Hous_i_county_output1$B25091_010_sumest / Hous_i_county_output1$B25091_001_sumest * 100
Hous_i_county_output1$hhs_mort50plus_per_moe <- moe_prop(Hous_i_county_output1$B25091_010_sumest,
                                                         Hous_i_county_output1$B25091_001_sumest,
                                                         Hous_i_county_output1$B25091_010_summoe,
                                                         Hous_i_county_output1$B25091_001_summoe)*100
Hous_i_county_output1$hhs_occup_1plus_per <- Hous_i_county_output1$hhs_occup_1plus_sumest / Hous_i_county_output1$B25014_001_sumest * 100
Hous_i_county_output1$hhs_occup_1plus_per_moe <- moe_prop(Hous_i_county_output1$hhs_occup_1plus_sumest,
                                                          Hous_i_county_output1$B25014_001_sumest,
                                                          Hous_i_county_output1$hhs_occup_1plus_summoe,
                                                          Hous_i_county_output1$B25014_001_summoe)*100
Hous_i_county_output1$hhs_occup_1plus_owner_per <- Hous_i_county_output1$hhs_occup_1plus_owner_sumest / Hous_i_county_output1$B25014_001_sumest * 100
Hous_i_county_output1$hhs_occup_1plus_owner_per_moe <- moe_prop(Hous_i_county_output1$hhs_occup_1plus_owner_sumest,
                                                                Hous_i_county_output1$B25014_001_sumest,
                                                                Hous_i_county_output1$hhs_occup_1plus_owner_summoe,
                                                                Hous_i_county_output1$B25014_001_summoe)*100
Hous_i_county_output1$hhs_occup_1plus_renter_per <- Hous_i_county_output1$hhs_occup_1plus_renter_sumest / Hous_i_county_output1$B25014_001_sumest * 100
Hous_i_county_output1$hhs_occup_1plus_renter_per_moe <- moe_prop(Hous_i_county_output1$hhs_occup_1plus_renter_sumest,
                                                                 Hous_i_county_output1$B25014_001_sumest,
                                                                 Hous_i_county_output1$hhs_occup_1plus_renter_summoe,
                                                                 Hous_i_county_output1$B25014_001_summoe)*100
Hous_i_county_output1$hhs_same1yr_owner_per <- Hous_i_county_output1$B07013_005_sumest / Hous_i_county_output1$B07013_001_sumest * 100
Hous_i_county_output1$hhs_same1yr_owner_per_moe <- moe_prop(Hous_i_county_output1$B07013_005_sumest,
                                                            Hous_i_county_output1$B07013_001_sumest,
                                                            Hous_i_county_output1$B07013_005_summoe,
                                                            Hous_i_county_output1$B07013_001_summoe)*100
Hous_i_county_output1$hhs_same1yr_renter_per <- Hous_i_county_output1$B07013_006_sumest / Hous_i_county_output1$B07013_001_sumest * 100
Hous_i_county_output1$hhs_same1yr_renter_per_moe <- moe_prop(Hous_i_county_output1$B07013_006_sumest,
                                                             Hous_i_county_output1$B07013_001_sumest,
                                                             Hous_i_county_output1$B07013_006_summoe,
                                                             Hous_i_county_output1$B07013_001_summoe)*100
Hous_i_county_output1$hhs_same1yr_75kminus_per <- Hous_i_county_output1$hhs_same1yr_75kminus_sumest / Hous_i_county_output1$B07010_001_sumest * 100
Hous_i_county_output1$hhs_same1yr_75kminus_per_moe <- moe_prop(Hous_i_county_output1$hhs_same1yr_75kminus_sumest,
                                                               Hous_i_county_output1$B07010_001_sumest,
                                                               Hous_i_county_output1$hhs_same1yr_75kminus_summoe,
                                                               Hous_i_county_output1$B07010_001_summoe)*100
Hous_i_county_output1$hhs_same1yr_75kplus_per <- Hous_i_county_output1$B07010_022_sumest / Hous_i_county_output1$B07010_001_sumest * 100
Hous_i_county_output1$hhs_same1yr_75kplus_per_moe <- moe_prop(Hous_i_county_output1$B07010_022_sumest,
                                                              Hous_i_county_output1$B07010_001_sumest,
                                                              Hous_i_county_output1$B07010_022_summoe,
                                                              Hous_i_county_output1$B07010_001_summoe)*100
Hous_i_county_output1$tenure_vacant_rent_per <- Hous_i_county_output1$B25004_002_sumest / Hous_i_county_output1$B25002_001_sumest * 100
Hous_i_county_output1$tenure_vacant_rent_per_moe <- moe_prop(Hous_i_county_output1$B25004_002_sumest,
                                                             Hous_i_county_output1$B25002_001_sumest,
                                                             Hous_i_county_output1$B25004_002_summoe,
                                                             Hous_i_county_output1$B25002_001_summoe)*100
Hous_i_county_output1$tenure_vacant_sale_per <- Hous_i_county_output1$B25004_004_sumest / Hous_i_county_output1$B25002_001_sumest * 100
Hous_i_county_output1$tenure_vacant_sale_per_moe <- moe_prop(Hous_i_county_output1$B25004_004_sumest,
                                                             Hous_i_county_output1$B25002_001_sumest,
                                                             Hous_i_county_output1$B25004_004_summoe,
                                                             Hous_i_county_output1$B25002_001_summoe)*100
Hous_i_county_output2 <- Hous_i_county_output1[c("group","tenure_renter_per", "tenure_renter_per_moe",
                                                 "tenure_owner_per", "tenure_owner_per_moe",
                                                 "hhs_rent30plus_per", "hhs_rent30plus_per_moe",
                                                 "hhs_rent50plus_per", "hhs_rent50plus_per_moe",
                                                 "hhs_mort30plus_per", "hhs_mort30plus_per_moe",
                                                 "hhs_mort50plus_per", "hhs_mort50plus_per_moe",
                                                 "hhs_occup_1plus_per", "hhs_occup_1plus_per_moe",
                                                 "hhs_occup_1plus_owner_per", "hhs_occup_1plus_owner_per_moe",
                                                 "hhs_occup_1plus_renter_per", "hhs_occup_1plus_renter_per_moe",
                                                 "hhs_same1yr_owner_per", "hhs_same1yr_owner_per_moe",
                                                 "hhs_same1yr_renter_per", "hhs_same1yr_renter_per_moe",
                                                 "hhs_same1yr_75kminus_per", "hhs_same1yr_75kminus_per_moe",
                                                 "hhs_same1yr_75kplus_per", "hhs_same1yr_75kplus_per_moe",
                                                 "tenure_vacant_rent_per", "tenure_vacant_rent_per_moe",
                                                 "tenure_vacant_sale_per", "tenure_vacant_sale_per_moe")]
Hous_i_county_output3 <- melt(Hous_i_county_output2, id.vars="group")
Hous_i_county_output3$year <- i
Houslist1[[i]] <- Hous_i_county_output3
  
#state level 
Hous_i_state <- get_acs(geography = "state", 
                        variables = Vars5, 
                        state = "CA", 
                        year = i)
  
#subset dataframe for selectemp tracts
Hous_i_state_input <- Hous_i_state[-1]
colnames(Hous_i_state_input)[1:2] <- c("group", "var")

Hous_i_state_input1 <- Hous_i_state_input %>%
                           mutate(var = gsub("B25070_007", "hhs_rent30plus", var)) %>%
                           mutate(var = gsub("B25070_008", "hhs_rent30plus", var)) %>%
                           mutate(var = gsub("B25070_009", "hhs_rent30plus", var)) %>%
                           mutate(var = gsub("B25070_010", "hhs_rent30plus", var)) %>%
                           mutate(var = gsub("B25091_007", "hhs_mort30plus", var)) %>%
                           mutate(var = gsub("B25091_008", "hhs_mort30plus", var)) %>%
                           mutate(var = gsub("B25091_009", "hhs_mort30plus", var)) %>%
                           mutate(var = gsub("B25091_010", "hhs_mort30plus", var)) %>%
                           mutate(var = gsub("B25014_005", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B25014_006", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B25014_007", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B25014_011", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B25014_012", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B25014_013", "hhs_occup_1plus", var)) %>%
                           mutate(var = gsub("B07010_013", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_015", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_016", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_017", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_018", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_019", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_020", "hhs_same1yr_75kminus", var)) %>%
                           mutate(var = gsub("B07010_021", "hhs_same1yr_75kminus", var))
Hous_i_state_input2 <- subset(Hous_i_state_input, Hous_i_state_input$var %in% c("B25070_010","B25091_010"))
Hous_i_state_input3 <- subset(Hous_i_state_input, Hous_i_state_input$var %in% c("B25014_005","B25014_006",
                                                                                "B25014_007","B25014_011",
                                                                                "B25014_012","B25014_013")) %>%
                                  mutate(var = gsub("B25014_005", "hhs_occup_1plus_owner", var)) %>%
                                  mutate(var = gsub("B25014_006", "hhs_occup_1plus_owner", var)) %>%
                                  mutate(var = gsub("B25014_007", "hhs_occup_1plus_owner", var)) %>%
                                  mutate(var = gsub("B25014_011", "hhs_occup_1plus_renter", var)) %>%
                                  mutate(var = gsub("B25014_012", "hhs_occup_1plus_renter", var)) %>%
                                  mutate(var = gsub("B25014_013", "hhs_occup_1plus_renter", var))
Hous_i_state_input4 <- rbind(Hous_i_state_input1, Hous_i_state_input2, Hous_i_state_input3)

#estimate aggregate results
Hous_i_state_output <- Hous_i_state_input4 %>%
                           group_by(group,var) %>%
                               summarize(sumest = sum(estimate), 
                                         summoe = moe_sum(moe, estimate))
Hous_i_state_output1 <- dcast(melt(Hous_i_state_output, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Hous_i_state_output1$tenure_renter_per <- Hous_i_state_output1$B25003_003_sumest / Hous_i_state_output1$B25003_001_sumest * 100
Hous_i_state_output1$tenure_renter_per_moe <- moe_prop(Hous_i_state_output1$B25003_003_sumest,
                                                       Hous_i_state_output1$B25003_001_sumest,
                                                       Hous_i_state_output1$B25003_003_summoe,
                                                       Hous_i_state_output1$B25003_001_summoe)*100
Hous_i_state_output1$tenure_owner_per <- Hous_i_state_output1$B25003_002_sumest / Hous_i_state_output1$B25003_001_sumest * 100
Hous_i_state_output1$tenure_owner_per_moe <- moe_prop(Hous_i_state_output1$B25003_002_sumest,
                                                      Hous_i_state_output1$B25003_001_sumest,
                                                      Hous_i_state_output1$B25003_002_summoe,
                                                      Hous_i_state_output1$B25003_001_summoe)*100
Hous_i_state_output1$hhs_rent30plus_per <- Hous_i_state_output1$hhs_rent30plus_sumest / Hous_i_state_output1$B25070_001_sumest * 100
Hous_i_state_output1$hhs_rent30plus_per_moe <- moe_prop(Hous_i_state_output1$hhs_rent30plus_sumest,
                                                        Hous_i_state_output1$B25070_001_sumest,
                                                        Hous_i_state_output1$hhs_rent30plus_summoe,
                                                        Hous_i_state_output1$B25070_001_summoe)*100
Hous_i_state_output1$hhs_rent50plus_per <- Hous_i_state_output1$B25070_010_sumest / Hous_i_state_output1$B25070_001_sumest * 100
Hous_i_state_output1$hhs_rent50plus_per_moe <- moe_prop(Hous_i_state_output1$B25070_010_sumest,
                                                        Hous_i_state_output1$B25070_001_sumest,
                                                        Hous_i_state_output1$B25070_010_summoe,
                                                        Hous_i_state_output1$B25070_001_summoe)*100
Hous_i_state_output1$hhs_mort30plus_per <- Hous_i_state_output1$hhs_mort30plus_sumest / Hous_i_state_output1$B25091_001_sumest * 100
Hous_i_state_output1$hhs_mort30plus_per_moe <- moe_prop(Hous_i_state_output1$hhs_mort30plus_sumest,
                                                        Hous_i_state_output1$B25091_001_sumest,
                                                        Hous_i_state_output1$hhs_mort30plus_summoe,
                                                        Hous_i_state_output1$B25091_001_summoe)*100
Hous_i_state_output1$hhs_mort50plus_per <- Hous_i_state_output1$B25091_010_sumest / Hous_i_state_output1$B25091_001_sumest * 100
Hous_i_state_output1$hhs_mort50plus_per_moe <- moe_prop(Hous_i_state_output1$B25091_010_sumest,
                                                        Hous_i_state_output1$B25091_001_sumest,
                                                        Hous_i_state_output1$B25091_010_summoe,
                                                        Hous_i_state_output1$B25091_001_summoe)*100
Hous_i_state_output1$hhs_occup_1plus_per <- Hous_i_state_output1$hhs_occup_1plus_sumest / Hous_i_state_output1$B25014_001_sumest * 100
Hous_i_state_output1$hhs_occup_1plus_per_moe <- moe_prop(Hous_i_state_output1$hhs_occup_1plus_sumest,
                                                         Hous_i_state_output1$B25014_001_sumest,
                                                         Hous_i_state_output1$hhs_occup_1plus_summoe,
                                                         Hous_i_state_output1$B25014_001_summoe)*100
Hous_i_state_output1$hhs_occup_1plus_owner_per <- Hous_i_state_output1$hhs_occup_1plus_owner_sumest / Hous_i_state_output1$B25014_001_sumest * 100
Hous_i_state_output1$hhs_occup_1plus_owner_per_moe <- moe_prop(Hous_i_state_output1$hhs_occup_1plus_owner_sumest,
                                                               Hous_i_state_output1$B25014_001_sumest,
                                                               Hous_i_state_output1$hhs_occup_1plus_owner_summoe,
                                                               Hous_i_state_output1$B25014_001_summoe)*100
Hous_i_state_output1$hhs_occup_1plus_renter_per <- Hous_i_state_output1$hhs_occup_1plus_renter_sumest / Hous_i_state_output1$B25014_001_sumest * 100
Hous_i_state_output1$hhs_occup_1plus_renter_per_moe <- moe_prop(Hous_i_state_output1$hhs_occup_1plus_renter_sumest,
                                                                Hous_i_state_output1$B25014_001_sumest,
                                                                Hous_i_state_output1$hhs_occup_1plus_renter_summoe,
                                                                Hous_i_state_output1$B25014_001_summoe)*100
Hous_i_state_output1$hhs_same1yr_owner_per <- Hous_i_state_output1$B07013_005_sumest / Hous_i_state_output1$B07013_001_sumest * 100
Hous_i_state_output1$hhs_same1yr_owner_per_moe <- moe_prop(Hous_i_state_output1$B07013_005_sumest,
                                                           Hous_i_state_output1$B07013_001_sumest,
                                                           Hous_i_state_output1$B07013_005_summoe,
                                                           Hous_i_state_output1$B07013_001_summoe)*100
Hous_i_state_output1$hhs_same1yr_renter_per <- Hous_i_state_output1$B07013_006_sumest / Hous_i_state_output1$B07013_001_sumest * 100
Hous_i_state_output1$hhs_same1yr_renter_per_moe <- moe_prop(Hous_i_state_output1$B07013_006_sumest,
                                                            Hous_i_state_output1$B07013_001_sumest,
                                                            Hous_i_state_output1$B07013_006_summoe,
                                                            Hous_i_state_output1$B07013_001_summoe)*100
Hous_i_state_output1$hhs_same1yr_75kminus_per <- Hous_i_state_output1$hhs_same1yr_75kminus_sumest / Hous_i_state_output1$B07010_001_sumest * 100
Hous_i_state_output1$hhs_same1yr_75kminus_per_moe <- moe_prop(Hous_i_state_output1$hhs_same1yr_75kminus_sumest,
                                                              Hous_i_state_output1$B07010_001_sumest,
                                                              Hous_i_state_output1$hhs_same1yr_75kminus_summoe,
                                                              Hous_i_state_output1$B07010_001_summoe)*100
Hous_i_state_output1$hhs_same1yr_75kplus_per <- Hous_i_state_output1$B07010_022_sumest / Hous_i_state_output1$B07010_001_sumest * 100
Hous_i_state_output1$hhs_same1yr_75kplus_per_moe <- moe_prop(Hous_i_state_output1$B07010_022_sumest,
                                                             Hous_i_state_output1$B07010_001_sumest,
                                                             Hous_i_state_output1$B07010_022_summoe,
                                                             Hous_i_state_output1$B07010_001_summoe)*100
Hous_i_state_output1$tenure_vacant_rent_per <- Hous_i_state_output1$B25004_002_sumest / Hous_i_state_output1$B25002_001_sumest * 100
Hous_i_state_output1$tenure_vacant_rent_per_moe <- moe_prop(Hous_i_state_output1$B25004_002_sumest,
                                                             Hous_i_state_output1$B25002_001_sumest,
                                                             Hous_i_state_output1$B25004_002_summoe,
                                                             Hous_i_state_output1$B25002_001_summoe)*100

Hous_i_state_output1$tenure_vacant_sale_per <- Hous_i_state_output1$B25004_004_sumest / Hous_i_state_output1$B25002_001_sumest * 100
Hous_i_state_output1$tenure_vacant_sale_per_moe <- moe_prop(Hous_i_state_output1$B25004_004_sumest,
                                                            Hous_i_state_output1$B25002_001_sumest,
                                                            Hous_i_state_output1$B25004_004_summoe,
                                                            Hous_i_state_output1$B25002_001_summoe)*100
Hous_i_state_output2 <- Hous_i_state_output1[c("group","tenure_renter_per", "tenure_renter_per_moe",
                                               "tenure_owner_per", "tenure_owner_per_moe",
                                               "hhs_rent30plus_per", "hhs_rent30plus_per_moe",
                                               "hhs_rent50plus_per", "hhs_rent50plus_per_moe",
                                               "hhs_mort30plus_per", "hhs_mort30plus_per_moe",
                                               "hhs_mort50plus_per", "hhs_mort50plus_per_moe",
                                               "hhs_occup_1plus_per", "hhs_occup_1plus_per_moe",
                                               "hhs_occup_1plus_owner_per", "hhs_occup_1plus_owner_per_moe",
                                               "hhs_occup_1plus_renter_per", "hhs_occup_1plus_renter_per_moe",
                                               "hhs_same1yr_owner_per", "hhs_same1yr_owner_per_moe",
                                               "hhs_same1yr_renter_per", "hhs_same1yr_renter_per_moe",
                                               "hhs_same1yr_75kminus_per", "hhs_same1yr_75kminus_per_moe",
                                               "hhs_same1yr_75kplus_per", "hhs_same1yr_75kplus_per_moe",
                                               "tenure_vacant_rent_per", "tenure_vacant_rent_per_moe",
                                               "tenure_vacant_sale_per", "tenure_vacant_sale_per_moe")] 
Hous_i_state_output3 <- melt(Hous_i_state_output2, id.vars="group")
Hous_i_state_output3$year <- i
Houslist2[[i]] <- Hous_i_state_output3
  
}

#Combine results from all three geographical scales
Hous_results <- rbind(do.call(rbind, Houslist), do.call(rbind, Houslist1), do.call(rbind, Houslist2))

###
#Variable Group 6 - transport indicators
###

#define variables needed
Vars6 <- c("B08201_001E", "B08201_001M",
           "B08201_003E", "B08201_003M",
           "B08301_001E", "B08301_001M",
           "B08301_002E", "B08301_002M",
           "B08301_003E", "B08301_003M",
           "B08301_004E", "B08301_004M",
           "B08301_010E", "B08301_010M",
           "B08301_019E", "B08301_019M",
           "B08301_018E", "B08301_018M",
           "B08301_016E", "B08301_016M",
           "B08301_017E", "B08301_017M",
           "B08301_020E", "B08301_020M")

#Extract data 
#create a loop for multiple-year data
Tranlist <- list()
Tranlist1 <- list()
Tranlist2 <- list()

for (i in years){
  
#cTransus tract level 
Tran_i <- get_acs(geography = "tract", 
                  variables = Vars6, 
                  state = "CA", 
                  year = i)
  
#subset dataframe for selectemp tracts
Tran_i_input <- subset(Tran_i, Tran_i$GEOID %in% Tracts$GEOID)
Tran_i_input <- merge(Tran_i_input, Tracts)
colnames(Tran_i_input)[3] <- "var"
Tran_i_input1 <- Tran_i_input %>%
                     mutate(var = gsub("B08301_016", "commute_other", var)) %>%
                     mutate(var = gsub("B08301_017", "commute_other", var)) %>%
                     mutate(var = gsub("B08301_020", "commute_other", var))

#estimate aggregate results
Tran_i_output <- Tran_i_input1 %>%
                    group_by(group,var) %>%
                       summarize(sumest = sum(estimate), 
                                 summoe = moe_sum(moe, estimate))
Tran_i_output1 <- dcast(melt(Tran_i_output, id.vars=1:2), 
                        group ~ var + variable, fun.aggregate = sum)
Tran_i_output1$hhs_1veh_per <- Tran_i_output1$B08201_003_sumest / Tran_i_output1$B08201_001_sumest * 100
Tran_i_output1$hhs_1veh_per_moe <- moe_prop(Tran_i_output1$B08201_003_sumest,
                                            Tran_i_output1$B08201_001_sumest,
                                            Tran_i_output1$B08201_003_summoe,
                                            Tran_i_output1$B08201_001_summoe)*100
Tran_i_output1$commute_car_per <- Tran_i_output1$B08301_002_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_car_per_moe <- moe_prop(Tran_i_output1$B08301_002_sumest,
                                               Tran_i_output1$B08301_001_sumest,
                                               Tran_i_output1$B08301_002_summoe,
                                               Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_car_alone_per <- Tran_i_output1$B08301_003_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_car_alone_per_moe <- moe_prop(Tran_i_output1$B08301_003_sumest,
                                                     Tran_i_output1$B08301_001_sumest,
                                                     Tran_i_output1$B08301_003_summoe,
                                                     Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_car_carpool_per <- Tran_i_output1$B08301_004_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_car_carpool_per_moe <- moe_prop(Tran_i_output1$B08301_004_sumest,
                                                       Tran_i_output1$B08301_001_sumest,
                                                       Tran_i_output1$B08301_004_summoe,
                                                       Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_transit_per <- Tran_i_output1$B08301_010_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_transit_per_moe <- moe_prop(Tran_i_output1$B08301_010_sumest,
                                                   Tran_i_output1$B08301_001_sumest,
                                                   Tran_i_output1$B08301_010_summoe,
                                                   Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_walk_per <- Tran_i_output1$B08301_019_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_walk_per_moe <- moe_prop(Tran_i_output1$B08301_019_sumest,
                                                Tran_i_output1$B08301_001_sumest,
                                                Tran_i_output1$B08301_019_summoe,
                                                Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_bike_per <- Tran_i_output1$B08301_018_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_bike_per_moe <- moe_prop(Tran_i_output1$B08301_018_sumest,
                                                Tran_i_output1$B08301_001_sumest,
                                                Tran_i_output1$B08301_018_summoe,
                                                Tran_i_output1$B08301_001_summoe)*100
Tran_i_output1$commute_other_per <- Tran_i_output1$commute_other_sumest / Tran_i_output1$B08301_001_sumest * 100
Tran_i_output1$commute_other_per_moe <- moe_prop(Tran_i_output1$commute_other_sumest,
                                                 Tran_i_output1$B08301_001_sumest,
                                                 Tran_i_output1$commute_other_summoe,
                                                 Tran_i_output1$B08301_001_summoe)*100
Tran_i_output2 <- Tran_i_output1[c("group","hhs_1veh_per", "hhs_1veh_per_moe",
                                   "commute_car_per", "commute_car_per_moe",
                                   "commute_car_alone_per", "commute_car_alone_per_moe",
                                   "commute_car_carpool_per", "commute_car_carpool_per_moe",
                                   "commute_transit_per", "commute_transit_per_moe",
                                   "commute_walk_per", "commute_walk_per_moe",
                                   "commute_bike_per", "commute_bike_per_moe",
                                   "commute_other_per", "commute_other_per_moe")]
Tran_i_output3 <- melt(Tran_i_output2, id.vars="group")
Tran_i_output3$year <- i
Tranlist[[i]] <- Tran_i_output3
  
#county level 
Tran_i_county <- get_acs(geography = "county", 
                         variables = Vars6, 
                         state = "CA", 
                         year = i)

#subset dataframe for selectemp tracts
Tran_i_county_input <- Tran_i_county[-1] %>% mutate(NAME = gsub(", California", "", NAME))
colnames(Tran_i_county_input)[1:2] <- c("group", "var")
Tran_i_county_input <- subset(Tran_i_county_input, Tran_i_county_input$group %in% Counties)
Tran_i_county_input1 <- Tran_i_county_input %>%
                            mutate(var = gsub("B08301_016", "commute_other", var)) %>%
                            mutate(var = gsub("B08301_017", "commute_other", var)) %>%
                            mutate(var = gsub("B08301_020", "commute_other", var))

#estimate aggregate results
Tran_i_county_output <- Tran_i_county_input1 %>%
                            group_by(group,var) %>%
                                summarize(sumest = sum(estimate), 
                                          summoe = moe_sum(moe, estimate))
Tran_i_county_output1 <- dcast(melt(Tran_i_county_output, id.vars=1:2), 
                               group ~ var + variable, fun.aggregate = sum)
Tran_i_county_output1$hhs_1veh_per <- Tran_i_county_output1$B08201_003_sumest / Tran_i_county_output1$B08201_001_sumest * 100
Tran_i_county_output1$hhs_1veh_per_moe <- moe_prop(Tran_i_county_output1$B08201_003_sumest,
                                                   Tran_i_county_output1$B08201_001_sumest,
                                                   Tran_i_county_output1$B08201_003_summoe,
                                                   Tran_i_county_output1$B08201_001_summoe)*100
Tran_i_county_output1$commute_car_per <- Tran_i_county_output1$B08301_002_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_car_per_moe <- moe_prop(Tran_i_county_output1$B08301_002_sumest,
                                                      Tran_i_county_output1$B08301_001_sumest,
                                                      Tran_i_county_output1$B08301_002_summoe,
                                                      Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_car_alone_per <- Tran_i_county_output1$B08301_003_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_car_alone_per_moe <- moe_prop(Tran_i_county_output1$B08301_003_sumest,
                                                            Tran_i_county_output1$B08301_001_sumest,
                                                            Tran_i_county_output1$B08301_003_summoe,
                                                            Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_car_carpool_per <- Tran_i_county_output1$B08301_004_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_car_carpool_per_moe <- moe_prop(Tran_i_county_output1$B08301_004_sumest,
                                                              Tran_i_county_output1$B08301_001_sumest,
                                                              Tran_i_county_output1$B08301_004_summoe,
                                                              Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_transit_per <- Tran_i_county_output1$B08301_010_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_transit_per_moe <- moe_prop(Tran_i_county_output1$B08301_010_sumest,
                                                          Tran_i_county_output1$B08301_001_sumest,
                                                          Tran_i_county_output1$B08301_010_summoe,
                                                          Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_walk_per <- Tran_i_county_output1$B08301_019_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_walk_per_moe <- moe_prop(Tran_i_county_output1$B08301_019_sumest,
                                                       Tran_i_county_output1$B08301_001_sumest,
                                                       Tran_i_county_output1$B08301_019_summoe,
                                                       Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_bike_per <- Tran_i_county_output1$B08301_018_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_bike_per_moe <- moe_prop(Tran_i_county_output1$B08301_018_sumest,
                                                       Tran_i_county_output1$B08301_001_sumest,
                                                       Tran_i_county_output1$B08301_018_summoe,
                                                       Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output1$commute_other_per <- Tran_i_county_output1$commute_other_sumest / Tran_i_county_output1$B08301_001_sumest * 100
Tran_i_county_output1$commute_other_per_moe <- moe_prop(Tran_i_county_output1$commute_other_sumest,
                                                        Tran_i_county_output1$B08301_001_sumest,
                                                        Tran_i_county_output1$commute_other_summoe,
                                                        Tran_i_county_output1$B08301_001_summoe)*100
Tran_i_county_output2 <- Tran_i_county_output1[c("group","hhs_1veh_per", "hhs_1veh_per_moe",
                                                 "commute_car_per", "commute_car_per_moe",
                                                 "commute_car_alone_per", "commute_car_alone_per_moe",
                                                 "commute_car_carpool_per", "commute_car_carpool_per_moe",
                                                 "commute_transit_per", "commute_transit_per_moe",
                                                 "commute_walk_per", "commute_walk_per_moe",
                                                 "commute_bike_per", "commute_bike_per_moe",
                                                 "commute_other_per", "commute_other_per_moe")]
Tran_i_county_output3 <- melt(Tran_i_county_output2, id.vars="group")
Tran_i_county_output3$year <- i
Tranlist1[[i]] <- Tran_i_county_output3
  
#state level 
Tran_i_state <- get_acs(geography = "state", 
                        variables = Vars6, 
                        state = "CA", 
                        year = i)
  
#subset dataframe for selectemp tracts
Tran_i_state_input <- Tran_i_state[-1]
colnames(Tran_i_state_input)[1:2] <- c("group", "var")
Tran_i_state_input1 <- Tran_i_state_input %>%
                           mutate(var = gsub("B08301_016", "commute_other", var)) %>%
                           mutate(var = gsub("B08301_017", "commute_other", var)) %>%
                           mutate(var = gsub("B08301_020", "commute_other", var))

#estimate aggregate results
Tran_i_state_output <- Tran_i_state_input1 %>%
                           group_by(group,var) %>%
                               summarize(sumest = sum(estimate), 
                                         summoe = moe_sum(moe, estimate))
Tran_i_state_output1 <- dcast(melt(Tran_i_state_output, id.vars=1:2), 
                              group ~ var + variable, fun.aggregate = sum)
Tran_i_state_output1$hhs_1veh_per <- Tran_i_state_output1$B08201_003_sumest / Tran_i_state_output1$B08201_001_sumest * 100
Tran_i_state_output1$hhs_1veh_per_moe <- moe_prop(Tran_i_state_output1$B08201_003_sumest,
                                                  Tran_i_state_output1$B08201_001_sumest,
                                                  Tran_i_state_output1$B08201_003_summoe,
                                                  Tran_i_state_output1$B08201_001_summoe)*100
Tran_i_state_output1$commute_car_per <- Tran_i_state_output1$B08301_002_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_car_per_moe <- moe_prop(Tran_i_state_output1$B08301_002_sumest,
                                                     Tran_i_state_output1$B08301_001_sumest,
                                                     Tran_i_state_output1$B08301_002_summoe,
                                                     Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_car_alone_per <- Tran_i_state_output1$B08301_003_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_car_alone_per_moe <- moe_prop(Tran_i_state_output1$B08301_003_sumest,
                                                           Tran_i_state_output1$B08301_001_sumest,
                                                           Tran_i_state_output1$B08301_003_summoe,
                                                           Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_car_carpool_per <- Tran_i_state_output1$B08301_004_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_car_carpool_per_moe <- moe_prop(Tran_i_state_output1$B08301_004_sumest,
                                                             Tran_i_state_output1$B08301_001_sumest,
                                                             Tran_i_state_output1$B08301_004_summoe,
                                                             Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_transit_per <- Tran_i_state_output1$B08301_010_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_transit_per_moe <- moe_prop(Tran_i_state_output1$B08301_010_sumest,
                                                         Tran_i_state_output1$B08301_001_sumest,
                                                         Tran_i_state_output1$B08301_010_summoe,
                                                         Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_walk_per <- Tran_i_state_output1$B08301_019_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_walk_per_moe <- moe_prop(Tran_i_state_output1$B08301_019_sumest,
                                                      Tran_i_state_output1$B08301_001_sumest,
                                                      Tran_i_state_output1$B08301_019_summoe,
                                                      Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_bike_per <- Tran_i_state_output1$B08301_018_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_bike_per_moe <- moe_prop(Tran_i_state_output1$B08301_018_sumest,
                                                      Tran_i_state_output1$B08301_001_sumest,
                                                      Tran_i_state_output1$B08301_018_summoe,
                                                      Tran_i_state_output1$B08301_001_summoe)*100
Tran_i_state_output1$commute_other_per <- Tran_i_state_output1$commute_other_sumest / Tran_i_state_output1$B08301_001_sumest * 100
Tran_i_state_output1$commute_other_per_moe <- moe_prop(Tran_i_state_output1$commute_other_sumest,
                                                       Tran_i_state_output1$B08301_001_sumest,
                                                       Tran_i_state_output1$commute_other_summoe,
                                                       Tran_i_state_output1$B08301_001_summoe)*100

Tran_i_state_output2 <- Tran_i_state_output1[c("group","hhs_1veh_per", "hhs_1veh_per_moe",
                                               "commute_car_per", "commute_car_per_moe",
                                               "commute_car_alone_per", "commute_car_alone_per_moe",
                                               "commute_car_carpool_per", "commute_car_carpool_per_moe",
                                               "commute_transit_per", "commute_transit_per_moe",
                                               "commute_walk_per", "commute_walk_per_moe",
                                               "commute_bike_per", "commute_bike_per_moe",
                                               "commute_other_per", "commute_other_per_moe")]
Tran_i_state_output3 <- melt(Tran_i_state_output2, id.vars="group")
Tran_i_state_output3$year <- i
Tranlist2[[i]] <- Tran_i_state_output3
  
}

#Combine results from all three geographical scales
Tran_results <- rbind(do.call(rbind, Tranlist), do.call(rbind, Tranlist1), do.call(rbind, Tranlist2))

#Total car variable not needed, remove results
Tran_results1 <- Tran_results[!Tran_results$variable%in%c("commute_car_per","commute_car_per_moe"),]


###
#Generate output
###

#Combine results from all six indicator groups
TCC_results <- rbind(Dem_results, Econ_results, En_results, Hlth_results, Hous_results, Tran_results1)

#Perform significance test across multiple-year census data with 2013 as the base year
TCC_results$var <- gsub("_moe", "", TCC_results$variable)
TCC_results1 <- TCC_results %>% 
                    rowwise() %>% 
                        mutate(category = grepl("moe", variable))

TCC_results1$category <- gsub("FALSE", "estimate", TCC_results1$category)
TCC_results1$category <- gsub("TRUE", "MOE", TCC_results1$category)

TCC_results2 <- spread(subset(TCC_results1, select = -c(variable)), key = category, value = value)
TCC_results2$var <- factor(TCC_results2$var, levels = c("Pop_tot", "Hisp_per", "NonHisp_whit_per", 
                                                        "NonHisp_color_all_per", "NonHisp_color_other_per","NonHisp_blac_per", "NonHisp_asia_per",
                                                        "NonHisp_PI_per", "NonHisp_AI_per", "NonHisp_two_per", "NonHisp_other_per",
                                                        "bornFor_per", "bornAs_per", "bornAf_per", "bornLA_per",
                                                        "income_median", "hhs_125kplus_per", "poverty_per", 
                                                        "ed_belowhs_per", "ed_aboveba_per", "emp_16plus_per",
                                                        "heating_elec_per", "heating_utilitygas_per", "heating_other_fossil_per", 
                                                        "heating_other_nonfossil_per", "heating_no_per", "hlth_ins_per",
                                                        "hlth_ins_prv_per", "hlth_ins_pub_per","tenure_renter_per", "tenure_owner_per",
                                                        "hhs_rent30plus_per",  "hhs_rent50plus_per", "hhs_mort30plus_per",
                                                        "hhs_mort50plus_per", "hhs_occup_1plus_per", "hhs_occup_1plus_owner_per",
                                                        "hhs_occup_1plus_renter_per", "hhs_same1yr_owner_per", "hhs_same1yr_renter_per",
                                                        "hhs_same1yr_75kminus_per", "hhs_same1yr_75kplus_per", "tenure_vacant_rent_per",
                                                        "tenure_vacant_sale_per", "hhs_1veh_per", 
                                                        "commute_car_alone_per", "commute_car_carpool_per", "commute_transit_per",
                                                        "commute_walk_per", "commute_bike_per", "commute_other_per"))
TCC_results3 <- TCC_results2 %>% 
                    group_by(group, var) %>% 
                        mutate(growth_per_base2013 = (estimate - first(estimate))/first(estimate) * 100) %>%
                        mutate(statsig_90per_value = (estimate - first(estimate)) / ((year - first(year))/5)^(1/2) / ((MOE/1.645)^2 + (first(MOE)/1.645)^2)^(1/2)) %>%
                        mutate(statsig_90per = ifelse( statsig_90per_value < 1.645, "nonsignificant", "significant")) %>%
                        mutate(statsig_95per_value = (estimate - first(estimate)) / ((year - first(year))/5)^(1/2) / ((MOE/1.96)^2 + (first(MOE)/1.96)^2)^(1/2)) %>%
                        mutate(statsig_95per = ifelse( statsig_95per_value < 1.96, "nonsignificant", "significant")) %>%
                        arrange(group, var)

#Generate output file
write.csv(TCC_results3, file = "TCC_fullresults.csv")




