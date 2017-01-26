
# Convert drainage data to mm and kg N/ha for Cobs
# step 1
#convert flow from ft3/area to lt/area by multiplying with 28.316
# step 2
#convert lt/area to lt/m2  (which is equivalent to mm) by dividing with 836.37
# step 3
#multiply with N conc (mg/lt)
# step 4
#adjust units (divide by 100)
# step 5
#result in kg N/ha leaching

#Link with example: https://dl.dropboxusercontent.com/u/57763837/Calculations%20of%20Drainage_Nitrate_COBS_2009_2014%20%28Sotirios%202016.05.24%29.xlsx

# 1 ft3 = .001m3 = 28.32 L water
# 1 ppm = 1 mg N/L water
# COBs plot is 200 ft by 45 ft, assuming half goes through the tile = 836.37 m^2

library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotrix)

nitwat<-read.csv("Data/Drainage_Nitrate_COBS_2009_2014.csv", header=TRUE)

nitwat$flow_L<-nitwat$flow_ft3*28.32
nitwat$flow_mm<-(nitwat$flow_ft3/(45*200)*12)*25.4
#nitwat$NO3Nkg_ha<- (nitwat$flow_ft3 * 28.32 * nitwat$NO3Nppm * .000001)*5.988

nitwat$NO3Nkg_ha<-((nitwat$flow_ft3 * 28.32)/836.37)*nitwat$NO3Nppm/100




byyear<-nitwat%>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))%>%
  mutate(year = year(date))%>%
  mutate(doy = yday(date))%>%
  group_by(year, trt, sump)%>%
  mutate(sumNO3Nkg_ha = NO3Nkg_ha %>% mapvalues(NA, 0) %>% cumsum)%>%
  mutate(sumflow_mm = flow_mm %>% mapvalues(NA, 0) %>% cumsum)%>%
  group_by(trt, year, date, doy)%>%
  summarise_each(funs(mean, std.error(., na.rm=TRUE)))

byyear$date<-format(as.Date(byyear$date), "%d/%m/%Y")
byyear<-byyear[order(as.Date(byyear$date,format="%d/%m/%Y")),,drop=FALSE]


ggplot()+
  geom_ribbon(data=SC, aes(x=doy, ymin = sumflow_mm_mean - sumflow_mm_std.error, ymax = sumflow_mm_mean + sumflow_mm_std.error))+
  geom_line(data=SC, aes(x = doy, y = sumflow_mm_mean, group = trt, colour = trt))+
  facet_wrap(~year)

ggplot(byyear, aes(x = doy, y =sumflow_mm, group = trt, colour = trt))+
  geom_line()+
  facet_wrap(~year)

prairie<-byyear%>%
  filter(trt == "P")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))

prairieplus<-byyear%>%
  filter(trt == "PF")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))

CC<-byyear%>%
  filter(trt == "CC")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))

CCW<-byyear%>%
  filter(trt == "CCW")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))

C2<-byyear%>%
  filter(trt == "C2")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))

S2<-byyear%>%
  filter(trt == "S2")%>%
  select(date, year, doy, NO3Nppm_mean, flow_mm_mean, sumflow_mm_mean, NO3Nkg_ha_mean, sumNO3Nkg_ha_mean, NO3Nppm_std.error, flow_mm_std.error, sumflow_mm_std.error, NO3Nkg_ha_std.error, sumNO3Nkg_ha_std.error)%>%
  ungroup()%>%
  mutate(NO3Nppm_mean = round(NO3Nppm_mean, digits = 2))%>%
  mutate(flow_mm_mean = round(flow_mm_mean, digits = 2))%>%
  mutate(sumflow_mm_mean = round(sumflow_mm_mean, digits = 2))%>%
  mutate(NO3Nkg_ha_mean = round(NO3Nkg_ha_mean, digits = 2))%>%
  mutate(sumNO3Nkg_ha_mean = round(sumNO3Nkg_ha_mean, digits = 2))%>%
  mutate(NO3Nppm_std.error = round(NO3Nppm_std.error, digits = 2))%>%
  mutate(flow_mm_std.error = round(flow_mm_std.error, digits = 2))%>%
  mutate(sumflow_mm_std.error = round(sumflow_mm_std.error, digits = 2))%>%
  mutate(NO3Nkg_ha_std.error = round(NO3Nkg_ha_std.error, digits = 2))%>%
  mutate(sumNO3Nkg_ha_std.error = round(sumNO3Nkg_ha_std.error, digits = 2))



#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = P
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-Prairie Drainage and Leaching.out")
write.table(prairie, file = "Data-Prairie Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)



#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = PF
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-PrairieFertilized Drainage and Leaching.out")
write.table(prairieplus, file = "Data-PrairieFertilized Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)



#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = CC
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-CC Drainage and Leaching.out")
write.table(CC, file = "Data-CC Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)


#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = CCW
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-CCW Drainage and Leaching.out")
write.table(CCW, file = "Data-CCW Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)


#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = C2
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-C2 Drainage and Leaching.out")
write.table(C2, file = "Data-C2 Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)


#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = S2
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-S2 Drainage and Leaching.out")
write.table(S2, file = "Data-S2 Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)


even<-filter(C2, year %in% c("2010","2012","2014"))
odd<-filter(S2, year %in% c ("2009", "2011", "2013"))
                           
CS<-rbind(even, odd)
CS<-CS[order(as.Date(CS$date, format="%d/%m/%Y")),,drop=FALSE]

seven<-filter(S2, year %in% c("2010","2012","2014"))
sodd<-filter(C2, year %in% c ("2009", "2011", "2013"))

SC<-rbind(seven, sodd)
SC<-SC[order(as.Date(SC$date, format="%d/%m/%Y")),,drop=FALSE]

#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = CS
 Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
  ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-CS Drainage and Leaching.out")
write.table(CS, file = "Data-CS Drainage and Leaching.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)

#Make an APSIM .out header
header<-paste0("APSIM = 7.7
Title = SC
Title     date         year       day      NO3Nconc subsurface_drain sum_subsurface_drain  leach_    sum_leach_     NO3Nconc_stderr   subsurface_drain_stderr     sum_subsurface_drain_stderr    leach__stderr    sum_leach__stderr        
 ()   (dd/mm/yyyy)     ()         ()         ()         (mm)            (mm)              (kg/ha)     (kg/ha)           ()                  (mm)                        (mm)                      (kg/ha)           (kg/ha)")      

#Write the files
write(header, file = "Data-SC Drainage and Leaching v2.out")
write.table(SC, file = "Data-SC Drainage and Leaching v2.out", 
            sep=paste(rep(" ",6), collapse=""), na="?", append=TRUE, quote=F, row.names = FALSE, 
            col.names=FALSE)
