rm(list = ls())
library(readstata13)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)

setwd('C:/Users/user/Desktop/cleanNassepV1')

nassepvUpdate<-read.dta13("RawData/NassepSixUpdate.dta")
nass_0<-nassepvUpdate %>% select(clu00,clu01,clu02,clu03,clu04,clu05,clu06,clu07,clu08,
                                 clu09,clu10,gps_clus__Latitude,gps_clus__Longitude,
                                 gps_clus__Accuracy,date_int,v1,v2, qw,hs1,perhs1,
                                 hs2,pp_1, perhs2,hs3,perhs3, c0_0,c1_1,c2_2,
                                 c3_3,n1,
                                 c1a,c1b,cc2,dist_ch, cc3,time_ch,c4a,
                                 c4b,dist_tow,c5a,c5b__1,c5b__2,c5b__3,
                                 c5b__4,c5b__5,c5b__6, c5b__7, c6,c7,c8,
                                 c9,c10, c11a,per1,c11b,per2,
                                 c11c,per3,c12,c13,c13b,chief,chieftelphone,
                                 asschief,asschieftelphone,villageelder,
                                 villageeldertelephone,date_end)

nass1<-nass_0 %>% rename(ClusterNumber=clu00,County=clu01,Subcounty=clu02,Division=clu03,
      Location=clu04,SubLocation=clu05,GEOCODE=clu06,EAName_1=clu07,EAtype_Residence=clu08,
      EAStatus=clu09,frame_component=clu10,clu_gps_lat=gps_clus__Latitude,clu_gps_long=gps_clus__Longitude,
      clu_gps_accuracy=gps_clus__Accuracy,date_start_interview=date_int,
      Total_Count_homesteads=v1,Total_households_Homesteads=v2,
      cluster_req_seg=qw,households_Seg_1=hs1,Percent_seg_1=perhs1,
      households_Seg_2=hs2,Pop_total_minus_pop_1=pp_1,
      Percent_seg_2=perhs2,hh_Segmet3=hs3,Percent_seg_3=perhs3,
      CumPercenths_0=c0_0,CumPerc_1=c1_1,CumPerc_2=c2_2,
      CumPer_3=c3_3,Last_2_dig_cluster=n1,
      #Segmentserial_1=segser1_1,Segmentserial_2=segser2_2,Segmentserial_3=segser3_3,
      #selected_seg_1=ss1_1,selected_seg_2=ss2_2,selected_seg_3=ss3_3,
  Cluster_Listed=c1a,Why_not_listed=c1b,distance_County_HQ_to_cluster=cc2,dist_Km_Metres=dist_ch,
    Time_County_HQ_to_cluster=cc3,time_hours_minutes=time_ch,appr_town_spend_night=c4a,
    Dist_townorfacility_cluster=c4b,distance_in_KM_Metres=dist_tow,clu_req_security=c5a,
    causes_insecurity_1=c5b__1,causes_insecurity_2=c5b__2,causes_insecurity_3=c5b__3,
  causes_insecurity_4=c5b__4,causes_insecurity_5=c5b__5,causes_insecurity_6=c5b__6,
  causes_insecurity_7=c5b__7, mode_transport=c6,terrain_cluster=c7,size_cluster=c8,
  households_within_cluster=c9,current_settlement_cluster=c10,
    native_languages_1=c11a,Per_1_lang=per1,native_languages_2=c11b,Per_2_lang=per2,
  native_languages_3=c11c,Per_3_lang=per3,
    main_econ_act=c12,EAName=c13,name_EA_asknown=c13b,name_chief=chief,telephone_chief=chieftelphone,
    name_Assist_Chief=asschief,telephone_Ast_chief=asschieftelphone,Village_Elder=villageelder,
    telephone_village_elder=villageeldertelephone,Date_End=date_end)

#head(nass1)

write.table(nass1, file="samplers_clean/cluster.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

###structures
structure<- read.dta13("RawData/structure.dta")
#head(structure)
nassepStructure<-merge(nassepvUpdate,structure, by="interview__id")
#head(nassepStructure)
clust2<-nassepStructure %>% select(clu00,structure__id,s01,
                        s03_gps__Latitude, s03_gps__Longitude, s03_gps__Accuracy ,
                        s03_gps__Altitude,  s03_gps__Timestamp, s03_gps_1,
                        s04, s05, s06,s06b)  
#head(clust2)  ##clean structure
clust3<-clust2 %>% rename(Cluster_number=clu00,structure_number=structure__id,name_structure=s01,
                          gps_str_latitude=s03_gps__Latitude, gps_str_longitude=s03_gps__Longitude,
                          gps_str_Accuracy=s03_gps__Accuracy,gps_str_Altitude=s03_gps__Altitude,
                          gps_str_Time=s03_gps__Timestamp,PointGPS=s03_gps_1,
                          comments_str=s04,str_residential=s05, comment_purpose=s06,
                          Feauture_type=s06b)

#View(clust2)
write.table(clust3, file="samplers_clean/structure.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')


household<- read.dta13("RawData/household.dta")
nassepHouseHold<-merge(nassepvUpdate,household, by="interview__id")
#View(nassepHouseHold)
cleanHouseUnsort<-subset(nassepHouseHold, select = c( clu00,clusterpart__id,structure__id,household__id,
                                                      h07,h08,h09,h10,h12,h13,h14,h15,h16,h17,h18,h19,
                                                      h20,h21,h23,h24,h24b,h25,h26b,h28))

house2<-cleanHouseUnsort %>%
  mutate(str_add=case_when(
    clusterpart__id==1 ~0,
    clusterpart__id==2 ~40,
    clusterpart__id==3 ~80,
    clusterpart__id==4 ~120,
    clusterpart__id==5 ~160,
    clusterpart__id==6 ~200
  )) %>%
  group_by(clu00) %>%
  mutate(s02=structure__id+str_add)
housfin<-subset(house2, select = c( clu00,s02,h07,h08,h09,h10,h12,h13,h14,h15,h16,h17,h18,h19,
                                    h20,h21,h23,h24,h24b,h25,h26b,h28 ))
#View(housfin)  ##clean household

hous3<-housfin %>% rename(cluster_number=clu00,Structure_number=s02,serial_num_HU=h07,
                          House_number=h08,HU_occupied=h09,reason_unoccupied=h10,
                          Name_HH_Head=h12,sex_head=h13,occupation=h14,Total_people=h15,
                          below_4_male=h16,below_4_female=h17,Five_seventeen_male=h18,
                          Five_seventeen_female=h19,eighteen_Above_male=h20,
                          eighteen_Above_female=h21,operate_business=h23,
                          Telephone_head=h24,Telephone_head_other=h24b,
                          Telephone_other_member=h25,relationship=h26b,comments=h28)
#View(hous3)

write.table(hous3, file="samplers_clean/household.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')




####for catographers cleaning points
#for feautures
r4<- read_dta("RawData/r4.dta")
#head(r4)
#attach(r4)
#View(r4)
r41<-r4 %>% select(r4__id,ft,Clu_feat,sp,name_feature,gps_f__Latitude, gps_f__Longitude,gps_f__Accuracy,gps_f__Timestamp)
#View(r41)
write.table(r41, file="carto/Features.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##codes here exctract the data to be send to catographers for verification mapping and segmenting
##daat2
s1 <- read_dta("RawData/s.dta")
#View(s1)
##households/structure during quick count
d3<-s1 %>% select(clu00,s11)
#View(d3)
hom_str<-d3 %>% transmute(cluster_number = clu00,lon = str_extract_all(s11,'-\\d\\.\\d+'),lat = str_extract_all(s11, '\\d{2}\\.\\d+')) %>% 
  unnest(cols = everything())
#head(hom_str)
write.table(hom_str, file="carto/Quick_Count_Structure.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##segmentation points


###--------------------------------------------take care
d4<-s1 %>% select(clu00,segselect,p3)
#d4
d4_1<-d4 %>% filter(!is.na(segselect))
#d4_1
seg_point<-d4_1 %>% transmute(cluster_number = clu00,num_seg=segselect, latitude = str_extract_all(p3,'-\\d\\.\\d+'),longitude = str_extract_all(p3, '\\d{2}\\.\\d+')) %>% 
  unnest(cols = everything())

write.table(seg_point, file="carto/SEG_points.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

###-------------------------------------------------take care

##centroid segment 1
d5<-s1 %>% select(clu00,d1)
seg_1<-d5 %>% transmute(cluster_number = clu00,latitude = str_extract_all(d1,'-\\d\\.\\d+'),longitude = str_extract_all(d1, '\\d{2}\\.\\d+')) %>% 
  unnest(cols = everything())

write.table(seg_1, file="carto/segment_1_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##centroid segment 2
d6<-s1 %>% select(clu00,d2)
seg_2<-d6 %>% transmute(cluster_number = clu00,latitude = str_extract_all(d2,'-\\d\\.\\d+'),longitude = str_extract_all(d2, '\\d{2}\\.\\d+')) %>% 
  unnest(cols = everything())

write.table(seg_2, file="carto/segment_2_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##centroid segment 3
d7<-s1 %>% select(clu00,d3)
seg_3<-d7 %>% transmute(cluster_number = clu00,latitude = str_extract_all(d3,'-\\d\\.\\d+'),longitude = str_extract_all(d3, '\\d{2}\\.\\d+')) %>% 
  unnest(cols = everything())

write.table(seg_3, file="carto/segment_3_centroids.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')


###codes for structures listed by the lister and the points picked by the mapper and total households in the structure
hm1 <- read_dta("RawData/hm1.dta")
m1<-hm1 %>% select(interview__id,hm1__id) %>% as.data.frame()
j1<-nassepvUpdate %>% select(interview__id,clu00,v1,v2)%>% as.data.frame()
n1<-m1 %>% left_join(j1, by="interview__id")
n2<-n1 %>% select(clu00,hm1__id,v1,v2) %>%
  rename(cluster_number=clu00, str_id=hm1__id, total_str=v1, tot_hous=v2)%>%as.data.frame()
kk1<-hom_str %>% group_by(cluster_number) %>% mutate(str_id = row_number())%>%as.data.frame()
#left joining when the MAPPER is super data
n3<-kk1 %>% left_join(n2, by=c("cluster_number", "str_id"))
n31<-n3 %>% select(cluster_number,str_id,total_str,tot_hous,lon,lat)%>%
  rename(cluster_number=cluster_number,str_id_MAPPER=str_id, total_str_LISTER=total_str,
         tot_hous_LISTER=tot_hous, lon=lon,lat=lat)

write.table(n31, file="Quality/aMAPPER.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

#left joining when the LISTER is super data
n4<-n2 %>% left_join(kk1, by=c("cluster_number", "str_id"))
n41<-n4 %>% select(cluster_number,str_id,total_str,tot_hous,lon,lat)%>%
  rename(cluster_number=cluster_number,str_id_LISTER=str_id, total_str_LISTER=total_str,
         tot_hous_LISTER=tot_hous, lon=lon,lat=lat)

write.table(n41, file="Quality/aLISTER.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

#full join MAPPER and LISTER to get differences
d<-full_join(n2,kk1, by=c("cluster_number","str_id"))
write.table(d, file="Quality/bothLISTER_MAPPER.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

###confirm that the struictures captured by the MAPPER during quick count compare with the LISTER

#points of households from the mapper
q1<-hom_str %>% group_by(cluster_number) %>% summarise(total_structure_MAPPER=n())%>% as.data.frame()
#Total listed by the LISTER during quick count
p1<-j1 %>% select(clu00, v1) %>% rename(cluster_number=clu00,total_structure_LISTER=v1)%>%as.data.frame()
c <- full_join(p1,q1, by="cluster_number")
write.table(c, file="Quality/LIST_MAP_AGGREGATE.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
##Ends here




