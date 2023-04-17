### These code is a little interlude I wrote to answer some questions
### that Cass had about a specific couple water bodies.

# ==============================================
#
#                   INTERLUDE
#
# ==============================================
#Folks want to know how many data points we have for each waterbody
# and also for each of two watersheds: Murtle Lake and Upper North Thompson River.
# test = rbind(
#   df_sp %>% st_drop_geometry() %>%
#     filter(is.na(LakeNumber)==F) %>%
#     group_by(Watershed, LakeNumber, MONITORING_LOCATION) %>%
#     summarise(MonitLoc_Mean = mean(RESULT, na.rm=T),
#               MonitLoc_Min = min(RESULT, na.rm=T),
#               MonitLoc_Max = max(RESULT, na.rm=T),
#               `Records per Monitoring Station` = n()) %>%
#     mutate(`Waterbody Mean Calcium (mg/L)` = mean(MonitLoc_Mean),
#            `Number of Distinct Monitoring Locations per Waterbody` = n()) %>%
#     group_by(Watershed) %>%
#     mutate(`Subwatershed Mean Calcium (mg/L)` = mean(`Waterbody Mean Calcium (mg/L)`)) %>%
#     rename(WaterbodyNumber = LakeNumber),
#   df_sp %>% st_drop_geometry() %>%
#     filter(is.na(RiverNumber)==F) %>%
#     group_by(Watershed, RiverNumber, MONITORING_LOCATION) %>%
#     summarise(MonitLoc_Mean = mean(RESULT, na.rm=T),
#               MonitLoc_Min = min(RESULT, na.rm=T),
#               MonitLoc_Max = max(RESULT, na.rm=T),
#               `Records per Monitoring Station` = n()) %>%
#     mutate(`Waterbody Mean Calcium (mg/L)` = mean(MonitLoc_Mean),
#            `Number of Distinct Monitoring Locations per Waterbody` = n()) %>%
#     group_by(Watershed) %>%
#     mutate(`Subwatershed Mean Calcium (mg/L)` = mean(`Waterbody Mean Calcium (mg/L)`)) %>%
#     rename(WaterbodyNumber = RiverNumber)
# ) %>%
#   filter(Watershed %in% c("Murtle Lake","Upper North Thompson River")) %>%
#   left_join(df_sp %>% st_drop_geometry() %>% select(LakeNumber,LakeName,COLLECTION_END) %>% rename(WaterbodyNumber = LakeNumber)) %>%
#   select(Watershed, `Subwatershed Mean Calcium (mg/L)`, LakeName, 
#          WaterbodyNumber, `Waterbody Mean Calcium (mg/L)`,
#          `Number of Distinct Monitoring Locations per Waterbody`,
#          MONITORING_LOCATION, COLLECTION_END, MonitLoc_Mean, MonitLoc_Max, MonitLoc_Min,
#          `Records per Monitoring Station`) %>%
#   rename(`Collection Date` = COLLECTION_END, Subwatershed = Watershed, `Waterbody Name` = LakeName) %>%
#   distinct()
# View(test)
# 
# openxlsx::write.xlsx(x = test, file = "C:/Users/CMADSEN/Downloads/LocalRWork/output/Calcium of 2 Subwatersheds.xlsx")

#Just 4 waterbodies for Murtle Lake, and 1 for Upper North Thompson River.
#Let's double check that that is true.

# unique(df_sp[df_sp$Watershed == "Murtle Lake",]$LakeNumber)
# #4 unique lakes in Murtle Lake
# unique(df_sp[df_sp$Watershed == "Murtle Lake",]$RiverNumber)
# #No rivers in Murtle Lake.

# df_sp[df_sp$LakeNumber%in%c("356354524",
#                             "356358584",
#                             "356362728",
#                             "356363845",
#                             "356350151"),c(29,30)] %>%
#   group_by(LakeName) %>% summarise(n())
# unique(df_sp[df_sp$Watershed == "Upper North Thompson River",]$LakeNumber)
# #1 unique lake in Upper North Thompson River
# unique(df_sp[df_sp$Watershed == "Upper North Thompson River",]$RiverNumber)
#No rivers in Upper North Thompson River.

#How many calcium data points per subwatershed for Murtle Lake
# and Upper North Thompson River?

#How many calcium data points per waterbody, in subwatersheds Murtle Lake
# and Upper North Thompson River?