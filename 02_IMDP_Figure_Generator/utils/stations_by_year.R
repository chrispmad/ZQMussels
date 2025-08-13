#Are there any stations we would like to exclude from the excel-type figures?
#In 2021, we will exclude:
if(my.year == 2022){
  stations.to.include = c("Golden","Dawson Creek","Mt. Robson",
                          "Osoyoos","Olsen","Penticton Roving",
                          "Radium","Pacific","Fraser Valley Roving",
                          "Keremeos")
  
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}
if(my.year == 2023){
  stations.to.include = c("Golden","Radium","Olsen","Yahk",
                          "Pacific","Osoyoos","Hwy 97c","Mt. Robson",
                          "Keremeos","Dawson Creek","Lower Mainland Roving",
                          "Sumas Border","Cutts (Hwy 93)","Penticton Roving")
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}
if(my.year == 2024){
  stations.to.include = c("Golden","Radium","Olsen","Yahk",
                          "Pacific","Osoyoos","Hwy 97c","Mt. Robson",
                          "Keremeos (Hwy 3)","Dawson Creek","Lower Mainland Roving",
                          "Sumas Border","Cutts (Hwy 93)","Penticton Roving",
                          "Douglas Crossing")
  stations.to.put.aside = c("Scheduled Inspection","Boat Launch - Okanagan",
                            "Okanagan",
                            "Penticton Roving - Hwy 33", 
                            "Penticton Roving - Inspection Event")
}

# Just in case it's different, which stations do we want to show in the 
# leaflet maps?
if(my.year == 2022){
  leaflet.stations.to.include = c("Golden","Radium","Olsen","Dawson Creek",
                                  "Mt. Robson","Penticton Roving","Osoyoos","Fraser Valley Roving")
} 
if(my.year == 2023){
  leaflet.stations.to.include = c("Golden","Olsen","Dawson Creek","Mt. Robson",
                                  "Penticton Roving","Osoyoos","Lower Mainland Roving",
                                  "Yahk")
}
if(my.year == 2024){
  leaflet.stations.to.include = c(
    "Golden","Olsen","Dawson Creek","Mt. Robson",
    "Radium","Sumas Border",
    "Penticton Roving","Osoyoos","Lower Mainland Roving",
    "Yahk","Pacific",
    "Keremeos (Hwy 3)","Hwy 97c","Cutts (Hwy 93)","Douglas Crossing")
}

#Make one more vector of stations that we want to drop in the figures. These are roving stations.
if(my.year == 2022){
  rovers_to_drop = c("Scheduled Inspection","Other","Okanagan","Sumas Border")
} 
if(my.year == 2023){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}
if(my.year == 2024){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}
if(my.year == 2025){
  rovers_to_drop = c("Other","Okanagan",
                     "Scheduled Inspection (Other Notification)",
                     "Scheduled Inspection (Cbsa Notification)")
}

#Establish colour scheme for roving stations that we keep.
if(my.year == 2022){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos","Greenwood","Kaleden",
                                  "Lower Mainland Roving","Pacific",
                                  "Penticton Roving"),
                      StationType = "Roving")
}
if(my.year == 2023){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos (Hwy 3)","Greenwood","Kaleden",
                                  "Lower Mainland Roving","Pacific",
                                  "Sumas Border",
                                  "Penticton Roving","Cutts (Hwy 93)"),
                      StationType = "Roving")
}
if(my.year == 2024){
  rovers = data.frame(Station = c("Hwy 97c",
                                  "Keremeos (Hwy 3)","Greenwood","Kaleden",
                                  "Lower Mainland Roving","Pacific",
                                  "Sumas Border",
                                  "Penticton Roving",
                                  "Cutts (Hwy 93)",
                                  "Douglas Crossing"),
                      StationType = "Roving")
}
if(my.year == 2025){
  rovers = data.frame(Station = c("Hwy 97c","Keremeos (Hwy 3)","Greenwood","Kaleden",
                                  "Lower Mainland Roving","Pacific",
                                  "Sumas Border",
                                  "Penticton Roving","Cutts (Hwy 93)"),
                      StationType = "Roving")
}


