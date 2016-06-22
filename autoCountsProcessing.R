rm(list=ls())
# Import libraries
library(GISTools)
library(rgdal)
# install.packages("maptools") # For installing the package
require("maptools")

# -------- Setup --------- #
setwd('/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2')

gridsize=400


#-------- Functions --------- #
manuallyMap = function(roads_autoCounts, stname, osmid, scalefactor){
  for(i in 1:nrow(roads_autoCounts@data)){
    if(is.na(roads_autoCounts@data$osmName[i]) ==F & is.na(roads_autoCounts@data$tc_1[i]) ==T){
      if( roads_autoCounts@data$osmName[i] == stname){
        d = subset(roads_autoCounts@data, roads_autoCounts@data$osm_id ==  osmid)[1,]
        roads_autoCounts@data$tc_1[i] = d[["tc_1"]] / scalefactor
        roads_autoCounts@data$tc_2[i] = d[["tc_2"]]/ scalefactor
        roads_autoCounts@data$tc_3[i] = d[["tc_3"]]/ scalefactor
        roads_autoCounts@data$tc_4[i] = d[["tc_4"]]/ scalefactor
        roads_autoCounts@data$tc_5[i] = d[["tc_5"]]/ scalefactor
        roads_autoCounts@data$tc_6[i] = d[["tc_6"]]/ scalefactor
        roads_autoCounts@data$tc_7[i] = d[["tc_7"]]/ scalefactor
        roads_autoCounts@data$tc_8[i] = d[["tc_8"]]/ scalefactor
        roads_autoCounts@data$tc_9[i] = d[["tc_9"]]/ scalefactor
        roads_autoCounts@data$tc_10[i] = d[["tc_10"]]/ scalefactor
        roads_autoCounts@data$tc_11[i] = d[["tc_11"]]/ scalefactor
        roads_autoCounts@data$tc_12[i] = d[["tc_12"]]/ scalefactor
        roads_autoCounts@data$tc_13[i] = d[["tc_13"]]/ scalefactor
        roads_autoCounts@data$tc_14[i] = d[["tc_14"]]/ scalefactor
        roads_autoCounts@data$tc_15[i] = d[["tc_15"]]/ scalefactor
        roads_autoCounts@data$tc_16[i] = d[["tc_16"]]/ scalefactor
        roads_autoCounts@data$tc_17[i] = d[["tc_7"]]/ scalefactor
        roads_autoCounts@data$tc_18[i] = d[["tc_18"]]/ scalefactor
        roads_autoCounts@data$tc_19[i] = d[["tc_19"]]/ scalefactor
        roads_autoCounts@data$tc_20[i] = d[["tc_20"]]/ scalefactor
        roads_autoCounts@data$tc_21[i] = d[["tc_21"]]/ scalefactor
        roads_autoCounts@data$tc_22[i] = d[["tc_22"]]/ scalefactor
        roads_autoCounts@data$tc_23[i] = d[["tc_23"]]/ scalefactor
        roads_autoCounts@data$tc_24[i] = d[["tc_24"]]/ scalefactor
        roads_autoCounts@data$tc_1_24[i] = d[["tc_1_24"]]/ scalefactor
        roads_autoCounts@data$tc_8_9[i] = d[["tc_8_9"]]/ scalefactor
        roads_autoCounts@data$tc_10_16[i] = d[["tc_10_16"]]/ scalefactor
        roads_autoCounts@data$tc_17_18[i] = d[["tc_17_18"]]/ scalefactor
      }
    }
  }
  return(roads_autoCounts)
}

manuallyMapById = function(roads_autoCounts, osmid_tbc, osmid_new, scalefactor){
  for(i in 1:nrow(roads_autoCounts@data)){
    if(is.na(roads_autoCounts@data$osmName[i]) ==F ){
      if( roads_autoCounts@data$osm_id[i] == osmid_tbc){
        d = subset(roads_autoCounts@data, roads_autoCounts@data$osm_id ==  osmid_new)[1,]
        roads_autoCounts@data$tc_1[i] = d[["tc_1"]] / scalefactor
        roads_autoCounts@data$tc_2[i] = d[["tc_2"]]/ scalefactor
        roads_autoCounts@data$tc_3[i] = d[["tc_3"]]/ scalefactor
        roads_autoCounts@data$tc_4[i] = d[["tc_4"]]/ scalefactor
        roads_autoCounts@data$tc_5[i] = d[["tc_5"]]/ scalefactor
        roads_autoCounts@data$tc_6[i] = d[["tc_6"]]/ scalefactor
        roads_autoCounts@data$tc_7[i] = d[["tc_7"]]/ scalefactor
        roads_autoCounts@data$tc_8[i] = d[["tc_8"]]/ scalefactor
        roads_autoCounts@data$tc_9[i] = d[["tc_9"]]/ scalefactor
        roads_autoCounts@data$tc_10[i] = d[["tc_10"]]/ scalefactor
        roads_autoCounts@data$tc_11[i] = d[["tc_11"]]/ scalefactor
        roads_autoCounts@data$tc_12[i] = d[["tc_12"]]/ scalefactor
        roads_autoCounts@data$tc_13[i] = d[["tc_13"]]/ scalefactor
        roads_autoCounts@data$tc_14[i] = d[["tc_14"]]/ scalefactor
        roads_autoCounts@data$tc_15[i] = d[["tc_15"]]/ scalefactor
        roads_autoCounts@data$tc_16[i] = d[["tc_16"]]/ scalefactor
        roads_autoCounts@data$tc_17[i] = d[["tc_7"]]/ scalefactor
        roads_autoCounts@data$tc_18[i] = d[["tc_18"]]/ scalefactor
        roads_autoCounts@data$tc_19[i] = d[["tc_19"]]/ scalefactor
        roads_autoCounts@data$tc_20[i] = d[["tc_20"]]/ scalefactor
        roads_autoCounts@data$tc_21[i] = d[["tc_21"]]/ scalefactor
        roads_autoCounts@data$tc_22[i] = d[["tc_22"]]/ scalefactor
        roads_autoCounts@data$tc_23[i] = d[["tc_23"]]/ scalefactor
        roads_autoCounts@data$tc_24[i] = d[["tc_24"]]/ scalefactor
        roads_autoCounts@data$tc_1_24[i] = d[["tc_1_24"]]/ scalefactor
        roads_autoCounts@data$tc_8_9[i] = d[["tc_8_9"]]/ scalefactor
        roads_autoCounts@data$tc_10_16[i] = d[["tc_10_16"]]/ scalefactor
        roads_autoCounts@data$tc_17_18[i] = d[["tc_17_18"]]/ scalefactor
      }
    }
  }
  return(roads_autoCounts)
}

# -------- Read in Data --------- #
# step 0: create buffer around traffic counts

# Step 0.5: get centroid of buffer

# Step 1: Snap centroid to lines
data_centroids = readOGR(paste(getwd(), "/mapping/_main_/data/yvr-open-data-traffic-counts/traffic-count-centroids/",sep=""), layer="buff_centroids")
roads = readOGR(paste(getwd(),"/mapping/_main_/data/OSM-roads/",  sep=""), layer=paste("osm_roads_clipped_split_",gridsize,"m", sep=""))



# snap points to lines
snappedPoints = snapPointsToLines(data_centroids, roads, 50)
# writeOGR(snappedPoints, "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/temp/", layer="buff_centroids_snapped", driver="ESRI Shapefile")
 
# # Step 2: Add buffer to centroid
snappedPoints_buffer = gBuffer( snappedPoints, width=0.5, byid=TRUE )
writeOGR( snappedPoints_buffer, 
          paste(getwd(), "/mapping/_main_/data/yvr-open-data-traffic-counts/traffic-count-centroids/buff_centroids_snapped_buffer_05m.geojson",sep=""), layer="buff_centroids_snapped_buffer_05m", driver="GeoJSON", check_exists=F, overwrite_layer=T)

# Step 3: Spatial join
osm_counts  = over(roads, snappedPoints_buffer)  # Get district data
roads@data = cbind(roads@data, osm_counts)

# writeOGR( roads,  "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/temp/", layer="autocounts_confirmed_test", driver="ESRI Shapefile")
# plot(roads)

# Step 4: match the counts to the streets
# Create new names 
'
some rules:
- differentiate between residential and nonresidential
'

roads_data = roads@data
colnames(roads_data)

# edit osmNames
roads_data$osmName = toupper(roads_data$name)
roads_data$osmName
roads_data$osmName = gsub("CAMBIE STREET BRIDGE", "CAMBIE STREET", roads_data$osmName)
roads_data$osmName = gsub("CAMBIE STREET RAMP", "CAMBIE STREET", roads_data$osmName)
# roads_data$osmName = gsub("STANLEY PARK CAUSEWAY", "WEST GEORGIA STREET", roads_data$osmName)
sort(unique(roads_data$osmName))

# Edit traffic count names
nn = toupper(roads_data$Name)
nn = gsub(" E ", " EAST ", nn)
nn = gsub(" W ", " WEST ", nn)
nn
nn = gsub(" ST", " STREET", nn)
nn
nn = gsub(" AV", " AVENUE", nn)

sort(unique(roads_data$osmName))
sort(unique(nn))
roads_data$countName = nn


cno = c()
for (i in 1:nrow(roads_data)){
  temp = paste(unlist(strsplit(nn, " ")[i])[2:length(unlist(strsplit(nn, " ")[i]))], collapse=" ")
  cno = c(cno, temp)
}

roads_data$countNameOnly = cno

# add this into the roads data:
roads_autoCounts = roads
roads_autoCounts@data = roads_data

# writeOGR(roads_autoCounts, "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/temp/", layer="autocounts_confirmed_tester", driver="ESRI Shapefile")

# subset out the non null values that correspond to the traffic count data
trafficCountData = subset(roads_autoCounts@data, is.na(roads_autoCounts@data$tc_1)==F)

roads_autoCounts@data$osmName
for (i in 1:nrow(trafficCountData)){
  for(j in 1:nrow(roads_autoCounts@data)){
    if(is.na(roads_autoCounts@data$osmName[j]) == F & is.na(roads_autoCounts@data$tc_1[j])==T){
      if (trafficCountData$countNameOnly[i] == roads_autoCounts@data$osmName[j] & 
            trafficCountData$type[i] == roads_autoCounts@data$type[j]){
                roads_autoCounts@data$tc_1[j] = trafficCountData$tc_1[i]
                roads_autoCounts@data$tc_2[j] = trafficCountData$tc_2[i]
                roads_autoCounts@data$tc_3[j] = trafficCountData$tc_3[i]
                roads_autoCounts@data$tc_4[j] = trafficCountData$tc_4[i]
                roads_autoCounts@data$tc_5[j] = trafficCountData$tc_5[i]
                roads_autoCounts@data$tc_6[j] = trafficCountData$tc_6[i]
                roads_autoCounts@data$tc_7[j] = trafficCountData$tc_7[i]
                roads_autoCounts@data$tc_8[j] = trafficCountData$tc_8[i]
                roads_autoCounts@data$tc_9[j] = trafficCountData$tc_9[i]
                roads_autoCounts@data$tc_10[j] = trafficCountData$tc_10[i]
                roads_autoCounts@data$tc_11[j] = trafficCountData$tc_11[i]
                roads_autoCounts@data$tc_12[j] = trafficCountData$tc_12[i]
                roads_autoCounts@data$tc_13[j] = trafficCountData$tc_13[i]
                roads_autoCounts@data$tc_14[j] = trafficCountData$tc_14[i]
                roads_autoCounts@data$tc_15[j] = trafficCountData$tc_15[i]
                roads_autoCounts@data$tc_16[j] = trafficCountData$tc_16[i]
                roads_autoCounts@data$tc_17[j] = trafficCountData$tc_17[i]
                roads_autoCounts@data$tc_18[j] = trafficCountData$tc_18[i]
                roads_autoCounts@data$tc_19[j] = trafficCountData$tc_19[i]
                roads_autoCounts@data$tc_20[j] = trafficCountData$tc_20[i]
                roads_autoCounts@data$tc_21[j] = trafficCountData$tc_21[i]
                roads_autoCounts@data$tc_22[j] = trafficCountData$tc_22[i]
                roads_autoCounts@data$tc_23[j] = trafficCountData$tc_23[i]
                roads_autoCounts@data$tc_24[j] = trafficCountData$tc_24[i]
                roads_autoCounts@data$tc_1_24[j] = trafficCountData$tc_1_24[i]
                roads_autoCounts@data$tc_8_9[j] = trafficCountData$tc_8_9[i]
                roads_autoCounts@data$tc_10_16[j] = trafficCountData$tc_10_16[i]
                roads_autoCounts@data$tc_17_18[j] = trafficCountData$tc_17_18[i]
      }
    }
  }
}


# Now take care of the situations where no driving can happen
for(i in 1:nrow(roads_autoCounts@data)){
  if(is.na(roads_autoCounts@data$tc_1[i])==T){
    if(roads_autoCounts@data$type[i] == "footway" | 
         roads_autoCounts@data$type[i] == "cycleway" | 
         roads_autoCounts@data$type[i] == "pedestrian" | 
         roads_autoCounts@data$type[i] == "steps" |
         roads_autoCounts@data$type[i] == "service" | 
         roads_autoCounts@data$type[i] == "living_street"){
            roads_autoCounts@data$tc_1[i] = 0
            roads_autoCounts@data$tc_2[i] = 0
            roads_autoCounts@data$tc_3[i] = 0
            roads_autoCounts@data$tc_4[i] = 0
            roads_autoCounts@data$tc_5[i] = 0
            roads_autoCounts@data$tc_6[i] = 0
            roads_autoCounts@data$tc_7[i] = 0
            roads_autoCounts@data$tc_8[i] = 0
            roads_autoCounts@data$tc_9[i] = 0
            roads_autoCounts@data$tc_10[i] = 0
            roads_autoCounts@data$tc_11[i] = 0
            roads_autoCounts@data$tc_12[i] = 0
            roads_autoCounts@data$tc_13[i] = 0
            roads_autoCounts@data$tc_14[i] = 0
            roads_autoCounts@data$tc_15[i] = 0
            roads_autoCounts@data$tc_16[i] = 0
            roads_autoCounts@data$tc_17[i] = 0
            roads_autoCounts@data$tc_18[i] = 0
            roads_autoCounts@data$tc_19[i] = 0
            roads_autoCounts@data$tc_20[i] = 0
            roads_autoCounts@data$tc_21[i] = 0
            roads_autoCounts@data$tc_22[i] = 0
            roads_autoCounts@data$tc_23[i] = 0
            roads_autoCounts@data$tc_24[i] = 0
            roads_autoCounts@data$tc_1_24[i] = 0
            roads_autoCounts@data$tc_8_9[i] = 0
            roads_autoCounts@data$tc_10_16[i] = 0
            roads_autoCounts@data$tc_17_18[i] = 0
    }
  }
}
colnames(roads_autoCounts@data)
# writeOGR(roads_autoCounts, "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/temp/", layer="autocounts_confirmed_tester_matched_noFossil", driver="ESRI Shapefile", overwrite_layer=T)

# Take care of Stanley Park Causeway
roads_autoCounts = manuallyMapById(roads_autoCounts, 74233349, 44032486, 1)
roads_autoCounts = manuallyMapById(roads_autoCounts, 4755898, 44032486, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 44032490, 44032486, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 44032489, 44032486, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 44032488, 44032486, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 42000575, 44032486, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 44032485, 44032486, 1)
roads_autoCounts = manuallyMapById(roads_autoCounts, 257712148, 44032486, 1)

# Now clean up manually the streets that need matching:
roads_autoCounts = manuallyMap(roads_autoCounts, "NORTH LAGOON DRIVE", 44032485, 4)
roads_autoCounts = manuallyMap(roads_autoCounts, "STANLEY PARK DRIVE", 44032485, 4)
roads_autoCounts = manuallyMap(roads_autoCounts, "PIPELINE ROAD", 44032485, 4)
roads_autoCounts = manuallyMap(roads_autoCounts, "PACIFIC BOULEVARD", 43710647, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "COOPERAGE WAY", 31879556, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "HAMILTON STREET", 30735346, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "MAINLAND STREET", 174056664, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "BAYSHORE DRIVE", 48525827, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "DENMAN STREET", 48525827, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "ALBERTA STREET", 71359363, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 11TH AVENUE", 30636538, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST BROADWAY", 74271362, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 8TH AVENUE", 4323338, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 8TH AVENUE", 4323338, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 7TH AVENUE", 4323338, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 6TH AVENUE", 4323338, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 6TH AVENUE", 4323338, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 4TH AVENUE", 4518134, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 3RD AVENUE", 4518134, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 15TH AVENUE", 4676402, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 14TH AVENUE", 30581542, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 13TH AVENUE", 30581542, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 13TH AVENUE", 30581542, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 17TH AVENUE", 4676402, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 17TH AVENUE", 4676402, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 18TH AVENUE", 4676402, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 18TH AVENUE", 4676402, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 29TH AVENUE", 22748134, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 29TH AVENUE", 22748134, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 21ST AVENUE", 169381409, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 22ND AVENUE", 230785267, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 23RD AVENUE", 49459251, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST KING EDWARD AVENUE", 22940080, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "HARRIET STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "SAINT CATHERINES STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "DURWARD AVENUE", 44178353, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "SAINT GEORGE STREET", 22748146, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "ROSS STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "CHESTER STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "SOMERVILLE STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WINDSOR STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "ELGIN STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "SHERBROOKE STREET", 23608280, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "LANARK STREET" ,26944733, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 51ST AVENUE", 230785267, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 51ST AVENUE", 230785267, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 19TH AVENUE", 169380802, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 19TH AVENUE", 169380802, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "WATSON STREET", 22748138, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "QUEBEC STREET" ,203179032, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 35TH AVENUE", 22748107, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 37TH AVENUE" ,22748107, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 39TH AVENUE" ,45388718, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 53TH AVENUE" ,26827438, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 54TH AVENUE" ,26827438, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "EAST 56TH AVENUE" ,30597880, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "ARGYLE STREET", 23102296, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "NASSAU DRIVE" ,29094071, 1)
roads_autoCounts = manuallyMap(roads_autoCounts, "VICTORIA DRIVE", 31248273, 1.5)
roads_autoCounts = manuallyMap(roads_autoCounts, "WEST 1ST AVENUE" , 51744592, 1)

# roads_autoCounts = manuallyMap(roads_autoCounts, , , 1)
# roads_autoCounts = manuallyMap(roads_autoCounts,  ,, 1)
# roads_autoCounts = manuallyMap(roads_autoCounts, , , 1)

# now take care of the other forks
# west 2nd st
roads_autoCounts = manuallyMapById(roads_autoCounts, 95130168, 74226355, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 121968427, 74226355, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 56701457, 74226355, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 95130167, 74226355, 2)

# main st
roads_autoCounts = manuallyMapById(roads_autoCounts, 71359361, 121018123, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 51469445, 121018123, 2)

# Cambie bridge
roads_autoCounts = manuallyMapById(roads_autoCounts, 255863563, 24812234, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 32333986, 24812234, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 59123669, 24812234, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 23188859, 24812234, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 56864227, 24812234, 2)

# knight st:
roads_autoCounts = manuallyMapById(roads_autoCounts, 51528346, 31248273, 2)
roads_autoCounts = manuallyMapById(roads_autoCounts, 51528348, 31248273, 2)

# check if the data has been written
# subset(roads_autoCounts@data, roads_autoCounts@data$osmName ==  "NORTH LAGOON DRIVE")[1,][c("tc_1","tc_2","tc_3","tc_4","tc_5","tc_6","tc_7","tc_8","tc_9","tc_10","tc_11","tc_12","tc_13","tc_14","tc_15","tc_16","tc_7","tc_18","tc_19","tc_20","tc_21","tc_22","tc_23","tc_24","tc_1_24","tc_8_9","tc_10_16","tc_17_18")]
# subset(roads_autoCounts@data, roads_autoCounts@data$osmName ==  "MAINLAND STREET")[1,][c("tc_1","tc_2","tc_3","tc_4","tc_5","tc_6","tc_7","tc_8","tc_9","tc_10","tc_11","tc_12","tc_13","tc_14","tc_15","tc_16","tc_7","tc_18","tc_19","tc_20","tc_21","tc_22","tc_23","tc_24","tc_1_24","tc_8_9","tc_10_16","tc_17_18")]

# add in the 10_14
roads_autoCounts@data$tc_10_14 = roads_autoCounts@data$tc_10 +roads_autoCounts@data$tc_11 + roads_autoCounts@data$tc_12 + roads_autoCounts@data$tc_13 + roads_autoCounts@data$tc_14

# to shp
# writeSpatialShape(roads_autoCounts, "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/generated-traffic-counts-osm-split/osm_trafficCounts_split_100_1.shp") # write shapefile
writeOGR(roads_autoCounts, paste(getwd(),"/mapping/_main_/data/yvr-open-data-traffic-counts/generated-traffic-counts-osm-split/", sep=""), layer=paste("osm_trafficCounts_split_", gridsize, sep=""), driver="ESRI Shapefile", overwrite_layer=T)
# to geojson
writeOGR(roads_autoCounts, paste(getwd(),"/mapping/_main_/data/yvr-open-data-traffic-counts/generated-traffic-counts-osm-split/","osm_trafficCounts_split_", gridsize,".geojson", sep=""), 
                            layer=,"roads_autoCounts", driver="GeoJSON", check_exists=F, overwrite_layer=T)



# --------------------------------------------------------------------- #
# 
# plot(roads_autoCounts)
# shades = auto.shading(roads_autoCounts@data$tc_1_24, n = 7, cutter=rangeCuts)
# choropleth(sp=roads_autoCounts, v=roads_autoCounts@data$tc_1_24,shades, na.rm=T)



# -------------------------------------------------------------------------  
# autocounts = read.csv(paste(getwd(), "/mapping/_main_/data/yvr-open-data-traffic-counts/traffic-counts-locations/directional_volume_traffic_counts.csv", sep=""), header=T)

# # print column names
# for(i in 1:length(colnames(autocounts))){
#   print(colnames(autocounts)[i])
# }
# 
# # Group by address
# groupedByAddress = aggregate(cbind(HOUR_1, HOUR_2, HOUR_3, HOUR_4, HOUR_5, HOUR_6, HOUR_7, HOUR_8, HOUR_9, 
#                        HOUR_10, HOUR_11, HOUR_12, HOUR_13, HOUR_14, HOUR_15, HOUR_16, HOUR_17, 
#                        HOUR_18, HOUR_19, HOUR_20, HOUR_21, HOUR_22, HOUR_23, HOUR_24, TOTAL_24_HOURS, 
#                        TOTAL_HOURS_8_TO_9, TOTAL_HOURS_10_TO_16, TOTAL_HOURS_17_TO_18
#                 ) ~ autocounts$LOCATION_NAME+
#                   autocounts$ST_SEGMENT_ID +
#                   autocounts$COORDINATES +
#                   autocounts$ON_STREET +
#                   autocounts$AT_STREET , 
#           data=autocounts, mean, na.rm=TRUE)


# snappedPoints_buffer = readOGR(paste(getwd(), "/mapping/_main_/data/yvr-open-data-traffic-counts/traffic-count-centroids/buff_centroids_snapped_buffer_05m.geojson",  sep=""), "OGRGeoJSON")
# proj4string(snappedPoints_buffer)
# set projection for proj
# projection_utm10n = CRS("+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m
#                         +no_defs")
# snappedPoints_buffer = spTransform(snappedPoints_buffer,projection_utm10n)

# for(i in 1:length(colnames(roads_autoCounts@data))){
#   print(colnames(roads_autoCounts@data)[i])
# }

# writeOGR(roads_autoCounts, "/Users/Jozo/Dropbox/_Projects/ubc-micromet/DIYSCO2/mapping/_main_/data/yvr-open-data-traffic-counts/temp/", layer="autocounts_confirmed_tester_matched", driver="ESRI Shapefile", overwrite_layer=T)
