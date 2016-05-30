# 10 most populars tags
url_datasets_id<-"https://data.hdx.rwlabs.org/api/3/action/package_search?facet.field=[%22tags%22]&facet.limit=30&rows=0"
datasets_id<-fromJSON(url_datasets_id)

length(datasets_id[[3]])
datasets_id[3]

length(datasets_id[[3]][[3]][[1]])

datasets_id[[3]][[3]][1]

$vocab_Topics
datasets_id[[3]]$facets$vocab_Topics[order(unlist(datasets_id[[3]]$facets$vocab_Topics),
                                           decreasing=T)]

names(datasets_id[[3]]$facets$vocab_Topics[order(unlist(datasets_id[[3]]$facets$vocab_Topics),
                                                 decreasing=T)])


# dataset on education 
url_datasets_id<-"https://data.hdx.rwlabs.org/api/3/action/package_search?fq=vocab_Topics:education"
datasets_id<-fromJSON(url_datasets_id)

length(datasets_id[[3]])

datasets_id[[3]][1] #count results
datasets_id[[3]][2] # layout of the results
datasets_id[[3]][3] # type of results
datasets_id[[3]][[4]] # 10 results
length(datasets_id[[3]][[4]][[1]]) # 37 entries per result

names(datasets_id[[3]][[4]]$tags)




# dataset example
setwd("C:/Users/Xavier/Documents/Datasauri/IFRC/HDX/")
url_datasets_id<-"https://data.hdx.rwlabs.org/api/3/action/package_list"
datasets_id<-fromJSON(url_datasets_id)

datasets_l<-unlist(datasets_id[3])
length(datasets_l)


url<-paste0("https://data.hdx.rwlabs.org/api/3/action/package_show?id=",
            datasets_l[1])

datasets_id<-fromJSON(url)[3]
datasets_id[[1]]$tags
names(datasets_id[[1]]$tags)
datasets_id[[1]]$resource_id
names(datasets_id[[1]])


https://data.humdata.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&filters={%22ADM0_CODE%22:%20%22118%22}


https://data.humdata.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&filters={%22ADM0_CODE%22:%20%22118%22}
https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178/filters="ADM1_NAME":"Irak"
url<-"https://data.humdata.org/api/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&ADM0_NAME=Irak"
datas<-fromJSON(url)[3]
length(datas[[1]])
datas[[1]][3]
datas[[1]][6]
names(datas[[1]])
datas[[1]][[3]][[1]][6]


url<-"https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178"
datas<-fromJSON(url)[3]
names(datas[[1]])
length(datas[[1]][[3]])
names(datas[[1]][2])
names(datas[[1]][3])

length(datas[[1]][[3]])
length(datas[[1]][[3]][[1]])
names(datas[[1]][[3]][[1]])
table(datas[[1]][[3]][[10]][11])

https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&filters={"ADM0_CODE":"118"}


#     country<-as.character(input$country) #"Iraq" # Malawi, Yemen, Guinea, Sierra Leone, Iraq
#     print(country)
#     variable<-as.character(input$var3) #"FCS"
#     cut<-as.character(input$cut) # "AdminUnits"
#     start<-as.POSIXct("1990-01-01T00:00:00",
#                       format = "%Y-%m-%dT%H:%M:%S")
#     end<-as.POSIXct("2016-12-01T00:00:00",
#                     format = "%Y-%m-%dT%H:%M:%S")
#     
#     query <- list('table'="pblStatsSum4Maps",
#                   'where'=paste0("ADM0_NAME = ","'",country,"'",
#                                  " AND Variable='",variable,"'",
#                                  " AND IndpVars='",cut,"'",
#                                  " AND SvyDate>='",start,"'",
#                                  " AND SvyDate<='",end,"'")


url<-'https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&filters={"ADM0_NAME":"Guinea","IndpVars": "AdminUnits","Variable":"rCSI"}'
data_wfp<-fromJSON(url)[3]

data_wfp<-lapply(datas[[1]][[3]],
                 unlist)
data_wfp<-do.call(rbind,
                  data_wfp)
dim(data_wfp)
colnames(data_wfp)
head(data_wfp)
variable<-"Mean"

data_wfp_select<-aggregate(data_wfp[,variable],
                           by=list(data_wfp[,"AdminStrata"]),
                           FUN=function(x) mean(as.numeric(x),
                                                na.rm=T))


# read the shapefile
if(country=="Guinea"){
  shape<-readShapePoly("shapes/Guinea/GIN_adm1.shp")  
  shape@data$NAME_1<-as.character(shape@data$NAME_1)
  shape@data$NAME_1[2]<-"Conakry"
  shape@data$NAME_1[8]<-"N'Zerekore"
}

# merge the aggregated wfp data with shapefile ####
shape@data$stats<-array(NA,
                        length(shape@data$NAME_1))
for(i in 1:length(data_wfp_select[,1])){
  
#   if(country=="Guinea"){
    NO<-agrep(data_wfp_select[i,1],as.character(shape@data$NAME_1),
              ignore.case=TRUE,
              fixed=T)  # set param -max.distance-
#   }
#   if(country=="Yemen"|country=="Malawi"|country=="Iraq"|country=="Sierra Leone"){   
#     NO<-agrep(data_wfp_select[i,1],as.character(shape@data$NAME_1),
#               ignore.case=TRUE,
#               fixed=T)  # set param -max.distance-
#   }    
  if(identical(NO, integer(0))){
    print(i)
  }else{
    shape@data$stats[NO]<-data_wfp_select[i,2] # assign the stats in MM into the stats variable of the shapefile
  }
  
}
data_wfp_select[10,]
# map ####
pal = colorBin('OrRd',
               shape@data$stats,
               bins=length(unique(shape@data$stats)))

leaflet(shape) %>%
  setView(median(bbox(shape)[1,]), 
          median(bbox(shape)[2,]),
          zoom=5)%>%
  addPolylines(color="grey",
               weight=2) %>%
  addPolygons(
    stroke = F, fillOpacity = 0.5, smoothFactor = 0.5,
    color = ~pal(shape@data$stats)
  )





#     
#     bin.m<-do.call(rbind,
#                    bin)
#         
#     bin.m<-data.frame(bin.m)
#     print(dim(bin.m))
#



names(datas[[1]])
length(datas[[1]][[3]])
names(datas[[1]][2])
names(datas[[1]][3])
length(datas[[1]][[3]])

length(datas[[1]][[3]][[1]])
names(datas[[1]][[3]][[1]])

datas[[1]][[3]][[i]][10]
str(datas[[1]][[3]][1])

for(i in 1:length(datas[[1]][[3]])){
  print(length(datas[[1]][[3]][[i]]))
}



rbind.fill(datas[[1]][[3]][[i]])
library(data.table)
dt<-lapply(datas[[1]][[3]],
           unlist)
)
dt2<-do.call(rbind,dt)
dim(dt2)
colnames(dt2)


rbind.fill(datas[[1]][[3]][[i]])

for(i in 1:23){
  print(datas[[1]][[3]][[i]]$AdminStrata)
}

https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&filters={"ADM0_NAME":"Guinea","Variable":"rCSI","IndpVars"="AdminUnits"}






http://data.humdata.org/api/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&limit=5

Query example (first 5 results)

http://data.humdata.org/api/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&limit=5
Query example (results containing 'jones')

http://data.humdata.org/api/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&q=jones
Query example (via SQL statement)

http://data.humdata.org/api/action/datastore_search_sql?sql=SELECT * from "91c78d24-eab3-40b5-ba91-6b29bcda7178" WHERE title LIKE 'jones'


url<-'http://data.humdata.org/api/action/datastore_search?resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178&limit=5&q=title:jones'

fileobj = urllib.urlopen(url)
print fileobj.read()

