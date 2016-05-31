which(datasets_l=="mvam-food-security-monitoring-databank")

url<-paste0("https://data.hdx.rwlabs.org/api/3/action/package_show?id=",datasets_l[2214])
data_meta<-fromJSON(url)[3]$result
print(data_meta$resource.id)

NO<-which(data_meta$resources$datastore_active==TRUE)
url<-paste0("https://data.hdx.rwlabs.org/api/3/action/datastore_search?resource_id=",
            data_meta$resources$id[NO[1]])
data_content<-fromJSON(url)[3]$result

names(data_content)
data_content[3]

mvam_data<-data_content[[3]]
docs_bulk(mvam_data,
          "mvam","fcs")

Search(index="mvam", size=1)$hits$hits


docs_bulk(apply(iris, 1, as.list), index="iris", type="flowers")


url<-"https://data.hdx.rwlabs.org/api/3/action/datastore_search?offset=100&resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178"
data_content2<-fromJSON(url)[3]$result
names(data_content2)
data_content2[2]
data_content2[6]
data_content2[4]

table(data_content[[3]]$ADM0_NAME)
table(data_content2[[3]]$ADM0_NAME)
table(data_content[[3]]$SvyYear)
table(data_content2[[3]]$SvyYear)

url<-paste0("https://data.hdx.rwlabs.org/",data_content2[[4]]$"next")
data_content3<-fromJSON(url)[3]$result

url<-paste0("https://data.hdx.rwlabs.org/api/3/action/datastore_search?offset=1000&resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178")
data_content4<-fromJSON(url)[3]$result
table(data_content4[[3]]$SvyYear)

url<-paste0("https://data.hdx.rwlabs.org/api/3/action/datastore_search?offset=39998&resource_id=91c78d24-eab3-40b5-ba91-6b29bcda7178")
data_content4<-fromJSON(url)[3]$result
table(data_content4[[3]]$SvyYear)





data_u<-unlist(jsonlite::fromJSON(url)[3]$result) # get the file info

names(data_u)<-make.unique(names(data_u))  # makes unique colnames

data_u<-as.data.frame(t(data_u)) # transform as dataframe

countries<-unlist(fromJSON(as.character(unlist(data_u["solr_additions"]))))
if(length(countries)>0){
  for(j in 1:length(countries)){
    data_u[paste0("solr_additions",j)]<-countries[j] # create a variable for the sublist
  }
}

data_ls[[Files]]<-data_u[-which(colnames(data_u)=="solr_additions")]

print(paste("file", Files))


docs_bulk("https://data.hdx.rwlabs.org/api/3/action/package_show?id=141121-sierra-leone-health-facilities")
