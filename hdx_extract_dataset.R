# rm(list=ls())
# check if the required libraries exist, if not install them  ####
required_lib =c("RJSONIO",
                "jsonlite",
                "dplyr",
                "gtools", "foreach", "parallel", "doParallel")

install_required_libs<-function(){ # (pumped from Tim Essam)
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i])}
  }
}

install_required_libs()
lapply(required_lib, require, character.only=T)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)


# downlaod ####
url_datasets_id<-"https://data.hdx.rwlabs.org/api/3/action/package_list"
datasets_id<-fromJSON(url_datasets_id)

datasets_l<-unlist(datasets_id[3])
length(datasets_l)

data_ls<-list()

get_resource <- function (resource) {
  print(paste("file", resource))
  
  url<-paste0("https://data.hdx.rwlabs.org/api/3/action/package_show?id=",
              resource)
  
  data_u<-unlist(jsonlite::fromJSON(url)[3]$result) # get the file info
  
  data_u<-as.data.frame(t(data_u)) # transform as dataframe
  
  countries<-paste(unlist(fromJSON(as.character(unlist(data_u["solr_additions"])))), sep=", ")
  if(length(grep("resources.url[0-9]*$",
                 names(data_u), value=T)) > 1){
    resource_urls = apply(data_u[,
                                 grep("resources.url[0-9]*$",
                                      names(data_u), value=T)], 1 , paste , collapse = ", " )
    
  } else {
    resource_urls = data_u$resources.url
  }
  if(length(grep("tags.name[0-9]*$",
                 names(data_u), value=T)) > 1){
    tags  = apply(data_u[,
                         grep("tags.name[0-9]*$",
                              names(data_u), value=T)], 1 , paste , collapse = ", " )
    
  } else {
    tags = data_u$tags.name
  }
  
  data_u["countries"] = paste(countries, collapse = ", ")
  cols = c("id",
           "dataset_source",
           "license_id",
           "type",
           "name",
           "notes",
           "title",
           "countries")
  data_u = data_u[, cols]
  data_u$tags = tags
  data_u$resources.urls = resource_urls
  write.csv2(data_u,
             paste("~/tmp/hdx/data", resource, ".csv", sep="", collapse = ""),
             col.names=T)
  return(data_u)
}

data = foreach(i =(datasets_l), .packages = required_lib) %dopar% {
                 try({get_resource(i)})
}
write.csv2(rbindlist(data, fill=T), "data_m5.csv", col.names = T)