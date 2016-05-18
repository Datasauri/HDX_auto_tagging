rm(list=ls())
# check if the required libraries exist, if not install them  ####
required_lib =c("RJSONIO",
                "jsonlite",
                "dplyr",
                "gtools")

install_required_libs<-function(){ # (pumped from Tim Essam)
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i])}
  }
}

install_required_libs()
lapply(required_lib, require, character.only=T)

# downlaod ####
setwd("C:/Users/Xavier/Documents/Datasauri/IFRC/HDX/")
url_datasets_id<-"https://data.hdx.rwlabs.org/api/3/action/package_list"
datasets_id<-fromJSON(url_datasets_id)

datasets_l<-unlist(datasets_id[3])
length(datasets_l)

data_ls<-list()

for(Files in 2133:length(datasets_l)){ # length(datasets_l)
  
  url<-paste0("https://data.hdx.rwlabs.org/api/3/action/package_show?id=",
              datasets_l[Files])
  
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
  
}

data_m<-as.data.frame(bind_rows(data_ls)) #bind the list of list

data_m.c<-do.call(cbind, # !! get rid of the Json character & some geo coordinates
                  lapply(data_m,
                         FUN= function(x) gsub(";|</p>|<br />|<p>|\r|\n|\t|\v|\f",
                                               "",
                                               x)
                  )
)

# order the results
tags_vars<-grep("tags",colnames(data_m.c))
tags_vars_sorted<-mixedsort(colnames(data_m.c)[tags_vars])

all<-1:dim(data_m.c)[2]
exc<-grep(paste0(c("name",
                   "resources.format",
                   "resources.name",
                   "resources.url",tags_vars_sorted),
                 collapse="|"),
          colnames(data_m.c))

data_m.c.b<-cbind(data_m.c[,c("name",
                              "resources.format",
                              "resources.name",
                              "resources.url")],
                  data_m.c[,tags_vars_sorted],
                  data_m.c[,all[-exc]])

write.csv2(data_m.c.b,
           "data_m5.csv",
           col.names=T,
           sep=",")
dim(data_m.c.b)








# data.3w = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/nepal-3w-14july2015-consolidated.csv")
# 
# data.ww = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/who_what.csv")
# data.fts = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/fts_nepal.csv")
# 
# require(ggplot2)
# cont.organizations = ddply(data.ww, .(Organisation.Name), summarise, count=length(Cluster))
# qplot(reorder(Organisation.Name,count), count, data=subset(cont.organizations, count > 86)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# qplot(Cluster, count, data=(ddply(data.ww, .(Cluster), summarise, count=length(Cluster))))
# 
# dim(subset(data.3w, !is.na(Targets)))
# dim(subset(data.3w, is.na(Targets)))
# 
# dim(subset(data.3w, TOTAL >0))
# dim(subset(data.3w, is.na(TOTAL)))
# 
# group_packages_ids=group_show(url="https://data.hdx.rwlabs.org/",
#                               limit=10000,
#                               id=c("nepal-earthquake"),
#                               as = "table")$packages$id
# 
# packages_group_nepal = (lapply(group_packages_ids,  function(x) 
#   package_show(url="https://data.hdx.rwlabs.org/",
#                id=x,
#                as="table")))
# 
# tag.saved = c()
# pepe = lapply(packages_group_nepal, as.data.frame(function(x){
#   name = x$name
#   tag.saved = unique(c(tag.saved, x$tags$name))
#   tags = sum(match(tag.saved, x$tags$name))
#   textags = paste(x$tags$name, collapse=", ")
#   
#   # tags = paste(sort(x$tags$name), collapse = ", ")
#   type = x$type
#   humanitarian.codes = c("3w", "4w", "crisis", "cluster", "damage")
#   baseline.codes = c("education", "population", "food", "census")
#   datatype = if (length(intersect(as.vector(x$tags$name), as.vector(humanitarian.codes)))>0) 1 else(if (length(intersect(as.vector(x$tags$name), as.vector(baseline.codes)))>0) 2 else 3)
#   return(data.frame(name, tags, type, datatype, textags))
#   }))
# data = rbindlist(pepe)
# 
# train.data = data[value.datatype < 3,]
# test.data = data[value.datatype > 2,]
# test.labels = test.data$value.tags
# train.labels = train.data$value.tags
# knn(train=train.data, test=test.data, cl=as.factor(train.labels))
# 
# train.data$value.datatype = as.factor(train.data$value.datatype)
# test.data$value.datatype = as.factor(test.data$value.datatype)
# m  = NaiveBayes(value.datatype ~ value.tags, data=train.data)
# 
# prediction = predict(m, test.data)
# 
# test.data$classified = prediction$class
