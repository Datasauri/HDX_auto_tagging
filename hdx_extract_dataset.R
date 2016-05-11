data.3w = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/nepal-3w-14july2015-consolidated.csv")

data.ww = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/who_what.csv")
data.fts = read.csv("/home/danito/proj/hhton/the_port_ors_hdx/data/Nepal/humanitarian/fts_nepal.csv")

require(ggplot2)
cont.organizations = ddply(data.ww, .(Organisation.Name), summarise, count=length(Cluster))
qplot(reorder(Organisation.Name,count), count, data=subset(cont.organizations, count > 86)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
qplot(Cluster, count, data=(ddply(data.ww, .(Cluster), summarise, count=length(Cluster))))

dim(subset(data.3w, !is.na(Targets)))
dim(subset(data.3w, is.na(Targets)))

dim(subset(data.3w, TOTAL >0))
dim(subset(data.3w, is.na(TOTAL)))

group_packages_ids=group_show(url="https://data.hdx.rwlabs.org/",
                              limit=10000,
                              id=c("nepal-earthquake"),
                              as = "table")$packages$id

packages_group_nepal = (lapply(group_packages_ids,  function(x) 
  package_show(url="https://data.hdx.rwlabs.org/",
               id=x,
               as="table")))

tag.saved = c()
pepe = lapply(packages_group_nepal, as.data.frame(function(x){
  name = x$name
  tag.saved = unique(c(tag.saved, x$tags$name))
  tags = sum(match(tag.saved, x$tags$name))
  textags = paste(x$tags$name, collapse=", ")
  
  # tags = paste(sort(x$tags$name), collapse = ", ")
  type = x$type
  humanitarian.codes = c("3w", "4w", "crisis", "cluster", "damage")
  baseline.codes = c("education", "population", "food", "census")
  datatype = if (length(intersect(as.vector(x$tags$name), as.vector(humanitarian.codes)))>0) 1 else(if (length(intersect(as.vector(x$tags$name), as.vector(baseline.codes)))>0) 2 else 3)
  return(data.frame(name, tags, type, datatype, textags))
  }))
data = rbindlist(pepe)

train.data = data[value.datatype < 3,]
test.data = data[value.datatype > 2,]
test.labels = test.data$value.tags
train.labels = train.data$value.tags
knn(train=train.data, test=test.data, cl=as.factor(train.labels))

train.data$value.datatype = as.factor(train.data$value.datatype)
test.data$value.datatype = as.factor(test.data$value.datatype)
m  = NaiveBayes(value.datatype ~ value.tags, data=train.data)

prediction = predict(m, test.data)

test.data$classified = prediction$class
