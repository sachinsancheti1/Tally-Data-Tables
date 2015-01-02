require("XML")
require("dplyr")
require("ggplot2")
require("gridExtra")
getwd()
xmlfile=xmlParse("Master.xml")
#xmlfile=xmlParseDoc("Master.xml")
xmltop = xmlRoot(xmlfile) #gives content of root


class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltop) #give name of node, PubmedArticleSet
xmlSize(xmltop) #how many children in node, 19
xmlName(xmltop[[1]]) #name of root's children

xmltop[[1]]
xmltop[[2]][[1]][[1]]
xmlSize(xmltop[[1]]) #number of nodes in each child
xmlSApply(xmltop[[1]], xmlName) #name(s)
xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
xmlSApply(xmltop[[1]], xmlSize) #size
#take a look at the MedlineCitation subnode of 1st child
xmltop[[1]][[1]]
#take a look at the PubmedData subnode of 1st child
xmltop[[1]][[1]]

#subnodes of 2nd child
#xmltop[[2]][[1]]

#Turning XML into a dataframe
#Madhu2012=ldply(xmlToList(xmlfile), data.frame) #completes with errors: "row names were found from a short variable and have been discarded"
#View(Madhu2012) #for easy checking that the data is properly formatted
xmltop[['HEADER']][['TALLYREQUEST']]
tt = xmlToList(xmltop[[2]])

sapply(xmlChildren(xmltop[[2]][[1]]), xmlName)
sapply(xmlChildren(xmltop[[2]][[1]][[2]][[1000]][[1]]), xmlSize)

jt = sum(sapply(xmlChildren(xmltop[[2]][[1]][[2]]), xmlSize))
nam = data.frame(name = character(0),size = numeric(0))
#c(as.vector(sapply(xmlChildren(xmltop[[2]][[1]][[2]][[1000]]), xmlName)),
#  as.vector(sapply(xmlChildren(xmltop[[2]][[1]][[2]][[1000]]), xmlSize)))
for(i in 1:jt)({
  jatt = data.frame(name = as.vector(sapply(xmlChildren(xmltop[[2]][[1]][[2]][[i]]), xmlName)),
             size = as.vector(sapply(xmlChildren(xmltop[[2]][[1]][[2]][[i]]), xmlSize)))
  nam = rbind(nam,jatt)
  rm(jatt)
})

# Summary table
nam.summary = nam %>% group_by(name) %>% summarise(total = n())

rt = as.numeric(rownames(nam[nam$name =="STOCKGROUP",]))
prod.master.names = data.frame()
tlist = character(0)
#ttt = as.data.frame(xmlToDataFrame(xmlChildren(xmltop[[2]][[1]][[2]][[3476]])))
for(i in 1:length(rt))({
  jatt = as.data.frame(xmlToDataFrame(xmlChildren(xmltop[[2]][[1]][[2]][[rt[i]]])))
  #prod.master.names = merge(prod.master.names,jatt)
  tlist = c(tlist,names(jatt))
  rm(jatt)
})

require(gtools)
require(dplyr)
tlist.n = tlist %>% unique
prod.master = matrix(numeric(0),ncol = length(tlist.n))
colnames(prod.master) <- as.vector(tlist.n)
for(i in 1:length(rt))({
  jatt = as.data.frame(xmlToDataFrame(xmlChildren(xmltop[[2]][[1]][[2]][[rt[i]]])))
  prod.master = rbind_list(prod.master,jatt,all = TRUE)
  rm(jatt)
})
View(prod.master)
