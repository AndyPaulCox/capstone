
#clean up workspace
rm(list=ls())
text_prediction<-function(v.txt1){
#v.txt1<-"what is "#text to predict completion




#Clean_text
v.txt0<-tolower(v.txt1)
v.txt0<-gsub("\\s+"," ",v.txt0)#remove excess whitespace
v.txt0<-gsub("^\\s+|\\s+$", "", v.txt0)#Remove leading and trailing whitespace
v.txt0<-gsub(" ","_",v.txt0)

n.wds<-length(unlist(strsplit(v.txt0,"_")))
#if number of wds is greater than 3 wds take last three words
if(n.wds>3){
  x<-stringr::str_locate_all(v.txt0,"_")
  x<-sort(unique(as.numeric(unlist(x))))
  x<-x[length(x)-2]
  v.txt0<-substr(v.txt0,x+1,nchar(v.txt0))
}

#if number of wds is 3
#First search qgrams
d.qgrams<-readRDS("dictionaries/qgrams.rds")
if(length(unlist(strsplit(v.txt0,"_")))==3){
  d.matches<-d.qgrams[grep(v.txt0,d.qgrams$first),]
  if(nrow(d.matches)>0){
    p<-d.matches$freq/sum(d.qgrams$freq)
    match.wds<-d.matches$last
    pred.all<-data.frame(cbind(match.wds,p),stringsAsFactors=FALSE)
    pred.all<-pred.all[order(pred.all$p,decreasing=T),]
  }else{
    x<-stringr::str_locate_all(v.txt0,"_")
    x<-sort(unique(as.numeric(unlist(x))))
    x<-x[length(x)-1]
    v.txt0<-substr(v.txt0,x+1,nchar(v.txt0))
  }
}
rm(d.qgrams)
#Case for trigrams
#First search trigrams
d.trigrams<-readRDS("dictionaries/trigrams.rds")
if(length(unlist(strsplit(v.txt0,"_")))==2){
  d.matches<-d.trigrams[grep(v.txt0,d.trigrams$first),]
  if(nrow(d.matches)>0){
    p<-d.matches$freq/sum(d.trigrams$freq)
    match.wds<-d.matches$last
    pred.all<-data.frame(cbind(match.wds,p),stringsAsFactors=FALSE)
    pred.all<-pred.all[order(pred.all$p,decreasing=T),]
  }else{
    x<-stringr::str_locate_all(v.txt0,"_")
    x<-sort(unique(as.numeric(unlist(x))))
    v.txt0<-substr(v.txt0,x+1,nchar(v.txt0))
  }
}
rm(d.trigrams)

#Case for bigrams
#First search bigrams
d.bigrams<-readRDS("dictionaries/bigrams.rds")
if(length(unlist(strsplit(v.txt0,"_")))==1){
  d.matches<-d.bigrams[grep(v.txt0,d.bigrams$first),]
  if(nrow(d.matches)>0){
    p<-d.matches$freq/sum(d.qgrams$freq)
    match.wds<-d.matches$last
    pred.all<-data.frame(cbind(match.wds,p),stringsAsFactors=FALSE)
    pred.all<-pred.all[order(pred.all$p,decreasing=T),]
  }
}
rm(d.bigrams)
#if not found search for probability as a unigram
d.unigrams<-readRDS("dictionaries/unigrams.rds")
if(nrow(d.matches)==0 & length(unlist(strsplit(v.txt0,"_")))==1){
  d.matches<-d.unigrams[grep(v.txt0,d.unigrams$names),]
  if(nrow(d.matches)>0){
    p<-d.matches$freq/sum(d.unigrams$freq)
    match.wds<-d.matches$names
    pred.all<-data.frame(cbind(match.wds,p))
    pred.all<-pred.all[order(pred.all$p,decreasing=T),]
  }
  if(nrow(d.matches)==0){
    pred.all<-data.frame(match.wds="DUNNO!",p=0.0,stringsAsFactors=FALSE)
  }
}
rm(d.unigrams)
pred.all$p<-as.numeric(pred.all$p)
predicted<-head(pred.all)
#head(pred.all)
 return(predicted)
}