
#clean up workspace
rm(list=ls())
setwd("/Users/AndyC/Dropbox/rdata/cousera/capstone_git/dictionaries/compiled_ngrams")
#Load the ditionaries here casue it speeds up use of the algorithm
d.qgrams<-readRDS("comp_qgrams.rds")
d.trigrams<-readRDS("comp_trigrams.rds")
d.bigrams<-readRDS("comp_bigrams.rds")
d.unigrams<-readRDS("comp_unigrams.rds")

text_prediction<-function(v.txt1){
#v.txt1<-"what is the point of this"#text to predict completion

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

  #Case for trigrams
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

  #Case for bigrams
  if(length(unlist(strsplit(v.txt0,"_")))==1){
    d.matches<-d.bigrams[grep(v.txt0,d.bigrams$first),]
    if(nrow(d.matches)>0){
      p<-d.matches$freq/sum(d.bigrams$freq)
      match.wds<-d.matches$last
      pred.all<-data.frame(cbind(match.wds,p),stringsAsFactors=FALSE)
      pred.all<-pred.all[order(pred.all$p,decreasing=T),]
    }
  }

  #if not found search for probability as a unigram
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

pred.all$p<-as.numeric(pred.all$p)
predicted<-head(pred.all)
predicted<-predicted[order(predicted$p,decreasing=TRUE),]
colnames(predicted)<-NULL
 return(predicted[,1])
}


# setwd("/Users/AndyC/Dropbox/rdata/cousera/capstone_git")
# #Profile the algorithm
# Rprof(tmp <- tempfile())
# y<-text_prediction("what is the point of this")
# Rprof()
# summaryRprof(tmp)