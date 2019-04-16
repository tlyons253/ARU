#' Wide output for mike
#'
#' will this even be included in the future
#'
#' finish documentation later
#' @param import object an object produced by batch import
#' @keywords TBD
#' @export 
#' @examples 
#' output_wide_aru()

output_wide_aru<-function(importobject, outfolder,DATE="",SITE=""){

#reshapes a list of frequencies from having a tower ID as a vector, to having unique vectors for each tower (wide format)
#also allows output of wide objects to a yet-uncreated folder in rds and csv formats
a<-lapply(importobject,function(x) reshape(x,idvar=c("DT", "prog.freq"),ids=unique(x$DT),timevar="TOWERID", 
drop=c("msrfreq","aru.unk","freq.id"),direction='wide'))
rm(importobject)
a1<-lapply (a,function(x) x[order(x$DT),])
rm(a)
dir.create(outfolder)

for (i in 1:length(a1)){
	outdata<-as.data.frame(a1[[i]])
	name<-paste0(outfolder,"/",names(a1)[i],"_",DATE,SITE,".rds")
	saveRDS(outdata, file=name)
	}

for(i in 1:length(a1)){
outdata<-as.data.frame(a1[[i]])
	name<-paste0(outfolder,"/",names(a1)[i],"_",DATE,".csv")
	write.csv(outdata, file=name)
	}
}

