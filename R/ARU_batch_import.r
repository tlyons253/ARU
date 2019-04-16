#' A function to import a set of csv files.
#'
#' produces a list where each frequency is a separate data frame and the tower the record came from is an
#' additional variable
#'
#' This function assumes csv file names follow a pattern where they file name ends as 'TOWER_X.csv where
#'  X is a number 0-9
#' @param folderdir is the folder containing the series of csv files you wish to import
#' @param FORMAT is a character string specifying the time stamp format 
#'	(be sure to specify a time zone if working with data that covers multiple time zones or includes ST and DST)
#'	provided the card was programmed using US date formating (mm/dd/YYYY) there should be no issue with the default
#' @param tz Specify the time zone you want to use. This is metadata that is stored internally, but will ease
#' transformations later on. The default is 'America/Bogata' which is GMT-5. This is set because our ARU's were initially
#' set to CDST (GMT-5) and left year round. The America/Bogtata time zone does not observe DST.
#' @keywords ARU
#' @export
#' @examples
#'a<-ARU_batch_import('C:/Users/mydirectory,FORMAT='%Y-%m-%d %H:%M:%S',tz='America/Bogota)




ARU_batch_import<-function(folderdir, FORMAT='%Y-%m-%d %H:%M:%S',tz='America/Bogota'){
	filed<-list.files(path=folderdir, pattern="*TOWER",full.names=TRUE)
	a<-lapply(filed,read.csv, header=FALSE, fill=TRUE, col.names= paste0("V", seq_len(39)),colClasses=c("factor", "numeric", "character",rep("integer",36)))
	names(a)<-substr(filed,nchar(filed)-9,nchar(filed)-4)
	a<-lapply(names(a),
			function(n,x){
				x[[n]]$TOWERID<-n
					return(x[[n]])
					},a
								)

make.data<-function(x){

	work.data<-subset(x,x[1]==4)
	work.data2<-work.data[c(-1,-6,-7,-10,-11,-14,-15,-18,-19, -22,-23, -26:-35)]

	mycolname<-c("freq.id","DT", "S1", "N1", "S2", "N2","S3", "N3", "S4", "N4","S5", "N5","S6", "N6"
	, "PW", "PI","aru.unk", "msrfreq", "TOWERID")
	names(work.data2)<-mycolname	
startval<-min(unique((work.data[,2])))
maxlength<-length(unique((work.data[,2])))
worklist<-(subset(x, grepl("PULSE",V1)))
worklist$V1<-as.character(worklist$V1)
worklist$V1<-as.factor(substr(worklist$V1,nchar(worklist$V1)-7,nchar(worklist$V1)))
worklist<-worklist[,1:3]
ifelse(maxlength!=(nrow(worklist)), worklist<-worklist[-(sum(maxlength+1)):-(nrow(worklist)),], worklist<-worklist)
ifelse(startval==0,worklist$freq.id<-(seq(0,max(unique((work.data[,2]))))),worklist$freq.id<-(seq(1,max(unique((work.data[,2]))))))
worklistb<-worklist[,-2:-3]
names(worklistb)<-c("prog.freq", "freq.id")
worklistb<-worklistb

mymergedata<-merge(work.data2,worklistb,by="freq.id")


}



a2<-lapply(a,make.data)
mymerge<-do.call("rbind",a2)
#above makes a list of raw csv files, processes them and makes 1 big data frame with all frequencies and all towers
mymerge[3:14]<-as.integer(as.matrix(mymerge[3:14]))#some issues with csv's having a signal column read in as a character

mymerge$DT<-as.POSIXct(mymerge$DT,tz=tz,format=FORMAT)
mymerge$prog.freq<-as.factor(mymerge$prog.freq)
mymerge$TOWERID<-as.factor(mymerge$TOWERID)

#splits data into a list of frequencies, with towers denoted by a factor variable
dflist<-split(mymerge, mymerge$prog.freq)
dd<-lapply (dflist,function(x) x[order(x$TOWERID),])

}





