#' Quickly visualize the ARU telemetry records
#'
#'Plot ARU records for a single frequency between two dates, one plot for each tower available
#'This is a stand alone script, and should not be assigned to an object
#'
#' Takes imported csv files of raw ARU data and converts them into a plot for a specific frequency
#' Signal strength records are rounded to the nearest 200db to better identify sedentary periods
#' Contaminated records (noise db<-13700) have not been removed
#'
#' @param batch_import_object The object created by ARU_batch_import
#' @param Freq The position or exact name of the frequency you want to view.
#' @param FROM Starting date to display data, mm/dd/yyyy
#' @param TO Ending date to display data, mm/dd/yyyy
#' @keywords ARUplot
#' @export
#' @examples
#' names(batch_import_object) # obtain the positions and exact names for the different frequencies available
#' ARU_quickplot(mydata, 3,FROM="05/22/2013",TO="05/28/2013")






ARU_quickplot<-function(batch_import_object, Freq,FROM="",TO=""){
x<-batch_import_object[[Freq]]#subset just the frequency you want
x<-transform(x,roundDT =align.time(DT,5))#cleans up the timestamps across tower
x<-x[order(x$roundDT),]
#round signal values to nearest 200 to better ID sedentary periods v. active
mround <- function(x,base){
        base*round(x/base)
}

x[,c(3,5,7,9,11,13)]<-mround(x[,c(3,5,7,9,11,13)],200)
newtry1<-subset(x,format(x$roundDT,"%m/%d/%Y %H:%M:%S")>=FROM & format(x$roundDT,"%m/%d/%Y %H:%M:%S")<= TO)
#subset just a range of days
split.tower.freqpre<-split(newtry1, newtry1$TOWERID)#produces an object for the specified frequency comprised of a list
#of one data frame for each tower for which data was recorded in FROM-TO time period
split.tower.freq<-split.tower.freqpre[sapply(split.tower.freqpre,function(x) dim(x)[1]) >0]# kicks out data frames that have no records between from and to




for (i in 1:length(split.tower.freq)){
x11()
par(mar=c(5.1,5.1,5.1,3.1))
	plot(split.tower.freq[[i]]$roundDT, split.tower.freq[[i]]$S1, type="p",xlab="", ylab="", xaxt='n', col=1, ylim=c(-15000, -6000))
points(split.tower.freq[[i]]$roundDT, split.tower.freq[[i]]$S2, type="p", xaxt='n', col=2)
points(split.tower.freq[[i]]$roundDT,split.tower.freq[[i]]$S3, type="p", xaxt='n', col=3)
points(split.tower.freq[[i]]$roundDT, split.tower.freq[[i]]$S4, type="p", xaxt='n', col=4)
points(split.tower.freq[[i]]$roundDT, split.tower.freq[[i]]$S5, type="p", xaxt='n', col=5)
points(split.tower.freq[[i]]$roundDT, split.tower.freq[[i]]$S6, type="p", xaxt='n', col=6)
#manually create intervals for x axis
hrintv<-seq.POSIXt(as.POSIXct("00:00",format="%H:%M"), as.POSIXct("22:00",format="%H:%M"), by="4 hours")
dateintv<-unique(format(split.tower.freq[[i]]$roundDT, "%m/%d/%Y"))
rephrintv<-rep(hrintv,each=length(dateintv))
majorticks<-paste(dateintv,rephrintv)
compdate<-format(Sys.time(),"%Y-%m-%d")
majorticks<-as.POSIXct(gsub(compdate,"",majorticks),tz="America/Bogota",format= "%m/%d/%Y %H:%M:%S")
datetick<-subset(majorticks, grepl("00:00:00",majorticks))
#label axis
axis.POSIXct(1,at=majorticks, labels=format(majorticks,"%H:%M"), tck= 0.02,las=2)
axis.POSIXct(3,at=datetick,labels=format(datetick,"%b %d"), tck=-0.04, las=2)
axis.POSIXct(1,at=unique(align.time(split.tower.freq[[i]]$roundDT,60*60)), labels="", tck= 0.02,las=2)
legend("bottom", c("N", "NE", "SE", "S", "SW", "NW"),pch=1, col=c(1,2,3,4,5,6), horiz=TRUE)
title (main=names(pheasant.importobject[Freq]),line=4)
title(sub = names(split.tower.freq)[i])
	}
}
