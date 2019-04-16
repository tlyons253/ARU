#' A function to create  a worklist for an ARU manufactured by JDJC Corp. Fisher, IL
#'
#' Takes a data frame with named columns TunedFreq,pw,pint and a grouping column, at minimum
#' and produces a setup text file that can be loaded directly to an SD card. This is a stand-alone script, do not 
#' assign it to an object
#'
#' This function requires the input table to have columns with the exact names TunedFreq,the frequency the ARU will
#'  look for, pw, the pulse width, and pint, the pulse interval
#' This function automatically appends the date the file was created to the file name
#' @param masterlist the input table, see above for details about this table
#' @param outpath where the text file will be created
#' @param name the name of the setup file to be created
#' @param Group_var a grouping variable that can be used to subset frequencies. If not needed, you still must create one and set all the values the same
#' @param Group_value the level of Group_var to subset and select
#' @param numant the number of antennas. Needed when computing a SYNC time, default is 6
#' @param ovrh the overhead, default of 0.2. Only needed for computing a SYNC time
#' @param SYNC do you want a SYNC time appended to the start of the worklist? Defaults to TRUE
#' @keywords worklist
#' @export
#' @examples
#' make_worklist(mytable,outpath='C:/Users/myDesktop/',name='worklist',Group_var=Landscape,Group_value='SIB')
#' @import dplyr
#' @importFrom schoolmath is.whole
#' @import xts
#' @import lubridate
#' @importFrom magrittr '%>%'



make_worklist<-function(masterlist,outpath,name,Group_value="",Group_var="",numant=6,ovrh=0.2, SYNC=TRUE){
mysubset<-lazyeval::interp(~y==x,.values=list(y=as.name(Group_var),x=Group_value))
subfreq<-masterlist%>%dplyr::filter_(mysubset)

#create the data section of the worklist
pulse<-paste0(paste("PULSE_V1",round(subfreq$TunedFreq,3),sep=" "),",")
pulselo<-paste0(round(subfreq$TunedFreq-0.003,3),",")
pulsehi<-paste0(round(subfreq$TunedFreq+0.003,3),",")
unk<-paste0(rep(63,length(pulse)),",")
pw<-paste0(subfreq$pw,",")
pwlo<-paste0(subfreq$pw-5,",")
pwhi<-paste0(subfreq$pw+5,",")
pint<-paste0(subfreq$pint,",")
unk2<-paste0(rep(15,length(pulse)),",")
unk3<-rep(32,length(pulse))
worklist<-data.frame(pulse,pulselo,pulsehi,unk,pw,pwlo,pwhi,pint,unk2,unk3)

sum_transmitters<-subfreq%>%group_by(pw,pint)%>%summarize(count=n())#tally transmitters by pw and pint


searchtime<-function(aggdata){
	(((((aggdata$pw*5)+aggdata$pint)*numant)*(aggdata$count/1000))+(((aggdata$pw*5)+aggdata$pint)*numant)*(aggdata$count/1000)*ovrh)
	
	
	}
sum_transmitters$stime<-searchtime(sum_transmitters)

matchtime<-function(aggdata){
	mytime<-sum(aggdata$stime)
	a<-seq(1,1000,1)
	b<-3600/a
	d<-data.frame(a,b)
	d2<-d[is.whole(d$b),]
	min(d2[d2[,1] >= mytime,1])
}	



f<-matchtime(sum_transmitters)


top<-ifelse(SYNC==TRUE,paste(paste('[WORKLIST]', 'SYNC',sep='\n'),f),"[WORKLIST]")
mid<-"GOTO 1"
bottom<-"[/WORKLIST]"

#write the text file to a directory. This trick writes each bit at a time (THE SYNC, GOTO1,etc) commands
bintrick<-file(paste0(outpath,name,Sys.Date(),".txt"),open="wb")
write.table(top,bintrick,sep= "",col.names=F, row.names=F,quote=F,eol="\n")
write.table(worklist,bintrick,sep= "",col.names=F, row.names=F,quote=F,append=T,eol="\n")
write.table(mid,bintrick,sep= "",col.names=F, row.names=F,quote=F,append=T,eol="\n")
write.table(bottom,bintrick,sep= "",col.names=F, row.names=F,quote=F,append=T,eol="\n")
close(bintrick)
}


