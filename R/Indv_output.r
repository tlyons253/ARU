#'Individual output
#'
#'complete later
#' @param mydata object created by batch import
#' @keywords TBD
#' @export 
#' @examples 
#' Indv_output()

Indv_output<-function(mydata,outfolder, DATE=""){
     dir.create(outfolder)
for (i in 1:length(mydata)){
	name<-paste0(outfolder,"/",names(mydata)[i],"_",DATE,".rds")
	saveRDS(mydata, file=name)
	}
}

