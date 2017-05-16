#' Label high courts decisions as in favor (yes) or against (not) the appelant.
#' 
#' This function relies on the kwic function from package quanteda to mine the text of the judicial decisions
#' in order to identify whether it favored or not appelant.
#' 
#' .
#' @param docs with high court decision.
#' @param word word to be searched. Usualy its "provimento" for appeals or "concedido|denegado"
#'             for writ. 
#' @param valuetype how to interpret keyword expressions: "glob" for "glob"-style wildcard expressions;
#'                  "regex" for regular expressions; or "fixed" for exact matching. (copied from quanteda)

#' @keywords numbers
#' @import stringr
#' @return data.frame with two columns: docname and the decision: "yer" or "not"
#' @export

decision_sg<-function(docs,word,valuetype){
  d<-kwic(docs,word,valuetype = valuetype)
  d<-as.data.frame(d,stringAsFactor=F)
  d<-d[duplicated(d$docname)==FALSE,]
  d<-d[str_detect(d$pre,"opinou")==FALSE,]
  d$decisao<-ifelse(str_detect(d$pre,"negaram"),"not",d$pre)
  d$decisao<-ifelse(str_detect(d$pre,"deram"),"yes",d$decisao)
  d$decisao<-ifelse(str_detect(d$pre,"parcial"),"yes",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"da-\\s*se"),"yes",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"nega-\\s*se"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"negar-\\s*lhe"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"nao comporta"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"negram"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"negaragaram"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"nego"),"not",d$decisao)
  d$decisao<-ifelse(str_detect(d$decisao,"negar"),"not",d$decisao)
  d<-d[c(1,7)]
  names(d)[2]<-"favor.appelant"
  return(d)
}
#' @examples
#' decision_sg(docs,"provimento")






