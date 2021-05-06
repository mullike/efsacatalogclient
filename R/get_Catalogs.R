#source(paste0(getwd(),"/helpers/catalog_client_global.R"))

#source("/catalog_client_global.R")


# library(data.table)
# library(mongolite)
# library(httr)
# library(jsonlite)
# library(tidyverse)
# library(stringr)
# library(assertive) 

r4EUAPI_urlbase<- "http://127.0.0.1"
ocp_ppim_subscription_key <- "842d2a2c001148ce859ba21425aa93dc"
ua <- httr::user_agent("r4EU API Client")




 
post_r4eu_api <- function(path,bodytagValue=NULL,artifact,artifact_loc) {
  
  artifact_fn <- paste0(artifact,".txt")
  
  supported_Mimes <- c("application/json" , "multipart/related","application/octet-stream","text/plain")
  
  url <- httr::modify_url(r4EUAPI_urlbase, path = path)
  
  if ( !is.null(bodytagValue) ) { 
    bodyIs = bodytagValue 
  } 
  else { 
    bodyIs = NULL
  }
  
  #,"Ocp-Apim-Subscription-Key"= ocp_ppim_subscription_key

  resp <-httr::GET(url,
                #add_headers("Content-Type" = "application/json"),
               query = bodyIs ,
                #encode = "json",
               httr::verbose()
               )
  
  parsed_fp <- content(resp, "text")
  print("Returned type is")
  print(http_type(resp))
  
  if ( !is.element(http_type(resp),supported_Mimes)  ) {
    stop("API did not return json of multipart mime types", call. = FALSE)
  }
  
  
  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "EFSA API request failed for function call [%s]with status code[%s]\n%s\n<%s>",
        deparse(sys.calls()[[sys.nframe()-1]]),
        status_code(resp),
        parsed_fp,
        resp$url
      ),
      call. = FALSE
    )
  }
  
  # 
  # structure(
  #   list(
  #     #content = xmlIs,
  #     artifact_loc=artifact_loc,
  #     path = path,
  #     response = resp
  #   ),
  #   class = "post_efsa_api"
  # )
  
  parsed_fp
}


wrap_api_queryList<- function ( action, vars, values ) {
  
  action <- paste0("\"",action,"\"") #GetResourceList
  delimieters <- ""
  if((length(vars)) > 1 ){
    delimieters <- rep(",",length(vars))
    delimieters[length(vars)] <- ""
  } 
  query_args <- paste("list(",paste0( vars, "= \"",values,"\"",delimieters, collapse=" "),")")
  query_args
  l<- eval(parse(text=query_args))
  l
}




 
#' @title get_catalogs
#'
#' @description This function will return \code{\link{data.table}} containing the catalog or list of catalogs
#'              supplied
#'
#' @param x a comma separated list of catalogs you want downloaded
#'
#' @return list of
#' @export
#'
#' @examples
#' \dontrun{
#' get_Catalogs("COUNTRY.EUSRrepCountry")
#' }
get_Catalogs <- function(x){
  vars <-   c("catalogs")
  values <- c(x)
  querylist <- wrap_api_queryList ("getCatalogs",vars,values)
  call_artifact <-"model_catalogs"
  
  print(paste("API request body is",querylist))
  
  dwn_load<-list(name=cat_name,issue="None",dwn_success=FALSE) # assume false.. EFSA API bit rubbish
  artifact_loc<-""
  
  resp <- try({ post_r4eu_api("/getCatalogs",querylist,call_artifact,manifest_catalogs$manifest_cat_subdir)  })
  
  if (class(resp) != "try-error") {     
   
    DT_catalogs <- as.data.table(fromJSON(resp))
    fil_loc <- paste0(getwd(),"/DT_catalog.rds")
    saveRDS(DT_catalogs, fil_loc)
    fil_loc <- paste0(getwd(),"/DT_catalog.json")
    write(resp,fil_loc)
    
    dwn_load$dwn_success=TRUE
    
  } else{
    dwn_load$issue<- paste0("get_Catalogs::post_r4EU_api:: Falied to API CALL ::",resp)
  }
  
  return(dwn_load)  
}  

#getCatalogs("COUNTRY.EUSRrepCountry")

#getCatalogs("COUNTRY.EUSRrepCountry,LANG.LANG,PARAM.serovarsamr,ZOO_CAT_MATRIX.ZOO_CAT_MATRIX,UNIT.amrsmpUn,SAMPNT.zooss,COUNTRY.COUNTRY,ZOO_CAT_SMPTYP.ZOO_CAT_SMPTYP,PRGTYP.zooSampContext,SAMPLR.SAMPLR,AMRPROG.AMRPROG,SAMPSTR.SAMPSTR,NUTS.nuts2013,ANLYMD.amram,PARAM.AMRSub,ZOO_CAT_FIXMEAS.number,ZOO_CAT_FIXMEAS.mic,ZOO_CAT_FIXMEAS.izd,PARAM.esbl,PARAM.ampc,PARAM.carba,POSNEG.POSNEG,YESNO.zoo")
