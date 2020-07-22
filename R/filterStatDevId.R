#' @title Work around for extracting stationary duplicated devices.
#'
#' @description This function is a helper which extracts devices id's and filters them 
#' along the pipeline to cut unnecesary computation due to a bug in the HMM.
#'
#' @param connections  - matrix object containting events along the time axis with row.names
#' 
#' @return a list comprising a data.table and a character vector containing stationary device id's.
#'
#' @import data.table
#'

filterStatDevId <- function(connections){
  
  # get duplicated 2-way
  
  dupId <- which(duplicated(connections) | duplicated(connections, fromLast = TRUE))
  
  connDupId <- row.names(connections[dupId, ])
  
  # if row entries are unique keep row name, where row name = deviceId
  dupId <- c()
  
  for(i in connDupId){
    
    unqLen <- length(unique(connections[i,]))
    
    if( unqLen == 1){
      
        dupId <- c(dupId, i)
    }
    
  }
  
  # data.table with filtered connections
  
  rnames <- row.names(connections)
  
  filtered <- !(rnames %in% dupId)
  
  connections2 <- connections[filtered,]
  
  # vector with filtered devices
  
  devId <- rnames[filtered]
  
  return(list(filteredId = devId, 
              filteredConns = connections))
}

