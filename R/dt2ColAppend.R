#' @title Append vector to a 2-col data.table  
#'
#' @description Little helper. 
#' If nrow from 2-col data.table is less than the length of some arbitrary vector
# append vector to data.table with vector values in 1st column and some other value
# in the 2nd column 
# else return input dt
#'
#' @param dt data.table object
#' 
#' @param vec vector object
#' 
#' @return data.table object
#'
#' @import data.table
#'

dt2ColAppend <- function(dt, vec){
  
  # type check
  
  if(is.data.table(dt) & is.vector(vec)){
    
    message("Invalid objects.")
    
  }
  
  nr <- nrow(dt)
  
  vl <- length(vec)
  
  if(nr == 0 | vl == 0){
    message("One of the arguments has 0 length.")
  }
  
  rowLen <- nr < vl
  
  valMatch <- !(vec %in% dt[,1])
  
  if(rowLen & any(valMatch)){
    
    vec <- vec[valMatch]
    
    dt.names <- colnames(dt)
    
    vec.dt <- data.table(vec, rep(1, vl))
    
    setnames(vec.dt, dt.names)
    
    dt <- rbindlist(list(dt, vec.dt))
  }
  
  return(dt)
}
