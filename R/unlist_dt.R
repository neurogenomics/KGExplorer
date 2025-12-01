#' Unlist a \link[data.table]{data.table}
#' 
#' \link[data.table]{data.table}s can sometimes have columns that are nested lists. 
#' This function will either drop these columns or convert them into 
#' character strings.
#' @param dat \link[data.table]{data.table}
#' @param drop Drop columns that are lists.
#' @param collapse Character to collapse lists with.
#'  Used only when drop is \code{FALSE}.
#' @returns \link[data.table]{data.table}
#' @export
#' @examples
#' dat <- data.table::data.table(a=1:3,b=list(1:2,3:4))
#' unlist_dt(dat)
unlist_dt <- function(dat,
                      drop=FALSE,
                      collapse=";"){
  lst_cols <- sapply(dat, is.list)
  if(isTRUE(drop)){
    messager("Dropping",length(lst_cols),"data.table columns.")
    dat[,-c(lst_cols),with=FALSE]
  } else{
    messager("Unlisting",length(lst_cols),"data.table columns.")
    dat[,lapply(.SD,
                function(x) if(is.list(x)) paste(unique(unlist(x)),
                                                 collapse = collapse) else x),
        .SDcols = names(dat),by=.I]
  }
}