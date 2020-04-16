#' Function
#'
#' Function
#'
#' @usage fpgrowth(tracts, supp=10.0, zmin=0, zmax=-1)
#'
#' @return value
#' @keywords internal
  
fpgrowth <- function (tracts, supp=10.0, zmin=0, zmax=-1)
{
  r = .Call("f4r_fpgrowth", tracts, supp, zmin, zmax, PACKAGE = "SOAR")
  if (is.null(r)) warning("an error occurred, maybe out of memory")
  return(r)
}

