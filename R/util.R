#' Find the names of all the fields for a certain site who's entries are all NA
#'
#' @param df
#' @param site
#'
#' @return
#' @export
#'
#' @examples
find_notna <- function(df,df_meta,site1) {
  out <- dplyr::filter(df,site == site1) %>%
    dplyr::select_if(function(x) any(!is.na(x))) %>%
    dplyr::select(!tidyselect::matches("date|site|lon|lat|depth")) %>%
    names()

  names(out) <- filter(df_meta,var %in% out) %>%
    select(name) %>%
    as_vector()

  return(out)
}
