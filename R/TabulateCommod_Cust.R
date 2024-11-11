#' Generate a table of volume by type, month, location and origin for user-specifed year
#'
#' @param year  e.g. 2024
#'
#' @return a tibble
#' @export
#'
#' @examples #TabulateCommod_Cust(2024)
TabulateCommod_Cust <-  function(year) {

  # load and wrangle
  load(str_c("/dbfs/mnt/lab/restricted/APHW-Dir/tables/Datasets with risk and commodities/HMRC_Data_",year,"_risk_commodity.RData"), envir = .GlobalEnv)
  cust <- get((str_c("hmrc_data_",year,"_risk_commodity")), envir = .GlobalEnv)
  names(cust) <- names(cust) %>%  make.names()
  cust$count <- 1

  # tabulate
  custTab <- cust %>%
    select(count, Commodity.Code , Month.Clearance, Goods.Location, Origin.Country..Item.) %>%
    group_by(Commodity.Code , Month.Clearance, Goods.Location, Origin.Country..Item.) %>%
    dplyr::summarise(importrows = sum(count))

  print(str_c("number of rows = ", nrow(custTab)))
  #custTab <<- custTab
  write.csv(custTab, "cust.csv", row.names = FALSE)
  #saveRDS(cust, file = "cust.RData")
}
