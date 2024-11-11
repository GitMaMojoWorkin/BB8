#' Subset large dataset to user-specified year and month
#'
#' @param year Single year,from 2021 onwards
#' @param plants If not animals, select plants=TRUE
#' @param month Select by number 1 - 12
#'
#' @return a dataframe
#' @export
#'
#' @examples #SubsetMonths_Cust(2024, plants=FALSE, month=3)
SubsetMonths_Cust <- function(year, plants=FALSE, month) {

  # load and wrangle
  load(str_c("/dbfs/mnt/lab/restricted/APHW-Dir/tables/Datasets with risk and commodities/HMRC_Data_",year,"_risk_commodity.RData"), envir = .GlobalEnv)
  cust <- get((str_c("hmrc_data_",year,"_risk_commodity")), envir = .GlobalEnv)
  names(cust) <- names(cust) %>%  make.names()
  cust$count <- 1

  # subset to POAO/plants
  if (plants==TRUE) {
    cust <- cust %>% filter(Plant.checks =="Yes")
  } else {
    cust <- cust %>% filter(Plant.checks =="No")}

  # subset to clearance month
  if (month=="1") {
    cust <- cust %>% filter(Month.Clearance == "January")
  } else if (month=="2") {
    cust <- cust %>% filter(Month.Clearance == "February")
  } else if (month=="3") {
    cust <- cust %>% filter(Month.Clearance == "March")
  } else if (month=="4") {
    cust <- cust %>% filter(Month.Clearance == "April")
  } else if (month=="5") {
    cust <- cust %>% filter(Month.Clearance == "May")
  } else if (month=="6") {
    cust <- cust %>% filter(Month.Clearance == "June")
  } else if (month=="7") {
    cust <- cust %>% filter(Month.Clearance == "July")
  } else if (month=="8") {
    cust <- cust %>% filter(Month.Clearance == "August")
  } else if (month=="9") {
    cust <- cust %>% filter(Month.Clearance == "September")
  } else if (month=="10") {
    cust <- cust %>% filter(Month.Clearance == "October")
  } else if (month=="11") {
    cust <- cust %>% filter(Month.Clearance == "November")
  } else if (month=="12") {
    cust <- cust %>% filter(Month.Clearance == "December")
  }

  print(str_c("number of rows = ", nrow(cust)))
  #cust <<- cust
  write.csv(cust, "Months_Cust.csv", row.names = FALSE)
  #saveRDS(cust, file = "cust.RData")
}

