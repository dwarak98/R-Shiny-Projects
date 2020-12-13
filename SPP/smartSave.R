setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helperMethods.R")
library(lubridate)



getWindPenetration <- function(df) {
  df %>% filter(variable %in% c("wind_pen_actual", "wind_pen_STF", "wind_pen_MTF"))
  
  
}

createBlankCSV <- function(name){
  file.create(name)
  
}

smartSaveCSV <- function(df,csvfile) {
  existingdf <- read.csv(path)
  existingdf[[dateColName]] <- ymd_hms(existingdf[[dateColName]])
  existingdf$ModifiedAt <- ymd_hms(existingdf$ModifiedAt)
  rbind(existingdf,df) %>%
    arrange(desc(ModifiedAt)) %>% 
    distinct(Interval,variable, .keep_all = TRUE) %>%
    write.csv(path,row.names = FALSE)

}

df <- getForecastVsActual() %>%
  getWindPenetration()

dateColName <- "Interval"
df[[dateColName]] <- ymd_hms(df[[dateColName]])
df$ModifiedAt <- Sys.time()
df$ModifiedAt <- ymd_hms(df$ModifiedAt)

month_year <- format(as.Date(Sys.Date()), "%m_%Y")
name <- paste("Wpen_Forecast_Vs_Actual_",month_year,".csv",sep="")

day_of_the_month <- format(as.Date(Sys.Date()), "%d")
path <- paste("Data/",name,sep="")
if(day_of_the_month ==1){
  
  createBlankCSV(path)
  write.csv(df,path,row.names = FALSE)
}


smartSaveCSV(df,path)

