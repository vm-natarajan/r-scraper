library(xml2);
library(dplyr);
library(rdrop2);
scrapeEvents <- function(){
  drop_auth(rdstoken = "auth/token.RDS");
  file.sources = list.files(path = c("scraper/","util/"),pattern="*.R",full.names = TRUE);
  sapply(file.sources,source,.GlobalEnv);
  bms <- getBMSEvents();
  event_set <- rbind(bms);
  fdate <- format(as.Date(event_set$date, format="%Y-%m-%d"),format = '%b %d')
  event_set <- cbind(event_set,fdate = fdate);
  saveData(data = event_set,'eventfeeds');
  return('Upload Success!!!');
}
