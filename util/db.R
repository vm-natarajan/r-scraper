dbWrite <- function(record){
  library(RMySQL);
  userName <- 'root';
  passWord <- 'Testtest1';
  dbName <- 'stories';
  hostName <- 'localhost';
  db = dbConnect(MySQL(), user=userName, password=passWord, dbname=dbName, host=hostName);
  dbWriteTable(db, value = record, name = "stories", append = TRUE ,row.names = FALSE) 
}


dbRead <- function(tableName){
  library(RMySQL);
  userName <- 'root';
  passWord <- 'Testtest1';
  dbName <- 'stories';
  hostName <- 'localhost';
  db = dbConnect(MySQL(), user=userName, password=passWord, dbname=dbName, host=hostName);
  rs <- dbGetQuery(db,paste("select * from",tableName,"order by Date;"));
  return(rs);
}