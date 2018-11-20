library(mongolite)

options(mongodb = list(
  "host" = "ds151293.mlab.com:51293",
  "username" = "vm-natarajan",
  "password" = "Mani0303#"
))
databaseName <- "newsfeeds"

#saveData(t,'stories')
#saveData(t,'newsfeeds')

saveData <- function(data,collectionName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame((data))
  db$insert(data)
}

processer <- function(source,destination) {
  dt <- loadData(source);
  # Connect to the database
  db <- mongo(collection = destination,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
 
  word_count <- dt$detail %>% strsplit(" ") %>% lapply(FUN = length) %>% unlist();
  readtime <- floor(word_count/200);
  readtime <- lapply(readtime, function(x) if(x == 0) x <- 1 else x <- x) %>% unlist();
  for(x in 1:nrow(dt)){
    findq <- paste0('{"url":"',dt$url[x],'"}',sep = '')
    updateq <- paste0('{"$set":{"readtime": "',readtime[x],'"}}',sep = '')
    db$update(findq,updateq)
  }
  
}

#dt <- loadData('stories')
#dt <- loadData('newsfeeds')
loadData <- function(collectionName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}