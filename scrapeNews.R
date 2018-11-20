library(xml2);
library(dplyr);
library(rdrop2);
scrapeNews <- function(){
    drop_auth(rdstoken = "auth/token.RDS");
    file.sources = list.files(path = c("scraper/","util/"),pattern="*.R",full.names = TRUE);
    sapply(file.sources,source,.GlobalEnv);
    bbc <- getBBCFeeds();
    fp <- getFirstPostFeeds();
    #ht <- getHindustanTimesFeeds();
    #mc <- getMoneyControlFeeds();
    #th <- getTheHinduFeeds();
    #toi <- getTimesOfIndiaFeeds();
    #bs <- getBusinessStandardFeeds();
    #et <- getEconomicTimesFeeds();
    #ie <- getIndianExpressFeeds();
    #ndtv <- getNDTVFeeds();
    #ny <- getNYTimesFeeds();
    #news_set <- rbind(bbc,fp,ht,mc,th,toi,bs,et,ie,ndtv,ny);
    news_set <- rbind(bbc,fp);
    fdate <- format(as.Date(news_set$date, format="%Y-%m-%d"),format = '%b %d')
    news_set <- cbind(news_set,fdate = fdate);
    #news_set <- news_set[order(as.Date(news_set$date, format="%m/%d/%Y"),decreasing = TRUE),]
    write.csv(x = news_set[c('source','url')],file = 'td.csv')
    saveData(data = news_set,'newsfeeds');
    drop_delete('stories/td.csv');
    drop_upload('td.csv','stories');
    return('Upload Success!!!');
}

scrapeNewsDetails <- function(){
  
  drop_auth(rdstoken = "auth/token.RDS");
  file.sources = list.files(path = c("scraper/","util/"),pattern="*.R",full.names = TRUE);
  sapply(file.sources,source,.GlobalEnv);
  data <- drop_read_csv('stories/td.csv',stringsAsFactors = FALSE);
 
  bbc <- getBBCStories(data);
  fp <- getFirstPostStories(data);
  #ht <- getHindustanTimesStories(data);
  #mc <- getMoneyControlStories(data);
  #th <- getTheHinduStories(data);
  #toi <- getTimesOfIndiaStories(data);
  #bs <- getBusinessStandardStories(data);
  #et <- getEconomicTimesStories(data);
  #ie <- getIndianExpressStories(data);
  #ndtv <- getNDTVStories(data);
  #ny <- getNYTimesStories(data);
  #news_set <- rbind(bbc,fp,ht,mc,th,toi,bs,et,ie,ndtv,ny);
  news_set <- rbind(bbc,fp);
  saveData(data = news_set,'stories');
  return('Upload Success!!!');
}

controller <- function(){
  scrapeNews();
  scrapeNewsDetails();
}