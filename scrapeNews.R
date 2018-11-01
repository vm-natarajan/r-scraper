library(xml2);
library(dplyr);
library(rdrop2);
scrapeNews <- function(){
    drop_auth(rdstoken = "auth/token.RDS");
    file.sources = list.files(path = "scraper/",pattern="*.R",full.names = TRUE);
    sapply(file.sources,source,.GlobalEnv);
    bbc <- getBBCFeeds();
    fp <- getFirstPostFeeds();
    ht <- getHindustanTimesFeeds();
    mc <- getMoneyControlFeeds();
    th <- getTheHinduFeeds();
    toi <- getTimesOfIndiaFeeds();
    news_set <- as.data.frame(rbind(bbc,fp,ht,mc,th,toi),StringAsFactor = FALSE);
    news_set <- news_set[order(as.Date(news_set$date, format="%m%d/%Y"),decreasing = TRUE),]
    write.csv(x = news_set,file = 'td.csv')
    drop_delete('stories/td.csv');
    drop_upload('td.csv','stories');
    return('Upload Success!!!');
}