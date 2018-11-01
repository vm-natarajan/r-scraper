scrapeNews <- function(rdriver){
  
    file.sources = list.files(path = "scraper/",pattern="*.R",full.names = TRUE);
    sapply(file.sources,source,.GlobalEnv);
    #bbc <- getBBCFeeds(rdriver = rdriver);
    # fp <- getFirstPostFeeds();
    # ht <- getHindustanTimesFeeds();
    # mc <- getMoneyControlFeeds();
    th <- getTheHinduFeeds(rdriver = rdriver);
    # toi <- getTimesOfIndiaFeeds();
    result_set <- rbind(bbc,fp,ht,mc,th,toi);
    
}