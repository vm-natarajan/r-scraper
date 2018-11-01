library(xml2);
library(dplyr);
getMoneyTimesOfIndiaFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://timesofindia.indiatimes.com/',url_section,'.cms');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%m/%d/%Y");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<a.*</a>',replacement = '',stories_desc);
  stories <- as.data.frame(cbind(date = pub_date,source = 'timesofindia',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getTimesOfIndiaFeeds <- function(){
  url_sections <- c('rssfeedstopstories','rssfeeds/1221656','rssfeeds/-2128936835','rssfeeds/296589292','rssfeeds/1898055','rssfeeds/4719161','rssfeeds/4719148','rssfeeds/3908999','rssfeeds/-2128672765','rssfeeds/5880659','rssfeeds/913168846','rssfeeds/784865811','rssfeeds/1081479906');
  sections <- c('top-news','india','india','world','business','sport','sport','health','science','technology','education','opinion','entertainment');
  toi_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getMoneyTimesOfIndiaFeeds(url_sections[x],sections[x]); 
    toi_set <- rbind(toi_set,section_set);
  }
  return(toi_set);
}