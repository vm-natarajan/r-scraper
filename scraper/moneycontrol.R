library(xml2);
library(dplyr);
getMoneyControlSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('http://www.moneycontrol.com/rss/',url_section,'.xml');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%m/%d/%Y")
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<img.*/>',replacement = '',stories_desc);
  stories <- as.data.frame(cbind(date = pub_date,source = 'moneycontrol',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getMoneyControlFeeds <- function(){
  url_sections <- c('business','latestnews','mfcolumns','pfcolumns','economy','marketreports','indianadrs','internationalmarkets','marketedge','marketoutlook','technology','entertainment','sports','worldnews');
  sections <- c('business','business','opinion','opinion','business','business','business','business','business','business','technology','entertainment','sport','world');
  mc_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getMoneyControlSectionFeeds(url_sections[x],sections[x]); 
    mc_set <- rbind(mc_set,section_set);
  }
  return(mc_set);
}