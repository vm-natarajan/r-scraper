getHindustanTimesSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0));
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://www.hindustantimes.com/rss/',url_section,'/rssfeed.xml');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%m-%d-%Y")
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = pub_date,source = 'firstpost',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  print(stories)
  return(stories);
}

getHindustanTimesFeeds <- function(){
  url_sections <- c('business','entertainment','tech-news','opinion','world','india','sports','lifestyle','education','travel');
  sections <- c('business','entertainment','technology','opinion','world','top-news','life-style','education','travel');
  ht_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getHindustanTimesSectionFeeds(url_sections[x],sections[x]); 
    ht_set <- rbind(ht_set,section_set);
  }
  return(ht_set);
}