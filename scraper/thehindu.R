getTheHinduSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://www.thehindu.com/',url_section,'/feeder/default.rss');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%m/%d/%Y")
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<img.*/>',replacement = '',stories_desc);
  stories <- as.data.frame(cbind(date = pub_date,source = 'thehindu',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getTheHinduFeeds <- function(){
  url_sections <- c('business','sci-tech/science','entertainment','life-and-style','opinion','news/international','news/national','sport');
  sections <- c('business','science','entertainment','life-style','opinion','world','top-news','sport');
  th_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getTheHinduSectionFeeds(url_sections[x],sections[x]); 
    th_set <- rbind(th_set,section_set);
  }
  return(th_set);
}