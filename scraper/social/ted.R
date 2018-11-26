getMediumFeedDetails <- function(url_section,section){
  print(url_section)
  stories <- data.frame(matrix(ncol = 5, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title","url");
  colnames(stories) <- col_names;
  url <- paste0('https://medium.com/feed/topic/',url_section);
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = pub_date,source = 'medium',section = section,title = stories_title,url = stories_uri));
  return(stories);
}

getMediumFeeds <- function(){
  url_sections <- c('popular','startups');
  sections <- c('popular','startups');
  md_set <- data.frame(matrix(ncol = 5, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getMediumFeedDetails(url_sections[x],sections[x]); 
    md_set <- rbind(md_set,section_set);
  }
  return(md_set);
}

