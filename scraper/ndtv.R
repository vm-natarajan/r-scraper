getNDTVSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('http://feeds.feedburner.com/',url_section);
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = pub_date,source = 'ndtv',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getNDTVFeeds <- function(){
  url_sections <- c('ndtvnews-top-stories','ndtvnews-latest','ndtvnews-trending-news','ndtvnews-india-news','ndtvnews-world-news','ndtvprofit-latest','ndtvmovies-latest','ndtvsports-latest','ndtvsports-cricket','gadgets360-latest','ndtvcooks-latest');
  sections <- c('top-news','top-news','top-news','india','world','business','entertainment','sport','sport','technology','health');
  ndtv_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getNDTVSectionFeeds(url_sections[x],sections[x]); 
    ndtv_set <- rbind(ndtv_set,section_set);
  }
  return(ndtv_set);
}

getNDTVStories <- function(data) {
  
  urls <- data[data$source=='ndtv',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = "div.ins_storybody p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story);
    rs <- as.data.frame(cbind(source = 'ndtv',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}