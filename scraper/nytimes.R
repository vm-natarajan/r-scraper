getNYTimesSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('http://rss.nytimes.com/services/xml/rss/nyt/',url_section,'.xml');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = pub_date,source = 'nytimes',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getNYTimesFeeds <- function(){
  url_sections <- c('World','US','Business','Technology','Sports','Science','Health','FashionandStyle','Travel','Arts');
  sections <- c('world','world','business','technology','sport','science','health','life-style','travel','entertainment');
  ny_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getNYTimesSectionFeeds(url_sections[x],sections[x]); 
    ny_set <- rbind(ny_set,section_set);
  }
  return(ny_set);
}

getNYTimesStories <- function(data) {
  
  urls <- data[data$source=='nytimes',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = "[name='articleBody'] p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story);
    rs <- as.data.frame(cbind(source = 'nytimes',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}