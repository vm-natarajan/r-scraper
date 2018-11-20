getIndianExpressSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://indianexpress.com/section/',url_section,'/feed/');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<img.*/>',replacement = '',stories_desc);
  stories <- as.data.frame(cbind(date = pub_date,source = 'indian-express',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getIndianExpressFeeds <- function(){
  url_sections <- c('technology','lifestyle','india','world','politics','life-style','opinion','health','europe','cricket','tennis','golf','hockey','sports','entertainment','hollywood','bollywood','asia');
  sections <- c('technology','life-style','india','world','politics','life-style','opinion','life-style','world','sport','sport','sport','sport','sport','entertainment','entertainment','entertainment','world');
  ie_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getIndianExpressSectionFeeds(url_sections[x],sections[x]); 
    ie_set <- rbind(ie_set,section_set);
  }
  return(ie_set);
}

getIndianExpressStories <- function(data) {
  
  urls <- data[data$source=='indian-express',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = ".full-details p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story);
    rs <- as.data.frame(cbind(source = 'indian-express',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}