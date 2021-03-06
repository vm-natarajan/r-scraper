getBBCSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 7, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('http://feeds.bbci.co.uk/news/',url_section,'/rss.xml');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<img.*/>',replacement = '',stories_desc);
  stories_img <- page_source %>% xml_find_all(xpath = '//item//media:thumbnail') %>% xml_attr('url')
  stories <- as.data.frame(cbind(date = pub_date,source = 'bbc',section = section,title = stories_title,description = stories_desc,url = stories_uri,img = stories_img));
  return(stories);
}

getBBCFeeds <- function(){
  url_sections <- c('technology','business','science_and_environment','entertainment_and_arts','health','politics','world','');
  sections <- c('technology','business','science','entertainment','health','politics','world','top-news');
  bbc_set <- data.frame(matrix(ncol = 7, nrow = 0));
   for(x in c(1:length(sections))){
     section_set <- getBBCSectionFeeds(url_sections[x],sections[x]); 
     bbc_set <- rbind(bbc_set,section_set);
   }
  return(bbc_set);
}


getBBCStories <- function(data) {
  
  urls <- data[data$source=='bbc',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = "[property='articleBody'] p,.vxp-media__summary p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story);
    rs <- as.data.frame(cbind(source = 'bbc',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}