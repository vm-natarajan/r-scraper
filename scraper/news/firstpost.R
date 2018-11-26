getFirstPostSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 7, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://www.firstpost.com/rss/',url_section,'.xml');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%A,%B %d,%Y %H:%M %p'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_img <- page_source %>% xml_find_all(xpath = '//item//media:content') %>% xml_attr('url')
  stories <- as.data.frame(cbind(date = pub_date,source = 'firstpost',section = section,title = stories_title,description = stories_desc,url = stories_uri,img = stories_img));
  return(stories);
}

getFirstPostFeeds <- function(){
  url_sections <- c('business','bollywood','tech','politics','world','india','sports');
  sections <- c('business','entertainment','technology','politics','world','top-news','sport');
  fp_set <- data.frame(matrix(ncol = 7, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getFirstPostSectionFeeds(url_sections[x],sections[x]); 
    fp_set <- rbind(fp_set,section_set);
  }
  fp_set <- fp_set[-which(fp_set$title == ''),]
  return(fp_set);
}

getFirstPostStories <- function(data) {
  
  urls <- data[data$source=='firstpost',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = ".article-full-content p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story);
    rs <- as.data.frame(cbind(source = 'firstpost',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}