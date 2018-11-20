getBusinessStandardSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://www.business-standard.com/rss/',url_section,'.rss');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%a, %d %b %Y %H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = pub_date,source = 'business-standard',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getBusinessStandardFeeds <- function(){
  url_sections <- c('home_page_top_stories','companies-101','economy-policy-102','finance-103','beyond-business-104','opinion-105','markets-106','technology-108','international-116','politics-155','heath-184','sports-192','entertainment-193','lifestyle-195');
  sections <- c('top-news','business','business','business','business','opinion','business','technology','world','politics','health','sport','entertainment','life-style');
  bs_set <- data.frame(matrix(ncol = 6, nrow = 0));
   for(x in c(1:length(sections))){
     section_set <- getBusinessStandardSectionFeeds(url_sections[x],sections[x]); 
     bs_set <- rbind(bs_set,section_set);
   }
  return(bs_set);
}

getBusinessStandardStories <- function(data) {
  
  urls <- data[data$source=='business-standard',]$url;
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = "span.p-content p") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story)
    story <- gsub('document.write.*});',replacement = '',story);
    rs <- as.data.frame(cbind(source = 'business-standard',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}