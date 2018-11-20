getEconomicTimesSectionFeeds <- function(url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  stories <- data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('https://economictimes.indiatimes.com/',url_section,'.cms');
  print(url);
  page_source <- read_xml(x = url);
  pub_date <- page_source %>% xml_find_all(xpath = '//item//pubDate') %>% xml_text(trim = TRUE);
  pub_date <- as.Date(strptime(pub_date,format = '%Y-%m-%dT%H:%M:%S'));
  pub_date <- format(pub_date, format="%Y-%m-%d");
  stories_uri <- page_source %>% xml_find_all(xpath = '//item//link') %>% xml_text(trim = TRUE);
  stories_title <- page_source %>% xml_find_all(xpath = '//item//title') %>% xml_text(trim = TRUE);
  stories_desc <- page_source %>% xml_find_all(xpath = '//item//description') %>% xml_text(trim = TRUE);
  stories_desc <- gsub('<a.*</a>',replacement = '',stories_desc);
  stories <- as.data.frame(cbind(date = pub_date,source = 'economic-times',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getEconomicTimesFeeds <- function(){
  url_sections <- c('rssfeedsdefault','rssfeedstopstories','markets/rssfeeds/1977021501','news/rssfeeds/1715249553','industry/rssfeeds/13352306','small-biz/rssfeeds/5575607','wealth/rssfeeds/837555174','tech/rssfeeds/13357270','opinion/rssfeeds/897228639');
  sections <- c('top-news','top-news','business','top-news','business','business','business','technology','opinion');
  et_set <- data.frame(matrix(ncol = 6, nrow = 0));
  for(x in c(1:length(sections))){
    section_set <- getEconomicTimesSectionFeeds(url_sections[x],sections[x]); 
    et_set <- rbind(et_set,section_set);
  }
  return(et_set);
}

getEconomicTimesStories <- function(urls) {
  
  detailed_stories <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  rs <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","url","detail");
  colnames(rs) <- col_names;
  for(x in 1:length(urls)){
    print(paste(x,' of ',length(urls)))
    story <- read_html(x = urls[x]) %>% html_nodes(css = "article div.Normal") %>% html_text() %>% paste(collapse = '');
    story <- gsub("[\r\n\t]", '', story)
    story <- gsub('document.write.*});',replacement = '',story);
    rs <- as.data.frame(cbind(source = 'economic-times',url = urls[x],detail = story));
    detailed_stories <- rbind(detailed_stories,rs);
  }
  return(detailed_stories);
}