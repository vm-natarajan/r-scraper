getBBCSectionFeeds <- function(rdriver,url_section,section){
  #url_section can be 1. technology, 2. business, 3. science_and_environment 4. entertainment_and_arts 5. health 6. politics 7. world 8. news
  today <- Sys.Date();
  today <- format(today, format="%m-%d-%Y")
  stories <- data.frame(matrix(ncol = 6, nrow = 0));
  col_names <- c("source","date","section","title", "description","url");
  colnames(stories) <- col_names;
  url <- paste0('http://feeds.bbci.co.uk/news/',url_section,'/rss.xml');
  print(url);
  rdriver$open();
  rdriver$navigate(url);
  rdriver$refresh();
  rdriver$screenshot(display = TRUE);
  page_source <- rdriver$getPageSource()[[1]] %>% read_html();
  print(page_source);
  stories_uri <- page_source %>% html_nodes(css = '#item a') %>% html_attr('href');
  stories_title <- page_source %>% html_nodes(css = '#item a') %>% html_text(trim = TRUE);
  stories_desc <- page_source %>% html_nodes(css = '#item div') %>% html_text(trim = TRUE);
  stories <- as.data.frame(cbind(date = today,source = 'bbc',section = section,title = stories_title,description = stories_desc,url = stories_uri));
  return(stories);
}

getBBCFeeds <- function(rdriver){
  url_sections <- c('technology','business','science_and_environment','entertainment_and_arts','health','politics','world','');
  sections <- c('technology','business','science','entertainment','health','politics','world','top-news');
  bbc_set <- data.frame(matrix(ncol = 6, nrow = 0));
   for(x in c(1:length(sections))){
     section_set <- getBBCSectionFeeds(rdriver,url_sections[x],sections[x]); 
     bbc_set <- rbind(bbc_set,section_set);
   }
  return(bbc_set);
}