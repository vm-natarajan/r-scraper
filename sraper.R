getTOIStories <- function(rdriver){
  
  stories <- data.frame(matrix(ncol = 3, nrow = 0));
  col_names <- c("title", "content","url");
  colnames(stories) <- col_names;
  url <- 'https://timesofindia.indiatimes.com/business';
  
  rdriver$open();
  rdriver$navigate(url);
  rdriver$refresh();
  rdriver$screenshot(display = TRUE);
  stories_uri <- rdriver$getPageSource()[[1]] %>% read_html() %>% html_nodes(css = '.main-content span.w_tle a') %>% html_attr('href')
  stories_uri <- stories_uri[grep('^/business',stories_uri)];
  url <- rdriver$getCurrentUrl();
  stories_url <- lapply(stories_uri, function(x) paste(url,x,sep = ''))
  rdriver$screenshot(display = TRUE);
  
  for(url in stories_url){
    
    rdriver$navigate(url);
    rdriver$screenshot(display = TRUE);
    title <- rdriver$findElement(using = 'css',value = 'arttitle')$getElementText();
    content <- rdriver$findElement(using = 'css',value = '.article_content .section1')$getElementText();
    top_comment <- rdriver$findElements(using = 'css',value = '.article_content .topcomment');
    
    if( length(top_comment) > 0 ){
      top_comment <- top_comment[[1]]$getElementText();
      content <- gsub(pattern = top_comment,replacement = '',x = content)
    }
    
    story <- data.frame(title,content,url);
    colnames(story) <- c("title", "content","url");
    stories <- rbind(stories,story);
    
  }
  
  return(stories);
  
}