getZomotoDealsDetails <- function(location,url_section,section){
  
  stories <- data.frame(matrix(ncol = 10, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","section","date","time","price","location","place","title","url","img");
  colnames(stories) <- col_names;
  url <- paste0('https://www.zomato.com/',location,'/restaurants?offers=1&page=1');
  pages <- as.numeric(read_html(x = url) %>% html_node(css = 'div.pagination-number b:nth-of-type(2)') %>% html_text(trim = TRUE));
  for(x in 1:pages){
    url <- paste0('https://www.zomato.com/',location,'/restaurants?offers=1&page=',x);
    page_source <- read_html(x = url);
    title <- page_source %>% html_nodes(css = 'a.result-title') %>% html_text(trim = TRUE);
    locality <- page_source %>% html_nodes(css = 'a.search-page-text') %>% html_text(trim = TRUE);
    place <- page_source %>% html_nodes(css = 'div.search-result-address') %>% html_text(trim = TRUE);
    cuisines <- page_source %>% html_nodes(css = 'div.search-page-text>div:nth-of-type(1) span:nth-of-type(2)') %>% html_text(trim = TRUE);
    price <- page_source %>% html_nodes(css = 'div.search-page-text>div:nth-of-type(2) span:nth-of-type(2)') %>% html_text(trim = TRUE) %>% gsub(pattern = '\u20b9',replacement = 'Rs.')
    deal <- page_source %>% html_nodes(css = 'a.zgreen') %>% html_text(trim = TRUE) %>% gsub(pattern = '\u20b9',replacement = 'Rs.');
    event_date <-  page_source %>% html_nodes(css = 'div.browse-card.d-inline-block>a:nth-of-type(2) div:nth-child(6)') %>% html_text(trim = TRUE) %>% strsplit(split = ' , ');
    event_date <- sapply(event_date, "[", 1);
    event_date <- as.Date(strptime(event_date,format = '%a, %d %b'));
    event_date <- format(event_date, format="%Y-%m-%d");
    current_date <- Sys.Date();
    ifelse(is.na(event_date),yes = event_date <- current_date , no = current_date <- current_date)
    for(x in 1:length(event_date)){
      if(event_date[x] < current_date){
        tmp <- as.POSIXlt(event_date[x]);
        tmp$year <- tmp$year+1;
        event_date[x] <- as.Date(tmp)
      }
    }
    com_title <- page_source %>% html_nodes(css = 'div.browse-card.d-inline-block>a:nth-of-type(2) div:nth-child(8)') %>% html_text(trim = TRUE) %>% gsub(pattern = '\u20b9',replacement = 'Rs.') %>% gsub(pattern = 'Free',replacement = '0')
    event_place <- page_source %>% html_nodes(css = 'div.browse-card.d-inline-block>a:nth-of-type(2) div:nth-child(4)') %>% html_text(trim = TRUE);
    event_title <- page_source %>% html_nodes(css = 'div.browse-card.d-inline-block>a:nth-of-type(2) div:nth-child(2)') %>% html_text(trim = TRUE);
    event_img <- page_source %>% html_nodes(css = 'div.browse-card.d-inline-block div.lazy-load-image') %>% html_attr('data-src') %>% gsub(pattern = '=.*',replacement = '');
    event_url <- page_source %>% html_nodes(css = 'div.browse-card.d-inline-block>a:nth-child(1)') %>% html_attr('href') %>% gsub(pattern = '\\?.*',replacement = '');
    event_url <- paste0('https://www.eventshigh.com',event_url );
  }
  
  events <- as.data.frame(cbind(source = 'EventsHigh',section = section,date = event_date,time = event_time,price = event_price, location = location,place = event_place,title = event_title,url = event_url,img = event_img));
  return(events);
}

getEHEvents <- function(){
  locations <- c('chennai','mumbai');
  url_sections <- c('food','concerts+and+shows','parties+and+nightlife','adventure+and+sports','classes+and+workshops');
  sections <- c('food-and-drinks','entertainment','entertainment','sports-and-wellness','conference-and-workshops');
  #'Food & Drinks','Entertainment','Sports & Wellness','Meetups','Conference & Workshops','Education','Art & Music'
  eh_set <- data.frame(matrix(ncol = 10, nrow = 0));
  for(li in 1:length(locations)){
    for(x in c(1:length(sections))){
      section_set <- getEventsHighEventDetails(locations[li],url_sections[x],sections[x]); 
      eh_set <- rbind(eh_set,section_set);
    }
  }
  
  return(eh_set);
}

