getBookMyShowEventDetails <- function(location,url_section,section){
  
  stories <- data.frame(matrix(ncol = 10, nrow = 0),stringsAsFactors = FALSE);
  col_names <- c("source","section","date","time","price","location","place","title","url","img");
  colnames(stories) <- col_names;
  url <- paste0('https://in.bookmyshow.com/',location,'/events/',url_section);
  print(url);
  page_source <- read_html(x = url);
  event_date <- page_source %>% html_nodes(css = 'div.__evt-date-col') %>% html_text() %>% gsub(pattern = '\n',replacement = '') %>% trimws() %>% gsub(pattern = '\t\t\tonwards',replacement = '') %>% gsub(pattern = '\t\t\t',replacement = '-');
  event_date <- as.Date(strptime(event_date,format = '%d-%b'));
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
  event_price <- page_source %>% html_nodes(css = 'div.card-tag:nth-child(4)') %>% html_text(trim = TRUE) %>% gsub(pattern = '\n',replacement = '');
  event_place <- page_source %>% html_nodes(css = 'div.card-tag:nth-child(2)') %>% html_text() %>% gsub(pattern = '\n',replacement = '') %>% trimws();
  event_title <- page_source %>% html_nodes(css = '.card-title') %>% html_text() %>% gsub(pattern = '\n',replacement = '') %>% trimws();
  event_img <- page_source %>% html_nodes(css = ".card-img>img:nth-child(1)") %>% html_attr('data-src') %>% gsub(pattern = '\\?.*',replacement = '');
  event_img <- paste0('https:',event_img );
  event_url <- page_source %>% html_nodes(css = "#deventDetails aside>a") %>% html_attr('href')
  event_url <- paste0('https://in.bookmyshow.com',event_url );
  events <- as.data.frame(cbind(source = 'book my show',section = section,date = event_date,time = "",price = event_price, location = location,place = event_place,title = event_title,url = event_url,img = event_img));
  return(events);
}

getBMSEvents <- function(){
  locations <- c('chennai','mumbai');
  url_sections <- c('food-and-drinks','comedy','gaming','adventure','meetups','workshops','conferences','music','performances','exhibitions','amusement-parks');
  sections <- c('food-and-drinks','entertainment','entertainment','sports-and-wellness','meetups','conference-and-workshops','conference-and-workshops','art-and-music','art-and-music','conference-and-workshops','entertainment');
  #'Food & Drinks','Entertainment','Sports & Wellness','Meetups','Conference & Workshops','Education','Art & Music'
  bms_set <- data.frame(matrix(ncol = 10, nrow = 0));
  for(li in 1:length(locations)){
    for(x in c(1:length(sections))){
      section_set <- getBookMyShowEventDetails(locations[li],url_sections[x],sections[x]); 
      bms_set <- rbind(bms_set,section_set);
    }
  }
  
  return(bms_set);
}

