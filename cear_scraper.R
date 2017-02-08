library(rvest)
library(XML)
library(httr)
library(data.table)
library(stringr)
library(dplyr)

setwd("D:/ndhen/OneDrive/School/HSERV project/Scraper")

# fix 'cannot be authenticated error'
set_config(config(ssl_verifypeer = 0L))

# allow creation of user tag and whatever
myurl <- "http://healtheconomics.tuftsmedicalcenter.org/cear2/search/search_s.aspx"
hello <- html_session(myurl)
form <- html_form(hello)
filled_form <- set_values(form[[1]],
                          `ctl00$MainContent$Tab$tabpanel1$query0` = "epilepsy")
hello.out <- submit_form(session = hello, filled_form, 
                   'ctl00$MainContent$Tab$tabpanel1$Button0')

uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

####### For weight scraping

# takes site, returns table of results
get_weight_table <- function(weight_site){
  table <- weight_site %>%
    html_nodes("table tr") %>%
    html_text()
  return(table)
}

# takes weight id number, returns weight site
get_weight_site <- function(session, number){
  myurl <- str_c("http://healtheconomics.tuftsmedicalcenter.org/cear2/search/weight0.aspx?l=1&articleid=", 
                 number)
  weight_site <- jump_to(session, myurl, user_agent(uastring))
  return(weight_site)
}

# takes table, returns whether it has a valid result
has_weights <- function(table){
  return(ifelse(length(table) != 1,
                TRUE,
                FALSE))
}


process_weights <- function(session, number, dt){
  weight_site <- get_weight_site(session, number)
  table <- get_weight_table(weight_site)
  if(has_weights(table)){
    for(i in 3:(length(table) - 1)){
      dt <- rbind(dt, parse_weight_dt(table[i]))
    }
  }
  return(dt)
}

# takes a row of the weights table and returns a parsed version of that row
parse_weight_dt <- function(row){
  row <- substr(row, 1, nchar(row) - 6)
  from_end <- get_utility(row)
  temp <- data.table(article_id = substr(row, 1, 13),
                     health_state = substr(row, 14, nchar(row) - from_end - 1),
                     utility = substring(row, nchar(row) - from_end))
  return(temp)
}

# takes a string and returns spaces from end of utility substring
get_utility <- function(row){
  options(warn = -1)
  for(i in 6:1){
    temp <- substring(row, nchar(row) - i)
    if(as.numeric(temp) >= 0 & as.numeric(temp) <= 1 & !is.na(as.numeric(temp))){
      options(warn = 0)
      return(i)
    }
  }
  options(warn = 0)
  return(NA)
}

# initializes empty weight data table
init_weight_dt <- function(){
  temp <- data.table(article_id = vector(),
                     health_state = vector(),
                     utility = vector())
  return(temp)
}

# contains code to scrape complete range of weights
weight_wrapper <- function(session, year, start, end, dt){
  for(i in as.numeric(start):as.numeric(end)){
    id <- str_c(year, "-01-", str_pad(i, 5, pad = "0"))
    dt <- process_weights(session, id, dt)
    print(id)
    Sys.sleep(0.5)
  }
  return(dt)
}

# aaaaand scrape!
weights <- init_weight_dt()
weights_2002 <- weight_wrapper(hello.out, "2002", "00000", "03046", weights)
write.csv(weights_2002, "weights_2002.csv")
weights_2003 <- weight_wrapper(hello.out, "2003", "00000", "03046", weights)
write.csv(weights_2003, "weights_2003.csv")
weights_2004 <- weight_wrapper(hello.out, "2004", "00000", "03046", weights)
write.csv(weights_2004, "weights_2004.csv")
weights_2005 <- weight_wrapper(hello.out, "2005", "00000", "03046", weights)
write.csv(weights_2005, "weights_2005.csv")
weights_2006 <- weight_wrapper(hello.out, "2006", "00000", "03046", weights)
write.csv(weights_2006, "weights_2006.csv")
weights_2007 <- weight_wrapper(hello.out, "2007", "02660", "03528", weights)
write.csv(weights_2007, "weights_2007.csv")
weights_2008 <- weight_wrapper(hello.out, "2008", "03046", "05560", weights)
write.csv(weights_2008, "weights_2008.csv")
weights_2009 <- weight_wrapper(hello.out, "2009", "03528", "06448", weights)
write.csv(weights_2009, "weights_2009.csv")
weights_2010 <- weight_wrapper(hello.out, "2010", "05560", "07718", weights)
write.csv(weights_2010, "weights_2010.csv")
weights_2011 <- weight_wrapper(hello.out, "2011", "06448", "09736", weights)
write.csv(weights_2011, "weights_2011.csv")
weights_2012 <- weight_wrapper(hello.out, "2012", "07718", "10878", weights)
write.csv(weights_2012, "weights_2012.csv")
weights_2013 <- weight_wrapper(hello.out, "2013", "09736", "17011", weights)
write.csv(weights_2013, "weights_2013.csv")
weights_2014 <- weight_wrapper(hello.out, "2014", "17011", "21400", weights)
write.csv(weights_2014, "weights_2014.csv")
weights_2015 <- weight_wrapper(hello.out, "2015", "10878", "21400", weights)
write.csv(weights_2015, "weights_2015.csv")
weights <- rbind(weights_2015, weights_2014, weights_2013, weights_2012,
                 weights_2011, weights_2010, weights_2009, weights_2008,
                 weights_2007, weights_2006, weights_2005, weights_2004,
                 weights_2003, weights_2002)
write.csv(weights, "weights.csv")


# test urls
# has results : http://healtheconomics.tuftsmedicalcenter.org/cear2/search/weight0.aspx?l=1&articleid=2015-01-21069
# no results : http://healtheconomics.tuftsmedicalcenter.org/cear2/search/weight0.aspx?l=1&articleid=2015-01-21155

########## For article scraping

# takes site, returns number of results
get_article_table <- function(article_site){
  table <- article_site %>%
    html_nodes("table tr") %>%
    html_text()
  return(table)
}

# takes article id number, returns article site
get_article <- function(session, number){
  myurl <- str_c("http://healtheconomics.tuftsmedicalcenter.org/cear2/search/detail.aspx?ArticleId=", 
                 number)
  article_site <- jump_to(session, myurl, user_agent(uastring))
  return(article_site)
}

# takes table, returns whether it has a valid result
has_article <- function(table){
  return(ifelse(length(table) != 22,
                FALSE,
                TRUE))
}

# takes session, article ID and data table. Returns data table with updated article characteristics
process_article <- function(session, number, dt){
  article <- get_article(session, number)
  table <- get_article_table(article)
  if(has_article(table)){
    dt <- rbind(dt, parse_article_dt(table))
  }
  return(dt)
}

# initializes the data table holding article info
init_article_dt <- function(){
  temp <- data.table(article_id = vector(),
                     full_ref = vector(),
                     study_type = vector(),
                     review = vector(),
                     intervention = vector(),
                     disease_class = vector(),
                     setting = vector(),
                     funding = vector())
  return(temp)
}

# takes article, parses info strings
parse_article_dt <- function(table){
  temp <- data.table(cea_id = substr(table[[2]], 25, 37),
                     full_ref = substring(table[[3]], 16),
                     study_type = substring(table[[4]], 12),
                     review = substring(table[[5]], 13),
                     intervention = substring(table[[6]], 22),
                     disease_class = substring(table[[7]], 24),
                     setting = substring(table[[8]], 22),
                     funding = substring(table[[10]], 30))
  return(temp)
}

# takes a year, range of article IDs, and data table. Returns a data table with 
# the parsed info of all articles in that range / year.
article_wrapper <- function(session, weights, dt){
  weights_uniq <- weights %>%
    group_by(article_id) %>%
    distinct(article_id)
  for(i in 1:dim(weights_uniq)[[1]]){
    id <- weights_uniq[i,]$article_id
    dt <- process_article(session, id, dt)
    print(id)
    Sys.sleep(0.5)
  }
  return(dt)
}

# And now we scrape!
articles <- init_article_dt()
articles <- article_wrapper(hello.out, weights, articles)

# merge datasets
analysis <- weights %>%
  left_join(articles)
write.csv(analysis, "analysis.csv")