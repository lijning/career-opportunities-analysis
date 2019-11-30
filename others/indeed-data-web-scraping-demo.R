#
# title: webscraping Indeed
# author: Jianing Li
#

###############
# Test scraping one page of search results

library(rvest)
library(stringr)
library(tidyverse)

website.base.url <- 'http://www.indeed.com'

local.page.url <- 'data/raw/Indeed_searchresults_demo.html'
if(file.exists(local.page.url)){
  webpage <- read_html(local.page.url)
}else{
  webpage <- read_html(str_c(website.base.url, 
                             "/jobs?q=data+scientist+%2420%2C000&l=New+York&start=10"))
  write_html(webpage, local.page.url)
}

# The title of each search result, containing a link to detail page.
job.title.nodes <- webpage %>% 
  html_nodes("a.jobtitle")

# The links to detail pages
job.detail.links <- job.title.nodes %>%
  html_attr("href") %>% 
  (function(x){stringr::str_c(website.base.url,x)})

# Job Keys
job.keys <- webpage %>% 
  html_nodes("div.row.result") %>% 
  html_attr("data-jk")
empn.ids <- webpage %>% 
  html_nodes("div.row.result") %>% 
  html_attr("data-empn")
# A sample url of detail page on the searching page
# jobs?q=data%20scientist%20%2420%2C000&l=New%20York&start=10&advn=3705709802749162&vjk=f65536ea5ecb18ac
# vjk: job.keys, advn: empn.ids (employer?)


# Job Title Name
job.title.nodes %>% 
  html_text(trim = TRUE) %>%
  stringr::str_replace_all("\n","")
# [1] "Data Engineer/Scientist"

# Tag name
html_name(job.title.node)

# selector for short description: #resultsCol ul


########
# Make it a data frame

# The search results
search.results <- webpage %>% 
  html_nodes("div.row.result")

read.search.result.row <- function(row.node){
  job.title.node <- row.node %>% 
    html_node("a.jobtitle")
  job.title <- job.title.node %>% 
    html_text(trim = TRUE) %>%
    stringr::str_replace_all("\n","")
  company.name <- row.node %>% 
    html_node('.company') %>% html_text(TRUE)
  company.rate <- row.node %>% 
    html_node('.ratingsContent') %>% html_text(TRUE)
  job.location <- row.node %>% 
    html_node('.location') %>% html_text(TRUE)
  posted.date <- row.node %>% 
    html_node('.date') %>% html_text(TRUE)
  salary.text <- row.node %>% 
    html_node(".salaryText") %>% html_text(TRUE)
  short.description <- row.node %>% 
    html_node('ul') %>% html_text(TRUE)
  job.detail.link <- job.title.node %>% 
    html_attr("href") %>% 
    (function(x){stringr::str_c(website.base.url,x)})
  job.keys <- html_attr(row.node, "data-jk")
  empn.ids <- html_attr(row.node, "data-empn")
  return(list(title=job.title,company=company.name,
              empn_rate=company.rate,
              posted_time=posted.date,
              location=job.location,
              salary=salary.text,
              preview=short.description,
              link=job.detail.link,
              job_key=job.keys,empn_id=empn.ids))
}



read.search.result.page <- function(page.node){
  search.results <- page.node %>% 
    html_nodes("div.row.result")
  df.results <- search.results %>% 
    map_dfr(read.search.result)
  return(df.results)
}

df.results <- read.search.result.page(webpage)

##########
# Test scraping one of the detail page
local.page.url <- 'data/raw/Indeed_jobdetail_demo.html'
if(file.exists(local.page.url)){
  webpage <- read_html(local.page.url)
}else{
  webpage <- read_html(str_c(website.base.url,
                              "/viewjob?jk=f65536ea5ecb18ac"))
  write_html(webpage, local.page.url)
}

webpage %>% 
  html_node("#jobDescriptionText") %>% 
  html_text(TRUE)

#########

