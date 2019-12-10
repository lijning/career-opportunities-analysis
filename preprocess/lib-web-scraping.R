###
# description: helper functions to read search results from Indeed.com
# author: Jianing
###

library(rvest)
library(tidyverse)

if(!file.exists("data/raw")){
  setwd(rprojroot::find_rstudio_root_file())
}

JOB.TYPE <- list(intern="internship", full.time="fulltime")


format.search.url <-
  function(key.words,
           loc.words = 'Anywhere',
           start.pos = 1,
           title.words = "",
           company.name = "",
           job.type = "") {
    key.words = key.words %>% trimws() %>% str_replace_all(" ", "+")
    title.words = title.words %>% trimws() %>% str_replace_all(" ", "+")
    company.name = company.name %>% trimws() %>% str_replace_all(" ", "+")
    location.words = loc.words %>% trimws() %>% str_replace_all(" ", "+")
    
    start.pos <- as.character((start.pos - 1) * 50)
    simple.query <-
      "/jobs?q={key.words}&l={Location}&start={Offset}&limit=50"
    
    base.query <-
      "/jobs?as_and={key.words}&radius=50&l={Location}&start={Offset}"
    advanced.query <-
      "&as_ttl={TitleName}&as_cmp={CompanyName}&jt={JobType}"
    noted.query <-
      "&limit=50&sort=date&psf=advsrch&from=advancedsearch"
    
    url.path <- str_c(base.query, advanced.query, noted.query) %>%
      str_replace('\\{key\\.words\\}', key.words) %>%
      str_replace('\\{Location\\}', location.words) %>%
      str_replace('\\{TitleName\\}', title.words) %>%
      str_replace('\\{CompanyName\\}', company.name) %>%
      str_replace('\\{JobType\\}', job.type) %>%
      str_replace('\\{Offset\\}', start.pos)
    
    url.domain <- 'http://www.indeed.com'
    return(str_c(url.domain, url.path))
  }

read.search.result.row <- function(row.node){
  url.domain <- 'http://www.indeed.com'
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
    (function(x){stringr::str_c(url.domain,x)})
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
  results.count <- page.node %>% 
    html_node('#searchCountPages') %>% 
    html_text(TRUE) %>% 
    str_extract('\\d*,?\\d+(?= jobs)') %>% 
    parse_number()
  df.results <- search.results %>% 
    map_dfr(read.search.result.row)
  return(list(data=df.results, total=results.count))
}


search.job <- function(key.words="", location="Anywhere", job.type="", 
                       company.name="", title.words=""){
  t1 = proc.time()
  first.url <-
    format.search.url(
      key.words = key.words,
      loc.words = location,
      title.words = title.words,
      company.name = company.name,
      job.type = job.type
    )
  print(c("First URL:", first.url))
  
  file.name <- str_extract(first.url, "(?<=and=).+(?=&limit)") %>% 
    str_remove_all("(&radius=\\d*)|(&start=\\d*)")
  dump.path <- str_c('data/raw/', file.name, '.csv')
  
  if (file.exists(dump.path)) {
    results.df <- read_csv(dump.path)
  } else{
    first.page <- read_html(first.url)
    cur.result <- read.search.result.page(first.page)
    results.df <- data.frame(cur.result$data)
    page.cnt <- cur.result$total %/% 50
    
    print(c('Total rows number: ', cur.result$total))
    
    for(page.idx in seq(2, page.cnt)){
      cur.url <- format.search.url(key.words = key.words, loc.words = location,
                                  title.words = title.words, 
                                  company.name = company.name, 
                                  job.type = job.type,
                                  start.pos = page.idx)
      Sys.sleep(runif(1,min = 1, max = 5)) # in seconds
      cur.result <- cur.url %>% 
        read_html() %>% read.search.result.page()
      results.df <- bind_rows(results.df, cur.result$data)
      if(nrow(results.df) > cur.result$total)break
    }
    results.df$job_type <- job.type
    
    write_csv(results.df, dump.path)
  }
  
  t2 = proc.time()
  print(t2 - t1)
  return(list(data=results.df, path=dump.path))
  # return(results.df)
}

retrieve.job.detail.page <- function(url){

  page <- read_html(url)
  content <- page %>% 
    html_node("#jobDescriptionText") %>% 
    html_children() %>% 
    html_text() %>% 
    str_c(sep = " | ", collapse = TRUE) %>% 
    str_replace_all("\\n", " | ")
  return(content)
}

add.job.details.on <- function(df=FALSE, path,
                               max.size = 3000, 
                               force=FALSE){
  if(!file.exists(path)){
    warning(path)
  }
  if("detail" %in% colnames(df)){
    print("Already scraped data.")
    return(df)
  }
  workload <- length(df$link)
  if(workload>3000 & force){
    warning("Over 3000 requests to make. Dangerous.")
    return(df)
  }
  #if(workload>max.size){
  #  df = df[1:max.size,]
  #}
  count = 1
  
  t1 = proc.time()
  v.details = c()
  for(lk in df$link){
    count = count + 1
    if(count > max.size){
      v.details = c(v.details, NA)
    } else {
      content <- retrieve.job.detail.page(lk)
      v.details = c(v.details, content)
      Sys.sleep(runif(1,min = 1, max = 3))
    }
  }
  df$detail <- v.details
  t2 = proc.time()
  print(t2 - t1)
  write_csv(df, path = path)
  return(df)
}

