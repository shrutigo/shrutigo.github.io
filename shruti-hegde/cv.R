library(vitae)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(glue)

education <- function(){
  edu_raw <- read_csv(here("data", "edu.csv"))
  edu <-
    edu_raw %>% 
    mutate(pi = if_else(!is.na(pi), glue("Advisor: {pi}"), as.character(NA)),
           dissertation = glue("\\textbf{[dissertation]}", .open = "[", .close = "]")) %>% 
    gather(dissertation, pi, key = "whytype", value = "why")
  
  detailed_entries(edu, 
                   when = glue("{date}"),
                   what = degree,
                   where = location,
                   with = university,
                   why = why,
                   .protect = FALSE)
}


jobs <- function(){
  jobs_raw <- read_csv(here::here("data", "jobs.csv"))
  jobs <-
    jobs_raw %>% 
    fill(Start, End, What, With, Where) %>%  
    mutate(When = case_when(
      is.na(End) ~ glue("{Start}--Present"),
      TRUE ~ glue("{Start}--{End}")
    )) %>% 
    mutate(date_to_order = if_else(is.na(End), today() + years(2), as.Date(parse_date_time2(End, "my"))) )
  
  jobs %>% 
    arrange(desc(date_to_order)) %>% 
    detailed_entries(
      what = What,
      when = When,
      with = With,
      where = Where,
      why = Why,
      .protect = FALSE
    )  
}

publications <- function(){
  scholar::get_publications("BL5HwQ0AAAAJ", flush = TRUE) %>%
    arrange(desc(year)) %>%
    detailed_entries(
      what = title,
      with = author,
      where = journal
    )
}

skills <- function(){
  skills_raw <- read_tsv(here::here("data", "skills.tsv"), col_names=FALSE)
  colnames(skills_raw) <- c("skill_type", "skills")
  skills_raw %>%
    detailed_entries(what = skill_type, why = skills) 
}

presentations <- function(type){
  presentations_raw <- read_csv(here("data", "presentations.csv"))
  
  if(type == 'research'){
    presentations_raw %>% 
      filter(Tag == "Presentation") %>%
      detailed_entries(what = Title,
                       when = When,
                       with = Conference,
                       where = Location,
                       why = Award)
  }else{
    presentations_raw %>% 
      filter(Tag == "Poster") %>%
      detailed_entries(what = Title,
                       when = When,
                       with = Conference,
                       where = Location,
                       why = Award) 
  }
}

teaching <- function(){
  data.frame(
    when = c("08/2017-12/2017"),
    what = c("Chemical Engineering Tutor"),
    why = c("Course Taught: Thermodynamics II", "Guest lectures on thermodynamic cycles and energy systems")
  ) %>%
    detailed_entries(
      when = when,
      what = what,
      why = why 
    )
}

