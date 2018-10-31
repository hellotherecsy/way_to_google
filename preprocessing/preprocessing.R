library('ggplot2')
library('dplyr')
library('purrr')
library('stringr')

## Data Loading 

jobs = read.csv("../data/job_skills.csv", header = TRUE)

# Title: The title of the job
# Category: Category of the job
# Location: Location of the job
# Responsibilities: Responsibilities for the job
# Minimum Qualifications: Minimum Qualifications for the job
# Preferred Qualifications: Preferred Qualifications for the job

## 2. ETL 
# (1) remove null colomuns 
jobs <- jobs[!( jobs$Preferred.Qualifications == '' | jobs$Responsibilities  == '' ),]

# (2) Company count - Remove YouTube 
jobs %>% count(Company)
jobs <- jobs[jobs$Company != 'YouTube',]
nrow(jobs) # 1227

# (3) Set Job Location
unique(jobs$Location)
jobs$city <- word(jobs$Location, sep=',' , 1)
jobs$country <- word(jobs$Location, sep=',' , -1)
jobs$city
jobs$country

jobs$city

# (4) Set Job Type  
jobs$job_type <- ifelse( (grepl("Intern ", jobs$Title)) == TRUE, "Internships",
                                 ifelse( (grepl("Intern,", jobs$Title)) == TRUE, "Internships", "Full-time"))


# (5) Set minimum exprience Years 
jobs$dummy <- str_extract(jobs$Minimum.Qualifications, '([0-9]+) year')
jobs$dummy <- ifelse(is.na(jobs$dummy), "0 year", jobs$dummy)

split_year <- function(var){
  return(as.numeric(unlist(str_split(var, " "))[1]))
}
jobs$experience <- sapply(jobs$dummy, split_year)
jobs <- subset(jobs, select = -c(dummy))

jobs$experience

# (6) What is Language 
occurence_mini <- c()
occurence_prefer <- c()

languages <- c('swift','matlab','mongodb','hadoop','cosmos', 'mysql','spark', 'pig', 
               'python', 'java','java,',  'c[++]', 'php', 'javascript', 'objective-c', 
               'ruby', 'perl','c ','c#', ' r,','go,','ajax','elasticsearch',
               'angular','sas','sql','ios','spss','shell','js,','nodejs','html' )

jobs$Minimum.Qualifications <- tolower(jobs$Minimum.Qualifications)
jobs$Preferred.Qualifications <- tolower(jobs$Preferred.Qualifications)

for(i in languages){
  temp_mimi   <- str_count(jobs$Minimum.Qualifications, i)
  temp_prefer <- str_count(jobs$Preferred.Qualifications, i)
  occurence_mini <- c(occurence_mini, sum(temp_mimi, na.rm = TRUE))
  occurence_prefer <- c(occurence_prefer, sum(temp_prefer, na.rm = TRUE))
}
occurence_mini
occurence_prefer
lanquage_cnt <- data.frame(cbind(languages, as.numeric(occurence_mini)))
lanquage_cnt %>% 
  arrange(desc(as.numeric(as.character(V2))))

names(lanquage_cnt) <- c("language", "cnt")
 

# (7) count job Category position
jobs  %>% count(Category) %>% arrange(desc(n))


# (8) Domain & Position 
domain <- c("software", "game", "quality", "data center","data science", "web", "security",
            "sale", "research", "iot", "ui",'cloud','customer','supplier','account'
            ,'android','finance','partner','business development','intern')
positions <- c("lead", "leader","manager", "developer", "engineer", 
               "consultant", "artist", "analyst", "scientist",'administrator', 'architect'
               , 'lead','head' ,'strategist','machine learning','staff','technical specialist',
               'writer','designer','researcher','intern','advisor','strategy',
               'counsel', 'recruiter')

jobs$Title <- tolower(jobs$Title)

jobs$domain <- NA
jobs$positions <- NA


for(i in domain){
  jobs$domain1 <- grepl(i, jobs$Title)
  jobs$domain <- ifelse(jobs$domain1 == "TRUE", i, jobs$domain)
}
jobs$domain <- ifelse(jobs$domain == FALSE, "other", jobs$domain)

for(i in positions){
  jobs$pos1 <- grepl(i, jobs$Title)
  jobs$positions <- ifelse(jobs$pos1 == "TRUE", i, jobs$positions)
}
jobs$positions <- ifelse(jobs$positions == FALSE, "other", jobs$positions)

jobs$positions[is.na(jobs$positions)] <- 'other'
jobs$domain[is.na(jobs$domain)] <- 'other' 

domain <- jobs %>% select(., Title, domain, positions)  


# (9) Education  

jobs$education <- NA

degree_list = c("ba", "bs", "b.tech","bachelor", "phd","ms","master", "mba","m.tech")
jobs$Minimum.Qualifications <- tolower(jobs$Minimum.Qualifications)

for(i in degree_list){
  jobs$deg1 <- grepl(i, jobs$Minimum.Qualifications )
  jobs$education <- ifelse(jobs$deg1 == "TRUE", i, jobs$education)
}
jobs$education <- ifelse(is.na(jobs$education), "other", jobs$education)
jobs <- subset(jobs, select = -c(deg1))



## Final Save to Csv 
jobs <- subset(jobs, select = -c(domain1,pos1))
write.csv(jobs, file = "../data/google_jobs.csv")

