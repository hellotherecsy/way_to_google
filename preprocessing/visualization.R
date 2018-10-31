

## Pie chart for Job Type ##
mytable <- table(jobs$job_type)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Full-time versus part-time")

job_type <- jobs %>% count(job_type)
job_type$n

## pie chart count
ggplot(job_type, aes(x = "", y=n, fill = factor(job_type))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(#axis.line = element_blank(), 
    plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Google Job Classification", 
       caption="Google Job Skills") + coord_polar(theta = "y", start=0)

 
### base_on degree ### 
edu <- as.data.frame(table(amazon$education))
colnames(edu) <- c("Education", "Freq")
p10 <- ggplot(data = edu, aes(Education, Freq, fill = Education)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Freq), color="black", size=3) +
  ggtitle("Jobs based on degrees") + 
  guides(fill=FALSE) +
  coord_flip() 
p10


## 
p15 <- ggplot(data = lanquage_cnt, aes(language, cnt, fill = language)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=cnt), color="black", size=3) +
  ggtitle("Languages in job description") + 
  guides(fill=FALSE) +
  coord_flip() 
p15