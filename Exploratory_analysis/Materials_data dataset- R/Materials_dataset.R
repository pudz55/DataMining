
df_data<-read.csv("C:/users/Puddi/Desktop/marketing_data.csv") 

install.packages("dplyr", repos='http://cran.us.r-project.org')
library(dplyr)

install.packages("ggplot2", repos='http://cran.us.r-project.org')
library(ggplot2)

head(df_data, n = 5L)
##task-1-a
print.data.frame(df_data)

##task-1-b
nrow(df_data)
ncol(df_data)

##task-1-c
summary(df_data$Year_Birth)
summary(df_data$MntMeatProducts)

##task-1-d
d=df_data$Year_Birth
(unique(d))
length(unique(d))


##We had to create a new dataframe to show income as integer and remove special characters
df_data1<-df_data

df_data1[df_data1==""]<-NA
df_data1$Income = as.numeric(gsub("[\\$, ]", "", df_data1$Income))
df_data1$Income


##We  have to use second dataframe to mutate the values

df_data2 <- df_data1 %>%mutate(total_product_purchase = MntWines+MntFruits+MntMeatProducts
                               +MntFishProducts+MntSweetProducts+MntGoldProds)
df_data2 <- df_data2%>%mutate(Income = Income/12)
df_data2 <- df_data2%>%mutate(Year_Birth = 2022 - Year_Birth)



##Task-2a
salary_slab<-subset(df_data1, df_data1$Income > 100000)
(salary_slab)
nrow(salary_slab)

##Task-2b

data_interest <- subset(df_data1, df_data1$Year_Birth >=1960 & df_data1$Year_Birth <=1970)
nrow(data_interest)

##Task-2c

task2c<-df_data1[order(df_data1$Income, decreasing = TRUE), ]
head(select(task2c,1,5), n = 10L)



##task3a

task3a<-ggplot(df_data2, aes(x = Year_Birth, y = NumWebVisitsMonth)) +
geom_area(fill='#00FF00', alpha=2) +
facet_wrap(~ Country)
task3a+ylim(0,75)+ theme_dark()

##task3b

data_interest1 <- subset(df_data1, df_data1$Year_Birth >=1960 & df_data1$Year_Birth <=1970)
gfh<-group_by(data_interest ,Country)%>%summarise(Total=n())
##head(gfh, n=15L)

pie(gfh$Total , labels = paste0(gfh$Total),
    main = "No. of people born in ten year period (1960-1970) in each country", col = rainbow(length(gfh$Total)))
legend("topright", c(gfh$Country), cex = 0.8,
       fill = rainbow(length(gfh$Total)))



##Task4

asd1<-group_by(df_data2,Country,Education)%>%summarise(total_product_purchase)
task4<-asd1%>%ggplot(mapping=aes(y=Country,x=total_product_purchase,fill=Education))+
  geom_bar(stat='identity',position = 'dodge', width=.8)

task4+theme_gray()+xlim(0,3750)




