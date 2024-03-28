library("dplyr")
library("stringr")
library("tidyverse")
#df1<-read.csv("news.csv")
df1=news
print(df1)
dim(df1)
str(df1)
#Data Analysis
df1<-df1 %>% mutate(across(where(is.character), ~ as.character(str_squish(str_to_title(.)))))
glimpse(df1)
df1 <- df1 %>% 
  mutate (
    satisfaction = str_replace_all(satisfaction, "Neutral Or Dissatisfied", replacement = "No"),
    satisfaction = str_replace_all(satisfaction, "Satisfied", replacement = "Yes"),
    satisfaction = factor(satisfaction, levels = c("Yes", "No"))
  )
head(df1)
# df1<-df1%>%
#   mutate(
#     Arrival_Delay_in_Minutes = as.numeric( str_replace_na(Arrival_Delay_in_Minutes, mean(Arrival_Delay_in_Minutes, na.rm = TRUE))) 
#   )
# head(df1)
  x= table(df1$Customer_Type)
prop.table(x)
df1<-df1%>% 
  mutate (
    satisfaction = str_replace_all(satisfaction, "No", replacement = "0"),
    satisfaction = str_replace_all(satisfaction, "Yes", replacement = "1"),
  )
head(df1)
df1$satisfaction<-as.integer(as.character(df1$satisfaction))
sapply(df1,class)

Mean <- mean(df1$Age)
print(Mean)
n_distinct(df1$Distance)
table(df1$satisfaction)
prop.table(table(df1$satisfaction))

genderResult<- df1%>%group_by(Gender,satisfaction)%>%summarise(Mean= mean(Age),counts=n())
print(genderResult)

count(df1,Class)
cat("\n")
df1%>%group_by(Travel,satisfaction)%>%summarise(counts=n()) 
df1%>% group_by(Class,satisfaction)%>%summarise(counts=n()) 
df1 %>% group_by(Customer_Type,satisfaction) %>% summarise(counts=n()) 
df1 %>% group_by(Gender,Travel) %>% summarise(counts=n())

n_distinct(df1$gloc)
df1 %>% group_by(gloc,satisfaction) %>% summarise(counts=n()) 

df_final<- df1 %>% 
  group_by(satisfaction) %>%
  summarise(avg_Distance = mean(Distance),
            avg_wifi_service = mean(wifi),
            avg_Ease_of_Online_booking = mean(EOB),
            avg_Food_and_drink = mean(Food),
            avg_Online_boarding = mean(boarding),
            avg_Seat_comfort = mean(seatc),
            avg_Inflight_entertainment = mean(ENT),
            avg_On_board_service = mean(observ),
            avg_Leg_room_service = mean(Leg_room_service),
            avg_Baggage_handling = mean(Baggage_handling),
            avg_Checkin_service = mean(Checkin_service),
            avg_Inflight_service = mean(Inflight_service),
            avg_Cleanliness = mean(Cleanliness),
            avg_Departure_Delay = mean(Departure_Delay_in_Minutes),
            avg_Arrival_Delay_ = mean(Arrival_Delay_in_Minutes)
  )
print(df_final)

library(plotrix)
library(janitor)
library(corrplot)
library(gridExtra)
library(patchwork)
library(skimr)
library(RColorBrewer)
library(tidymodels)
library(themis)
library(vip)



#Data Visualization
#pie plot proportion of men to women
#The graph below shows the proportion of men to women from set of passengers.
df1 %>% 
  group_by(Gender) %>%
  summarize(counts = n()) %>%
  mutate(perc = (counts / sum(counts)) * 100) %>%
  arrange(desc(perc)) %>% 
  ggplot(aes("", counts)) +
  geom_col(
    position = "fill",
    color = "black",
    width = 1,
    aes(fill = factor(Gender))
  ) +
  geom_text(
    aes(label = str_c(round(perc), "%"), group = factor(Gender)),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 6,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual (values = c("#B9cf41", "#5741cf")) +
  theme_void() +
  labs(
    title = "Proportion of Men to Women",
    subtitle = "Pie Plot, proportion of Men to Women in Gender Var",
    fill = ""
  )
#proportion of passengers in each class
#The graph below shows the proportion of passengers in each class. 
df1 %>% 
  group_by(Class) %>%
  summarize(counts = n()) %>%
  mutate(perc = (counts / sum(counts)) * 100) %>%
  arrange(desc(perc)) %>% 
  ggplot(aes("", counts)) +
  geom_col(
    position = "fill",
    color = "black",
    width = 1,
    aes(fill = factor(Class))
  ) +
  geom_text(
    aes(label = str_c(round(perc), "%"), group = factor(Class)),
    position = position_fill(vjust = 0.5),
    color = "green",
    size = 6,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual (values = c("#0F7078", "#42132B", "#eeefff")) +
  theme_void() +
  labs(
    title = "Proportion of passengers in each class",
    subtitle = "",
    fill = ""
  )
#Histogram flight distance distribution
#Below is the Histogram plot from this graph we can conclude that majority of flight distance is between 400 to 3000 miles and Long distances flight are not prefered  customers.
df1 %>% 
  select(Distance) %>% 
  ggplot(aes(Distance)) +
  geom_histogram(color = "#0E1856", fill = "#0E1856", bins = 20, alpha = 0.3) +
  geom_vline(aes(xintercept = mean(Distance)), color = "#FD6041", size = 1) +
  labs(
    title = "Flight distance Distributions",
    subtitle = "Histogram Plot",
    x = "Flight Distance",
    y = "Count"
  )
df1<- df1 %>% 
  mutate (
    satisfaction = str_replace_all(satisfaction, "0", replacement = "No"),
    satisfaction = str_replace_all(satisfaction, "1", replacement = "Yes"),
    satisfaction = factor(satisfaction, levels = c("Yes", "No"))
  )
#3D pie satisfaction count
#The graph below shows the total counts of passengers who are satisfied and who are not.
satisfaction_count = df1 %>% count(satisfaction)
data <- c("No 56.66 ","Yes 43.333")
pie3D(satisfaction_count$n, labels = data, radius =1, col = c("yellow", "blue"),explode=0.3, main ="Satisfaction count")

#Bar type of customer
#The barplot graph shows 80,000 of the customers are Loyal to the Airlines and 20,000 of them are Disloyal customers.
barplot(table(df1$Customer),main="Loyal vs Disloyal Customers")


#From the graph below customers of Eco plus class have rated poor, customers of Eco class have rated good and that of Business class have rated very good for seat comfort.
df1 %>% 
  select(seatc, satisfaction, Class) %>%  
  count(seatc, satisfaction, Class) %>% 
  ggplot(aes(x = seatc, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  facet_wrap(vars(Class))+
  labs(
    title = "Satisfaction by Seat Comfort and Class",
    x = "Seat Comfort" ,
    y = "Count",
    fill = ""
  )
#Bar online boarding vs satisfaction
#The graph below shows a high variation along with rates 1,2,3,4 and 5 for online boarding which reduce the time the passenger take for check in purpose.
table_boarding <- tableGrob(df1 %>% 
                              select(boarding, satisfaction) %>%  
                              count(boarding, satisfaction))
df1 %>% 
  select(boarding, satisfaction) %>%  
  count(boarding, satisfaction) %>% 
  ggplot(aes(x = boarding, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  labs(
    title = "Online boarding vs Satisfaction",
    x = "Online boarding" ,
    y = "Count",
    fill = ""
  )
#Histogram No.of gates andNo.of Passengers
#The below is the histogram which shows the traffic of passengers at the different gates, the number of passengers are more at gate no. 3.
hist(df1$gloc, main="No of Gates VS No of passengers", xlab="No of Gates", ylab= "No of passengers",col="light blue",border="black",breaks = seq(from=0, to=5, by=1),ylim=c(0,300))


#The graph below shows the class proportion by Custoomer Type shows that there are loyal and disloyal customers in all three class but disloyal customers are less in Eco Plus compared to Eco and Business.
ggplot(df1,aes(x= Class, fill= Customer_Type)) + geom_bar() + labs(title = "Class Type with Detailed Distribution in terms of Customers", x="Class", y= "No. of passengers", fill = "Customer type")
#


#The graph below shows that as the flight distace increases to above 200 km the arrival time also increases and goes to 4 hours.
line_data <- df1 %>% select(Distance, Arrival_Delay_in_Minutes) %>% filter(Distance <300, Arrival_Delay_in_Minutes >15 )
ggplot(line_data, aes(x = Distance, y = Arrival_Delay_in_Minutes)) +
  stat_summary(fun.y = "mean", geom = "line", size = 1, linetype = "solid") +
  labs(title = "Flight_Distance vs  Arrival_Delay_in_Minutes", x = "Flight_Distance(km)", y = "Arrival_Delay_in_Minutes (min)")

