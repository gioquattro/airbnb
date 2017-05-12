library(data.table)
library(sqldf)
library(ggplot)

w1 = fread("output/words_count.csv")
w2 = fread("output/word_count.csv")
w3 = fread("output/word_pairs_count.csv")

w12 = sqldf("select w1.word, w1.N N1, w2.N N2
             from w1
             join w2 on w1.word=w2.word")

ggplot(w12, aes(x=N1, y=N2)) + 
  geom_point(alpha=0.025) +
  scale_x_sqrt( breaks=c(0,1,2,3,4,5,10,20,30,60,100) ) +
  scale_y_sqrt( breaks=c(0,1,3,5,10,20,30,60,100,200,300,600,1000))

ggplot(w1, aes(N)) + 
  geom_histogram(col="black", fill="green", bins=104) + 
  scale_x_sqrt(breaks=c(1,2,3,4,5,7,10,20,30,40,50,70,100)) +
  scale_y_sqrt(breaks=c(10,100,1000,3000,5000,10000,20000,30000))

ggplot(w2, aes(N)) + 
  geom_histogram(col="black", fill="green", bins=104) + 
  scale_x_sqrt(breaks=c(1,2,3,4,5,7,10,20,30,40,50,70,100,500,1000)) +
  scale_y_sqrt(breaks=c(10,100,1000,3000,5000,10000,20000,30000))

ggplot(w3, aes(N)) + 
  geom_histogram(col="black", fill="green", bins=104) + 
  scale_x_sqrt(breaks=c(1,2,3,4,5,7,10,20,30,40,50,70,100,500,1000)) +
  scale_y_sqrt(breaks=c(10,100,1000,3000,5000,10000,20000,30000))
