data<- read.csv("survey_data.csv", header = T, na.strings="")


require(ggplot2)

x <- data.frame(data$RESPONSE.ID, data$GENDER)
colnames(x) <- c("ID", "Gender")
x<- na.omit(x)

#ordering levels and changing levels names
x$Gender <- factor(x$Gender, levels = c("Man", "Woman", "Non-binary  or Other", "Prefer not to say" ))

df2<-as.data.frame(with(x, table( Gender)))
df2$p<-  df2$Freq/sum(df2$Freq)

df2
ggplot(df2, aes(Gender, Freq))+
  geom_bar(stat="identity")


ggplot(df2, aes(Gender, p, ))+
  geom_bar(stat="identity")

