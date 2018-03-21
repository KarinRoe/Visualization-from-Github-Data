data<- read.csv("survey_data.csv", header = T, na.strings="")


require(ggplot2)
require(plyr)

x <- data.frame(data$RESPONSE.ID, data$GENDER, data$AGE, data$DISCOURAGING.BEHAVIOR.UNWELCOMING.LANGUAGE, data$FORMAL.EDUCATION, data$AGE.AT.FIRST.COMPUTER.INTERNET)
colnames(x) <- c("ID", "Gender", "Age", "Bad.experience", "Education", "AGE.first.PC.w.In")
summary(x)
x<- na.omit(x)

#ordering levels and changing levels names
x$Gender <- factor(x$Gender, levels = c("Man", "Woman", "Non-binary  or Other", "Prefer not to say" ))
levels(x$Education) <- c("BA","Ph.D.","<sec.HS", "MA","High School", "College", "Appre.")
x$Education <- factor(x$Education, ordered = TRUE, levels = c("<sec.HS", "High School", "Appre.", "College", "BA", "MA", "Ph.D."))
x$Bad.experience <- factor(x$Bad.experience, levels = c("Yes", "No" ))
x$AGE.first.PC.w.In <- factor(x$AGE.first.PC.w.In, ordered = TRUE, levels = c("Younger than 13 years old", "13 - 17 years old", "18 - 24 years old", "25 - 45 years old", "Older than 45 years old"))

#first PC with internet against Gender
df2<-as.data.frame(with(x, table(AGE.first.PC.w.In, Gender)))
df2<- ddply(df2, .(Gender), transform, p = Freq/sum(Freq))

ggplot(df2, aes(AGE.first.PC.w.In, p, fill=AGE.first.PC.w.In))+
  facet_grid(~Gender)+ 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_blank(), axis.ticks=element_blank())

##Age against Gender - ????????has NAs still in them
# first make a dataframe with frequencies
####df3 <- as.data.frame(with(x, table(Age,Gender)))

# next: compute percentages per group
####df3 <- ddply(df3, .(Gender), transform, p = Freq/sum(Freq))
# and plot
###ggplot(df3, aes(Age, p, fill=Age))+
  facet_grid(~Gender)+
  geom_bar(stat="identity")



#education against Gender
df1<-as.data.frame(with(x, table(Education, Gender)))
df1<- ddply(df1, .(Gender), transform, p = Freq/sum(Freq))

ggplot(df1, aes(Education, p, fill=Education))+
  facet_grid(~Gender)+ 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_blank(), axis.ticks=element_blank())


######### unwelcoming language
# first make a dataframe with frequencies
df <- as.data.frame(with(x, table(Bad.experience,Gender)))

# next: compute percentages per group
df <- ddply(df, .(Gender), transform, p = Freq/sum(Freq))
# and plot
ggplot(df, aes(Bad.experience, p, fill=Bad.experience))+
  facet_grid(~Gender)+
  geom_bar(stat="identity")

