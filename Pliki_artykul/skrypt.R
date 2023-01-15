library(tidyverse)
library(psych)
library(effsize)

lista_plikow <- list.files("csv")

lista_plikow <- paste("csv", lista_plikow, sep = "/")

dane1 <- read_csv(lista_plikow)

# dane1 <- read.csv("Documents/Studia/II-Rok/Metodologia/Flanker/data/dane1.csv")

dane2 <- dane1 %>%
  select(kolorowanka, Stimuli, Odp.keys, Odp.rt, Entropia, participant, `Wiek (w latach)`)

dane3 <- subset(dane2, Stimuli!="")

dane3$Stimuli[dane3$Stimuli == "<<<<<"] <- "left,left,left,left,left"
dane3$Stimuli[dane3$Stimuli == "<<><<"] <- "left,left,right,left,left"
dane3$Stimuli[dane3$Stimuli == ">>>>>"] <- "right,right,right,right,right"
dane3$Stimuli[dane3$Stimuli == ">><>>"] <- "right,right,left,right,right"
dane3$Stimuli[dane3$Stimuli == "--<--"] <- "no,no,left,no,no"
dane3$Stimuli[dane3$Stimuli == "-->--"] <- "no,no,right,no,no"
dane3$Stimuli[dane3$Stimuli == ">>>>> "] <- "right,right,right,right,right"

df3 <- dane3%>%
  mutate(Type = case_when(Stimuli=="left,left,left,left,left"|Stimuli=="right,right,right,right,right" ~ "Congurent",
                          Stimuli=="left,left,right,left,left"|Stimuli=="right,right,left,right,right" ~ "Incongurent",
                          Stimuli=="no,no,left,no,no"|Stimuli=="no,no,right,no,no" ~ "Neutral"))%>%
  rename('Ans' = "Odp.keys",
         'Time' = 'Odp.rt',
         'Entropy' = 'Entropia',
         'Wiek' = 'Wiek (w latach)')%>%
  mutate(Corr = case_when((Stimuli=="left,left,left,left,left"&Ans=="left") ~ "correct",
                          (Stimuli=="right,right,left,right,right"&Ans=="left") ~ "correct",
                          (Stimuli=="no,no,left,no,no"&Ans=="left") ~ "correct",
                          (Stimuli=="right,right,right,right,right"&Ans=="right") ~ "correct",
                          (Stimuli=="left,left,right,left,left"&Ans=="right") ~ "correct",
                          (Stimuli=="no,no,right,no,no"&Ans=="right") ~ "correct",
                          (Stimuli=="left,left,left,left,left"&Ans=="right") ~ "incorrect",
                          (Stimuli=="right,right,left,right,right"&Ans=="right") ~ "incorrect",
                          (Stimuli=="no,no,left,no,no"&Ans=="right") ~ "incorrect",
                          (Stimuli=="right,right,right,right,right"&Ans=="left") ~ "incorrect",
                          (Stimuli=="left,left,right,left,left"&Ans=="left") ~ "incorrect",
                          (Stimuli=="no,no,right,no,no"&Ans=="left") ~ "incorrect"))

df3 <- df3 %>% 
  mutate(mandala = if_else(nchar(kolorowanka) == 115, "tak", "nie"))

str(dane3)

wyniki <- df3%>%
  group_by(participant)
asymetria <- df3%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time),  Med_rt=median(Time), IQR=IQR(Time), Qx=(IQR/2), V_Q=((Qx/Med_rt)*100), A_Qx=((quantile(Time, 0.25)+quantile(Time,0.75)-2*median(Time))/(IQR(Time)/2)))
wyniki1 <- wyniki%>%
  group_by(mandala, Entropy)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time),  Med_rt=median(Time), IQR=IQR(Time), Qx=(IQR/2), V_Q=((Qx/Med_rt)*100), A_Qx=((quantile(Time, 0.25)+quantile(Time,0.75)-2*median(Time))/(IQR(Time)/2)))
wyniki2 <- wyniki%>%
  group_by(mandala, Type, Entropy)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time),  Med_rt=median(Time), IQR=IQR(Time), Qx=(IQR/2), V_Q=((Qx/Med_rt)*100))
wyniki3 <- wyniki%>%
  group_by(mandala, Type, Corr, Entropy)%>%
  summarise(N=n())

wiek <- df3%>%
  group_by(Entropy)%>%
  summarise(wiek=mean(Wiek))

box1 <- ggplot(wyniki1, aes(x=mandala, y=Med_rt, color=Entropy)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1))

ggplot(df3, aes(x=mandala, y=Time, color=Entropy)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y="Czas reakcji", x = "Kolorowanka z mandalą", color="Entropia")

box2a <- ggplot(wyniki2, aes(x= Type, y = Med_rt, color = Entropy)) +
  geom_boxplot() +
  geom_boxplot(aes(color=mandala))

ggplot(wyniki2, aes(x=Type, y=Med_rt, color=mandala)) +
  geom_boxplot()

ggplot(wyniki3, aes(x=Corr, y=N)) +
  geom_boxplot(aes(color=Entropy)) +
  geom_boxplot(aes(color=mandala))

time <- df3%>%
  pull(Time)

shapiro.test(time)
hist(time)
ks.test(time, pnorm)

ggplot(df3, aes(x=Time)) +
  geom_histogram(binwidth = 0.075, color="black", fill="white") +
  geom_vline(aes(xintercept=median(Time)),
             color="blue", linetype="dashed", size=1) +
  labs(x="Czas reakcji", y = "Ilość")

Entropy <- df3%>%
  select(Entropy)%>%
  pull()
Kolor <- df3%>%
  select(mandala)%>%
  pull()
aov(Time ~ mandala + Entropy + mandala:Entropy, data=df3)%>%
  summary()
wilcox.test(time ~ Entropy)
wilcox.test(time ~ Kolor, paired=TRUE)
cohen.d(time, Kolor, paired = TRUE)
cohen.d(time, Entropy)
kruskal.test(time, typy)
typy <- df3%>%
  select(Type)%>%
  pull()

aov(Time ~ mandala + Entropy + Type + mandala:Entropy + mandala:Type + Type:Entropy, data=df3)%>%
  summary()

ggplot(df3, aes(x=Type, y=Time, color=Entropy))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,1)) +
  labs(x="Typ", y="Czas reakcji", color="Entropia")
