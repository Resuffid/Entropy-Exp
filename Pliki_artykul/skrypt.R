library(tidyverse)

lista_plikow <- list.files("csv")

lista_plikow <- paste("csv", lista_plikow, sep = "/")

dane1 <- read_csv(lista_plikow)

# dane1 <- read.csv("Documents/Studia/II-Rok/Metodologia/Flanker/data/dane1.csv")

dane2 <- dane1 %>%
  select(kolorowanka, Stimuli, Odp.keys, Odp.rt, Entropia, participant)

dane3 <- subset(dane2, Stimuli!="")

dane3$Stimuli[dane3$Stimuli == "<<<<<"] <- "left,left,left,left,left"
dane3$Stimuli[dane3$Stimuli == "<<><<"] <- "left,left,right,left,left"
dane3$Stimuli[dane3$Stimuli == ">>>>>"] <- "right,right,right,right,right"
dane3$Stimuli[dane3$Stimuli == ">><>>"] <- "right,right,left,right,right"
dane3$Stimuli[dane3$Stimuli == "--<--"] <- "no,no,left,no,no"
dane3$Stimuli[dane3$Stimuli == "-->--"] <- "no,no,right,no,no"
dane3$Stimuli[dane3$Stimuli == ">>>>> "] <- "right,right,right,right,right"

df3 <- dane3%>%
  mutate(Type = if(Stimuli=="left,left,left,left,left" | "right,right,right,right,right")
    {"Congurent"}
    else if(Stimuli=="left,left,right,left,left"|"right,right,left,right,right")
      {"Incongurent"}
    else
      {"neutral"})

df3 <- dane3%>%
  mutate(Type = case_when(Stimuli=="left,left,left,left,left"|Stimuli=="right,right,right,right,right" ~ "Congurent",
                          Stimuli=="left,left,right,left,left"|Stimuli=="right,right,left,right,right" ~ "Incongurent",
                          Stimuli=="no,no,left,no,no"|Stimuli=="no,no,right,no,no" ~ "Neutral"))%>%
  rename('Ans' = "Odp.keys",
         'Time' = 'Odp.rt',
         'Entropy' = 'Entropia')%>%
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

str(dane3)

wyniki <- df3%>%
  group_by(participant)
wyniki1 <- wyniki%>%
  group_by(kolorowanka, Entropy)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time))
wyniki2 <- wyniki%>%
  group_by(kolorowanka, Type, Entropy)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time))
wyniki3 <- wyniki%>%
  group_by(kolorowanka, Type, Corr, Entropy)%>%
  summarise(N=n())

