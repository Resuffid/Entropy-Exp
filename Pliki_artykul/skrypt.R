library(tidyverse)
library(psych)
library(effsize)
library(ez)
library(EnvStats)
library(e1071)
library(rstatix)
library(afex)
library(lme4)
library(lmerTest)
library(emmeans)

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

ggplot(df3b1, aes(x=mandala, y=Time, color=Entropy)) +
  geom_boxplot(outlier.shape = NA) +
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
  geom_histogram(binwidth = 0.1, color="black", fill="white") +
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

aov(Time ~ Entropy, data=df3)

wilcox.test(time ~ Entropy)
wilcox.test(time ~ Kolor, paired=TRUE)
cohen.d(time, Kolor, paired = TRUE)
cohen.d(timeb1, entropiab1)
kruskal.test(time, typy)
typy <- df3%>%
  select(Type)%>%
  pull()

aov(Time ~ mandala + Entropy + Type + mandala:Entropy + mandala:Type + Type:Entropy, data=df3)%>%
  summary()

ggplot(df3b1, aes(x=Type, y=Time, color=Entropy))+
  geom_boxplot(outlier.shape = NA) +
  labs(x="Typ", y="Czas reakcji", color="Entropia")

mean(df3$Wiek)

dane1%>%
  group_by(`Płeć (K/M)`)%>%
  summarise(N=n())
t.test(timeb1~entropiab1)
t.test(time~Kolor, paired=TRUE)
anova(time~typy)


wyniczki1 <- df3b1%>%
  filter(Time<1)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time), V_x=((SD_rt/M_rt)*100), A_Sx=skewness(Time))

wyniczki2 <- df3b1%>%
  filter(Time<1)%>%
  group_by(Entropy, mandala)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time), V_x=((SD_rt/M_rt)*100), A_Sx=skewness(Time))

wyniczki3 <- df3b1%>%
  filter(Time<1)%>%
  group_by(Entropy, mandala, Type)%>%
  summarise(M_rt=mean(Time), SD_rt=sd(Time), V_x=((SD_rt/M_rt)*100), A_Sx=skewness(Time))

df3b1 <- df3%>%
  filter(Time<1)
ggplot(df3b1, aes(x=Type, y=Time, color=Entropy))+
  geom_boxplot()+
  labs(x="Typ", y="Czas reakcji", color="Entropia")

ggplot(df3b1, aes(x=mandala, y=Time, color=Entropy)) +
  geom_boxplot()+
  labs(y="Czas reakcji", x = "Kolorowanka z mandalą", color="Entropia")

timeb1 <- df3b1%>%
  select(Time)%>%
  pull()

entropiab1 <- df3b1%>%
  select(Entropy)%>%
  pull()

kolorb1 <- df3b1%>%
  select(mandala)%>%
  pull()

kolorb2 <- df3b1$mandala

timeb2 <- df3b1$Time
as.factor(kolorb2)
  
t.test(timeb1~entropiab1)
t.test(time~Kolor, paired=TRUE)

t_test(df3b1, formula=Time~, paired = TRUE)

table(kolorb2)

anova<-ezANOVA(
  data=df3b1,
  dv=.(Time),
  wid=.(participant),
  within=.(mandala),
  between=.(Entropy)
  )

anova1<-ezANOVA(
  data=df3,
  dv=.(Time),
  wid=.(participant),
  within_full=.(mandala),
  between=.(Entropy)
)
anova1


shapiro.test(df3b1$Time)
ks.test(df3b1$Time, pnorm)

ggplot(df3b1, aes(x=Time)) +
  geom_histogram(binwidth = 0.1, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Time)),
             color="blue", linetype="dashed", size=1) +
  labs(x="Czas reakcji", y = "Ilość")

print(anova)
anova%>% summary

df3b1a <- df3b1%>%
  mutate(num=c(1:3549))

ezANOVA(
  data=df3,
  dv=Time,
  wid=participant,
  between=Entropy,
  within=mandala
)

aov_mix <- aov_ez(id = "participant",
       dv = "Time",
       data = df3,
       between = "Entropy",
       within = "mandala")

test_sphericity(aov_mix)

summary(aov_mix)

mixed_anova <- aov(Time ~ Entropy*mandala + Error(participant/mandala), data=df3b1)
summary(mixed_anova)

aov(Time~Entropy*mandala, data=df3b1)%>%
  summary()

aov_4(Time ~ Entropy + (mandala|participant),
      data = df3b1)

aov_4(Time~Entropy+(mandala|participant), data=df3)


# wolę krótką nazwę zmiennej
d <- df3b1
d %>% 
  group_by(Entropy, mandala) %>% 
  summarise(M = mean(Time))

# namalujmy sobie te rozklady
ggplot(d, aes(x=mandala, y=Time, color=Entropy)) + 
  geom_violin()

# a jak wygląda zwykły box?
ggplot(d, aes(x=mandala, y=Time, color=Entropy)) + 
  geom_boxplot()

# sprawdźmy osobno dla każdego badanego
ggplot(d, aes(x=mandala, y=Time, color=Entropy)) + 
  geom_boxplot() +
  facet_wrap(vars(participant))

# najprościej policzyć to liniowym modelem mieszanym
# paczki lme4 i lmerTest
model <- "Time ~ Entropy + mandala + Entropy:mandala + (1|participant)"
model_fit <- lmerTest::lmer(Time ~ Entropy + mandala + Entropy:mandala + (1|participant), data = d)
summary(model_fit)
# wydaje się, że macie istotną statystycznie interakcję ale brak efektów głównych

# sprawdźmy kontrasty
# emmeans - estimated marginal means 
# średnie estymowane są z modelu, pozwala to skorygować ew. brak balansu wynikający np. z braków danych (np. bo odcieliśmy > 1 sec.)
emmeans(model_fit, specs = pairwise ~ Entropy:mandala)

# wydaje się, że zrobienie mandali ZWIĘKSZA czas reakcji ale tylko w warunku WYSOKIEJ entropii.
# ciekawe!

model1 <- "Time ~ Entropy + Type + mandala + Entropy:Type + mandala:Type + (1|participant)"
model_fit1 <- lmerTest::lmer(Time ~ Entropy + Type + mandala + Entropy:Type + mandala:Type + (1|participant), data=d)
summary(model_fit1)
emmeans(model_fit1, specs = pairwise ~ Entropy:Type)
emmeans(model_fit1, specs = pairwise ~ mandala:Type)

ggplot(d, aes(x=Type, y=Time, color=mandala)) +
  geom_boxplot(outlier.shape = NA)+
  labs(y="Czas reakcji", x = "Typ", color="Mandala")

res.anova <- aov(Time~Type, data=d)
summary(res.anova
        )
TukeyHSD(res.anova)
