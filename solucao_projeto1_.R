#SOLUÃÃO DO PROJETO COM FEEDBACK:
  #"DetecÃ§Ã£o de Fraudes no TrÃ¡fego de Cliques
  #em Propagandas de AplicaÃ§Ãµes Mobile"
            #FCD/ReAZURE

#Objetivo: criar um modelo capaz de prever se um usuÃ¡ria farÃ¡ o 
#download de um aplicativo depois de clicar em um anÃºncio de
#aplicativo para dispositivos mÃ³veis
#Para isto utilizamos os dados de traino disponÃ­veis em
#https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data


#1.configurando o diretÃ³rio de trabalho
setwd("D:/Projetos")
getwd()


#2. Lendo o arquivo ".csv" e gravando em um objeto
  #Dadas as limitaÃ§Ãµes de minha mÃ¡quina, decidi gerar um arquivo
#com uma amostra do arquivo original de treino. Isto foi realizado
#atravÃ©s da ferramenta "Partition and sample" do Azure. A amostra
#contÃ©m 5% do total de linhas de treino oferecidas originalmente.
#O arquivo foi convertido para "csv" tambÃ©m no Azure, utilizando
#a ferramenta "Convert to CSV".

DadosTalking <- read.csv("SampleTalking2.csv", sep = ",",
                         stringsAsFactors = FALSE, 
                         encoding = "UTF-8")

#3. Fazendo uma primeira visualizaÃ§Ã£o analÃ­tica dos dados
#Visualizando as primeiras linhas do dataset
head(DadosTalking)

#Visualizando o dataset completo
#View(DadosTalking)

#Obtendo uma visÃ£o sintÃ©tica das caracterÃ­sticas das variÃ¡veis
str(DadosTalking)

#Obtendo um resumo estatÃ­stico dos dados
summary(DadosTalking)

#nomeando as variÃ¡veis
colnames(DadosTalking) <- c("ip", "app", "device", "os",
                            "channel", "click_time",
                            "attributed_time",
                            "is_attributed")
#View(DadosTalking)

#Verificando se hÃ¡ valores para linhas da coluna "attributed_time"
#e se hÃ¡ valores diferentes de zero para a coluna "is_attributed"
library(dplyr)
DadosTalking1 <- DadosTalking %>%
  filter(is_attributed != 0)
#View(DadosTalking1)
str(DadosTalking1)


#3. Primeiras manipulaÃ§Ãµes de dados

#Replicando os dados de data/hora do click no anÃºncio para poder
#fazer manipulaÃ§Ãµes sem perder os originais no dataset
DadosTalking2 <- DadosTalking
DadosTalking2$click_time2 <- DadosTalking2$click_time
DadosTalking2$attributed_time2 <- DadosTalking2$attributed_time

#Transformando os dados de data/hora em POSIXct (tanto do click 
#no anÃºncio quanto do click para download)
DadosTalking2$click_time <- as.POSIXct(DadosTalking2[,6], 
                                       format = "%Y-%m-%d %H:%M:%S")

DadosTalking2$attributed_time <- as.POSIXct(DadosTalking2[,7], 
                                       format = "%Y-%m-%d %H:%M:%S")

str(DadosTalking2)
#View(DadosTalking2)

#Calculando a diferenÃ§a entre o tempo do click no anÃºncio e o tempo
#do click para download
DadosTalking2$time_diference <- DadosTalking2$attributed_time - 
  DadosTalking2$click_time
  #Quando tentei transformar em POSIXct ele coloca as datas tambÃ©m.


#criando uma coluna que diga qual o dia da semana ocorreu o click
install.packages("lubridate")
library(lubridate)
DadosTalking2$DiaDaSemana_Click <- wday(DadosTalking$click_time)
#NÃ£o hÃ¡ clicks para sexta, sÃ¡bado e domingo

#Separando os dados de click no anÃºncio em ano, mÃªs, dia e horas
library(tidyr)
DadosTalking3 <- DadosTalking2 %>%
  separate(click_time2, into = c("click_time_date", "click_time_hour"), 
           sep = "\\ ")
#View(DadosTalking3)
DadosTalking3 <- DadosTalking3 %>%
  separate(click_time_date, into = c("click_time_year", 
                                     "click_time_month", 
                                     "click_time_day"), sep = "-")
  #Quando tentei transformar em POSIXct ele coloca as datas tambÃ©m.
  #sendo assim deixei como caracter, sabendo que isto me impossibilita
  #de utiliÃ¡-los como variÃ¡vel explicativa


#4. Realizando anÃ¡lise exploratÃ³ria

#conferindo se o ano Ã© o mesmo para todos os clicks
DadosTalking4 <- DadosTalking3 %>%
  filter(click_time_year == 2017)
View(DadosTalking4)

DadosTalking5 <- DadosTalking3 %>%
  filter(click_time_year != 2017)
#View(DadosTalking5)

#conferindo se o mÃªs Ã© o mesmo para todos os clicks
DadosTalking6 <- DadosTalking3 %>%
  filter(click_time_month == 11)
#View(DadosTalking6)

DadosTalking7 <- DadosTalking3 %>%
  filter(click_time_month != 11)
#View(DadosTalking7)

#Construindo um histograma das diferenÃ§as de tempo entre click no anÃºncio
#e click para download

#para isto precisamos primeiro selecionar somente os casos em que houve
#download
DadosTalking8 <- DadosTalking3
#transformamos os dados missing em string "NA"
library(stringr)
DadosTalking8$time_diference <- 
  str_replace_na(DadosTalking8$time_diference, replacement = "NA")
#View(DadosTalking8)
str(DadosTalking8)
#e selecionamos somente os casos em que nÃ£o hÃ¡ "NA"
DadosTalking8 <- DadosTalking8 %>%
  filter(time_diference != "NA")
#View(DadosTalking8)
#transformamos a diferenÃ§a de tempo em nÃºmeros inteiros
DadosTalking9 <- DadosTalking8
#e finalmente podemos construir o histograma
DadosTalking9$time_diference <- as.integer(DadosTalking9$time_diference)
View(DadosTalking9)
hist(DadosTalking9$time_diference, 
     main = "Histograma do tempo baixar-click")
hist(DadosTalking9$time_diference, breaks = 30, 
     main = "Histograma do tempo baixar-click")
hist(DadosTalking9$time_diference, breaks = 50, 
     main = "Histograma do tempo baixar-click")
hist(DadosTalking9$time_diference, breaks = 100, 
     main = "Histograma do tempo baixar-click")
  #podemos concluir que em sua grande maioria hÃ¡ pouquÃ­ssima diferenÃ§a
  #de tempo entre o click e o download, para os casos em que isto ocorre


#analisando a distribuiÃ§Ã£o dos clicks entre dias da semana
hist(DadosTalking9$DiaDaSemana_Click, breaks = c(1,2,3,4,5), 
     main = "distribuiÃ§Ã£o dos cliques por dia da semana",
     ylab = "nÃºmero de clicks", xlab = "Dia da semana")
#podemos perceber que a distribuiÃ§Ã£o estÃ¡ bastante equitativa, exceto
#na segunda-feira. Mas isto pode ter ocorrido devido Ã  amostra.

#analisando a distribuiÃ§Ã£o dos clicks por dia do mÃªs
DadosTalking9$click_time_day <- as.integer(DadosTalking9$click_time_day)
hist(DadosTalking9$click_time_day, breaks = c(5, 6, 7, 8, 9), 
     main = "distribuiÃ§Ã£o dos cliques por dia do mÃªs de novembro", 
     ylab = "nÃºmero de clicks", xlab = "Dia do mÃªs")
#podemos perceber que a distribuiÃ§Ã£o estÃ¡ bastante equitativa, exceto
#no dia 6. Mas isto pode ter ocorrido devido Ã  amostra.


#analisando a distribuiÃ§Ã£o dos clicks ao longo do dia
DadosTalking11 <- DadosTalking3
#primeiro fiz uma sÃ©rie de scaterplots usando loop
DadosTalking11$click_time_day <- 
  as.integer(DadosTalking11$click_time_day)
str(DadosTalking11)
dias <- c(6,7,8,9)

ddm.plot <- function(dias){
  ggplot(DadosTalking11[DadosTalking11$click_time_day == dias, ], 
         aes(x = click_time, y = is_attributed)) + 
    geom_point() +
    ylab("horÃ¡rio") +
    labs(title = paste("distribuiÃ§Ã£o dos clicks no dia ", 
                       as.character(dias))) +
    theme(text = element_text(size = 20))
}  

library(ggplot2)
lapply(dias, ddm.plot)

#contudo, devido Ã s limitaÃ§Ãµes da minha mÃ¡quina, resolvi fazer os plots
#uma dia de cada vez

#para o dia 6
DadosTalking12 <- DadosTalking11 %>%
  filter(click_time_day == 6)

ggplot(DadosTalking12, aes(x=click_time, y=is_attributed))+
  geom_point()+
  labs(title = paste("distribuiÃ§Ã£o dos clicks no dia 6"))+
  ylab("Baixou = 1 ; NÃ£o Baixou = 0")+
  xlab("HorÃ¡rio do clik")+
  theme(text = element_text(size = 20))

#para o dia 7
DadosTalking13 <- DadosTalking11 %>%
  filter(click_time_day == 7)

ggplot(DadosTalking13, aes(x=click_time, y=is_attributed))+
  geom_point()+
  labs(title = paste("distribuiÃ§Ã£o dos clicks no dia 7"))+
  ylab("Baixou = 1 ; NÃ£o Baixou = 0")+
  xlab("HorÃ¡rio do clik")+
  theme(text = element_text(size = 20))

#para do dia 8
DadosTalking14 <- DadosTalking11 %>%
  filter(click_time_day == 8)

ggplot(DadosTalking14, aes(x=click_time, y=is_attributed))+
  geom_point()+
  labs(title = paste("distribuiÃ§Ã£o dos clicks no dia 8"))+
  ylab("Baixou = 1 ; NÃ£o Baixou = 0")+
  xlab("HorÃ¡rio do clik")+
  theme(text = element_text(size = 20))

#para o dia 9
DadosTalking15 <- DadosTalking11 %>%
  filter(click_time_day == 9)

ggplot(DadosTalking15, aes(x=click_time, y=is_attributed))+
  geom_point()+
  labs(title = paste("distribuiÃ§Ã£o dos clicks no dia 9"))+
  ylab("Baixou = 1 ; NÃ£o Baixou = 0")+
  xlab("HorÃ¡rio do clik")+
  theme(text = element_text(size = 20))
  #os dados dizem que hÃ¡ uma menor chance de um click em download durante
  #o horÃ¡rio das 18h Ã s 21h.

#5. Construindo o modelo

DadosTalking16 <- DadosTalking3
#View(DadosTalking16)
str(DadosTalking16)

#transformando as variÃ¡veis qualitativas em fatores e as numÃ©ricas
#em integer
DadosTalking16$ip <- as.factor(DadosTalking16$ip)
DadosTalking16$app <- as.factor(DadosTalking16$app)
DadosTalking16$device <- as.factor(DadosTalking16$device)
DadosTalking16$os <- as.factor(DadosTalking16$os)
DadosTalking16$channel <- as.factor(DadosTalking16$channel)
DadosTalking16$is_attributed <- as.factor(as.character(DadosTalking16$is_attributed))
DadosTalking16$click_time_year <- as.integer(DadosTalking16$click_time_year)
DadosTalking16$click_time_month <- as.integer(DadosTalking16$click_time_month)
DadosTalking16$click_time_day <- as.integer(DadosTalking16$click_time_day)

str(DadosTalking16)

#o modelo
library(randomForest)

modelo <- randomForest(is_attributed ~ ip
                       + app
                       + device
                       + os
                       + channel
                       + click_time
                       + click_time_day
                       + DiaDaSemana_Click,
                       data = DadosTalking16, 
                       ntree = 100,
                       nodesize = 10, 
                       importance = TRUE)

#parei por aqui. pelo que eu entendi, o programa diz que hÃ¡ muitas classes
#para uma mesma vairÃ¡vel (acima de 53). Eu ja imaginava que isto iria
#acontecer para o "click_time", mas nÃ£o consegui criar agrupÃ¡-los em uma
#quantidae menor de classes. De qualquer maneira, dados como "ip", "app"
#e "device" tambÃ©m apresentam muitas classes, e nÃ£o me parece fazer sentido
#agrupÃ¡-los em novas classes (qual seria o critÃ©rio?).
