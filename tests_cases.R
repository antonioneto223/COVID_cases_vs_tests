setwd("C:/Users/Antonio/Desktop/covid-brazil/Regressao")

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp","readr","dplyr","gbutils",
             "ggpubr", "lubridate","aweek")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


rm(list=ls())   # Limpando todos os dados

general <- read.csv(file = 'dada_states.csv',sep = ";")
general$date <- as.Date(general$date, format= "%d/%m/%Y")

##
##  GETTING DATA PER MONTH
##
months_i<-c("2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01",
          "2021-06-01","2021-07-01")
months_f<-c("2021-01-31","2021-02-28","2021-03-31","2021-04-30","2021-05-31",
            "2021-06-30","2021-07-31")
cols <- c("state","date","deaths_per_100k_inhabitants",
          "totalCases_per_100k_inhabitants",
          "vaccinated_per_100_inhabitants",
          "tests_per_100k_inhabitants")

dados_mes<-list()
for (i in 1:(length(months_i)-1)){
  first <- general[,cols] %>% filter(general$date == as.Date(months_i[i]))
  last <- general[,cols] %>% filter(general$date == as.Date(months_f[i]))
  merged <- merge(first,last,by.x = "state", by.y="state")
  merged$month<-month.abb[month(months_i[i])]
  dados <- merged[,c("state","month")]
  dados["monthly_deaths_100k"]<-merged$deaths_per_100k_inhabitants.y-
    merged$deaths_per_100k_inhabitants.x
  dados["monthly_cases_100k"]<-merged$totalCases_per_100k_inhabitants.y-
    merged$totalCases_per_100k_inhabitants.x
  dados["monthly_tests_100k"]<-merged$tests_per_100k_inhabitants.y-
    merged$tests_per_100k_inhabitants.x
  dados<- filter(dados,dados$monthly_tests_100k !=0)
  dados<- dados[-c(nrow(dados)),]
  dados_mes[[i]]<-dados
}
dados1<-do.call("rbind",dados_mes)


## Retirando outliers em testes
Q <- quantile(dados1$monthly_tests_100k, probs=c(0.05, 0.95), na.rm = FALSE)
iqr <- IQR(dados1$monthly_tests_100k)  
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
dados1<- subset(dados1, (dados1$monthly_tests_100k>low & dados1$monthly_tests_100k<up)) #removendo outliers de cima



## GRAFICO DE DISPERSAO
dados1 %>% 
  ggplot(aes(x = monthly_tests_100k, 
             y = monthly_cases_100k)) +
  geom_point(color = "grey20", alpha = 0.6, size = 2) +
  geom_text_repel(aes(label = state)) +
  geom_smooth(aes(x = monthly_tests_100k,
                   y = monthly_cases_100k),
               method = "lm", color = "#FDE725FF", se = F) +
  theme_bw()

#############################################################
## FAZENDO REGRESSAO DO BRASIL
#############################################################

## Correlacao de pearson
chart.Correlation((dados1[3:5]), histogram = TRUE)

#Estimando a Regressão Simples
modelo_cases <- lm(formula = monthly_cases_100k ~ monthly_tests_100k,
                      data = dados1)
summary(modelo_cases)

## Residuos seguem normalidade?
sf.test(modelo_cases$residuals)
dados1 %>%
  mutate(residuos = modelo_cases$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 8,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_cases$residuals),
                            sd = sd(modelo_cases$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

## Printando modelo regressao linear
ggplotly(
  ggplot(dados1, aes(x = monthly_tests_100k, y = monthly_cases_100k)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90,) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

## Poderia melhorar com Box-Cox?
lambda_BC <- powerTransform(dados1$monthly_cases_100k) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox e estimando novo modelo
dados1$bc_cases <- (((dados1$monthly_cases_100k ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)
modelo_bc <- lm(formula = bc_cases ~ monthly_tests_100k, 
                data = dados1)
summary(modelo_bc)

## Parametros do modelo padronizado
confint(modelo_bc, level = 0.95) # siginificância 5%
plot_summs(modelo_bc, colors = "#287D8EFF") #função plot_summs do pacote ggstance
plot_summs(modelo_bc, scale = TRUE, colors = "#287D8EFF")

## Salvando fitted values no dataset e visualizando
dados1$yhat_cases <- modelo_cases$fitted.values
dados1$yhat_cases_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))
dados1 %>%
  ggplot() +
  geom_smooth(aes(x = monthly_cases_100k, y = yhat_cases, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = monthly_cases_100k, y = yhat_cases),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = monthly_cases_100k, y = yhat_cases_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = monthly_cases_100k, y = yhat_cases_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = monthly_cases_100k, y = monthly_cases_100k), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Monthly cases", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

################################################################################
##  GETTING EUROPE DATA
################################################################################
general_ue <- read.csv(file = 'data_ue.csv',sep = ",")
general_ue$year_week<-week2date(general_ue$year_week)
nations_ue <- filter(general_ue, general_ue$level=="national")
nations_ue_2021<-filter(nations_ue,year(nations_ue$year_week)==2021)

bymonth <- nations_ue_2021 %>%
  group_by(country_code,month(year_week)) %>%
  summarize(cases = sum(new_cases, na.rm=TRUE),
            tests = sum(tests_done, na.rm=TRUE))
dados_ue <- merge(bymonth,unique(nations_ue_2021[,c("population","country_code")]), 
                  by.x = "country_code", by.y="country_code")
dados_ue$cases_100k <- 100000*dados_ue$cases/dados_ue$population
dados_ue$tests_100k <- 100000*dados_ue$tests/dados_ue$population
dados_ue$`month(year_week)`<-month.abb[dados_ue$`month(year_week)`]
dados_ue$cases<-NULL
dados_ue$population<-NULL
dados_ue$tests<-NULL
# Excluding Aug (not finished yet)
dados_ue <- filter(dados_ue,dados_ue$`month(year_week)`!="Aug")


## Retirando outliers em testes
Q_ue <- quantile(dados_ue$tests_100k, probs=c(0.05, 0.95), na.rm = FALSE)
iqr_ue <- IQR(dados_ue$tests_100k)  
up_ue <-  Q_ue[2]+1.5*iqr_ue # Upper Range  
low_ue<- Q_ue[1]-1.5*iqr_ue # Lower Range
dados_ue<- subset(dados_ue, 
                  (dados_ue$tests_100k>low_ue & dados_ue$tests_100k<up_ue)) #removendo outliers de cima


#############################################################
## FAZENDO REGRESSAO PARA EUROPA
#############################################################

## Correlacao de pearson
chart.Correlation((dados_ue[3:4]), histogram = TRUE)

## GRAFICO DE DISPERSAO
dados_ue %>% 
  ggplot(aes(x = tests_100k, 
             y = cases_100k)) +
  geom_point(color = "grey20", alpha = 0.6, size = 2) +
  geom_text_repel(aes(label = country_code)) +
  geom_smooth(aes(x = tests_100k,
                  y = cases_100k),
              method = "lm", color = "#FDE725FF", se = F) +
  theme_bw()

# Fazendo regressao
modelo_ue <- lm(formula = cases_100k ~ tests_100k,
                   data = dados_ue)
summary(modelo_ue)

## Printando modelo regressao linear
ggplotly(
  ggplot(dados_ue, aes(x = tests_100k, y = cases_100k)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90,) +
    labs(x = "Testes",
         y = "Casos") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

## Residuos seguem normalidade?
sf.test(modelo_ue$residuals)
dados_ue %>%
  mutate(residuos = modelo_ue$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 10,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_ue$residuals),
                            sd = sd(modelo_ue$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

## Poderia melhorar com Box-Cox?
lambda_ue <- powerTransform(dados_ue$cases_100k) #função powerTransform do pacote car#
lambda_ue

#Inserindo o lambda de Box-Cox e estimando novo modelo
dados_ue$bc_cases <- (((dados_ue$cases_100k ^ lambda_ue$lambda) - 1) / 
                      lambda_ue$lambda)
modelo_bc_ue <- lm(formula = bc_cases ~ tests_100k, 
                data = dados_ue)
summary(modelo_bc_ue)

## Parametros do modelo padronizado
confint(modelo_bc_ue, level = 0.95) # siginificância 5%
plot_summs(modelo_bc_ue, colors = "#287D8EFF") #função plot_summs do pacote ggstance
plot_summs(modelo_bc_ue, scale = TRUE, colors = "#287D8EFF")

## Salvando fitted values no dataset e visualizando
dados_ue$yhat_cases <- modelo_ue$fitted.values
dados_ue$yhat_cases_bc <- (((modelo_bc_ue$fitted.values*(lambda_ue$lambda))+
                            1))^(1/(lambda_ue$lambda))
dados_ue %>%
  ggplot() +
  geom_smooth(aes(x = cases_100k, y = yhat_cases, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = cases_100k, y = yhat_cases),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = cases_100k, y = yhat_cases_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = cases_100k, y = yhat_cases_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = cases_100k, y = cases_100k), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Monthly cases", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


##
# Histograma comparando
##
font_size=1.05
a=1000
hist(dados1$monthly_tests_100k,freq=TRUE,
     xlim=c(0,50000),right = FALSE,
     col="gray",main="Histograma de testes (/100k)",xlab="Testagem",
     ylab="Frequência",cex=font_size)
axis(2,at=pretty(dados1$monthly_tests_100k),
     labels=chartr(".", ",", 
                   as.character(format(pretty(dados1$monthly_tests_100k),nsmall=1))))
hist(dados_ue$tests_100k,freq = TRUE,
     xlim=c(0,50000),right=FALSE,
     col=rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50"), 
     cex=font_size,add=T,
     breaks = seq(min(dados_ue$tests_100k)-a,max(dados_ue$tests_100k)+a,a))

legend("topright", c("Brasil","Europa"), 
       col=c("gray",rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50"),"black"),
       lwd=c(3,3,2),cex=font_size)
