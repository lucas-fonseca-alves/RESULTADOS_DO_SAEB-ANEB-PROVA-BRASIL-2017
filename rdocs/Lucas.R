library(tidyverse)
library(psych)

# Definindo bancos
amostra_lucas <- read_csv("banco/amostra_222025665.csv")
amostra_maria <- read_csv("banco/amostra_222036041.csv")

#Juntando bancos

amostra_saeb <- bind_rows(amostra_lucas,amostra_maria)

#Arrumando nota_mt para ficar no padrão exemplo(220.25)

amostra_saeb <- amostra_saeb %>%
  mutate(NOTA_MT = str_replace(NOTA_MT, "^(\\d{3})", "\\1."))
  
amostra_saeb$NOTA_MT <- round(as.double(amostra_saeb$NOTA_MT),2)

#Caminho para salvar graficos
caminho_lucas <- "C:/Users/Cliente/Desktop/RESULTADOS_DO_SAEB-ANEB-PROVA-BRASIL-2017/resultados/Lucas"

#------------------------Tabela-----------------------------

#Construindo tabele de frequências das notas_mt por classes de 20 de 100 a 360

intervalos_mt <- cut(amostra_saeb$NOTA_MT, breaks = seq(100, 360, by = 20), right = FALSE)

#criando as tabelas de freq absoluta e relativa
freq_abs_mt <- table(intervalos_mt)
freq_rel_mt <- prop.table(freq_abs_mt)

#juntando as tabelas
tabela_mt <- data.frame(intervalos_mt = names(freq_abs_mt),
                        frequencia_absoluta = as.vector(freq_abs_mt),
                        frequencia_relativa = as.vector(freq_rel_mt))

#Melhorando a forma de visualização da coluna frequencia_relativa
tabela_mt$frequencia_relativa <-  round(tabela_mt$frequencia_relativa, 4)*100

#--------------------------Histograma------------------------------

#NOTA_MT (com classes de tamanho 20)

amostra_saeb %>%
  ggplot(aes(x = NOTA_MT)) +
  geom_histogram(binwidth = 20,
                 fill = "steelblue",
                 color = "black"
  )+
  scale_x_continuous(limits = c(90, 370), breaks = seq(100, 360, 20))+
  labs(title = "Histograma Notas Matemática", x = "Notas", y = "Frequência Absoluta")+
  theme_bw()
ggsave(filename = file.path(caminho_lucas, "histo_nota_mt.pdf"), width = 158, height = 93, units = "mm")

#----------------------tabela resumo------------------------------

describe(amostra_saeb$NOTA_MT)


#--------------------boxplot-------------------------------------
class(amostra_saeb$REGIAO)

#transformando região em factor
amostra_saeb$REGIAO <- as.factor(amostra_saeb$REGIAO)

#transformando os 'codigos' de região em 'nomes'
amostra_saeb$REGIAO <- factor(amostra_saeb$REGIAO,
                                     levels = c('1', '2', '3','4', '5'),
                                     labels = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste' )
)

#boxplot nota_mt

amostra_saeb %>% 
  ggplot(aes(x = reorder(REGIAO, NOTA_MT), y = NOTA_MT)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(100, 400), breaks = seq(100, 400, 50))+
  labs(x = "Regiões", y = "Notas Matemática")
theme_bw()
ggsave(filename = file.path(caminho_lucas, "boxplot_nota_mt.pdf"), width = 158, height = 93, units = "mm")


#Medidas Resumo Boxplot por região

#Nordeste

nordeste <- amostra_saeb %>% 
  filter(REGIAO == 'Nordeste')

describe(nordeste$NOTA_MT)

#Dois valores outliers 349.53, 345.99
#mediana = 200.20
boxplot.stats(nordeste$NOTA_MT)


#Norte

Norte <- amostra_saeb %>% 
  filter(REGIAO == 'Norte')

describe(Norte$NOTA_MT)

#valores outliers = 325.46
#mediana = 202.56
boxplot.stats(Norte$NOTA_MT)

#Centro-Oeste

CentroOeste <- amostra_saeb %>% 
  filter(REGIAO == 'Centro-Oeste')

describe(CentroOeste$NOTA_MT)

#valores outliers =
#mediana = 211.39
boxplot.stats(CentroOeste$NOTA_MT)

#Sul

Sul <- amostra_saeb %>% 
  filter(REGIAO == 'Sul')

describe(Sul$NOTA_MT)

#valores outliers = 109.53 354.03 352.64
#mediana = 230.37
boxplot.stats(Sul$NOTA_MT)

#Sudeste

Sudeste <- amostra_saeb %>% 
  filter(REGIAO == 'Sudeste')

describe(Sudeste$NOTA_MT)

#valores outliers = 
#mediana = 235.475 
boxplot.stats(Sudeste$NOTA_MT)

