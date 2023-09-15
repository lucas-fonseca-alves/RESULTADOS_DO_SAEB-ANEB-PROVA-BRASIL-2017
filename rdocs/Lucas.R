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
                  fill = "gray",
                 color = "black"
  )+
  scale_x_continuous(limits = c(90, 370), breaks = seq(100, 360, 20))+
  labs(x = "Notas", y = "Frequência Absoluta")
ggsave(filename = file.path(caminho_lucas, "histo_nota_mt.pdf"), width = 158, height = 93, units = "mm")

#----------------------tabela resumo------------------------------

describe(amostra_saeb$NOTA_MT)
summary(amostra_saeb$NOTA_MT)
var(amostra_saeb$NOTA_MT)

#--------------------boxplot-------------------------------------

#boxplot nota_mt

amostra_saeb %>% 
  ggplot(aes(x = '', y = NOTA_MT)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(100, 400), breaks = seq(100, 400, 50))+
  labs(x='', y = "Notas Matemática")
ggsave(filename = file.path(caminho_lucas, "boxplot_nota_mt.pdf"), width = 158, height = 93, units = "mm")


#Outliers: 352.6405 352.0601 348.8831 354.0257 355.0854 349.5291 352.6405 351.1681
boxplot.stats(amostra_saeb$NOTA_MT)

#--------------------------Região------------------------------------
class(amostra_saeb$REGIAO)

#transformando região em factor
amostra_saeb$REGIAO <- as.factor(amostra_saeb$REGIAO)

#transformando os 'codigos' de região em 'nomes'
amostra_saeb$REGIAO <- factor(amostra_saeb$REGIAO,
                              levels = c('1', '2', '3','4', '5'),
                              labels = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste' )
)

regiao <- amostra_saeb %>%
  filter(!is.na(REGIAO)) %>%
  count(REGIAO) %>%
  mutate(
    freq = (n/sum(n))*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(regiao) +
  aes(x = fct_reorder(REGIAO, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 100))+
  labs(x = "Região", y = "Frequência")
ggsave(filename = file.path(caminho_lucas, "barras_regiao.pdf"), width = 158, height = 93, units = "mm")

#---------------------------Localização------------------------------------
class(amostra_saeb$LOCALIZACAO)

#transformando região em factor
amostra_saeb$LOCALIZACAO <- as.factor(amostra_saeb$LOCALIZACAO)

#transformando os 'codigos' de região em 'nomes'
amostra_saeb$LOCALIZACAO <- factor(amostra_saeb$LOCALIZACAO,
                              levels = c('1', '2'),
                              labels = c('Urbana', 'Rural' )
)

localizacao <- amostra_saeb %>%
  filter(!is.na(LOCALIZACAO)) %>%
  count(LOCALIZACAO) %>%
  mutate(
    freq = (n/sum(n))*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(localizacao) +
  aes(x = fct_reorder(LOCALIZACAO, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200))+
  labs(x = "Localização", y = "Frequência")
ggsave(filename = file.path(caminho_lucas, "barras_localizacao.pdf"), width = 158, height = 93, units = "mm")


#------------------------------------SEXO------------------------------------
class(amostra_saeb$SEXO)

#transformando região em factor
amostra_saeb$SEXO <- as.factor(amostra_saeb$SEXO)

#transformando os 'codigos' de região em 'nomes'
amostra_saeb$SEXO <- factor(amostra_saeb$SEXO,
                                   levels = c('A', 'B'),
                                   labels = c('Masculino', 'Feminino' )
)

sexo <- amostra_saeb %>%
  filter(!is.na(SEXO)) %>%
  count(SEXO) %>%
  mutate(
    freq = (n/sum(n))*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(sexo) +
  aes(x = fct_reorder(SEXO, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200))+
  labs(x = "Sexo", y = "Frequência")
ggsave(filename = file.path(caminho_lucas, "barras_sexo.pdf"), width = 158, height = 93, units = "mm")


