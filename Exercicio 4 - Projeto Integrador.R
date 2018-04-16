#############################
#EXERCÍCIO 4  - Projeto Integrador 
#Larissa Fernandes 
##########################

library(readr)
enade14 <- read_csv2("https://raw.githubusercontent.com/neylsoncrepalde/introducao_ao_r/master/dados/enade_2014_amostra.csv")


###################################################################################
# 1 - Extraia a média, a mediana, mínimo, máximo, variância e desvio padrão da 
#idade para todos os alunos da nossa amostra aleatória.

summary(enade14$nu_idade) 
var(enade14$nu_idade) 
sd(enade14$nu_idade) 

#################################################################################
# 2 - Elabore uma tabela de frequência da quantidade de alunos por sexo. 
# Corrija a variável caso alguma categoria esteja “sobrando”.

enade14$sexo <- enade14$tp_sexo
enade14$sexo[enade14$sexo == "N"] 
freq(enade14$sexo,main = "alunos por sexo - Enade 2014")

#####################################################################################
# 3 - Agora extraia a média, a mediana, mínimo, máximo, variância e desvio padrão da 
# idade para cada categoria de sexo. 
# Exiba os mesmos resultados com um gráfico.
        
enade14$masculino <- ifelse(enade14$sexo == "M", 1, 0) 
summary(enade14$nu_idade[enade14$masculino == 1]) 
var(enade14$nu_idade[enade14$masculino == 1])
sd(enade14$nu_idade[enade14$masculino == 1])
boxplot(enade14$nu_idade[enade14$masculino == 1],col = "green", main = "Idade Homens - Enade 2014")
        
####################################################################################
# 4 - Agora extraia a média/mediana/mínimo /máximo, variância e desvio padrão da
# idade para cada categoria de cor-raça.
# Exiba os mesmos resultados com um gráfico.

summary(enade14$nu_idade[enade14$qe_i2 == "a"]) 
var(enade14$nu_idade[enade14$qe_i2 == "a"])
sd(enade14$nu_idade[enade14$qe_i2 == "a"])
boxplot(enade14$nu_idade[enade14$qe_i2 == "a"],col = "white",main = " Brancos - idades dos alunos  - ENADE 2014")

summary(enade14$nu_idade[enade14$qe_i2 == "b"]) 
var(enade14$nu_idade[enade14$qe_i2 == "b"])
sd(enade14$nu_idade[enade14$qe_i2 == "b"])
boxplot(enade14$nu_idade[enade14$qe_i2 == "b"],col = "pink",main = "Negros -  idades dos alunos  ENADE 2014")

summary(enade14$nu_idade[enade14$qe_i2 == "c"]) 
var(enade14$nu_idade[enade14$qe_i2 == "c"])
sd(enade14$nu_idade[enade14$qe_i2 == "c"])
boxplot(enade14$nu_idade[enade14$qe_i2 == "c"],col = "orange",main = "Pardos -  idades dos alunos - ENADE 2014")

summary(enade14$nu_idade[enade14$qe_i2 == "d"]) 
var(enade14$nu_idade[enade14$qe_i2 == "d"])
sd(enade14$nu_idade[enade14$qe_i2 == "d"])
boxplot(enade14$nu_idade[enade14$qe_i2 == "d"],col = "#yellow",main = "Oriental  -  idades dos alunos - ENADE 2014")

summary(enade14$nu_idade[enade14$qe_i2 == "e"]) 
var(enade14$nu_idade[enade14$qe_i2 == "e"])
sd(enade14$nu_idade[enade14$qe_i2 == "e"])
boxplot(enade14$nu_idade[enade14$qe_i2 == "e"],col = "brown",main = "Indigena - idadse dos alunos - ENADE 2014")

#####################################################################################
# 5 - Verifique a distribuição de alunos por região do país. Exiba uma tabela de frequências e um gráfico.

regiao <- enade14$regiao
regiao <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste") 
freq(enade14$regiao,col= "#red",main = "Alunos  por região do país - Enade 2014")


######################################################################################
# 6 - Vamos investigar a associação entre a renda e a cor. Faça uma tabela cruzada entre essas duas variáveis.

enade14$cor_raca <- enade14$qe_i2  

enade14$cor_raca[enade14$cor_raca == "a"] <- "Branco"
enade14$cor_raca[enade14$cor_raca == "b"] <- "Negro"
enade14$cor_raca[enade14$cor_raca == "c"] <- "Pardo"
enade14$cor_raca[enade14$cor_raca == "d"] <- "Amarelo"
enade14$cor_raca[enade14$cor_raca == "e"] <- "Indígena"

enade14$renda <- enade14$qe_i8

enade14$renda[enade14$renda == "a"] <- "Até 1,5"
enade14$renda[enade14$renda == "b"] <- "1,5 a 3"
enade14$renda[enade14$renda == "c"] <- "3 a 4,5"
enade14$renda[enade14$renda == "d"] <- "4,5 a 6"
enade14$renda[enade14$renda == "e"] <- "6 a 10"
enade14$renda[enade14$renda == "f"] <- "10 a 30"
enade14$renda[enade14$renda == "g"] <- ">30"

#######################################################################################

