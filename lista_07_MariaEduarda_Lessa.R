
# Lista 07
# Professor: Davi Moreira
# Disciplina: Análise de Dados
# Aluna: Maria Eduarda R. N. Lessa


  # Questão 1:

# Definir diretório:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_07")

# Instalar pacote "foreign" para ler dta:
install.packages("foreign")
library("foreign")

# Carregar base "fair.dta", atribuir a objeto "vote_growth":
vote_growth <- read.dta("fair.dta")

## letra a)

# Checar variáveis (colunas) e visualizar a base: 
colnames(vote_growth)
View(vote_growth)

# Apresentar a análise descritiva de todas as variáveis da base "vote_growth": 
summary(vote_growth)

## letra b)

# Análise de regressão de VOTE (VD) e GROWTH (VI): 
reg <- lm(VOTE ~ GROWTH, data = vote_growth)
summary(reg)

# Plotar dados e reta de regressão com IC:
require(ggplot2)
ggplot(data = vote_growth, aes(y = VOTE, x = GROWTH)) +
  geom_point(color = "lightblue") +
    theme_classic() +
      geom_smooth(method="lm", color = "gray", se = TRUE)

### i) 

# As variáveis utilizadas foram "VOTE" e "GROWTH". O modelo analisa o 
# efeito do crescimento econômico (a variável independente), sobre o 
# percentual de votos recebidos pelo candidato do partido incumbente (a 
# variável dependente). 

### ii)

# Imprimir resultados do modelo (tabela de regressão):
summary(reg)
# Calcular RMSE: 
require(magrittr)
mean(residuals(reg)^2) %>% sqrt()

# Valor de ^a = 51.860, desvio padrão = 0.882,  p-valor < 0.001 
# Valor de ^b = 0.654, desvio padrão = 0.161, p-valor < 0.001
# Valor do R quadrado = 0.356
# Valor do erro padrão dos resíduos (RSE) = 4.95, gl = 30
# Valor do RMSE: 4.797

## iii) 

# Os valores, tanto para o intercepto (a), quanto para o coeficiente de variação
# (b), são estatísticamente significativos, já que apresentaram p-valor < 0.001.
# O valor de ^b, considerando um índice de confiança de 95%, não inclui o zero e 
# assume um valor positivo (0.654), o que mostra que o efeito do crescimento 
# econômico sobre o voto no candidato do partido incumbente é significativo. 
# Quanto aos valores do R^2 e do RMSE, 0.356 e 4.797, respectivamente, apontam 
# que o crescimento econômico explica aproximadamente 0.35 da variação dos votos
# no candidato do partido incumbente e o erro padrão do resíduo de 4.95 pontos
# em uma escala de 0 a 100 (já que é medido na mesma unidade da variável 
# dependente, que neste caso é a porcentagem de voto), indica que o ajuste do 
# modelo é satisfatório.

## letra c)

# Análise de regressão de VOTE (VD), GROWTH (VI) e GOODNEWS (VI):
reg_multi <- lm(VOTE ~ GROWTH + GOODNEWS, data = vote_growth)
summary(reg_multi)

### i)

# Verificar o coeficiente de correlação das VIs:
cor(x = vote_growth$GOODNEWS, y = vote_growth$GROWTH, method = c("pearson"))

# A variável adicionada ao modelo é a "GOODNEWS", que avalia o crescimento  
# forte (ou acimo do esperado) em cada um dos trimestres da administração
# do candidato incumbente. Estas boas performances da economia são consideradas
# "boas notícias", já que tendem a influenciar o comportamento do eleitor, 
# segundo estudos da psicologia política. A inclusão desta variável tem como 
# objetivo analisar o seu efeito que sobre os votos no candidato do partido
# incumbente (a VD), para que seja possível, então, analisar o efeito da 
# primeira VI ("GROWTH") sobre a VD, controlando para a VI adicionada ("GOODNEWS"). 

### ii)

# Imprimir resultados do modelo (tabela de regressão):
summary(reg_multi)
# Calcular RMSE: 
mean(residuals(reg_multi)^2) %>% sqrt()

# Valor de ^a = 48.120, desvio padrão = 1.748,  p-valor < 0.001 
# Valor de ^bX (GROWTH) = 0.573, desvio padrão = 0.153, p-valor < 0.001
# Valor de ^bZ (GOODNEWS) = 0.718, desvio padrão = 0.296, p-valor = 0.01 
# Valor do R^2 = 0.464
# Valor do erro padrão dos resíduos = 4.6, gl = 29
# Valor do RMSE: 4.375

### iii)

# Com a inclusão da variável GOODNEWS, o R^2 mostra que 0.464 da variação dos 
# votos no candidato do partido incumbente é explicada pelas duas VIs analisadas
# (GROWTH e GOODNEWS). O valor do coeficiente de variação (^bX) para GROWTH
# neste modelo é de 0.573, que é menor do que aquele encontrado no modelo 
# bivariado, visto que este modelo apresenta os resultados de GROWTH sobre os 
# votos, controlando para o efeito de GOODNEWS, com p-valor < 0.001. Para 
# GOODNEWS, controlando para o efeito de GROWTH, o coeficiente de variação (^bZ)
# encontrado foi de 0.718 com p-valor = 0.01. Os resultados do p-valor e o fato
# dos ICs de 95% não incluírem o 0, mostra que os resultados são 
# estatisticamente significantes. Finalmente, o RMSE de 4.375 mostra que este 
# modelo é mais ajustado do que o bivariado apresentado na questão anterior.

### iv)

# O modelo multivariado apresentou RMSE menor do que o modelo bivariado, o que 
# significa que o erro padrão dos resíduos foi menor neste último modelo, 
# ou seja, a  diferença entre os valores observados e os valores estimados foi
# menor. Também, ao analisar os valores do R^2, nota-se que o modelo multivariado
# explicou uma parte maior da variação dos votos no candidato do partido incumbente.   

### v) 

# Analisar média dos resíduos do modelo 1:
resid_reg <- resid(reg)
mean(resid_reg)
# Plotar análise residual:
ggplot(data = vote_growth, aes(y = resid_reg, x = GROWTH)) +
  geom_point(color = "lightblue") +
    theme_classic() +
      geom_smooth(method="lm", color = "gray", se = F) +
        labs(title = "Análise Residual", y = "Resíduos", x = "Crescimento do PIB")

# O modelo 1 parece ser homocedástico, já que a dispersão dos resíduos não 
# parece diminuir ou aumentar com o aumento de X (GROWTH).

# Analisar resíduos modelo 2:
resid_reg_multi <- resid(reg_multi)
mean(resid_reg_multi)

# Plotar análise residual:
ggplot(data = vote_growth, aes(y = resid_reg_multi, x = GROWTH + GOODNEWS)) +
  geom_point(color = "lightblue") +
    theme_classic() +
      geom_smooth(method="lm", color = "gray", se = F) +
        labs(title = "Análise Residual", y = "Resíduos", 
             x = "Crescimento do PIB + Good News")

# O modelo 2 também parece ser homocedástico, já que a dispersão dos resíduos 
# não parece diminuir ou aumentar com o aumento de X e Z (GROWTH e GOODNEWS).


## letra d)

# Análise de regressão de VOTE (VD), GROWTH (VI) e WAR (VI):
reg_multi_2 <- lm(VOTE ~ GROWTH + WAR, data = vote_growth)
summary(reg_multi_2)

### i)

# Verificar o coeficiente de correlação das VIs:
cor(x = vote_growth$WAR, y = vote_growth$GROWTH, method = c("pearson"))

# A variável "WAR" apresenta fraca correlação com "GROWTH" (-0.12). A inclusão 
# da variável servirá para analisar e controlar os efeitos dos períodos de 
# guerra sobre "GROWTH" e sobre a variação do percentual de voto recebido pelo 
# candidato do partido incumbente. 

### ii)

# Imprimir resultados do modelo (tabela de regressão):
summary(reg_multi_2)
# Calcular RMSE: 
mean(residuals(reg_multi_2)^2) %>% sqrt()

# Valor de ^a = 52.246, desvio padrão = 0.919,  p-valor < 0.001 
# Valor de ^bX (GROWTH) = 0.628, desvio padrão = 0.160, p-valor < 0.001
# Valor de ^bW (WAR) = -3.951, desvio padrão = 2.989, p-valor = 1
# Valor do R^2 = 0.392
# Valor do erro padrão dos resíduos = 4.98, gl = 29
# Valor do RMSE: 4.659

### iii)

# Com a inclusão da variável WAR, o R^2 mostra que 0.392 da variação dos 
# votos no candidato do partido incumbente é explicada pelas duas VIs analisadas
# (GROWTH e WAR). O valor do coeficiente de variação (^bW) encontrado, 
# controlando para "GROWTH", foi de -3.951 com p-valor = 1. O resultado 
# encontrados não é estatisticamente significante, já que o valor do coeficiente
# de variação (^bW) apresenta p-valor = 1 e o IC de 95% inclui o zero. 
# Finalmente, o RSE de 4.89 e o RMSE de 4.659, mostram que este modelo é menos 
# ajustado do que aquele apresentado na letra c.

### iv) 

# Neste terceiro modelo o valor do RMSE aumentou, o que significa que ele é 
# menos ajustado do que o modelo anterior. A inclusão de WAR pouco afetou o 
# efeito de GROWTH sobre a variação dos votos, mas afetou o efeito.

### v)

# Analisar resíduos modelo 3:
resid_reg_multi_2 <- resid(reg_multi_2)
mean(resid_reg_multi_2)

# Plotar análise residual:
ggplot(data = vote_growth, aes(y = resid_reg_multi_2, x = GROWTH + WAR)) +
  geom_point(color = "lightblue") +
    theme_classic() +
      geom_smooth(method="lm", color = "gray", se = F) +
        labs(title = "Análise Residual", y = "Resíduos", 
             x = "Crescimento do PIB + Guerra")


# O modelo 3 também parece ser homocedástico, já que a dispersão dos resíduos 
# não parece apresentar um padrão (de diminuição ou aumento da variância) com 
# o aumento de X e W (GROWTH e WAR, respectivamente).

### vi)

# Padronizar os ^b:

# Modelo 2 (GROWTH e GOODNEWS): 
0.573 * (sd(vote_growth$GROWTH)/ sd(vote_growth$VOTE))
0.718 * (sd(vote_growth$GOODNEWS) / sd(vote_growth$VOTE))


# Modelo 3 (GROWTH e WAR): 
0.628 * (sd(vote_growth$GROWTH)/ sd(vote_growth$VOTE))
-3.951 * (sd(vote_growth$WAR) / sd(vote_growth$VOTE))


# A variável que tem maior efeito sobre o percentual de votos no candidato do 
# partido incumbente é "GROWTH", visto que, no segundo modelo, o incremento de 
# uma unidade nesta VI é responsavel por uma variação de cerca de 0.523 na VD
# (versus 0.337 de "GOODNEWS") e 0.573 no terceiro modelo (versus -0.193 de "WAR").


  # Questão 2:

## letra a)

# Pérez, E. O., & Tavits, M. (2017). Language Shapes People’s Time Perspective 
# and Support for Future-Oriented Policies. American Journal of Political 
# Science, 61(3), 715–727. Disponível em: <doi:10.1111/ajps.12290>.

## letra b)

# A variável dependente é a "percepção temporal" dos cidadãos. Os autores 
# buscam compreender se a língua falada influencia o apoio dos cidadãos às 
# políticas "future-oriented". Descobrem que nas línguas "futureless" os 
# eleitores têm uma percepção temporal diferente do que aqueles de línguas 
# "futured"; os primeiros têm maior probabilidade de endossar as políticas de 
# longo prazo, já que percebem o futuro como um "tempo" mais próximo do presente.
# No primeiro estudo, os autores avaliam o impacto de uma língua "futureless" - 
# o estoniano - e desenvolvem cinco modelos de análise, cada um com uma variável
# dependente diferente, mas todas com o mesmo objetivo de avaliar a "percepção 
# temporal" dos eleitores. Neste estudo, os autores esperam encontrar maior 
# coeficiente de variação para VDs que representem percepções de longo prazo, já
# que o estoniano, por ser uma língua "futureless", faz com que os seus falantes
# tenham percepções menos imediatistas e mais voltadas para o futuro (e esperam
# encontrar o efeito contrário para o russo, que é a língua "futured" avaliada
# neste primeiro estudo). No segundo estudo os autores expandem a análise para
# 60 países e incluem uma série de variáveis de controle, como gênero, idade,
# educação, renda, situação empregatícia, estado civil, apoio à democracia e 
# capital social. As línguas são classificadas em "futured" ou "futureless" de
# acordo com a teoria de "future time reference" (FTR; Chen 2013) de uma língua,
# recebe valor 1 se fortemente "futured" e 0 se fortemente "futureless". Os 
# autores desenvolvem seis modelos, os três primeiros têm como VD a probabilidade
# de os cidadãos apoiarem políticas que poupem o meio ambiente e os três 
# últimos, a VD é a probabilidade de o indivíduo poupar dinheiro. Neste segundo 
# estudo, os autores esperam um coeficiente de variação (^b) negativo para 
# países com línguas consideradas "futured", já que postulam que os cidadãos 
# falantes destas línguas estão mais propensos a apoiar políticas de curto prazo. 

## letra c)

# No primeiro estudo os autores encontram, assim como esperado, um ^b positivo
# e significante para o efeito do estoniano (uma língua "futureless") sobre as 
# percepções de longo prazo; e um ^b negativo e significante para o efeito do 
# estoniano sobre as percepções de curto prazo. No segundo estudo, os modelos 
# 3 e 6 agregam todas as variáveis políticas e demográficas mencionadas na 
# letra b) e o resultado que é apresentado, mais uma vez, corresponde aos 
# postulados dos autores: o resultado do ^b para o efeito de línguas "futured" 
# sobre o apoio a compromissos de longo prazo, mostrou-se negativo e significante. 

