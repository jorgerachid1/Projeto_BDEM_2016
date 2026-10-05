# script roteiro do BDEM - no repositório Projeto_BDEM_2016
# Antes de começar a fazer qualquer coisa:
# a) Coloque todos os arquivos postados no Classroom (já descompactados) dentro do repositório local Projeto_BDEM_2016
# b) commit este roteiro com a mensagem "dados, arquivos de texto e script roteiro BDEM" e envie para o repositório Projeto_BDEM_2016
# c) salve o script com outro nome (script_BDEM.R) e commit com a mensagem "script BDEM" e envie para o repositório Projeto_BDEM_2016

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##################################
# ETAPA 1: BANCO DE DADOS DO SIM
##################################
# Você deve criar e estar na branch SIM antes de inserir os comandos
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SIM_2016 com 1309774 linhas e 87 colunas com o nome de dados_sim
# Verificar se a leitura foi feita corretamente e a estrutura dos dados

dados_sim = read.csv("SIM_2016.csv", sep = ";", encoding = "latin1")
# Se a leitura não resultar em 1309774 linhas e 87 colunas, tente sep = ";" e/ou encoding = "latin1"
dim(dados_sim)          # deve ser 1309774  87
str(dados_sim)


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# As colunas serão: 1, 3, 9, 10, 11, 14, 17, 35, 47
# Nomes das respectivas variáveis: CONTADOR, TIPOBITO, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, CAUSABAS

dados_sim_1 = dados_sim[, c(1, 3, 9, 10, 11, 14, 17, 35, 47)]
names(dados_sim_1) = c("CONTADOR", "TIPOBITO", "IDADE", "SEXO", "RACACOR",
                        "ESC2010", "CODMUNRES", "TPMORTEOCO", "CAUSABAS")
str(dados_sim_1)


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11:8344      12:3763     13:16799    14:2157      15:38557     16:2995     17:7490
# 21:34362     22:19187    23:54276    24:21922     25:28041     26:66928    27:20769    28:13516     29:88094
# 31:135257    32:22868    33:141089   35:296359
# 41:74740     42:40270    43:87583
# 50:16749     51:17535    52:38074    53:12050

# Aluno responsável pela UF 27 (Alagoas - AL)
dados_sim_1$UF = substr(as.character(dados_sim_1$CODMUNRES), 1, 2)
dados_sim_2 = dados_sim_1[dados_sim_1$UF == "27", ]
dados_sim_2$UF = NULL
nrow(dados_sim_2)   # deve dar 20769, conforme a tabela de conferência acima (UF 27 - AL)


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIM - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis:
# TIPOBITO, SEXO, RACACOR, ESC2010, TPMORTEOCO, CAUSABAS
# Avalie também os valores das variável IDADE (não estranhe mas idade é composta de um dígito inicial que indica a unidade de medida)
# Unidades de medida a serem consideradas em IDADE: 0: minutos, 1: horas, 2: dias, 3: meses, 4: anos, 5: idade maior que 100 anos
# Atenção: a unidade de medida de IDADE no DICIONÀRIO do SIM está errada
# O propósito das avaliações acima é verificar se as categorias estão de acordo com o dicionário do SIM ou se aparecem categorias estranhas

table(dados_sim_2$TIPOBITO, useNA = "always")
table(dados_sim_2$SEXO, useNA = "always")
table(dados_sim_2$RACACOR, useNA = "always")
table(dados_sim_2$ESC2010, useNA = "always")
table(dados_sim_2$TPMORTEOCO, useNA = "always")
table(dados_sim_2$CAUSABAS, useNA = "always")

# IDADE: 1º dígito = unidade (0,1,2,3,4,5 - ver acima); 2 últimos dígitos = quantidade
idade_chr = formatC(dados_sim_2$IDADE, width = 3, flag = "0")
table(substr(idade_chr, 1, 1), useNA = "always")     # frequência das unidades de medida
summary(dados_sim_2$IDADE)


# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIM - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado",
# geralmente com código 9
# Verifique o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

dados_sim_2$SEXO[dados_sim_2$SEXO %in% c(0, 9)] = NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] = NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] = NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] = NA
dados_sim_2$CAUSABAS[dados_sim_2$CAUSABAS == "" | dados_sim_2$CAUSABAS == "9"] = NA

# IDADE: quantidade "99" (2 últimos dígitos) indica ignorado dentro de cada unidade
idade_chr = formatC(dados_sim_2$IDADE, width = 3, flag = "0")
dados_sim_2$IDADE[substr(idade_chr, 2, 3) == "99"] = NA

# conferência após a limpeza
table(dados_sim_2$SEXO, useNA = "always")
table(dados_sim_2$RACACOR, useNA = "always")
table(dados_sim_2$ESC2010, useNA = "always")
table(dados_sim_2$TPMORTEOCO, useNA = "always")
summary(dados_sim_2$IDADE)


# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SIM - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a PRIMEIRA LETRA da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1, 2),
                               labels = c("Fetal", "Não fetal"))

dados_sim_2$SEXO = factor(dados_sim_2$SEXO, levels = c(1, 2),
                           labels = c("Masculino", "Feminino"))

dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                              labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sim_2$ESC2010 = factor(dados_sim_2$ESC2010, levels = c(0, 1, 2, 3, 4, 5),
                              labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                                         "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)",
                                         "Superior incompleto", "Superior completo"))

dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8),
                                 labels = c("Na gravidez", "No parto", "No abortamento",
                                            "Até 42 dias após o término do parto",
                                            "De 43 dias a 1 ano após o término da gestação",
                                            "Não ocorreu nestes períodos"))

str(dados_sim_2)


# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SIM - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Criar um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 - SIM.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

# TORC: registro completo (sem NA) nas 87 variáveis originais do SIM_2016.csv, refeito o filtro da UF sobre dados_sim (não sobre dados_sim_2)
dados_sim_full_2 = dados_sim[substr(as.character(dados_sim$CODMUNRES), 1, 2) == "27", ]

# idade em dias (unidade 0/1 = minutos/horas -> < 1 dia; 2 = dias; 3 = meses -> dias aproximados) para as faixas neonatais
idade_chr = formatC(dados_sim_2$IDADE, width = 3, flag = "0")
unid = substr(idade_chr, 1, 1)
qtd  = as.numeric(substr(idade_chr, 2, 3))
dias_vida = ifelse(unid %in% c("0", "1"), 0,
             ifelse(unid == "2", qtd,
              ifelse(unid == "3", 30 * qtd, NA)))

# idade em anos completos (só faz sentido quando a unidade é "4" = anos) para idade fértil (15 a 49 anos)
idade_anos = ifelse(unid == "4", qtd, NA)
idade_fertil = !is.na(idade_anos) & idade_anos >= 15 & idade_anos <= 49

# classificação de CAUSABAS em capítulos/faixas do CID-10 (chave = letra + 2 primeiros dígitos, para comparação por intervalo)
causa_letra = substr(dados_sim_2$CAUSABAS, 1, 1)
causa_num   = suppressWarnings(as.numeric(substr(dados_sim_2$CAUSABAS, 2, 3)))
cid_key = ifelse(is.na(dados_sim_2$CAUSABAS), NA, paste0(causa_letra, formatC(causa_num, width = 2, flag = "0")))

externa = cid_key >= "V01" & cid_key <= "Y98"                                            # causas externas (V01-Y98)
natural = !is.na(cid_key) & !externa                                                     # causas básicas naturais
cb_i = natural & cid_key >= "A00" & cid_key <= "B99"                                      # infecciosas e parasitárias
cb_n = natural & ((cid_key >= "C00" & cid_key <= "D48") | (cid_key >= "D50" & cid_key <= "D89"))  # neoplasias e sangue
cb_c = natural & cid_key >= "I00" & cid_key <= "I99"                                      # aparelho circulatório
cb_r = natural & cid_key >= "J00" & cid_key <= "J99"                                      # aparelho respiratório
cb_o = natural & !cb_i & !cb_n & !cb_c & !cb_r                                            # demais causas naturais

# faixas neonatais (0-27 dias) e pós-neonatal (28-364 dias), apenas para óbitos não fetais
nt   = !is.na(dias_vida) & dias_vida <= 27 & dados_sim_2$TIPOBITO == "Não fetal"
nt_p = nt & dias_vida <= 6
nt_t = nt & dias_vida >= 7
pnt  = !is.na(dias_vida) & dias_vida >= 28 & dias_vida <= 364 & dados_sim_2$TIPOBITO == "Não fetal"

# óbitos maternos por TPMORTEOCO: precoces (gestação, parto, abortamento ou até 42 dias) e tardios (43 dias a 1 ano)
mt_dg = dados_sim_2$TPMORTEOCO == "Na gravidez"
mt_pt = dados_sim_2$TPMORTEOCO == "No parto"
mt_ab = dados_sim_2$TPMORTEOCO == "No abortamento"
mt_42 = dados_sim_2$TPMORTEOCO == "Até 42 dias após o término do parto"
mt_43 = dados_sim_2$TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação"
mt_p  = mt_dg | mt_pt | mt_ab | mt_42
mt    = mt_p | mt_43

soma = function(x) sum(x, na.rm = TRUE)

# calcula as 37 variáveis de contagem (TO a TO_MT_P_ESC) para um subconjunto de linhas
resumo_sim = function(linhas) {
  d87 = dados_sim_full_2[linhas, ]
  data.frame(
    TO = length(linhas), TORC = soma(complete.cases(d87)),
    TORCR = NA_integer_,       # PENDENTE: lista das 14 variáveis reduzidas do SIM ainda não definida
    TO_NN = soma(externa[linhas]), TO_N = soma(natural[linhas]),
    TO_CB_I = soma(cb_i[linhas]), TO_CB_N = soma(cb_n[linhas]), TO_CB_C = soma(cb_c[linhas]),
    TO_CB_R = soma(cb_r[linhas]), TO_CB_O = soma(cb_o[linhas]),
    TO_M = soma(dados_sim_2$SEXO[linhas] == "Masculino"), TO_F = soma(dados_sim_2$SEXO[linhas] == "Feminino"),
    TO_F_IF = soma(dados_sim_2$SEXO[linhas] == "Feminino" & idade_fertil[linhas]),
    TO_FT = soma(dados_sim_2$TIPOBITO[linhas] == "Fetal"),
    TO_NT = soma(nt[linhas]), TO_NT_P = soma(nt_p[linhas]), TO_NT_T = soma(nt_t[linhas]), TO_PNT = soma(pnt[linhas]),
    TONT_B  = soma(nt[linhas] & dados_sim_2$RACACOR[linhas] == "Branca"),
    TONT_PT = soma(nt[linhas] & dados_sim_2$RACACOR[linhas] == "Preta"),
    TONT_A  = soma(nt[linhas] & dados_sim_2$RACACOR[linhas] == "Amarela"),
    TONT_PD = soma(nt[linhas] & dados_sim_2$RACACOR[linhas] == "Parda"),
    TONT_I  = soma(nt[linhas] & dados_sim_2$RACACOR[linhas] == "Indígena"),
    TO_MT = soma(mt[linhas]), TO_MT_DG = soma(mt_dg[linhas]), TO_MT_PT = soma(mt_pt[linhas]),
    TO_MT_AB = soma(mt_ab[linhas]), TO_MT_42 = soma(mt_42[linhas]), TO_MT_43 = soma(mt_43[linhas]),
    TO_MT_P = soma(mt_p[linhas]), TO_MT_P_I = soma(mt_p[linhas] & dados_sim_2$SEXO[linhas] == "Feminino" & idade_fertil[linhas]),
    TO_MT_P_ES   = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Sem escolaridade"),
    TO_MT_P_EFI  = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Fundamental I (1ª a 4ª série)"),
    TO_MT_P_EFII = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Fundamental II (5ª a 8ª série)"),
    TO_MT_P_EM   = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Médio (antigo 2º grau)"),
    TO_MT_P_ESI  = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Superior incompleto"),
    TO_MT_P_ESC  = soma(mt_p[linhas] & dados_sim_2$ESC2010[linhas] == "Superior completo")
  )
}

linha_uf = cbind(data.frame(ANO = 2016, NIVEL = "UF", CODMUNRES = 27),
                  resumo_sim(seq_len(nrow(dados_sim_2))))

municipios = sort(unique(dados_sim_2$CODMUNRES))
linhas_municipio = do.call(rbind, lapply(municipios, function(cod) {
  linhas = which(dados_sim_2$CODMUNRES == cod)
  cbind(data.frame(ANO = 2016, NIVEL = "MUNICIPIO", CODMUNRES = cod), resumo_sim(linhas))
}))

SIM_AL = rbind(linha_uf, linhas_municipio)
rownames(SIM_AL) = NULL

dim(SIM_AL)          # deve ser 104  40 (1 linha de UF + 103 municípios)
head(SIM_AL)
str(SIM_AL)


# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SIM - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Exportar o banco de dados com o nome SIM_UF.csv (Exemplo: SIM_RJ.csv)

write.csv(SIM_AL, "SIM_AL.csv", row.names = FALSE)

# Ao terminar a Tarefa 8 fazer um commit com o comentário "dados SIM_UF 2016 e script - SIM - tarefas 1 a 8"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 2: BANCO DE DADOS DO SINASC
####################################
# Você deve criar e estar na branch SINASC antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SINASC_2016 com 2857800 linhas e 61 colunas com o nome de dados_sinasc
# Verificar se a leitura foi feita corretamente e a estrutura dos dados
# Por uma questão de padronização coloque todos os nomes das variáveis em letra maiúscula,
# usando o comando names(dados_sinasc) = toupper(names(dados_sinasc))

dados_sinasc = read.csv("SINASC_2016.csv", sep = ";", encoding = "latin1")
names(dados_sinasc) = toupper(names(dados_sinasc))
# Se a leitura não resultar em 2857800 linhas e 61 colunas, tente sep = ";" e/ou encoding = "latin1"
dim(dados_sinasc)          # deve ser 2857800  61
str(dados_sinasc)


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINASC - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# As colunas serão 3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61
# Nomes das respectivas variáveis: CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK, CONTADOR

dados_sinasc_1 = dados_sinasc[, c(3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61)]
names(dados_sinasc_1) = c("CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", "CODMUNRES",
                           "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", "APGAR5", "RACACOR",
                           "PESO", "IDANOMAL", "ESCMAE2010", "RACACORMAE", "SEMAGESTAC",
                           "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK", "CONTADOR")
str(dados_sinasc_1)


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 26602     12: 15773     13: 76703     14: 11376     15: 137681    16: 15521      17: 23870
# 21: 110493    22: 46986     23: 126246    24: 45366     25: 56083     26: 130733     27: 48164     28: 32218     29: 199830
# 31: 253520    32: 53413     33: 219129    35: 601437     
# 41: 155066    42: 95313     43: 141411
# 50: 42432     51: 53531     52: 95563     53: 43340

# Aluno responsável pela UF 27 (Alagoas - AL)
dados_sinasc_1$UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)
dados_sinasc_2 = dados_sinasc_1[dados_sinasc_1$UF == "27", ]
dados_sinasc_2$UF = NULL
nrow(dados_sinasc_2)   # deve dar 48164, conforme a tabela de conferência acima (UF 27 - AL)


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
# Avalie também os valores das variáveis quantitativas de IDADEMAE, SEMAGESTAC, APGAR5 e PESO

table(dados_sinasc_2$LOCNASC, useNA = "always")
table(dados_sinasc_2$ESTCIVMAE, useNA = "always")
table(dados_sinasc_2$GESTACAO, useNA = "always")
table(dados_sinasc_2$GRAVIDEZ, useNA = "always")
table(dados_sinasc_2$PARTO, useNA = "always")
table(dados_sinasc_2$SEXO, useNA = "always")
table(dados_sinasc_2$RACACOR, useNA = "always")
table(dados_sinasc_2$IDANOMAL, useNA = "always")
table(dados_sinasc_2$ESCMAE2010, useNA = "always")
table(dados_sinasc_2$RACACORMAE, useNA = "always")
table(dados_sinasc_2$TPAPRESENT, useNA = "always")
table(dados_sinasc_2$TPROBSON, useNA = "always")
table(dados_sinasc_2$PARIDADE, useNA = "always")
table(dados_sinasc_2$KOTELCHUCK, useNA = "always")

summary(dados_sinasc_2$IDADEMAE)
summary(dados_sinasc_2$SEMAGESTAC)
summary(dados_sinasc_2$APGAR5)
summary(dados_sinasc_2$PESO)


# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SINASC para identificar qual o código das categorias de cada variável
# KOTELCHUCK = 9 significa "Não informado"   TPROBSON = 11 significa "Não classificado por falta de informação"
# Em variáveis quantitativas como IDADEMAE verificar se existem valores como 9999 para NA

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA

# APGAR5: 99 indica ignorado
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
#corrigindo o que eu tinha esquecido na tarefa anterior
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
# conferência após a limpeza
table(dados_sinasc_2$LOCNASC, useNA = "always")
table(dados_sinasc_2$ESTCIVMAE, useNA = "always")
table(dados_sinasc_2$GESTACAO, useNA = "always")
table(dados_sinasc_2$GRAVIDEZ, useNA = "always")
table(dados_sinasc_2$PARTO, useNA = "always")
table(dados_sinasc_2$SEXO, useNA = "always")
table(dados_sinasc_2$IDANOMAL, useNA = "always")
table(dados_sinasc_2$ESCMAE2010, useNA = "always")
table(dados_sinasc_2$TPAPRESENT, useNA = "always")
table(dados_sinasc_2$TPROBSON, useNA = "always")
table(dados_sinasc_2$KOTELCHUCK, useNA = "always")
summary(dados_sinasc_2$APGAR5)


# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1, 2, 3, 4),
                                labels = c("Hospital", "Outros estabelecimentos de saúde",
                                           "Domicílio", "Outros"))

dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Solteira", "Casada", "Viúva",
                                              "Separada judicialmente/divorciada", "União estável"))

dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1, 2, 3, 4, 5, 6),
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas",
                                             "28 a 31 semanas", "32 a 36 semanas",
                                             "37 a 41 semanas", "42 semanas e mais"))

dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3),
                                  labels = c("Única", "Dupla", "Tripla ou mais"))

dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1, 2),
                               labels = c("Vaginal", "Cesário"))

dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1, 2),
                              labels = c("Masculino", "Feminino"))

dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                                 labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2),
                                  labels = c("Sim", "Não"))

dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0, 1, 2, 3, 4, 5),
                                    labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                                               "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)",
                                               "Superior incompleto", "Superior completo"))

dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1, 2, 3, 4, 5),
                                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1, 2, 3),
                                    labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))

dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0, 1),
                                  labels = c("Nulípara", "Multípara"))

dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1, 2, 3, 4, 5),
                                    labels = c("Não realizou pré-natal", "Inadequado", "Intermediário",
                                               "Adequado", "Mais que adequado"))

# TPROBSON não recebe legenda: é o código do Grupo de Robson gerado pelo sistema,
# e o dicionário do SINASC não define descrição textual para as 10 categorias

str(dados_sinasc_2)


# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$F_PESO = cut(dados_sinasc_2$PESO, breaks = c(-Inf, 2499, 3999, Inf),
                             labels = c("Baixo peso", "Peso normal", "Macrossomia"))

dados_sinasc_2$F_IDADE = cut(dados_sinasc_2$IDADEMAE,
                              breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
                              labels = c("<15", "15-19", "20-24", "25-29", "30-34",
                                         "35-39", "40-44", "45-49", "50+"))

dados_sinasc_2$F_APGAR5 = cut(dados_sinasc_2$APGAR5, breaks = c(-Inf, 6, Inf),
                               labels = c("Baixo", "Normal"))

# PEREG: peregrinação materna (nasceu em município diferente do de residência)
dados_sinasc_2$PEREG = NA
dados_sinasc_2$PEREG[dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES] = "Não"
dados_sinasc_2$PEREG[dados_sinasc_2$CODMUNNASC != dados_sinasc_2$CODMUNRES] = "Sim"
dados_sinasc_2$PEREG = factor(dados_sinasc_2$PEREG, levels = c("Não", "Sim"))

# ESTCIV: agrupamento de ESTCIVMAE (já convertida em fator na Tarefa 6) por presença de companheiro
dados_sinasc_2$ESTCIV = NA
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada")] = "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável")] = "Com companheiro"
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro"))

str(dados_sinasc_2)


# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# Criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

# Vetor lógico que identifica as linhas do Alagoas — o mesmo usado na Tarefa 3,
# válido tanto para dados_sinasc quanto para dados_sinasc_1 (mesma ordem de linhas)

filtro_al = dados_sinasc_1$UF == "27"

# TNRC: completude nas 61 variáveis originais do SINASC
REGCOMPLETO61 = complete.cases(dados_sinasc[filtro_al, ])

# TNRCR: completude nas variáveis selecionadas na Tarefa 2 (21 + UF = 22 colunas, batendo com o PDF)
REGCOMPLETO21 = complete.cases(dados_sinasc_1[filtro_al, ])

# anexando por posição (não por merge) — nesse ponto dados_sinasc_2 ainda está na ordem original
dados_sinasc_2$REGCOMPLETO61 = REGCOMPLETO61
dados_sinasc_2$REGCOMPLETO21 = REGCOMPLETO21

# TNLOC_AI: aproveitando o código bruto do LOCNASC (código 5) antes de virar fator na Tarefa 6
dados_sinasc_2$AI_BRUTO = (dados_sinasc[filtro_al, "LOCNASC"] == 5)

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", sep = ";", encoding = "latin1")

dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

dados_sinasc_2$F_PIG = NA
dados_sinasc_2$F_PIG[dados_sinasc_2$GRAVIDEZ == "Única" &
                       !is.na(dados_sinasc_2$PESO) & !is.na(dados_sinasc_2$PESO_P10) &
                       dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10] = "PIG"

dados_sinasc_2$F_PIG[dados_sinasc_2$GRAVIDEZ == "Única" &
                       !is.na(dados_sinasc_2$PESO) & !is.na(dados_sinasc_2$PESO_P10) & !is.na(dados_sinasc_2$PESO_P90) &
                       dados_sinasc_2$PESO >= dados_sinasc_2$PESO_P10 & dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90] = "AIG"

dados_sinasc_2$F_PIG[dados_sinasc_2$GRAVIDEZ == "Única" &
                       !is.na(dados_sinasc_2$PESO) & !is.na(dados_sinasc_2$PESO_P90) &
                       dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90] = "GIG"

dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

table(dados_sinasc_2$F_PIG, useNA = "always")
str(dados_sinasc_2)

# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada


# funções seguras para evitar erro em municípios com poucos registros
media_segura = function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
dp_segura = function(x) if (sum(!is.na(x)) < 2) NA else sd(x, na.rm = TRUE)
quantil_seguro = function(x, p) if (all(is.na(x))) NA else as.numeric(quantile(x, probs = p, na.rm = TRUE))

resumo_sinasc = function(df, nivel, codmunres) {
  data.frame(
    ANO = 2016,
    NIVEL = nivel,
    CODMUNRES = codmunres,
    TN = nrow(df),
    TNRC = sum(df$REGCOMPLETO61, na.rm = TRUE),
    TNRCR = sum(df$REGCOMPLETO21, na.rm = TRUE),
    TGI_15 = sum(df$F_IDADE == "<15", na.rm = TRUE),
    TGI_15_19 = sum(df$F_IDADE == "15-19", na.rm = TRUE),
    TGI_20_24 = sum(df$F_IDADE == "20-24", na.rm = TRUE),
    TGI_25_29 = sum(df$F_IDADE == "25-29", na.rm = TRUE),
    TGI_30_34 = sum(df$F_IDADE == "30-34", na.rm = TRUE),
    TGI_35_39 = sum(df$F_IDADE == "35-39", na.rm = TRUE),
    TGI_40_44 = sum(df$F_IDADE == "40-44", na.rm = TRUE),
    TGI_45_49 = sum(df$F_IDADE == "45-49", na.rm = TRUE),
    TGI_50 = sum(df$F_IDADE == "50+", na.rm = TRUE),
    TGIF = sum(df$F_IDADE %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"), na.rm = TRUE),
    IM_P25 = quantil_seguro(df$IDADEMAE, 0.25),
    IM_P50 = quantil_seguro(df$IDADEMAE, 0.50),
    IM_P75 = quantil_seguro(df$IDADEMAE, 0.75),
    IM_MD = media_segura(df$IDADEMAE),
    IM_DP = dp_segura(df$IDADEMAE),
    EM_S = sum(df$ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(df$ESCMAE2010 == "Fundamental I (1ª a 4ª série)", na.rm = TRUE),
    EM_FII = sum(df$ESCMAE2010 == "Fundamental II (5ª a 8ª série)", na.rm = TRUE),
    EM_M = sum(df$ESCMAE2010 == "Médio (antigo 2º grau)", na.rm = TRUE),
    EM_SI = sum(df$ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(df$ESCMAE2010 == "Superior completo", na.rm = TRUE),
    TGRC_B = sum(df$RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(df$RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A = sum(df$RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(df$RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I = sum(df$RACACORMAE == "Indígena", na.rm = TRUE),
    TGSC = sum(df$ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(df$ESTCIV == "Com companheiro", na.rm = TRUE),
    TGPRI = sum(df$PARIDADE == "Nulípara", na.rm = TRUE),
    TGNPRI = sum(df$PARIDADE == "Multípara", na.rm = TRUE),
    TGU = sum(df$GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(df$GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm = TRUE),
    TGD_22 = sum(df$GESTACAO == "Menos de 22 semanas", na.rm = TRUE),
    TGD_22_27 = sum(df$GESTACAO == "22 a 27 semanas", na.rm = TRUE),
    TGD_28_31 = sum(df$GESTACAO == "28 a 31 semanas", na.rm = TRUE),
    TGD_32_36 = sum(df$GESTACAO == "32 a 36 semanas", na.rm = TRUE),
    TGD_37_41 = sum(df$GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_42 = sum(df$GESTACAO == "42 semanas e mais", na.rm = TRUE),
    TGD_PRT = sum(df$GESTACAO %in% c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas"), na.rm = TRUE),
    TGD_AT = sum(df$GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_PST = sum(df$GESTACAO == "42 semanas e mais", na.rm = TRUE),
    DG_P25 = quantil_seguro(df$SEMAGESTAC, 0.25),
    DG_P50 = quantil_seguro(df$SEMAGESTAC, 0.50),
    DG_P75 = quantil_seguro(df$SEMAGESTAC, 0.75),
    DG_MD = media_segura(df$SEMAGESTAC),
    DG_DP = dp_segura(df$SEMAGESTAC),
    TKC_NR = sum(df$KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE),
    TKC_ID = sum(df$KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(df$KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(df$KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(df$KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
    TGPRG_S = sum(df$PEREG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(df$PEREG == "Não", na.rm = TRUE),
    TPV = sum(df$PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(df$PARTO == "Cesário", na.rm = TRUE),
    TRAP_C = sum(df$TPAPRESENT == "Cefálico", na.rm = TRUE),
    TRAP_P = sum(df$TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(df$TPAPRESENT == "Transversa", na.rm = TRUE),
    TGROB_1 = sum(df$TPROBSON == 1, na.rm = TRUE),
    TGROB_2 = sum(df$TPROBSON == 2, na.rm = TRUE),
    TGROB_3 = sum(df$TPROBSON == 3, na.rm = TRUE),
    TGROB_4 = sum(df$TPROBSON == 4, na.rm = TRUE),
    TGROB_5 = sum(df$TPROBSON == 5, na.rm = TRUE),
    TGROB_6 = sum(df$TPROBSON == 6, na.rm = TRUE),
    TGROB_7 = sum(df$TPROBSON == 7, na.rm = TRUE),
    TGROB_8 = sum(df$TPROBSON == 8, na.rm = TRUE),
    TGROB_9 = sum(df$TPROBSON == 9, na.rm = TRUE),
    TGROB_10 = sum(df$TPROBSON == 10, na.rm = TRUE),
    TNLOC_H = sum(df$LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(df$LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
    TNLOC_D = sum(df$LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(df$LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(df$AI_BRUTO, na.rm = TRUE),
    TRS_M = sum(df$SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(df$SEXO == "Feminino", na.rm = TRUE),
    TRRC_B = sum(df$RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(df$RACACOR == "Preta", na.rm = TRUE),
    TRRC_A = sum(df$RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(df$RACACOR == "Parda", na.rm = TRUE),
    TRRC_I = sum(df$RACACOR == "Indígena", na.rm = TRUE),
    TRP_BP = sum(df$F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(df$F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M = sum(df$F_PESO == "Macrossomia", na.rm = TRUE),
    PESO_P25 = quantil_seguro(df$PESO, 0.25),
    PESO_P50 = quantil_seguro(df$PESO, 0.50),
    PESO_P75 = quantil_seguro(df$PESO, 0.75),
    PESO_MD = media_segura(df$PESO),
    PESO_DP = dp_segura(df$PESO),
    TRPIG_P = sum(df$F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(df$F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(df$F_PIG == "GIG", na.rm = TRUE),
    TRAPG5_B = sum(df$F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(df$F_APGAR5 == "Normal", na.rm = TRUE),
    APG5_MD = media_segura(df$APGAR5),
    APG5_DP = dp_segura(df$APGAR5),
    TRAC = sum(df$IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(df$IDANOMAL == "Não", na.rm = TRUE)
  )
}

municipios = sort(unique(dados_sinasc_2$CODMUNRES))

SINASC_MUNICIPIO = do.call(rbind, lapply(municipios, function(m) {
  resumo_sinasc(dados_sinasc_2[dados_sinasc_2$CODMUNRES == m, ], nivel = "MUNICIPIO", codmunres = m)
}))

linha_estado = resumo_sinasc(dados_sinasc_2, nivel = "UF", codmunres = 27)

SINASC_UF = rbind(SINASC_MUNICIPIO, linha_estado)

dim(SINASC_UF)   # deve ter (nº de municípios do Alagoas + 1) linhas e 103 colunas
str(SINASC_UF)


## Ficou com alguns dados estranhos ##


# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016


# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)
# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SIM - tarefas 1 a 10"  e envie para o repositório Projeto_BDEM_2016

write.csv(SINASC_UF, "SINASC_AL.csv", row.names = FALSE)


####################################
# ETAPA 3: BANCOS DE DADOS DO SIDRA
####################################
# Você deve criar e estar na branch SIDRA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# dados_sidra_1 para população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv
# dados_sidra_2 para população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# dados_sidra_3 para população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv
# dados_sidra_4 para população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv
# Atenção que agora os arquivos têm nomes e códigos (com 7 dígitos) dos municípios (e alguns UF)

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

# tabela_6579 está em latin1 e tem "..." (dado inexistente) em um município do MT
dados_sidra_1 = read.csv("população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv",
                         sep = ";", fileEncoding = "latin1", na.strings = "...")
# as tabelas 1552 estão em UTF-8 com BOM
dados_sidra_2 = read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv",
                         sep = ";", fileEncoding = "UTF-8-BOM")
dados_sidra_3 = read.csv("população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv",
                         sep = ";", fileEncoding = "UTF-8-BOM")
dados_sidra_4 = read.csv("população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv",
                         sep = ";", fileEncoding = "UTF-8-BOM")

# dados_sidra_3 termina com uma linha vazia (;;;;;)
dados_sidra_3 = dados_sidra_3[!is.na(dados_sidra_3$CODMUNRES), ]

dim(dados_sidra_1); str(dados_sidra_1)
dim(dados_sidra_2); str(dados_sidra_2)
dim(dados_sidra_3); str(dados_sidra_3)
dim(dados_sidra_4); str(dados_sidra_4)
table(dados_sidra_4$F_IDADE)


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIDRA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Criar uma nova variável de nome CODUF com os códigos da UF nos bancos dados_sidra_1, dados_sidra_2, dados_sidra_4

dados_sidra_1$CODUF = substr(as.character(dados_sidra_1$CODMUNRES), 1, 2)
dados_sidra_2$CODUF = substr(as.character(dados_sidra_2$CODMUNRES), 1, 2)
dados_sidra_4$CODUF = substr(as.character(dados_sidra_4$CODMUNRES), 1, 2)
table(dados_sidra_1$CODUF)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Selecionar em dados_sidra_ 1 a dados_sidra_4 a UF de responsabilidade do aluno 
# e chamar os bancos de dados, respectivamente por sidra_1, sidra_2, sidra_3 e sidra_4

# Aluno responsável pela UF 27 (Alagoas - AL)
sidra_1 = dados_sidra_1[dados_sidra_1$CODUF == "27", ]
sidra_2 = dados_sidra_2[dados_sidra_2$CODUF == "27", ]
sidra_3 = dados_sidra_3[dados_sidra_3$CODMUNRES == 27, ]   # só tem linhas de UF, CODMUNRES já é o código da UF
sidra_4 = dados_sidra_4[dados_sidra_4$CODUF == "27", ]

nrow(sidra_1)   # 103: a UF + 102 municípios
nrow(sidra_2)   # 103
nrow(sidra_3)   # 19 faixas etárias
nrow(sidra_4)   # 102 municípios x 19 faixas = 1938

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4: Criar um banco de dados, de nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 4 - SIDRA.pdf”

# Faixas etárias agrupadas em <15, 15 a 49 e 50+
# sidra_3 (linha da UF) e sidra_4 (municípios) têm as mesmas faixas, então são empilhados
faixas = rbind(sidra_3[, c("CODMUNRES", "F_IDADE", "POP", "POPF")],
               sidra_4[, c("CODMUNRES", "F_IDADE", "POP", "POPF")])

faixas$GRUPO = ifelse(faixas$F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos"), "15",
               ifelse(faixas$F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                            "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"), "15_49", "50"))

pop = as.data.frame.matrix(tapply(faixas$POP, list(faixas$CODMUNRES, faixas$GRUPO), sum))
names(pop) = c("POPRC_15", "POPRC_15_49", "POPRC_50")
pop$CODMUNRES = as.numeric(rownames(pop))

popf = as.data.frame.matrix(tapply(faixas$POPF, list(faixas$CODMUNRES, faixas$GRUPO), sum))
names(popf) = c("POPRC_F_15", "POPRC_F_15_49", "POPRC_F_50")
popf$CODMUNRES = as.numeric(rownames(popf))

# Juntar tudo pela chave CODMUNRES (7 dígitos neste ponto)
SIDRA_UF = merge(sidra_1[, c("CODMUNRES", "POPRE_T")],
                 sidra_2[, c("CODMUNRES", "POPRC_T", "POPRC_M", "POPRC_F")], by = "CODMUNRES", all.x = TRUE)
SIDRA_UF = merge(SIDRA_UF, pop, by = "CODMUNRES", all.x = TRUE)
SIDRA_UF = merge(SIDRA_UF, popf, by = "CODMUNRES", all.x = TRUE)

SIDRA_UF$ANO = 2016
SIDRA_UF$NIVEL = ifelse(SIDRA_UF$CODMUNRES == 27, "UF", "MUNICIPIO")

# Os códigos de município do SIDRA têm 7 dígitos (o último é dígito verificador).
# Para ficar igual a SIM_AL.csv e SINASC_AL.csv (6 dígitos), retira-se o último dígito
SIDRA_UF$CODMUNRES = ifelse(SIDRA_UF$NIVEL == "UF", "27", substr(as.character(SIDRA_UF$CODMUNRES), 1, 6))

# linha da UF primeiro e ordem das colunas do PDF
SIDRA_UF = SIDRA_UF[order(SIDRA_UF$NIVEL != "UF", SIDRA_UF$CODMUNRES), ]
SIDRA_UF = SIDRA_UF[, c("ANO", "NIVEL", "CODMUNRES", "POPRE_T", "POPRC_T", "POPRC_M", "POPRC_F",
                        "POPRC_15", "POPRC_15_49", "POPRC_50", "POPRC_F_15", "POPRC_F_15_49", "POPRC_F_50")]
rownames(SIDRA_UF) = NULL

# Conferência: na linha da UF, a soma dos municípios deve bater com o total
colSums(SIDRA_UF[SIDRA_UF$NIVEL == "MUNICIPIO", 4:13]) - SIDRA_UF[SIDRA_UF$NIVEL == "UF", 4:13]
# faixas etárias devem somar o total
all(SIDRA_UF$POPRC_15 + SIDRA_UF$POPRC_15_49 + SIDRA_UF$POPRC_50 == SIDRA_UF$POPRC_T)

dim(SIDRA_UF)
str(SIDRA_UF)
head(SIDRA_UF)

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5:Exportar o banco de dados com o nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv)
# Ao terminar a Tarefa 5 commit com o comentário "dados SIDRA_UF 2016 e script - SIDRA - tarefas 1 a 5"  e envie para o repositório Projeto_BDEM_2016


####################################
# ETAPA 4: BANCOS DE DADOS DO ATLAS
####################################
# Você deve criar e estar na branch ATLAS antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# codigos_IBGE_2010 para códigos dos municípios - 2010.csv
# dados_atlas_1 para IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# dados_atlas_2 para IDHM - 2010 - municípios - Atlas Brasil.csv
# Atenção que agora alguns arquivos só têm os nomes dos municípios e das UFs, mas não têm os códigos

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - ATLAS - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Manipular o banco de dados e criar o banco de dados ATLAS_UF

# Criar o banco UF_codigo tipo tabela de correspondência
UF_codigo = data.frame(
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
         "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
         "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
         "Espírito Santo","Rio de Janeiro","São Paulo","Paraná",
         "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
         "Mato Grosso","Goiás","Distrito Federal"),
  
  SIGLA = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL",
            "SE","BA","MG","ES","RJ","SP",
            "PR","SC","RS","MS","MT","GO","DF"),
  
  CODUF = c(11,12,13,14,15,16,17,
            21,22,23,24,25,26,27,
            28,29,31,32,33,35,
            41,42,43,50,51,52,53)
)

# Retirar de dados_atlas_1 a linha do Brasil e adicionar (com merge by UF) as colunas de UF_codigo

# Criar o banco linha_estado somente com as linhas da UF e com as seguintes colunas:
# ANO=2016, NIVEL=UF, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M e IDHM_CA_F 

# Selecionar de linha_estado a UF da responsabilidade do aluno por CODMUNRES

# Criar em dados_atlas_2 a coluna com UF

# Retirar (UF) da variável município

# Acrescentar em codigos_IBGE_2010 a variável CODUF baseado nos dois primeiros dígitos de CODMUNRES

# Acrescentar a codigos_IBGE_2010 as variáveis de UF_codigo (merge by CODUF)

# Associar dados_atlas_2 a codigos_IBGE_2010 e nomear o novo arquivo por atlas_municipio
# Neste caso o merge será by.x = c("município","UF") e by.y = c("município","SIGLA")

# Remover de atlas_municipio a coluna UF.y criada no merge

# Selecionar somente a UF de responsabilidade do aluno através dos dois primeiros dógitos de CODMUNRES

# Criar banco ATLAS_MUNICIPIO com as linhas dos municípios e com as seguintes variáveis:
# ANO=2016, NIVEL=MUNICIPIO, CODMUNRES, IDHM_A=NA, IDHM_CA, IDHM_CA_M=NA, IDHM_CA_F=NA

# Criar banco final ATLAS_UF "juntando" os bancos linha_estado e ATLAS_MUNICIPIO


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - ATLAS - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Exportar o banco de dados com o nome ATLAS_UF.csv (Exemplo: ATLAS_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados ATLAS_UF 2016 e script - ATLAS - tarefas 1 a 3"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 5: BANCOS DE DADOS DO SINISA
####################################
# Você deve criar e estar na branch SINISA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler o bancos de dados abaixo listado com os respectivo nome
# dados_sinisa para agua e esgoto - município - 2016.csv
# Atenção que o arquivo tem códigos e nomes de municípios e muitos NAs. 
# Repare que os valores estão com o milhar indicado por ponto, o que não deve acontecer para o R não entender como decimal

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados
# Remover a pontuação de milhar e converter para formato numérico

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINISA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sinisa apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinisa_1

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Criar um banco de dados, de nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 3 - SINISA.pdf”

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Exportar o banco de dados com o nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv)
# Ao terminar a Tarefa 4 commit com o comentário "dados SINISA_UF 2016 e script - SINISA - tarefas 1 a 4"  e enviar para o repositório Projeto_BDEM_2016



################################
# ETAPA 6: CRIAÇÃO DE BDEM_UF
################################
# Você deve estar agora em main e antes de inserir qualquer comando desta ETAPA
# deverá fazer os merges de cada uma das 5 branches. A cada merge pode fazer o comentário "merge da branch TAL"
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Agregar os arquivos SIDRA_UF, ATLAS_UF, SINASC_UF, SIM_UF, SINISA_UF no banco BDEM_UF (Exemplo: BDEM_RJ)
# Leitura dos 5 bancos de dados expeortados das etapas anteriores

# Agregação dos bancos
# Lembre-se que SIDRA e ATLAS tem CODMUNRES com 7 dígitos e SINASC, SIM e SINISA com 6 dígitos
# Além disso dentro do merge all = TRUE garante a manutenção de qualquer município presente em um dos bancos envolvidos no merge


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - BDEM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Inserir os seguintes indicadores epidemiológicos (com apenas dias casas decimais) no BDEM_UF:
# TFG: Taxa de fecundidade geral
# TMG: Taxa de mortalidade geral
# RMM: Razão de mortalidade materna
# TMM: Taxa de mortalidade materna
# TMM_P: Taxa de mortalidade materna em até 42 dias
# TMN: Taxa de mortalidade neonatal
# TMN_P: Taxa de mortalidade neonatal precoce
# TMN_T: Taxa de mortalidade neonatal tardia
# TMI: Taxa de mortalidade infantil

# Conferir o banco BDEM_UF após inserção dos indicadores

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - BDEM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3: Exportar o banco de dados com o nome BDEM_UF.csv (Exemplo: BDEM_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados BDEM_UF 2016 e script - BDEM - tarefas 1 a 3"  e enviar para o repositório Projeto_BDEM_2016
 
