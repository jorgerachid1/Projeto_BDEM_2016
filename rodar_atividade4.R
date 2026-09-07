# Atividade 4 - roda tudo (copia arquivos, descompacta, commits e branch SIM)
# UF de trabalho: 27 (Alagoas)

library(gert)

repo <- "C:/Users/Jorge/OneDrive/Documentos/Projeto_BDEM_2016"
downloads <- file.path(Sys.getenv("USERPROFILE"), "Downloads")

commit_se_houver <- function(msg) {
  if (nrow(git_status(repo = repo, staged = TRUE)) > 0) git_commit(msg, repo = repo)
}

arquivos <- c(
  "script_roteiro_BDEM.R", "SINASC_2016.zip",
  "Tabela_PIG_Brasil.csv",
  "Variáveis - Projeto - Tarefa 3 - SINISA.pdf",
  "Variáveis - Projeto - Tarefa 4 - SIDRA.pdf",
  "Variáveis - Projeto - Tarefa 9 - SINASC.pdf",
  "Dicionário - SIM - Open DATASUS.pdf",
  "Dicionário - SINASC - Open DATASUS.pdf"
)

for (arq in arquivos) {
  origem <- file.path(downloads, arq)
  if (file.exists(origem)) file.copy(origem, file.path(repo, arq), overwrite = TRUE)
}

unzip(file.path(downloads, "SIM_2016.zip"), exdir = repo, overwrite = TRUE)

sinasc_zip <- file.path(downloads, "SINASC_2016.zip")
if (file.exists(sinasc_zip)) unzip(sinasc_zip, exdir = repo, overwrite = TRUE)

# csv grande (>90MB) precisa de git lfs, senão o GitHub recusa o push
csvs <- list.files(repo, pattern = "\\.csv$", full.names = TRUE)
if (any(file.size(csvs) > 90 * 1024 * 1024)) {
  system(sprintf('git -C "%s" lfs install', repo))
  system(sprintf('git -C "%s" lfs track "*.csv"', repo))
  git_add(".gitattributes", repo = repo)
  commit_se_houver("configurar git lfs para arquivos csv grandes")
}

# script_BDEM.R começa como cópia do roteiro (sem código ainda)
file.copy(file.path(repo, "script_roteiro_BDEM.R"), file.path(repo, "script_BDEM.R"), overwrite = TRUE)

git_branch_checkout("main", repo = repo)
git_add(".", repo = repo)
commit_se_houver("dados, arquivos de texto e script roteiro BDEM")

git_add("script_BDEM.R", repo = repo)
commit_se_houver("script BDEM")

if ("SIM" %in% git_branch_list(repo = repo)$name) {
  git_branch_checkout("SIM", repo = repo)
} else {
  git_branch_create("SIM", repo = repo, checkout = TRUE)
}

# monta o script_BDEM.R com o código até a tarefa N e commita
roteiro <- readLines(file.path(repo, "script_roteiro_BDEM.R"), encoding = "UTF-8")
Encoding(roteiro) <- "UTF-8"
pronto <- readLines(file.path(downloads, "script_BDEM.R"), encoding = "UTF-8")
Encoding(pronto) <- "UTF-8"

marco <- function(linhas, tarefa) grep(sprintf("Ao terminar a Tarefa %d commit", tarefa), linhas)[1]

commit_tarefa <- function(tarefa, msg) {
  conteudo <- c(pronto[1:marco(pronto, tarefa)],
                roteiro[(marco(roteiro, tarefa) + 1):length(roteiro)])
  writeLines(conteudo, file.path(repo, "script_BDEM.R"), useBytes = TRUE)
  git_add("script_BDEM.R", repo = repo)
  commit_se_houver(msg)
}

# Tarefa 1
dados_sim <- read.csv(file.path(repo, "SIM_2016.csv"), sep = ",", encoding = "UTF-8")
dim(dados_sim)
commit_tarefa(1, "script BDEM - SIM - tarefa 1")

# Tarefa 2
dados_sim_1 <- dados_sim[, c(1, 3, 9, 10, 11, 14, 17, 35, 47)]
names(dados_sim_1) <- c("CONTADOR", "TIPOBITO", "IDADE", "SEXO", "RACACOR",
                         "ESC2010", "CODMUNRES", "TPMORTEOCO", "CAUSABAS")
commit_tarefa(2, "script BDEM - SIM - tarefas 1 a 2")

# Tarefa 3
dados_sim_1$UF <- substr(as.character(dados_sim_1$CODMUNRES), 1, 2)
dados_sim_2 <- dados_sim_1[dados_sim_1$UF == "27", ]
dados_sim_2$UF <- NULL
nrow(dados_sim_2) # 20769
commit_tarefa(3, "script BDEM - SIM - tarefas 1 a 3")

# Tarefa 4
table(dados_sim_2$TIPOBITO, useNA = "always")
table(dados_sim_2$SEXO, useNA = "always")
table(dados_sim_2$RACACOR, useNA = "always")
table(dados_sim_2$ESC2010, useNA = "always")
table(dados_sim_2$TPMORTEOCO, useNA = "always")
table(dados_sim_2$CAUSABAS, useNA = "always")
summary(dados_sim_2$IDADE)
commit_tarefa(4, "script BDEM - SIM - tarefas 1 a 4")

# Tarefa 5
dados_sim_2$SEXO[dados_sim_2$SEXO %in% c(0, 9)] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA
dados_sim_2$CAUSABAS[dados_sim_2$CAUSABAS == "" | dados_sim_2$CAUSABAS == "9"] <- NA
idade_chr <- formatC(dados_sim_2$IDADE, width = 3, flag = "0")
dados_sim_2$IDADE[substr(idade_chr, 2, 3) == "99"] <- NA
commit_tarefa(5, "script BDEM - SIM - tarefas 1 a 5")

# Tarefa 6
dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1, 2),
                                labels = c("Fetal", "Não fetal"))
dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1, 2),
                            labels = c("Masculino", "Feminino"))
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                               labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sim_2$ESC2010 <- factor(dados_sim_2$ESC2010, levels = c(0, 1, 2, 3, 4, 5),
                               labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                                          "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)",
                                          "Superior incompleto", "Superior completo"))
dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8),
                                  labels = c("Na gravidez", "No parto", "No abortamento",
                                             "Até 42 dias após o término do parto",
                                             "De 43 dias a 1 ano após o término da gestação",
                                             "Não ocorreu nestes períodos"))
str(dados_sim_2)
commit_tarefa(6, "script BDEM - SIM - tarefas 1 a 6")

git_log(repo = repo, max = 10)

# falta só o push manual no Terminal:
# cd "C:/Users/Jorge/OneDrive/Documentos/Projeto_BDEM_2016"
# git push -u origin main
# git push -u origin SIM
