### Versioning

# run on terminal:
# git remote set-url origin https://github.com/HemingNM/spatialMetrics.git


## Verificar configurações do git e github
usethis::git_sitrep()


### Ignorar arquivos no git ---------------------------------
# usethis::use_git_ignore(c("proj_setup/"))

### branch - merge ---------------------------------
## criar um branch
usethis::pr_init(branch = "terra")

## criar nova função em novo script
# Ctrl + Shift + n
# função add1
# add1 <- funcion(x) x+1

# salve o arquivo na pasta R/ com o nome:
# 1-fun_add1.R

## modificar o readme
# escreva algo e salve o arquivo

# faça o commit das alterações

## Para conseguir sincronizar o branch com o GitHub, é preciso fazer o
# push para o GitHub usando pr_push()
# faça o push
usethis::pr_push()
# vai abrir a página do GitHub

## Agora temos 2 opções:
# 1. continuar trabalhando
# 2. finalizar o trabalho do branch

## 1. continuar trabalhando no branch local, sincronizando com o GitHub
# Para isso, pode fechar a janela do browser e
# continuar trabalhando normalmente no R:
# faça modificações, commit, push (pelo botão na aba git do RStudio)

## 2. finalizar o trabalho desse branch e juntar (merge) com o GitHub
# Quando quiser finalizar o trabalho e juntar os branches:
# faça o push para o GitHub usando a função pr_push()
usethis::pr_push()
# vai abrir a janela do browser
# revise as modificações nos arquivos pelo site
# clique no botâo "Create pull request"
# aceite o pull request (pr) pelo botão: Merge pull request
# após o aceite do pull request os branches estão fundidos

# apagar o branch criado
usethis::pr_finish()
# vai mudar para o branch master e apagar o branch atual