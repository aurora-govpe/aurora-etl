# Gerenciando Variáveis de Ambiente no R com `usethis`

O arquivo `.Renviron` é a forma padrão de armazenar credenciais, chaves de API e segredos no R sem deixá-los expostos no código. O pacote `usethis` facilita a localização e a edição desse arquivo.

---

### 1. Abrir o arquivo `.Renviron`

No console do R, utilize a função `edit_r_environ()`. Escolha o escopo de acordo com sua necessidade:

```r
# Instalar usethis caso ainda não tenha
install.packages("usethis")

# Opção A: Global (Usuário) - Recomendado
# Disponível para qualquer script ou projeto R na sua máquina
usethis::edit_r_environ(scope = "user")

# Opção B: Projeto (Local)
# Cria/edita um .Renviron exclusivo na pasta raiz do projeto ativo
usethis::edit_r_environ(scope = "project")
```

Um arquivo de texto em branco (ou com configurações existentes) será aberto no editor do RStudio.

---

### 2. Adicionar as variáveis

Cole suas variáveis seguindo a estrutura `CHAVE=valor`:

```text
SDS_ACCESS_KEY=sua_access_key_aqui
SDS_SECRET_KEY=sua_secret_key_aqui

IAEMPE_ACCESS_KEY=outra_access_key_aqui
IAEMPE_SECRET_KEY=outra_secret_key_aqui
```

**Regras importantes:**
* **Sem espaços** em volta do sinal de igual (`=`): use `VAR=valor`, e nunca `VAR = valor`.
* **Sem aspas**: não envolva os valores em aspas simples ou duplas.
* **Linha em branco final**: pressione `Enter` na última linha para deixar uma linha vazia no final do arquivo (requisito para leitura correta pelo R).
* Salve o arquivo com `Ctrl + S`.

---

### 3. Reiniciar a sessão do R

O arquivo `.Renviron` só é carregado no início da sessão. Para aplicar as mudanças:

* No RStudio: pressione **Ctrl + Shift + F10** (ou vá em **Session > Restart R**).
* Via comando: execute `.rs.restartR()`.

---

### 4. Testar o carregamento

Confirme se o R está lendo os valores na memória:

```r
# Deve imprimir sua chave no console
Sys.getenv("SDS_ACCESS_KEY")

# Deve retornar TRUE se a variável existir e não estiver vazia
Sys.getenv("SDS_SECRET_KEY") != ""
```

---

### 5. Segurança ao usar no escopo do Projeto

Se você optou por criar o `.Renviron` local do projeto (`scope = "project"`), **nunca envie esse arquivo para o Git**. Adicione-o imediatamente ao `.gitignore`:

```r
usethis::use_git_ignore(".Renviron")
```