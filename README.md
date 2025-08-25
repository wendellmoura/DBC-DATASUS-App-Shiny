# Filtro Interativo de Arquivos DBC DATASUS - App Shiny

Aplicação Shiny para visualização, filtragem dinâmica e exportação de arquivos DBC (DATASUS), com suporte a dicionário de códigos e tabela dinâmica interativa.

<img width="1256" height="583" alt="image" src="https://github.com/user-attachments/assets/2a423336-eda8-4281-9189-c318bd651577" />

<img width="1318" height="658" alt="image" src="https://github.com/user-attachments/assets/d7458bb4-481f-4dd3-8245-858cc53e3955" />


## Funcionalidades

- **Upload de múltiplos arquivos DBC**
- **Carregamento de dicionário de códigos (CSV)**
- **Filtragem por coluna, seleção ou digitação de valores**
- **Visualização dos dados filtrados**
- **Resumo estatístico e estrutura dos dados**
- **Tabela dinâmica interativa (pivot) com edição e exportação**
- **Exportação dos dados filtrados em CSV, RDS ou DBF**

---

## Instalação

### 1. Pré-requisitos

- **R** (versão >= 4.0)
- **RStudio** (opcional, mas recomendado)
- **Pacotes R necessários:**  
    - `shiny`
    - `shinythemes`
    - `shinycssloaders`
    - `read.dbc`
    - `dplyr`
    - `data.table`
    - `foreign`
    - `DT`
    - `rhandsontable`

### 2. Instale os pacotes obrigatórios

Execute os comandos abaixo no seu console R:

```R
install.packages(c(
  "shiny", "shinythemes", "shinycssloaders", "dplyr", 
  "data.table", "foreign", "DT", "rhandsontable"
))

# Pacote read.dbc pode ser instalado via GitHub:
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("danicat/read.dbc")
```

---

## Como Rodar a Aplicação

1. **Salve o código da aplicação em um arquivo chamado `app.R`**  
   (Copie todo o script apresentado acima.)

2. **Abra o arquivo no RStudio ou no R Console.**

3. **Execute:**

```R
shiny::runApp("app.R")
```
Ou, se preferir, apenas clique em "Run App" no RStudio.

4. **Acesse o app pelo navegador**, o endereço padrão será `http://127.0.0.1:xxxx` (porta aleatória local).

---

## Como Usar

### 1. Upload dos Arquivos

- Clique em **"Upload de Arquivos"** e selecione um ou mais arquivos `.dbc` do DATASUS.
- Opcionalmente, faça o upload de um **dicionário de códigos** em formato CSV, contendo as colunas:  
    - `coluna` (nome da coluna do dado)
    - `codigo` (código)
    - `descricao` (descrição do código)

### 2. Filtragem de Dados

- Selecione a coluna a ser filtrada.
- Escolha entre **selecionar valores** ou **digitar manualmente**.
- Opte por visualizar **códigos** ou **descrições** (caso tenha carregado dicionário).

### 3. Visualização

- Navegue entre as abas:
    - **Prévia dos Dados:** visualiza as primeiras linhas filtradas.
    - **Resumo Estatístico:** estatísticas básicas do conjunto filtrado.
    - **Estrutura dos Dados:** tipos das colunas e estrutura.
    - **Tabela Dinâmica:** crie pivôs interativos, edite células e exporte.

### 4. Exportação

- Escolha o formato desejado: `CSV`, `RDS` ou `DBF`.
- Para CSV, defina se inclui cabeçalho e escolha o separador.
- Clique em **"Exportar Dados"** para baixar o arquivo filtrado.

---

## Exemplo de Dicionário CSV

```csv
coluna,codigo,descricao
UF,35,São Paulo
UF,33,Rio de Janeiro
SEXO,1,Masculino
SEXO,2,Feminino
```

---

## Observações

- O app suporta arquivos grandes (até 1GB por upload).
- O dicionário é opcional, mas enriquece a visualização dos dados.
- A tabela dinâmica permite paginação e edição direta dos valores.
- Para exportação DBF, o pacote `foreign` é utilizado (limitado a data.frames padrão).

---

## Licença

MIT

---

## Autor

Desenvolvido por [Wendell Moura]  
Colabore ou reporte issues via [GitHub Issues](https://github.com/wendellmoura/DBC-DATASUS-App-Shiny/issues).
