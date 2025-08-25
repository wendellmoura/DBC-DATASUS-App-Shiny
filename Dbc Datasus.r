# =====================================================
# App Shiny: Filtro interativo de arquivos DBC com tabela dinâmica e dicionário
# =====================================================

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(read.dbc)
library(dplyr)
library(data.table)
library(foreign)
library(DT)
library(rhandsontable)

# --- Ajusta limite de upload
options(shiny.maxRequestSize = 1024*1024*1024)  # 1 GB

# =====================================================
# CONFIGURAÇÕES DE DIMENSÕES (AJUSTE AQUI)
# =====================================================

# Dimensões da janela do aplicativo
app_width <- "100%"      # Largura total da janela do navegador
app_height <- "100vh"    # Altura total da janela do navegador

# Dimensões do cabeçalho
header_height <- "60px"  # Altura do cabeçalho

# Dimensões das sidebars
sidebar_width <- "23%"   # Largura de cada sidebar (23% cada = 46% total)
sidebar_height <- "calc(100vh - 60px)"  # Altura das sidebars

# Dimensões do conteúdo principal
main_width <- "54%"      # Largura do conteúdo central (54%)
main_height <- "calc(100vh - 60px)"  # Altura do conteúdo central

# Dimensões individuais dos cards (altura x largura)
upload_card_height <- "calc(34vh - 40px)"   # Altura do card de upload
upload_card_width <- "100%"                 # Largura do card de upload

filter_card_height <- "calc(63vh - 40px)"   # Altura do card de filtro
filter_card_width <- "100%"                 # Largura do card de filtro

export_card_height <- "calc(56vh - 40px)"   # Altura do card de exportação
export_card_width <- "100%"                 # Largura do card de exportação

info_card_height <- "calc(40vh - 40px)"     # Altura do card de informações
info_card_width <- "100%"                   # Largura do card de informações

# Dimensões da tabela dinâmica
pivot_table_height <- "500px"  # Altura da tabela dinâmica (aumentada para caber a paginação)

# CSS personalizado
custom_css <- HTML(paste0("
    /* Configurações da janela do aplicativo */
    body {
        font-family: 'Segoe UI', 'Helvetica Neue', sans-serif;
        color: #333;
        background-color: #f8f9fa;
        font-size: 13px;
        height: ", app_height, ";
        width: ", app_width, ";
        overflow: hidden;
        margin: 0;
        padding: 0;
    }
    
    .container-fluid {
        padding: 0;
        height: 100%;
    }
    
    .row {
        margin: 0;
        height: 100%;
    }
    
    /* Painéis e cards - Estilos base */
    .compact-card {
        background-color: #fff;
        border-radius: 6px;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
        padding: 12px;
        margin-bottom: 10px;
        border: none;
        overflow-y: auto;
    }
    
    /* Cards individuais com dimensões personalizadas */
    .card-upload {
        height: ", upload_card_height, ";
        width: ", upload_card_width, ";
    }
    
    .card-filter {
        height: ", filter_card_height, ";
        width: ", filter_card_width, ";
    }
    
    .card-export {
        height: ", export_card_height, ";
        width: ", export_card_width, ";
    }
    
    .card-info {
        height: ", info_card_height, ";
        width: ", info_card_width, ";
    }
    
    .compact-card .card-header {
        background: none;
        border-bottom: 1px solid #eaeaea;
        padding: 0 0 8px 0;
        margin-bottom: 10px;
    }
    
    .compact-card .card-title {
        font-size: 13px;
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
        display: flex;
        align-items: center;
    }
    
    .compact-card .card-title i {
        margin-right: 5px;
        color: #3498db;
        font-size: 12px;
    }
    
    /* Inputs e controles */
    .form-group {
        margin-bottom: 8px;
    }
    
    .form-control, .selectize-input {
        border-radius: 4px;
        border: 1px solid #dcdfe6;
        padding: 5px 8px;
        font-size: 12px;
        height: auto;
        min-height: 32px;
    }
    
    .form-control:focus, .selectize-input.focus {
        border-color: #3498db;
        box-shadow: 0 0 0 2px rgba(52, 152, 219, 0.2);
    }
    
    .btn {
        border-radius: 4px;
        font-weight: 500;
        padding: 5px 10px;
        font-size: 12px;
    }
    
    .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
    }
    
    .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
    }
    
    .btn-info {
        background-color: #2ecc71;
        border-color: #2ecc71;
    }
    
    .btn-info:hover {
        background-color: #27ae60;
        border-color: #27ae60;
    }
    
    /* Abas */
    .nav-tabs {
        border-bottom: 1px solid #eaeaea;
        margin-bottom: 12px;
    }
    
    .nav-tabs .nav-item .nav-link {
        border: none;
        color: #7f8c8d;
        font-weight: 500;
        padding: 8px 12px;
        font-size: 12px;
    }
    
    .nav-tabs .nav-item .nav-link.active {
        color: #3498db;
        background-color: transparent;
        border-bottom: 2px solid #3498db;
    }
    
    .nav-tabs .nav-item .nav-link:hover {
        border: none;
        border-bottom: 2px solid #eaeaea;
    }
    
    /* Informações */
    .info-box {
        background-color: #f8f9fa;
        border-left: 2px solid #3498db;
        padding: 6px 8px;
        border-radius: 3px;
        margin-bottom: 8px;
        font-size: 11px;
    }
    
    .info-box h5 {
        font-size: 11px;
        font-weight: 600;
        margin: 0 0 3px 0;
        color: #2c3e50;
    }
    
    .info-box p {
        margin: 0;
        color: #7f8c8d;
        line-height: 1.3;
    }
    
    /* Layout específico */
    .main-header {
        padding: 12px 15px;
        border-bottom: 1px solid #eaeaea;
        background-color: #fff;
        height: ", header_height, ";
    }
    
    .app-title {
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
        font-size: 18px;
        display: flex;
        align-items: center;
    }
    
    .app-title i {
        margin-right: 6px;
        color: #3498db;
    }
    
    .app-subtitle {
        color: #7f8c8d;
        font-size: 12px;
        margin: 2px 0 0 0;
    }
    
    .sidebar {
        padding: 10px;
        height: ", sidebar_height, ";
        overflow-y: auto;
        background-color: #f8f9fa;
        border-right: 1px solid #eaeaea;
        width: ", sidebar_width, ";
    }
    
    .sidebar-right {
        padding: 10px;
        height: ", sidebar_height, ";
        overflow-y: auto;
        background-color: #f8f9fa;
        border-left: 1px solid #eaeaea;
        width: ", sidebar_width, ";
    }
    
    .main-content {
        padding: 10px;
        height: ", main_height, ";
        overflow-y: auto;
        width: ", main_width, ";
    }
    
    /* Ajustes para elementos de formulário */
    .shiny-input-container {
        padding-bottom: 3px;
    }
    
    .radio-group {
        margin-bottom: 6px;
    }
    
    .radio-group .shiny-options-group {
        display: flex;
        gap: 8px;
    }
    
    .radio-group label {
        margin-bottom: 0;
        font-weight: normal;
        font-size: 12px;
    }
    
    /* Ajustes para área de texto */
    textarea.form-control {
        min-height: 50px;
        resize: vertical;
    }
    
    /* Ajustes para selectize */
    .selectize-dropdown {
        font-size: 12px;
    }
    
    /* Ajustes para tabelas */
    .table {
        font-size: 12px;
    }
    
    /* Loader */
    .sk-spinner {
        color: #3498db;
        transform: scale(0.7);
    }
    
    /* Scrollbar personalizada */
    ::-webkit-scrollbar {
        width: 6px;
    }
    
    ::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
    }
    
    ::-webkit-scrollbar-thumb {
        background: #c4c4c4;
        border-radius: 10px;
    }
    
    ::-webkit-scrollbar-thumb:hover {
        background: #a0a0a0;
    }
    
    /* Ajustes para o conteúdo principal */
    .main-tab-content {
        height: calc(100vh - 150px);
        overflow-y: auto;
    }
    
    .tab-content-panel {
        height: 100%;
    }
    
    /* Tabela dinâmica */
    .pivot-table-container {
        height: ", pivot_table_height, ";
    }
    
    /* Ajustes específicos para o layout da tabela dinâmica */
    .pivot-layout .form-group {
        margin-bottom: 8px;
    }
    
    /* Paginação da tabela dinâmica */
    .pivot-pagination {
        margin-top: 10px;
        display: flex;
        justify-content: center;
        align-items: center;
    }
    
    .pivot-pagination .btn {
        margin: 0 5px;
        padding: 3px 8px;
        font-size: 11px;
    }
    
    .pivot-pagination-info {
        font-size: 11px;
        color: #7f8c8d;
        margin: 0 10px;
    }
"))

ui <- fluidPage(
    theme = shinytheme("flatly"),
    tags$head(
        tags$style(custom_css)
    ),
    
    div(class = "main-header",
        h1(class = "app-title", icon("database"), "Filtro DBC DATASUS"),
        p(class = "app-subtitle", "Visualização, filtragem, e exportação de dados DBC")
    ),
    
    div(class = "container-fluid",
        div(class = "row",
            # Sidebar esquerda - Upload e Filtros
            div(class = "col-md-3 sidebar",
                div(class = "compact-card card-upload",
                    div(class = "card-header",
                        h5(class = "card-title", icon("upload"), "Upload de Arquivos")
                    ),
                    fileInput("arquivos", "Arquivos DBC:", 
                              multiple = TRUE, 
                              accept = ".dbc",
                              buttonLabel = "Procurar...",
                              placeholder = "Nenhum arquivo",
                              width = "100%"),
                    
                    fileInput("dicionario", "Dicionário (CSV):",
                              accept = c(".csv"),
                              buttonLabel = "Carregar dicionário...",
                              placeholder = "Nenhum dicionário carregado",
                              width = "100%")
                ),
                
                div(class = "compact-card card-filter",
                    div(class = "card-header",
                        h5(class = "card-title", icon("filter"), "Filtros")
                    ),
                    uiOutput("coluna_ui"),
                    
                    div(class = "radio-group",
                        radioButtons("metodo_filtro", "Método:",
                                     choices = c("Selecionar" = "selecao", 
                                                 "Digitar" = "texto"),
                                     selected = "selecao", inline = TRUE)
                    ),
                    
                    conditionalPanel(
                        condition = "input.metodo_filtro == 'selecao'",
                        uiOutput("valores_ui")
                    ),
                    conditionalPanel(
                        condition = "input.metodo_filtro == 'texto'",
                        textAreaInput("valores_texto", NULL,
                                      placeholder = "Ex: valor1, valor2, valor3",
                                      rows = 2,
                                      width = "100%")
                    ),
                    
                    div(class = "radio-group",
                        radioButtons("mostrar_desc", "Mostrar:",
                                     choices = c("Códigos" = "codigos", 
                                                 "Descrições" = "descricoes"),
                                     selected = "codigos", inline = TRUE)
                    )
                )
            ),
            
            # Conteúdo principal central
            div(class = "col-md-6 main-content",
                tabsetPanel(
                    type = "tabs",
                    tabPanel(icon("table"),
                             div(class = "tab-content-panel",
                                 div(class = "card-header",
                                     h5(class = "card-title", icon("eye"), "Prévia dos Dados")
                                 ),
                                 withSpinner(DT::dataTableOutput("preview"), type = 6)
                             )
                    ),
                    tabPanel(icon("chart-bar"),
                             div(class = "tab-content-panel",
                                 div(class = "card-header",
                                     h5(class = "card-title", icon("stats", lib = "glyphicon"), "Resumo Estatístico")
                                 ),
                                 div(style = "height: 100%; overflow-y: auto;",
                                     verbatimTextOutput("resumo")
                                 )
                             )
                    ),
                    tabPanel(icon("project-diagram"),
                             div(class = "tab-content-panel",
                                 div(class = "card-header",
                                     h5(class = "card-title", icon("list-alt"), "Estrutura dos Dados")
                                 ),
                                 div(style = "height: 100%; overflow-y: auto;",
                                     verbatimTextOutput("estrutura")
                                 )
                             )
                    ),
                    tabPanel(icon("th"),
                             div(class = "tab-content-panel pivot-layout",
                                 div(class = "card-header",
                                     h5(class = "card-title", icon("table"), "Tabela Dinâmica")
                                 ),
                                 p("Edite os dados diretamente na tabela (duplo clique em uma célula).", 
                                   style = "font-size: 11px; color: #7f8c8d; margin-bottom: 10px;"),
                                 
                                 fluidRow(
                                     column(6,
                                            selectInput("pivot_rows", "Linhas:",
                                                        choices = NULL, multiple = TRUE,
                                                        selectize = TRUE, width = "100%")
                                     ),
                                     column(6,
                                            selectInput("pivot_cols", "Colunas:",
                                                        choices = NULL, multiple = TRUE,
                                                        selectize = TRUE, width = "100%")
                                     )
                                 ),
                                 
                                 fluidRow(
                                     column(6,
                                            selectInput("pivot_vals", "Valores:",
                                                        choices = NULL, width = "100%")
                                     ),
                                     column(4,
                                            selectInput("pivot_fun", "Função:",
                                                        choices = c("Soma" = "sum", 
                                                                    "Média" = "mean",
                                                                    "Contagem" = "length",
                                                                    "Mínimo" = "min",
                                                                    "Máximo" = "max"),
                                                        width = "100%")
                                     ),
                                     column(2,
                                            div(style = "margin-top: 25px;",
                                                downloadButton("download_pivot", icon("download"), "Exportar",
                                                               class = "btn-info btn-block")
                                            )
                                     )
                                 ),
                                 
                                 div(class = "pivot-table-container",
                                     withSpinner(rHandsontableOutput("pivot_table"), type = 6)
                                 ),
                                 
                                 # Controles de paginação
                                 div(class = "pivot-pagination",
                                     actionButton("pivot_first", icon("angle-double-left"), class = "btn-default"),
                                     actionButton("pivot_prev", icon("angle-left"), class = "btn-default"),
                                     uiOutput("pivot_page_info_ui"),
                                     actionButton("pivot_next", icon("angle-right"), class = "btn-default"),
                                     actionButton("pivot_last", icon("angle-double-right"), class = "btn-default")
                                 )
                             )
                    )
                )
            ),
            
            # Sidebar direita - Exportação e Informações
            div(class = "col-md-3 sidebar-right",
                div(class = "compact-card card-export",
                    div(class = "card-header",
                        h5(class = "card-title", icon("download"), "Exportação")
                    ),
                    radioButtons("formato", "Formato:",
                                 choices = c("CSV" = "csv", "RDS" = "rds", "DBF" = "dbf"),
                                 selected = "csv"),
                    
                    conditionalPanel(
                        condition = "input.formato == 'csv'",
                        checkboxInput("header_csv", "Cabeçalho", TRUE),
                        radioButtons("separador", "Separador:",
                                     choices = c("," = ",", ";" = ";", "Tab" = "\t"),
                                     selected = ",", inline = TRUE)
                    ),
                    
                    div(style = "margin-top: 8px;",
                        downloadButton("download", "Exportar Dados", 
                                       class = "btn-primary btn-block")
                    )
                ),
                
                div(class = "compact-card card-info",
                    div(class = "card-header",
                        h5(class = "card-title", icon("info-circle"), "Informações")
                    ),
                    div(class = "info-box",
                        htmlOutput("info_arquivos")
                    ),
                    div(class = "info-box",
                        htmlOutput("info_filtro")
                    ),
                    div(class = "info-box",
                        htmlOutput("info_dicionario")
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Variáveis reativas para controle de paginação
    pivot_page <- reactiveVal(1)
    pivot_data_full <- reactiveVal(NULL)
    rows_per_page <- 20  # Número de linhas por página
    
    # --- Lê e junta arquivos DBC
    dados <- reactive({
        req(input$arquivos)
        
        # Mostrar progresso durante o carregamento
        withProgress(message = 'Lendo arquivos DBC...', value = 0, {
            n <- length(input$arquivos$datapath)
            lista <- list()
            
            for (i in 1:n) {
                incProgress(1/n, detail = paste("Arquivo", i, "de", n))
                lista[[i]] <- read.dbc(input$arquivos$datapath[i])
            }
            
            dados_combinados <- bind_rows(lista, .id = "arquivo_origem")
        })
        
        return(dados_combinados)
    })
    
    # --- Lê o dicionário de códigos
    dicionario <- reactive({
        req(input$dicionario)
        
        # Verificar se o arquivo foi carregado
        if (is.null(input$dicionario)) {
            return(NULL)
        }
        
        # Ler o arquivo CSV
        dic <- read.csv(input$dicionario$datapath, 
                       stringsAsFactors = FALSE,
                       colClasses = c("character", "character", "character"))
        
        # Verificar se as colunas necessárias existem
        if (!all(c("coluna", "codigo", "descricao") %in% names(dic))) {
            showNotification("O dicionário precisa ter colunas: coluna, codigo, descricao", type = "error")
            return(NULL)
        }
        
        return(dic)
    })
    
    # --- Aplica o dicionário aos dados
    dados_com_dicionario <- reactive({
        req(dados())
        
        df <- dados()
        dic <- dicionario()
        
        # Se não há dicionário, retornar dados originais
        if (is.null(dic)) {
            return(df)
        }
        
        # Para cada coluna no dicionário, fazer o merge
        colunas_para_merge <- unique(dic$coluna)
        
        for (col in colunas_para_merge) {
            if (col %in% names(df)) {
                dic_filtrado <- dic %>% 
                    filter(coluna == col) %>%
                    select(codigo, descricao)
                
                df <- df %>%
                    left_join(dic_filtrado, by = setNames("codigo", col)) %>%
                    rename(!!paste0(col, "_desc") := descricao)
            }
        }
        
        return(df)
    })
    
    # --- Dados finais (com ou sem dicionário, conforme seleção)
    dados_finais <- reactive({
        req(dados_com_dicionario())
        
        df <- dados_com_dicionario()
        
        # Se o usuário escolheu ver descrições, priorizar colunas de descrição
        if (input$mostrar_desc == "descricoes") {
            # Manter colunas de descrição e remover colunas de código correspondentes
            desc_cols <- grep("_desc$", names(df), value = TRUE)
            code_cols <- gsub("_desc$", "", desc_cols)
            
            # Manter apenas colunas que não são códigos com descrição disponível
            cols_to_keep <- setdiff(names(df), code_cols)
            df <- df %>% select(all_of(cols_to_keep))
            
            # Renomear colunas de descrição para remover o sufixo _desc
            new_names <- gsub("_desc$", "", names(df))
            names(df) <- new_names
        }
        
        return(df)
    })
    
    # --- Dados filtrados (após aplicação dos filtros)
    dados_filtrados <- reactive({
        req(dados_finais(), input$coluna_alvo, valores_filtro())
        
        dados_finais() %>% 
            filter(.data[[input$coluna_alvo]] %in% valores_filtro()) %>%
            select(-arquivo_origem)  # Remove a coluna de origem para o resultado final
    })
    
    # --- Informações sobre os arquivos
    output$info_arquivos <- renderUI({
        req(dados())
        HTML(paste0(
            "<h5>Arquivos Carregados</h5>",
            "<p><strong>Qtd:</strong> ", length(input$arquivos$name), "<br>",
            "<strong>Registros:</strong> ", format(nrow(dados()), big.mark = "."), "<br>",
            "<strong>Colunas:</strong> ", ncol(dados()), "</p>"
        ))
    })
    
    # --- Informações sobre o dicionário
    output$info_dicionario <- renderUI({
        dic <- dicionario()
        
        if (is.null(dic)) {
            return(HTML("<h5>Dicionário</h5><p>Nenhum dicionário carregado</p>"))
        }
        
        colunas <- unique(dic$coluna)
        n_codigos <- nrow(dic)
        
        HTML(paste0(
            "<h5>Dicionário</h5>",
            "<p><strong>Colunas:</strong> ", paste(colunas, collapse = ", "), "<br>",
            "<strong>Códigos:</strong> ", format(n_codigos, big.mark = "."), "</p>"
        ))
    })
    
    # --- Atualiza dropdown de colunas
    output$coluna_ui <- renderUI({
        req(dados_finais())
        selectInput("coluna_alvo", "Selecione a coluna para filtrar:",
                    choices = colnames(dados_finais())[-1],  # Exclui a coluna 'arquivo_origem'
                    width = "100%")
    })
    
    # --- Atualiza seleção de valores com base na coluna escolhida
    output$valores_ui <- renderUI({
        req(dados_finais(), input$coluna_alvo)
        vals <- unique(dados_finais()[[input$coluna_alvo]])
        selectizeInput("valores", "Selecione os valores a manter:",
                       choices = vals, multiple = TRUE, 
                       width = "100%",
                       options = list(plugins = list('remove_button'), 
                                      placeholder = 'Selecione os valores...'))
    })
    
    # --- Informações sobre o filtro
    output$info_filtro <- renderUI({
        req(dados_filtrados())
        HTML(paste0(
            "<h5>Dados Filtrados</h5>",
            "<p><strong>Registros:</strong> ", format(nrow(dados_filtrados()), big.mark = "."), "<br>",
            "<strong>Colunas:</strong> ", ncol(dados_filtrados()), "</p>"
        ))
    })
    
    # --- Processa os valores de filtro baseado no método escolhido
    valores_filtro <- reactive({
        req(input$coluna_alvo)
        
        if (input$metodo_filtro == "selecao") {
            req(input$valores)
            return(input$valores)
        } else {
            req(input$valores_texto)
            # Divide os valores por vírgula e remove espaços em branco
            valores <- unlist(strsplit(input$valores_texto, ","))
            return(trimws(valores))
        }
    })
    
    # --- Atualiza as opções para a tabela dinâmica
    observe({
        req(dados_filtrados())
        df <- dados_filtrados()
        cols <- names(df)
        
        updateSelectInput(session, "pivot_rows", choices = cols, selected = NULL)
        updateSelectInput(session, "pivot_cols", choices = cols, selected = NULL)
        
        # Para valores, selecionar apenas colunas numéricas
        num_cols <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "pivot_vals", choices = num_cols, selected = num_cols[1])
    })
    
    # --- Tabela dinâmica reativa (dados completos)
    pivot_data_full_calc <- reactive({
        req(dados_filtrados(), input$pivot_vals, input$pivot_fun)
        
        df <- dados_filtrados()
        
        # Se não houver colunas selecionadas, retornar dados originais
        if (is.null(input$pivot_rows) && is.null(input$pivot_cols)) {
            return(df)
        }
        
        # Criar fórmula para agregação
        formula_str <- paste(input$pivot_vals, "~")
        
        if (!is.null(input$pivot_rows)) {
            formula_str <- paste(formula_str, paste(input$pivot_rows, collapse = "+"))
        }
        
        if (!is.null(input$pivot_cols)) {
            if (!is.null(input$pivot_rows)) {
                formula_str <- paste(formula_str, "+")
            }
            formula_str <- paste(formula_str, paste(input$pivot_cols, collapse = "+"))
        }
        
        formula_obj <- as.formula(formula_str)
        
        # Realizar a agregação
        aggregated_data <- aggregate(formula_obj, data = df, 
                                     FUN = get(input$pivot_fun, mode = "function"),
                                     na.action = na.pass)
        
        return(aggregated_data)
    })
    
    # Observador para atualizar os dados completos e resetar a página
    observe({
        data <- pivot_data_full_calc()
        pivot_data_full(data)
        pivot_page(1)  # Reset para a primeira página
    })
    
    # --- Tabela dinâmica paginada
    pivot_data_paginated <- reactive({
        req(pivot_data_full())
        
        data <- pivot_data_full()
        total_pages <- ceiling(nrow(data) / rows_per_page)
        current_page <- min(pivot_page(), total_pages)
        
        if (nrow(data) == 0) {
            return(data)
        }
        
        start_row <- (current_page - 1) * rows_per_page + 1
        end_row <- min(current_page * rows_per_page, nrow(data))
        
        return(data[start_row:end_row, , drop = FALSE])
    })
    
    # --- Atualiza informações de paginação
    output$pivot_page_info_ui <- renderUI({
        req(pivot_data_full())
        data <- pivot_data_full()
        total_pages <- ceiling(nrow(data) / rows_per_page)
        current_page <- pivot_page()
        
        span(paste("Página", current_page, "de", total_pages), 
             class = "pivot-pagination-info", 
             id = "pivot_page_info")
    })
    
    # --- Controles de paginação
    observeEvent(input$pivot_first, {
        pivot_page(1)
    })
    
    observeEvent(input$pivot_prev, {
        current_page <- pivot_page()
        if (current_page > 1) {
            pivot_page(current_page - 1)
        }
    })
    
    observeEvent(input$pivot_next, {
        req(pivot_data_full())
        current_page <- pivot_page()
        total_pages <- ceiling(nrow(pivot_data_full()) / rows_per_page)
        
        if (current_page < total_pages) {
            pivot_page(current_page + 1)
        }
    })
    
    observeEvent(input$pivot_last, {
        req(pivot_data_full())
        total_pages <- ceiling(nrow(pivot_data_full()) / rows_per_page)
        pivot_page(total_pages)
    })
    
    # --- Tabela dinâmica interativa (apenas página atual)
    output$pivot_table <- renderRHandsontable({
        req(pivot_data_paginated())
        
        # Criar a tabela interativa
        rhandsontable(pivot_data_paginated(), 
                      readOnly = FALSE, 
                      height = pivot_table_height,
                      rowHeaders = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    # --- Download da tabela dinâmica (todos os dados)
    output$download_pivot <- downloadHandler(
        filename = function() {
            paste0("tabela_dinamica_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            write.csv(pivot_data_full(), file, row.names = FALSE)
        }
    )
    
    # --- Preview dos dados com DT
    output$preview <- DT::renderDataTable({
        req(dados_filtrados())
        DT::datatable(
            head(dados_filtrados(), 100),  # Mostra apenas as 100 primeiras linhas
            options = list(
                scrollX = TRUE,
                pageLength = 10,  # Apenas 10 linhas por página
                searching = FALSE,
                lengthChange = FALSE,
                info = FALSE
            ),
            rownames = FALSE
        )
    })
    
    # --- Resumo estatístico
    output$resumo <- renderPrint({
        req(dados_filtrados())
        summary(dados_filtrados())
    })
    
    # --- Estrutura dos dados
    output$estrutura <- renderPrint({
        req(dados_filtrados())
        str(dados_filtrados())
    })
    
    # --- Download
    output$download <- downloadHandler(
        filename = function() {
            paste0("dados_filtrados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$formato)
        },
        content = function(file) {
            dados <- dados_filtrados()
            
            switch(input$formato,
                   "csv" = {
                       write.table(dados, file, 
                                   sep = input$separador, 
                                   dec = ".",
                                   row.names = FALSE, 
                                   col.names = input$header_csv)
                   },
                   "rds" = {
                       saveRDS(dados, file)
                   },
                   "dbf" = {
                       # Converte para data.frame tradicional para evitar problemas com o foreign
                       dados_df <- as.data.frame(dados)
                       write.dbf(dados_df, file)
                   })
        }
    )
}

shinyApp(ui, server)