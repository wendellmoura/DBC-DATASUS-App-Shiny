# =====================================================
# TABWEND
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
library(shinyjs)

# --- Ajusta limite de upload e memória
options(shiny.maxRequestSize = 1024*1024*1024)  # 1 GB
options(expressions = 10000)  # Aumenta o limite de expressões

# =====================================================
# CONFIGURAÇÕES DE DIMENSÕES
# =====================================================

# CSS personalizado com melhorias para visualização de tabelas
custom_css <- HTML("
    /* Configurações da janela do aplicativo */
    body {
        font-family: 'Segoe UI', 'Helvetica Neue', sans-serif;
        color: #333;
        background-color: #f8f9fa;
        font-size: 14px;
        width: 100%;
        overflow-x: hidden;
    }
    
    .container-fluid {
        padding: 0;
        height: 100vh;
    }
    
    .row {
        margin: 0;
        height: 100%;
    }
    
    /* Header compacto */
    .main-header {
        padding: 8px 15px;
        border-bottom: 1px solid #eaeaea;
        background-color: #fff;
        min-height: 50px;
    }
    
    .app-title {
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
        font-size: 16px;
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
    
    /* Sidebar compacta */
    .sidebar {
        padding: 10px;
        overflow-y: auto;
        background-color: #f8f9fa;
        border-right: 1px solid #eaeaea;
        height: 100%;
    }
    
    /* Conteúdo principal com foco nas tabelas */
    .main-content {
        padding: 10px;
        overflow-y: auto;
        height: 100%;
    }
    
    /* Painéis compactos */
    .compact-card {
        background-color: #fff;
        border-radius: 4px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
        padding: 10px;
        margin-bottom: 10px;
        border: none;
    }
    
    .compact-card .card-header {
        background: none;
        border-bottom: 1px solid #eaeaea;
        padding: 0 0 5px 0;
        margin-bottom: 8px;
    }
    
    .compact-card .card-title {
        font-size: 13px;
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
        display: flex;
        align-items: center;
    }
    
    /* Botões de controle de painel */
    .panel-toggle {
        background: none;
        border: none;
        color: #7f8c8d;
        cursor: pointer;
        padding: 0 5px;
        font-size: 12px;
    }
    
    .panel-toggle:hover {
        color: #3498db;
    }
    
    /* Inputs compactos */
    .form-group {
        margin-bottom: 8px;
    }
    
    .form-control, .selectize-input {
        border-radius: 3px;
        border: 1px solid #dcdfe6;
        padding: 5px 8px;
        font-size: 13px;
        height: auto;
        min-height: 32px;
    }
    
    .btn {
        border-radius: 3px;
        font-weight: 500;
        padding: 5px 10px;
        font-size: 13px;
    }
    
    /* Tabelas com máximo de espaço */
    .dataTables_wrapper {
        position: relative;
        height: 100%;
    }
    
    .dataTables_scroll {
        height: calc(100% - 60px);
    }
    
    .tab-content {
        height: calc(100% - 40px);
        overflow: hidden;
    }
    
    .tab-pane {
        height: 100%;
    }
    
    /* Container de tabela com altura máxima */
    .table-container {
        height: calc(100% - 40px);
        overflow: auto;
    }
    
    /* Controles de exportação compactos */
    .export-controls {
        display: flex;
        gap: 8px;
        align-items: center;
        margin-bottom: 20px;
        flex-wrap: wrap;
    }
    
    .export-controls .form-group {
        margin-bottom: 0;
        display: flex;
        align-items: center;
        gap: 5px;
    }
    
    .export-controls label {
        margin-bottom: 0;
        white-space: nowrap;
    }
    
    /* Footer compacto */
    .footer {
        background-color: #fff;
        border-top: 1px solid #eaeaea;
        padding: 8px 10px;
        display: flex;
        justify-content: space-between;
        font-size: 11px;
    }
    
    .footer-box {
        flex: 1;
        margin: 0 5px;
        background-color: #f8f9fa;
        border-left: 2px solid #3498db;
        padding: 5px 8px;
        border-radius: 2px;
    }
    
    .footer-box h5 {
        font-size: 11px;
        font-weight: 600;
        margin: 0 0 2px 0;
        color: #2c3e50;
    }
    
    .footer-box p {
        margin: 0;
        color: #7f8c8d;
        line-height: 1.3;
    }
    
    /* Ajustes para responsividade */
    @media (max-width: 768px) {
        .sidebar {
            height: auto;
            border-right: none;
            border-bottom: 1px solid #eaeaea;
        }
        
        .main-content {
            height: calc(100% - 200px);
        }
        
        .footer {
            flex-direction: column;
        }
        
        .footer-box {
            margin-bottom: 5px;
        }
    }
    
    /* Estilos para os controles de exportação */
    .export-options {
        background-color: #f8f9fa;
        border-radius: 4px;
        padding: 5px;
        margin-bottom: 15px;
        border: 1px solid #eaeaea;
    }
    
    .export-buttons {
        display: flex;
        gap: 10px;
    }
    
    .export-row {
        display: flex;
        align-items: center;
        gap: 15px;
        flex-wrap: wrap;
    }
    
    .export-option {
        display: flex;
        align-items: center;
        gap: 5px;
    }
    
    .export-option label {
        margin-bottom: 0;
        white-space: nowrap;
    }
")

# Definir a UI
ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("flatly"),
    tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$style(custom_css),
        tags$script('
            $(document).on("shiny:connected", function() {
                Shiny.setInputValue("window_width", window.innerWidth);
                Shiny.setInputValue("window_height", window.innerHeight);
            });
            
            $(window).on("resize", function() {
                Shiny.setInputValue("window_width", window.innerWidth);
                Shiny.setInputValue("window_height", window.innerHeight);
            });
            
            // Função para alternar visibilidade de painéis
            function togglePanel(panelId) {
                var panel = $("#" + panelId);
                var button = $("#toggle_" + panelId);
                if (panel.is(":visible")) {
                    panel.hide();
                    button.html("<i class=\\"fa fa-chevron-down\\"></i>");
                } else {
                    panel.show();
                    button.html("<i class=\\"fa fa-chevron-up\\"></i>");
                }
            }
        ')
    ),
    
    div(class = "main-header",
        div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            div(
                h1(class = "app-title", icon("table"), "TabWend"),
                p(class = "app-subtitle", "Visualizador dinâmico de dados DBC/Tabwin (V1.0 Beta)")
            ),
            actionButton("btn_ajuda", "Ajuda", 
                       class = "btn-help", 
                       icon = icon("question-circle"))
        )
    ),
    
    div(class = "container-fluid",
        div(class = "row",
            # Sidebar esquerda - Upload e Filtros (recolhível)
            div(class = "col-md-3 col-lg-2 sidebar",
                div(class = "compact-card",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        h5(class = "card-title", "Upload de Arquivos"),
                        actionButton("toggle_upload", NULL, icon = icon("chevron-up"), 
                                   class = "panel-toggle", onclick = "togglePanel('upload-panel')")
                    ),
                    div(id = "upload-panel",
                        fileInput("arquivos", "Arquivos DBC:", 
                                  multiple = TRUE, 
                                  accept = ".dbc",
                                  buttonLabel = "Procurar",
                                  placeholder = "Nenhum arquivo",
                                  width = "100%"),
                        
                        fileInput("dicionario", "Dicionário (CSV):",
                                  accept = c(".csv"),
                                  buttonLabel = "Procurar",
                                  placeholder = "Nenhum dicionário carregado",
                                  width = "100%")
                    )
                ),
                
                div(class = "compact-card",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        h5(class = "card-title", "Filtros"),
                        actionButton("toggle_filters", NULL, icon = icon("chevron-up"), 
                                   class = "panel-toggle", onclick = "togglePanel('filters-panel')")
                    ),
                    div(id = "filters-panel",
                        uiOutput("coluna_ui"),
                        
                        div(class = "radio-group",
                            radioButtons("metodo_filtro", "Método:",
                                         choices = c("Selecionar" = "selecao", 
                                                     "Pesquisar" = "texto"),
                                         selected = "selecao", inline = TRUE)
                        ),
                        
                        conditionalPanel(
                            condition = "input.metodo_filtro == 'selecao'",
                            uiOutput("valores_ui")
                        ),
                        conditionalPanel(
                            condition = "input.metodo_filtro == 'texto'",
                            textAreaInput("valores_texto", NULL,
                                          placeholder = "Insira valores separados por vírgula\nEx: valor1, valor2, valor3",
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
                )
            ),
            
            # Conteúdo principal central com foco nas tabelas
            div(class = "col-md-9 col-lg-10 main-content",
                tabsetPanel(
                    id = "mainTabs",
                    type = "tabs",
                    tabPanel("Prévia dos Dados",
                             div(class = "export-options",
                                 div(class = "export-row",
                                     div(class = "export-option",
                                         selectInput("export_format", NULL,
                                                     choices = c("CSV" = "csv", "RDS" = "rds", "DBF" = "dbf"),
                                                     selected = "csv", width = "100px")
                                     ),
                                     div(class = "export-buttons",
                                         downloadButton("download_com_desc", "Exportar com descrição", 
                                                        class = "btn-primary"),
                                         downloadButton("download_sem_desc", "Exportar sem descrição", 
                                                        class = "btn-default")
                                     )
                                 )
                             ),
                             
                             div(class = "table-container",
                                 withSpinner(DT::dataTableOutput("preview"), type = 6)
                             )
                    ),
                    tabPanel("Tabela Dinâmica",
                             fluidRow(
                                 column(3, 
                                        selectInput("pivot_rows", "Linhas:",
                                                    choices = NULL, multiple = TRUE,
                                                    selectize = TRUE, width = "100%")
                                 ),
                                 column(3, 
                                        selectInput("pivot_cols", "Colunas:",
                                                    choices = NULL, multiple = TRUE,
                                                    selectize = TRUE, width = "100%")
                                 ),
                                 column(2, 
                                        selectInput("pivot_vals", "Valores:",
                                                    choices = NULL, width = "100%")
                                 ),
                                 column(2, 
                                        selectInput("pivot_fun", "Função:",
                                                    choices = c("Soma" = "sum", 
                                                                "Média" = "mean",
                                                                "Contagem" = "length",
                                                                "Mínimo" = "min",
                                                                "Máximo" = "max"),
                                                    width = "100%")
                                 ),
                                 column(2, 
                                        div(style = "padding-top: 25px;",
                                            downloadButton("download_pivot", "Exportar",
                                                           class = "btn-info btn-block")
                                        )
                                 )
                             ),
                             
                             div(class = "table-container",
                                 withSpinner(DT::dataTableOutput("pivot_table_dt"), type = 6)
                             )
                    ),
                    tabPanel("Resumo Estatístico",
                             div(class = "table-container",
                                 verbatimTextOutput("resumo")
                             )
                    ),
                    tabPanel("Estrutura dos Dados",
                             div(class = "table-container",
                                 verbatimTextOutput("estrutura")
                             )
                    )
                )
            )
        )
    ),
    
    # Footer com informações
    div(class = "footer",
        div(class = "footer-box",
            htmlOutput("info_arquivos")
        ),
        div(class = "footer-box",
            htmlOutput("info_filtro")
        ),
        div(class = "footer-box",
            htmlOutput("info_dicionario")
        )
    )
)

# O servidor
server <- function(input, output, session) {
    
    # --- Lê e junta arquivos DBC com tratamento de erro
    dados <- reactive({
        req(input$arquivos)
        
        # Mostrar progresso durante o carregamento
        withProgress(message = 'Lendo arquivos DBC...', value = 0, {
            n <- length(input$arquivos$datapath)
            lista <- list()
            
            for (i in 1:n) {
                incProgress(1/n, detail = paste("Arquivo", i, "de", n))
                tryCatch({
                    lista[[i]] <- read.dbc(input$arquivos$datapath[i])
                }, error = function(e) {
                    showNotification(paste("Erro ao ler arquivo", input$arquivos$name[i], ":", e$message), 
                                    type = "error")
                    return(NULL)
                })
            }
            
            # Remove elementos NULL da lista
            lista <- lista[!sapply(lista, is.null)]
            
            if (length(lista) == 0) {
                showNotification("Nenhum arquivo foi carregado com sucesso.", type = "error")
                return(NULL)
            }
            
            # Verificar se há muitos dados e mostrar aviso
            total_rows <- sum(sapply(lista, nrow))
            if (total_rows > 100000) {
                showNotification(paste("Dataset grande carregado (", format(total_rows, big.mark = "."), 
                                      " registros). A visualização pode ser limitada."), 
                                type = "warning", duration = 10)
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
        tryCatch({
            dic <- read.csv(input$dicionario$datapath, 
                           stringsAsFactors = FALSE,
                           colClasses = c("character", "character", "character"))
            
            # Verificar se as colunas necessárias existen
            if (!all(c("coluna", "codigo", "descricao") %in% names(dic))) {
                showNotification("O dicionário precisa ter colunas: coluna, codigo, descricao", type = "error")
                return(NULL)
            }
            
            return(dic)
        }, error = function(e) {
            showNotification(paste("Erro ao ler dicionário:", e$message), type = "error")
            return(NULL)
        })
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
            # Manter colunas de descrição и remover colunas de código correspondentes
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
    
    # --- Dados completos com dicionário (para exportação)
    dados_completos_com_dicionario <- reactive({
        req(dados_com_dicionario(), input$coluna_alvo, valores_filtro())
        
        dados_com_dicionario() %>% 
            filter(.data[[input$coluna_alvo]] %in% valores_filtro()) %>%
            select(-arquivo_origem)
    })
    
    # --- Dados sem descrições (apenas códigos) para exportação
    dados_apenas_codigos <- reactive({
        req(dados_completos_com_dicionario())
        
        df <- dados_completos_com_dicionario()
        
        # Remover colunas de descrição (que terminam com _desc)
        desc_cols <- grep("_desc$", names(df), value = TRUE)
        
        if (length(desc_cols) > 0) {
            df <- df %>% select(-all_of(desc_cols))
        }
        
        return(df)
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
    
    # --- Dados para a tabela dinâmica (agregados)
    pivot_data <- reactive({
        req(dados_filtrados(), input$pivot_vals, input$pivot_fun)
        
        df <- dados_filtrados()
        
        # Se não houver colunas selecionadas, retornar dados originais
        if (is.null(input$pivot_rows) && is.null(input$pivot_cols)) {
            return(df)
        }
        
        # Usar dplyr para agregação (mais eficiente que aggregate)
        group_vars <- c(input$pivot_rows, input$pivot_cols)
        
        # Função de agregação
        agg_fun <- get(input$pivot_fun, mode = "function")
        
        # Agrupar e resumir
        pivot_data <- df %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(across(all_of(input$pivot_vals), agg_fun, na.rm = TRUE), .groups = 'drop')
        
        return(pivot_data)
    })
    
    # --- Renderização da tabela dinâmica com DT
    output$pivot_table_dt <- DT::renderDataTable({
        req(pivot_data())
        
        DT::datatable(
            pivot_data(),
            options = list(
                scrollX = TRUE,
                pageLength = 10,
                searching = TRUE,
                lengthChange = TRUE,
                info = TRUE,
                autoWidth = TRUE,
                dom = 'lrtip'  # Elementos de controle da tabela
            ),
            rownames = FALSE,
        )
    })
    
    # --- Preview dos dados com DT (com limite para grandes datasets)
    output$preview <- DT::renderDataTable({
        req(dados_filtrados())
        
        df <- dados_filtrados()
        
        # Limitar a visualização para 1000 registros em datasets grandes
        if (nrow(df) > 1000) {
            showNotification("Mostrando os primeiros 1000 registros. Exporte para ver todos os dados.", 
                            type = "warning", duration = 5)
            df <- head(df, 1000)
        }
        
        DT::datatable(
            df,
            options = list(
                scrollX = TRUE,
                pageLength = 10,
                searching = TRUE,
                lengthChange = TRUE,
                info = TRUE
            ),
            rownames = FALSE
        )
    })
    
    # --- Download handlers para exportação dos dados completos
    output$download_com_desc <- downloadHandler(
        filename = function() {
            paste0("dados_filtrados_com_descricao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format)
        },
        content = function(file) {
            # Mostrar notificação de carregamento
            id <- showNotification(
                "Preparando arquivo para download...",
                duration = NULL,
                closeButton = FALSE,
                type = "message"
            )
            # Garantir que a notificação seja removida ao final
            on.exit(removeNotification(id), add = TRUE)
            
            dados_export <- dados_completos_com_dicionario()
            
            # Adicionar um pequeno atraso para garantir que a notificação seja mostrada
            Sys.sleep(0.5)
            
            switch(input$export_format,
                   "csv" = {
                       write.table(dados_export, file, 
                                   sep = ",",  # Vírgula fixa como separador
                                   dec = ".",
                                   row.names = FALSE, 
                                   col.names = TRUE)  # Sempre com cabeçalho
                   },
                   "rds" = {
                       saveRDS(dados_export, file)
                   },
                   "dbf" = {
                       # Converte para data.frame tradicional para evitar problemas com o foreign
                       dados_df <- as.data.frame(dados_export)
                       write.dbf(dados_df, file)
                   })
            
            # Notificação de conclusão
            showNotification("Download concluído!", type = "message", duration = 3)
        }
    )
    
    output$download_sem_desc <- downloadHandler(
        filename = function() {
            paste0("dados_filtrados_sem_descricao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format)
        },
        content = function(file) {
            # Mostrar notificação de carregamento
            id <- showNotification(
                "Preparando arquivo para download...",
                duration = NULL,
                closeButton = FALSE,
                type = "message"
            )
            # Garantir que la notificação seja removida ao final
            on.exit(removeNotification(id), add = TRUE)
            
            dados_export <- dados_apenas_codigos()
            
            # Adicionar um pequeno atraso para garantir que la notificação seja mostrada
            Sys.sleep(0.5)
            
            switch(input$export_format,
                   "csv" = {
                       write.table(dados_export, file, 
                                   sep = ",",  # Vírgula fixa como separador
                                   dec = ".",
                                   row.names = FALSE, 
                                   col.names = TRUE)  # Sempre com cabeçalho
                   },
                   "rds" = {
                       saveRDS(dados_export, file)
                   },
                   "dbf" = {
                       # Converte para data.frame tradicional para evitar problemas com o foreign
                       dados_df <- as.data.frame(dados_export)
                       write.dbf(dados_df, file)
                   })
            
            # Notificação de conclusão
            showNotification("Download concluído!", type = "message", duration = 3)
        }
    )
    
    # --- Download handler para tabela dinâmica (multiformato)
    output$download_pivot <- downloadHandler(
        filename = function() {
            paste0("tabela_dinamica_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            # Mostrar notificação de carregamento
            id <- showNotification(
                "Preparando tabela dinâmica para download...",
                duration = NULL,
                closeButton = FALSE,
                type = "message"
            )
            # Garantir que la notificação seja removida ao final
            on.exit(removeNotification(id), add = TRUE)
            
            # Adicionar um pequeno atraso para garantir que la notificação seja mostrada
            Sys.sleep(0.5)
            
            # Determinar quais dados usar com base na escolha do usuário
            # Para a tabela dinâmica, usamos sempre os dados filtrados (sem distinção de descrição)
            req(dados_filtrados(), input$pivot_vals, input$pivot_fun)
            
            df <- dados_filtrados()
            
            # Se não houver colunas selecionadas, usar dados originais
            if (is.null(input$pivot_rows) && is.null(input$pivot_cols)) {
                pivot_data <- df
            } else {
                # Usar dplyr para agregação (mais eficiente que aggregate)
                group_vars <- c(input$pivot_rows, input$pivot_cols)
                
                # Função de agregação
                agg_fun <- get(input$pivot_fun, mode = "function")
                
                # Agrupar e resumir
                pivot_data <- df %>%
                    group_by(across(all_of(group_vars))) %>%
                    summarise(across(all_of(input$pivot_vals), agg_fun, na.rm = TRUE), .groups = 'drop')
            }
            
            # Escrita do arquivo conforme formato selecionado
            write.csv(pivot_data, file, row.names = FALSE)
            
            # Notificação de conclusão
            showNotification("Download da tabela dinâmica concluído!", type = "message", duration = 3)
        }
    )
    
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
    
    # --- Modal de Ajuda
    observeEvent(input$btn_ajuda, {
        showModal(modalDialog(
            title = list(icon("question-circle"), "Ajuda - Visualizador DBC DATASUS"),
            easyClose = TRUE,
            fade = TRUE,
            size = "l",
            footer = modalButton("Fechar"),
            
            div(style = "max-height: 60vh; overflow-y: auto;",
                h4("Instruções de Uso"),
                tags$ul(
                    tags$li("Faça upload de um ou mais arquivos .dbc do DATASUS"),
                    tags$li("Opcionalmente, carregue um dicionário CSV with the columns: coluna, codigo, descricao"),
                    tags$li("Selecione a coluna para filtrar e os valores desejados"),
                    tags$li("Escolha entre visualizar códigos ou descrições (se dicionário disponível)"),
                    tags$li("Navegue pelas abas para visualizar prévia, resumos, estrutura e tabelas dinâmicas"),
                    tags$li("Exporte os dados filtrados ou tabelas dinâmicas nos formatos disponíveis")
                ),
                
                hr(),
                
                h4("Dicas de Visualização"),
                tags$ul(
                    tags$li("Use os botões de recolher/expandir para otimizar o espaço"),
                    tags$li("Em dispositivos móveis, os painéis se ajustan automaticamente"),
                    tags$li("Use a barra de pesquisa nas tabelas para encontrar dados específicos"),
                    tags$li("Aproveite o máximo de espaço para visualização das tabelas")
                ),
                
                hr(),
                
                h4("Créditos e Desenvolvimento"),
                p("Desenvolvido com foco na visualização de dados do DATASUS"),
                tags$ul(
                    tags$li("Shiny: Framework para aplicações web em R"),
                    tags$li("read.dbc: Leitura de arquivos .dbc do DATASUS"),
                    tags$li("DT: Tabelas interativas e dinâmicas")
                )
            )
        ))
    })
}

# Criar a aplicação Shiny
shinyApp(ui = ui, server = server)
