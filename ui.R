# ===============================================================================
# INTERFACE SHINY: A NOITE ESTRELADA DE VAN GOGH - UI
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# GitHub: https://github.com/Diogorego20/pixel-poesia-r
# Data: Agosto 2025
# ===============================================================================

# Interface do usuário para aplicação Shiny interativa
# Esta interface permite ao usuário customizar todos os aspectos da obra

# Definição da interface do usuário
ui <- fluidPage(
  
  # ===============================================================================
  # CONFIGURAÇÃO DO TEMA E ESTILO
  # ===============================================================================
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
        color: #ffffff;
        font-family: 'Arial', sans-serif;
      }
      
      .main-header {
        background: rgba(0, 0, 0, 0.8);
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        text-align: center;
        border: 2px solid #4a90e2;
      }
      
      .control-panel {
        background: rgba(0, 0, 0, 0.6);
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
        border: 1px solid #333;
      }
      
      .art-display {
        background: rgba(0, 0, 0, 0.8);
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        border: 2px solid #4a90e2;
      }
      
      .slider-input .irs-bar {
        background: #4a90e2;
      }
      
      .slider-input .irs-handle {
        background: #4a90e2;
      }
      
      .btn-primary {
        background-color: #4a90e2;
        border-color: #4a90e2;
        font-weight: bold;
      }
      
      .btn-primary:hover {
        background-color: #357abd;
        border-color: #357abd;
      }
      
      h1, h2, h3 {
        color: #4a90e2;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
      }
      
      .well {
        background: rgba(0, 0, 0, 0.4);
        border: 1px solid #333;
      }
    "))
  ),
  
  # ===============================================================================
  # CABEÇALHO PRINCIPAL
  # ===============================================================================
  
  div(class = "main-header",
    h1("🎨 A Noite Estrelada Interativa", 
       style = "margin: 0; font-size: 2.5em; font-weight: bold;"),
    h3("Reprodução Digital da Obra de Vincent van Gogh", 
       style = "margin: 10px 0; font-style: italic;"),
    p("Criado por: ", strong("Diogo Rego"), " - Estudante de Estatística UFPB", 
      style = "margin: 5px 0; font-size: 1.1em;"),
    p("Projeto: ", strong("Pixel Poesia R"), " - Arte com Linguagem de Programação", 
      style = "margin: 5px 0; color: #cccccc;"),
    a("🔗 GitHub: pixel-poesia-r", 
      href = "https://github.com/Diogorego20/pixel-poesia-r", 
      target = "_blank", 
      style = "color: #4a90e2; text-decoration: none; font-weight: bold;")
  ),
  
  # ===============================================================================
  # LAYOUT PRINCIPAL
  # ===============================================================================
  
  fluidRow(
    
    # PAINEL DE CONTROLES (COLUNA ESQUERDA)
    column(4,
      
      # Seção: Configurações Gerais
      div(class = "control-panel",
        h3("⚙️ Configurações Gerais"),
        
        sliderInput("largura", 
                   "Largura da Tela:",
                   min = 400, max = 1200, value = 800, step = 50,
                   class = "slider-input"),
        
        sliderInput("altura", 
                   "Altura da Tela:",
                   min = 300, max = 900, value = 600, step = 50,
                   class = "slider-input"),
        
        selectInput("estilo_pincelada", 
                   "Estilo de Pincelada:",
                   choices = list(
                     "Orgânico (Van Gogh Original)" = "organico",
                     "Geométrico (Moderno)" = "geometrico", 
                     "Expressivo (Dramático)" = "expressivo"
                   ),
                   selected = "organico")
      ),
      
      # Seção: Elementos do Céu
      div(class = "control-panel",
        h3("🌌 Elementos do Céu"),
        
        sliderInput("num_estrelas", 
                   "Número de Estrelas:",
                   min = 3, max = 20, value = 11, step = 1,
                   class = "slider-input"),
        
        sliderInput("tamanho_lua", 
                   "Tamanho da Lua:",
                   min = 8, max = 25, value = 15, step = 1,
                   class = "slider-input"),
        
        sliderInput("intensidade_espirais", 
                   "Intensidade das Espirais:",
                   min = 1, max = 12, value = 6, step = 1,
                   class = "slider-input"),
        
        colourInput("cor_ceu", 
                   "Cor do Céu:", 
                   value = "#1e3a8a"),
        
        colourInput("cor_estrelas", 
                   "Cor das Estrelas:", 
                   value = "#ffeb3b")
      ),
      
      # Seção: Elementos da Paisagem
      div(class = "control-panel",
        h3("🏘️ Elementos da Paisagem"),
        
        checkboxInput("mostrar_cipreste", 
                     "Mostrar Cipreste", 
                     value = TRUE),
        
        checkboxInput("mostrar_vila", 
                     "Mostrar Vila", 
                     value = TRUE),
        
        checkboxInput("mostrar_montanhas", 
                     "Mostrar Montanhas", 
                     value = TRUE)
      ),
      
      # Seção: Presets Artísticos
      div(class = "control-panel",
        h3("🎭 Presets Artísticos"),
        
        actionButton("preset_classico", 
                    "Van Gogh Clássico", 
                    class = "btn btn-primary btn-block",
                    style = "margin-bottom: 10px;"),
        
        actionButton("preset_minimalista", 
                    "Minimalista Moderno", 
                    class = "btn btn-primary btn-block",
                    style = "margin-bottom: 10px;"),
        
        actionButton("preset_expressivo", 
                    "Expressionista", 
                    class = "btn btn-primary btn-block",
                    style = "margin-bottom: 10px;"),
        
        actionButton("preset_contemporaneo", 
                    "Contemporâneo", 
                    class = "btn btn-primary btn-block")
      ),
      
      # Botão de Geração
      div(class = "control-panel", style = "text-align: center;",
        actionButton("gerar_arte", 
                    "🎨 GERAR OBRA DE ARTE", 
                    class = "btn btn-primary btn-lg",
                    style = "font-size: 1.2em; padding: 15px 30px; font-weight: bold;")
      )
    ),
    
    # ÁREA DE EXIBIÇÃO DA ARTE (COLUNA DIREITA)
    column(8,
      div(class = "art-display",
        h3("🖼️ Sua Obra de Arte"),
        
        # Área de loading
        conditionalPanel(
          condition = "input.gerar_arte == 0",
          div(style = "padding: 50px; text-align: center;",
            h4("👆 Configure os parâmetros e clique em 'GERAR OBRA DE ARTE'", 
               style = "color: #cccccc; font-style: italic;"),
            p("Explore diferentes combinações para criar sua própria interpretação", 
              style = "color: #999999;"),
            p("da icônica 'Noite Estrelada' de Vincent van Gogh!", 
              style = "color: #999999;")
          )
        ),
        
        # Área da imagem gerada
        conditionalPanel(
          condition = "input.gerar_arte > 0",
          div(
            plotOutput("obra_arte", 
                      width = "100%", 
                      height = "500px"),
            
            br(),
            
            # Informações técnicas
            div(style = "background: rgba(0,0,0,0.3); padding: 10px; border-radius: 5px; margin-top: 15px;",
              h5("📊 Informações Técnicas:", style = "color: #4a90e2; margin-bottom: 10px;"),
              textOutput("info_tecnica"),
              br(),
              downloadButton("download_arte", 
                           "💾 Download da Obra", 
                           class = "btn btn-success",
                           style = "font-weight: bold;")
            )
          )
        )
      ),
      
      # Seção: Sobre a Obra Original
      div(class = "control-panel", style = "margin-top: 20px;",
        h3("📚 Sobre a Obra Original"),
        p(strong("'A Noite Estrelada'"), " (1889) é uma das obras mais famosas de Vincent van Gogh.", 
          style = "text-align: justify;"),
        p("Pintada durante sua estadia no hospício de Saint-Rémy-de-Provence, a obra retrata a vista de sua janela", 
          "com elementos imaginários como a vila e o cipreste proeminente.", 
          style = "text-align: justify;"),
        p("Esta reprodução digital permite explorar como van Gogh poderia ter criado sua arte", 
          "utilizando as tecnologias de programação disponíveis hoje.", 
          style = "text-align: justify; font-style: italic; color: #cccccc;")
      )
    )
  ),
  
  # ===============================================================================
  # RODAPÉ
  # ===============================================================================
  
  hr(style = "border-color: #333; margin-top: 40px;"),
  
  div(style = "text-align: center; padding: 20px; color: #666;",
    p("Desenvolvido com ❤️ em R por Diogo Rego - UFPB", 
      style = "margin: 5px 0;"),
    p("Projeto Pixel Poesia R - Transformando código em arte", 
      style = "margin: 5px 0; font-style: italic;"),
    p("© 2025 - Reprodução educacional da obra de Vincent van Gogh", 
      style = "margin: 5px 0; font-size: 0.9em;")
  )
)

