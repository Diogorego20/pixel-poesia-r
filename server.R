# ===============================================================================
# SERVIDOR SHINY: A NOITE ESTRELADA DE VAN GOGH - SERVER
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# GitHub: https://github.com/Diogorego20/pixel-poesia-r
# Data: Agosto 2025
# ===============================================================================

# Lógica do servidor para aplicação Shiny interativa
# Contém todas as funções de geração artística e interatividade

# ===============================================================================
# FUNÇÃO PRINCIPAL DE CRIAÇÃO ARTÍSTICA (VERSÃO SHINY)
# ===============================================================================

criar_noite_estrelada_shiny <- function(largura = 800, altura = 600,
                                       num_estrelas = 11,
                                       intensidade_espirais = 6,
                                       tamanho_lua = 15,
                                       cor_ceu = "#1e3a8a",
                                       cor_estrelas = "#ffeb3b",
                                       mostrar_cipreste = TRUE,
                                       mostrar_vila = TRUE,
                                       mostrar_montanhas = TRUE,
                                       estilo_pincelada = "organico") {
  
  # ===============================================================================
  # CONFIGURAÇÃO INICIAL DO GRÁFICO
  # ===============================================================================
  
  # Configuração da área de plotagem
  par(mar = c(0, 0, 0, 0), bg = "#0a0a2e", xpd = FALSE)
  plot(0, 0, type = "n", xlim = c(0, largura), ylim = c(0, altura), 
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  # ===============================================================================
  # FUNÇÕES AUXILIARES
  # ===============================================================================
  
  # Conversão de cor hexadecimal para RGB
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", hex)
    r <- as.numeric(paste0("0x", substr(hex, 1, 2))) / 255
    g <- as.numeric(paste0("0x", substr(hex, 3, 4))) / 255
    b <- as.numeric(paste0("0x", substr(hex, 5, 6))) / 255
    return(c(r, g, b))
  }
  
  # ===============================================================================
  # FUNDO DO CÉU COM GRADIENTE CUSTOMIZÁVEL
  # ===============================================================================
  
  rgb_ceu <- hex_to_rgb(cor_ceu)
  
  # Criação do gradiente do céu (mais suave e realista)
  for (i in 1:150) {
    y_pos <- altura * 0.35 + (altura * 0.65) * (i/150)
    intensidade_base <- 0.2 + 0.6 * (i/150)
    intensidade_variacao <- 0.1 * sin(i * 0.1) # Variação sutil
    intensidade_final <- intensidade_base + intensidade_variacao
    
    rect(0, y_pos, largura, y_pos + altura * 0.005, 
         col = rgb(rgb_ceu[1] * intensidade_final, 
                  rgb_ceu[2] * intensidade_final, 
                  rgb_ceu[3] * intensidade_final, 
                  alpha = 0.9), 
         border = NA)
  }
  
  # ===============================================================================
  # MONTANHAS DE FUNDO (OPCIONAL)
  # ===============================================================================
  
  if (mostrar_montanhas) {
    # Montanhas com perfil mais orgânico
    n_pontos_mont <- 20
    x_mont <- seq(0, largura, length.out = n_pontos_mont)
    y_mont_base <- altura * 0.35
    
    # Geração de perfil montanhoso usando funções senoidais
    y_mont <- y_mont_base + 
              altura * 0.08 * sin(x_mont * 0.01) + 
              altura * 0.04 * sin(x_mont * 0.02) + 
              altura * 0.02 * sin(x_mont * 0.05)
    
    # Adicionar pontos base para fechar o polígono
    mont_x <- c(0, x_mont, largura, largura, 0)
    mont_y <- c(0, y_mont, 0, 0, 0)
    
    polygon(mont_x, mont_y, col = "#1a1a3a", border = NA)
  }
  
  # ===============================================================================
  # VILA COM CASAS E IGREJA (OPCIONAL)
  # ===============================================================================
  
  if (mostrar_vila) {
    # Configuração da vila
    vila_y_base <- ifelse(mostrar_montanhas, max(y_mont) * 0.9, altura * 0.35)
    
    # Casas da vila com mais variação
    for (i in 1:10) {
      casa_x <- largura * (0.25 + i * 0.05 + runif(1, -0.01, 0.01))
      casa_y <- vila_y_base + runif(1, -5, 5)
      casa_largura <- runif(1, 12, 22)
      casa_altura <- runif(1, 18, 30)
      
      # Corpo da casa com cor variada
      cor_casa <- sample(c("#2d2d4a", "#3a3a5a", "#252540"), 1)
      rect(casa_x, casa_y, casa_x + casa_largura, casa_y + casa_altura, 
           col = cor_casa, border = "#404060", lwd = 0.5)
      
      # Telhado com forma mais realista
      telhado_altura <- runif(1, 8, 15)
      telhado_x <- c(casa_x - 2, casa_x + casa_largura/2, casa_x + casa_largura + 2)
      telhado_y <- c(casa_y + casa_altura, casa_y + casa_altura + telhado_altura, casa_y + casa_altura)
      polygon(telhado_x, telhado_y, col = "#1a1a2e", border = "#404060", lwd = 0.5)
      
      # Janelas com luz (probabilidade baseada na posição)
      if (runif(1) > 0.4) {
        janela_largura <- casa_largura * 0.3
        janela_altura <- casa_altura * 0.25
        janela_x <- casa_x + (casa_largura - janela_largura) / 2
        janela_y <- casa_y + casa_altura * 0.4
        
        rect(janela_x, janela_y, janela_x + janela_largura, janela_y + janela_altura, 
             col = "#ffeb3b", border = "#ffc107", lwd = 0.5)
        
        # Efeito de luz da janela
        rect(janela_x - 1, janela_y - 1, janela_x + janela_largura + 1, janela_y + janela_altura + 1, 
             col = rgb(1, 1, 0, alpha = 0.3), border = NA)
      }
    }
    
    # Igreja com torre (elemento arquitetônico destacado)
    igreja_x <- largura * 0.58
    igreja_y <- vila_y_base
    
    # Corpo principal da igreja
    rect(igreja_x, igreja_y, igreja_x + 35, igreja_y + 45, 
         col = "#2d2d4a", border = "#404060", lwd = 1)
    
    # Torre da igreja (elemento vertical secundário)
    torre_largura <- 12
    torre_altura <- 85
    torre_x <- igreja_x + (35 - torre_largura) / 2
    
    rect(torre_x, igreja_y + 45, torre_x + torre_largura, igreja_y + 45 + torre_altura, 
         col = "#2d2d4a", border = "#404060", lwd = 1)
    
    # Topo pontiagudo da torre
    ponta_x <- c(torre_x - 2, torre_x + torre_largura/2, torre_x + torre_largura + 2)
    ponta_y <- c(igreja_y + 45 + torre_altura, igreja_y + 45 + torre_altura + 15, igreja_y + 45 + torre_altura)
    polygon(ponta_x, ponta_y, col = "#1a1a2e", border = "#404060", lwd = 1)
  }
  
  # ===============================================================================
  # CIPRESTE (ELEMENTO VERTICAL DOMINANTE) - OPCIONAL
  # ===============================================================================
  
  if (mostrar_cipreste) {
    cipreste_x <- largura * 0.12
    cipreste_base_y <- altura * 0.05
    cipreste_topo_y <- altura * 0.98
    
    # Geração mais sofisticada do cipreste
    n_pontos <- 80
    y_pontos <- seq(cipreste_base_y, cipreste_topo_y, length.out = n_pontos)
    
    # Largura variável com transição suave
    largura_base <- 25
    largura_topo <- 3
    larguras <- largura_base * exp(-3 * (y_pontos - cipreste_base_y) / (cipreste_topo_y - cipreste_base_y)) + largura_topo
    
    # Ondulações baseadas no estilo de pincelada
    if (estilo_pincelada == "organico") {
      ondulacao <- 4 * sin(seq(0, 10*pi, length.out = n_pontos)) + 
                   2 * sin(seq(0, 20*pi, length.out = n_pontos))
    } else if (estilo_pincelada == "geometrico") {
      ondulacao <- rep(0, n_pontos)
    } else { # "expressivo"
      ondulacao <- 6 * sin(seq(0, 15*pi, length.out = n_pontos)) + 
                   3 * cos(seq(0, 25*pi, length.out = n_pontos))
    }
    
    # Construção do contorno do cipreste
    x_esquerda <- cipreste_x - larguras/2 + ondulacao
    x_direita <- cipreste_x + larguras/2 + ondulacao
    
    cipreste_x_coords <- c(x_esquerda, rev(x_direita))
    cipreste_y_coords <- c(y_pontos, rev(y_pontos))
    
    # Desenho do cipreste com gradiente de cor
    polygon(cipreste_x_coords, cipreste_y_coords, col = "#0d1117", border = "#1a1a2e", lwd = 1)
    
    # Adicionar textura interna (ramos)
    for (i in seq(1, n_pontos, by = 5)) {
      if (i < n_pontos - 5) {
        ramo_y <- y_pontos[i]
        ramo_x_base <- cipreste_x + ondulacao[i]
        ramo_comprimento <- larguras[i] * 0.3
        
        # Ramos esquerdos
        lines(c(ramo_x_base, ramo_x_base - ramo_comprimento), 
              c(ramo_y, ramo_y + ramo_comprimento * 0.3), 
              col = "#0a0a0a", lwd = 1)
        
        # Ramos direitos
        lines(c(ramo_x_base, ramo_x_base + ramo_comprimento), 
              c(ramo_y, ramo_y + ramo_comprimento * 0.3), 
              col = "#0a0a0a", lwd = 1)
      }
    }
  }
  
  # ===============================================================================
  # ESPIRAIS DO CÉU (CARACTERÍSTICA ICÔNICA DE VAN GOGH)
  # ===============================================================================
  
  desenhar_espiral_avancada <- function(centro_x, centro_y, raio_max, rotacao = 0, 
                                       cor = "#4a90e2", espessura = 2) {
    # Parâmetros da espiral
    n_voltas <- 3 + runif(1, 0, 2)
    t <- seq(0, n_voltas * 2 * pi, length.out = 300)
    raio <- seq(8, raio_max, length.out = length(t))
    
    # Coordenadas base da espiral
    x <- centro_x + raio * cos(t + rotacao)
    y <- centro_y + raio * sin(t + rotacao)
    
    # Aplicar estilo de pincelada
    if (estilo_pincelada == "organico") {
      # Variação orgânica suave
      x <- x + rnorm(length(x), 0, 1.5)
      y <- y + rnorm(length(y), 0, 1.5)
    } else if (estilo_pincelada == "expressivo") {
      # Variação dramática e expressiva
      x <- x + rnorm(length(x), 0, 3) + sin(t * 5) * 2
      y <- y + rnorm(length(y), 0, 3) + cos(t * 5) * 2
    }
    # Para "geometrico", manter coordenadas originais
    
    # Desenhar múltiplas linhas para criar espessura e textura
    for (offset_x in seq(-espessura, espessura, by = 0.5)) {
      for (offset_y in seq(-espessura, espessura, by = 0.5)) {
        if (sqrt(offset_x^2 + offset_y^2) <= espessura) {
          alpha_offset <- 1 - sqrt(offset_x^2 + offset_y^2) / espessura
          lines(x + offset_x, y + offset_y, 
                col = rgb(0.29, 0.56, 0.89, alpha = alpha_offset * 0.8), 
                lwd = 1)
        }
      }
    }
  }
  
  # Geração das espirais baseada na intensidade
  num_espirais <- min(max(intensidade_espirais, 1), 15)
  
  # Posições pré-definidas para espirais principais (mais artísticas)
  espirais_principais <- list(
    list(x = largura * 0.3, y = altura * 0.82, raio = 70, rot = 0),
    list(x = largura * 0.55, y = altura * 0.88, raio = 85, rot = pi/3),
    list(x = largura * 0.75, y = altura * 0.78, raio = 60, rot = pi/2),
    list(x = largura * 0.4, y = altura * 0.72, raio = 45, rot = pi),
    list(x = largura * 0.65, y = altura * 0.92, raio = 40, rot = 3*pi/2),
    list(x = largura * 0.82, y = altura * 0.85, raio = 55, rot = pi/4)
  )
  
  # Desenhar espirais principais
  for (i in 1:min(num_espirais, length(espirais_principais))) {
    esp <- espirais_principais[[i]]
    desenhar_espiral_avancada(esp$x, esp$y, esp$raio, esp$rot, "#4a90e2", 2)
  }
  
  # Adicionar espirais menores se intensidade for alta
  if (num_espirais > length(espirais_principais)) {
    espirais_extras <- num_espirais - length(espirais_principais)
    for (i in 1:espirais_extras) {
      centro_x <- largura * runif(1, 0.2, 0.8)
      centro_y <- altura * runif(1, 0.65, 0.95)
      raio <- runif(1, 25, 50)
      rotacao <- runif(1, 0, 2*pi)
      
      desenhar_espiral_avancada(centro_x, centro_y, raio, rotacao, "#6ba3f5", 1.5)
    }
  }
  
  # ===============================================================================
  # ESTRELAS E LUA (ELEMENTOS LUMINOSOS)
  # ===============================================================================
  
  rgb_estrelas <- hex_to_rgb(cor_estrelas)
  
  # Função avançada para desenhar estrelas
  desenhar_estrela_avancada <- function(x, y, tamanho = 5, brilho = 1) {
    # Halo externo (efeito de luminosidade)
    for (r in seq(tamanho * 3, tamanho * 1.5, by = -0.5)) {
      alpha_halo <- (tamanho * 3 - r) / (tamanho * 1.5) * 0.2 * brilho
      symbols(x, y, circles = r, add = TRUE, 
              fg = NA, bg = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = alpha_halo), 
              inches = FALSE)
    }
    
    # Centro brilhante da estrela
    symbols(x, y, circles = tamanho, add = TRUE, 
            fg = NA, bg = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = brilho), 
            inches = FALSE)
    
    # Raios da estrela (padrão de 8 pontas)
    for (angulo in seq(0, 2*pi, length.out = 9)[-9]) {
      # Raios principais (mais longos)
      if (angulo %% (pi/2) < 0.1) {
        comprimento <- tamanho * 4
        espessura <- 2
      } else {
        comprimento <- tamanho * 2.5
        espessura <- 1
      }
      
      x_fim <- x + cos(angulo) * comprimento
      y_fim <- y + sin(angulo) * comprimento
      
      lines(c(x, x_fim), c(y, y_fim), 
            col = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = brilho * 0.8), 
            lwd = espessura)
    }
  }
  
  # Geração das estrelas com posições mais artísticas
  set.seed(42) # Para reprodutibilidade
  for (i in 1:num_estrelas) {
    # Distribuição não uniforme (mais estrelas na parte superior)
    estrela_x <- largura * runif(1, 0.1, 0.9)
    estrela_y <- altura * (0.7 + 0.25 * rbeta(1, 2, 1)) # Concentrar no topo
    
    # Evitar sobreposição com espirais principais
    muito_perto_espiral <- FALSE
    for (esp in espirais_principais[1:min(num_espirais, length(espirais_principais))]) {
      distancia <- sqrt((estrela_x - esp$x)^2 + (estrela_y - esp$y)^2)
      if (distancia < 50) {
        muito_perto_espiral <- TRUE
        break
      }
    }
    
    if (!muito_perto_espiral) {
      tamanho <- runif(1, 3, 7)
      brilho <- runif(1, 0.7, 1.0)
      desenhar_estrela_avancada(estrela_x, estrela_y, tamanho, brilho)
    }
  }
  
  # Lua (elemento lunar proeminente)
  lua_x <- largura * 0.85
  lua_y <- altura * 0.82
  
  # Halo da lua (múltiplas camadas)
  for (r in seq(tamanho_lua * 2.5, tamanho_lua * 1.2, by = -1)) {
    alpha_lua <- (tamanho_lua * 2.5 - r) / (tamanho_lua * 1.3) * 0.3
    symbols(lua_x, lua_y, circles = r, add = TRUE, 
            fg = NA, bg = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = alpha_lua), 
            inches = FALSE)
  }
  
  # Corpo principal da lua
  symbols(lua_x, lua_y, circles = tamanho_lua, add = TRUE, 
          fg = NA, bg = cor_estrelas, inches = FALSE)
  
  # Raios da lua (padrão radial)
  for (angulo in seq(0, 2*pi, length.out = 16)[-16]) {
    comprimento_raio <- tamanho_lua * 2.2
    x_fim <- lua_x + cos(angulo) * comprimento_raio
    y_fim <- lua_y + sin(angulo) * comprimento_raio
    
    lines(c(lua_x + cos(angulo) * tamanho_lua * 1.1, x_fim), 
          c(lua_y + sin(angulo) * tamanho_lua * 1.1, y_fim), 
          col = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = 0.9), 
          lwd = 2)
  }
  
  # ===============================================================================
  # ASSINATURA ARTÍSTICA E METADADOS
  # ===============================================================================
  
  # Assinatura do artista digital
  text(largura * 0.02, altura * 0.02, 
       "Reprodução Digital: Diogo Rego (UFPB) - Pixel Poesia R", 
       col = "#ffffff", cex = 0.7, adj = 0, family = "sans")
  
  text(largura * 0.02, altura * 0.06, 
       "Obra Original: A Noite Estrelada - Vincent van Gogh (1889)", 
       col = "#cccccc", cex = 0.6, adj = 0, family = "sans")
  
  # Timestamp da criação
  timestamp <- format(Sys.time(), "%d/%m/%Y %H:%M")
  text(largura * 0.98, altura * 0.02, 
       paste("Criado em:", timestamp), 
       col = "#666666", cex = 0.5, adj = 1, family = "mono")
}

# ===============================================================================
# DEFINIÇÃO DO SERVIDOR SHINY
# ===============================================================================

server <- function(input, output, session) {
  
  # ===============================================================================
  # PRESETS ARTÍSTICOS (BOTÕES DE CONFIGURAÇÃO RÁPIDA)
  # ===============================================================================
  
  # Preset Van Gogh Clássico
  observeEvent(input$preset_classico, {
    updateSliderInput(session, "num_estrelas", value = 11)
    updateSliderInput(session, "intensidade_espirais", value = 6)
    updateSliderInput(session, "tamanho_lua", value = 15)
    updateColourInput(session, "cor_ceu", value = "#1e3a8a")
    updateColourInput(session, "cor_estrelas", value = "#ffeb3b")
    updateCheckboxInput(session, "mostrar_cipreste", value = TRUE)
    updateCheckboxInput(session, "mostrar_vila", value = TRUE)
    updateCheckboxInput(session, "mostrar_montanhas", value = TRUE)
    updateSelectInput(session, "estilo_pincelada", selected = "organico")
  })
  
  # Preset Minimalista Moderno
  observeEvent(input$preset_minimalista, {
    updateSliderInput(session, "num_estrelas", value = 5)
    updateSliderInput(session, "intensidade_espirais", value = 3)
    updateSliderInput(session, "tamanho_lua", value = 12)
    updateColourInput(session, "cor_ceu", value = "#2d3748")
    updateColourInput(session, "cor_estrelas", value = "#f7fafc")
    updateCheckboxInput(session, "mostrar_cipreste", value = FALSE)
    updateCheckboxInput(session, "mostrar_vila", value = FALSE)
    updateCheckboxInput(session, "mostrar_montanhas", value = TRUE)
    updateSelectInput(session, "estilo_pincelada", selected = "geometrico")
  })
  
  # Preset Expressionista
  observeEvent(input$preset_expressivo, {
    updateSliderInput(session, "num_estrelas", value = 15)
    updateSliderInput(session, "intensidade_espirais", value = 10)
    updateSliderInput(session, "tamanho_lua", value = 20)
    updateColourInput(session, "cor_ceu", value = "#1a202c")
    updateColourInput(session, "cor_estrelas", value = "#fbd38d")
    updateCheckboxInput(session, "mostrar_cipreste", value = TRUE)
    updateCheckboxInput(session, "mostrar_vila", value = TRUE)
    updateCheckboxInput(session, "mostrar_montanhas", value = TRUE)
    updateSelectInput(session, "estilo_pincelada", selected = "expressivo")
  })
  
  # Preset Contemporâneo
  observeEvent(input$preset_contemporaneo, {
    updateSliderInput(session, "num_estrelas", value = 8)
    updateSliderInput(session, "intensidade_espirais", value = 4)
    updateSliderInput(session, "tamanho_lua", value = 18)
    updateColourInput(session, "cor_ceu", value = "#2b6cb0")
    updateColourInput(session, "cor_estrelas", value = "#ed8936")
    updateCheckboxInput(session, "mostrar_cipreste", value = TRUE)
    updateCheckboxInput(session, "mostrar_vila", value = TRUE)
    updateCheckboxInput(session, "mostrar_montanhas", value = TRUE)
    updateSelectInput(session, "estilo_pincelada", selected = "organico")
  })
  
  # ===============================================================================
  # GERAÇÃO DA OBRA DE ARTE
  # ===============================================================================
  
  # Variável reativa para armazenar a obra atual
  obra_atual <- reactiveVal(NULL)
  
  # Observador para o botão de geração
  observeEvent(input$gerar_arte, {
    # Mostrar indicador de carregamento
    showNotification("🎨 Criando sua obra de arte...", type = "message", duration = 3)
    
    # Gerar a obra com os parâmetros atuais
    obra_atual(list(
      largura = input$largura,
      altura = input$altura,
      num_estrelas = input$num_estrelas,
      intensidade_espirais = input$intensidade_espirais,
      tamanho_lua = input$tamanho_lua,
      cor_ceu = input$cor_ceu,
      cor_estrelas = input$cor_estrelas,
      mostrar_cipreste = input$mostrar_cipreste,
      mostrar_vila = input$mostrar_vila,
      mostrar_montanhas = input$mostrar_montanhas,
      estilo_pincelada = input$estilo_pincelada,
      timestamp = Sys.time()
    ))
    
    showNotification("✅ Obra criada com sucesso!", type = "success", duration = 2)
  })
  
  # ===============================================================================
  # RENDERIZAÇÃO DA OBRA DE ARTE
  # ===============================================================================
  
  output$obra_arte <- renderPlot({
    req(obra_atual()) # Requer que uma obra tenha sido gerada
    
    params <- obra_atual()
    
    criar_noite_estrelada_shiny(
      largura = params$largura,
      altura = params$altura,
      num_estrelas = params$num_estrelas,
      intensidade_espirais = params$intensidade_espirais,
      tamanho_lua = params$tamanho_lua,
      cor_ceu = params$cor_ceu,
      cor_estrelas = params$cor_estrelas,
      mostrar_cipreste = params$mostrar_cipreste,
      mostrar_vila = params$mostrar_vila,
      mostrar_montanhas = params$mostrar_montanhas,
      estilo_pincelada = params$estilo_pincelada
    )
  }, width = function() input$largura, height = function() input$altura)
  
  # ===============================================================================
  # INFORMAÇÕES TÉCNICAS
  # ===============================================================================
  
  output$info_tecnica <- renderText({
    req(obra_atual())
    
    params <- obra_atual()
    
    paste(
      sprintf("Dimensões: %d × %d pixels", params$largura, params$altura),
      sprintf("Estrelas: %d | Lua: tamanho %d", params$num_estrelas, params$tamanho_lua),
      sprintf("Espirais: intensidade %d | Estilo: %s", params$intensidade_espirais, params$estilo_pincelada),
      sprintf("Elementos: %s%s%s", 
              ifelse(params$mostrar_cipreste, "Cipreste ", ""),
              ifelse(params$mostrar_vila, "Vila ", ""),
              ifelse(params$mostrar_montanhas, "Montanhas", "")),
      sprintf("Criado em: %s", format(params$timestamp, "%d/%m/%Y às %H:%M:%S")),
      sep = " | "
    )
  })
  
  # ===============================================================================
  # DOWNLOAD DA OBRA
  # ===============================================================================
  
  output$download_arte <- downloadHandler(
    filename = function() {
      req(obra_atual())
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("noite_estrelada_", timestamp, ".png")
    },
    content = function(file) {
      req(obra_atual())
      
      params <- obra_atual()
      
      # Criar arquivo temporário com alta resolução
      png(file, width = params$largura * 2, height = params$altura * 2, 
          res = 300, bg = "#0a0a2e")
      
      criar_noite_estrelada_shiny(
        largura = params$largura * 2,
        altura = params$altura * 2,
        num_estrelas = params$num_estrelas,
        intensidade_espirais = params$intensidade_espirais,
        tamanho_lua = params$tamanho_lua * 2,
        cor_ceu = params$cor_ceu,
        cor_estrelas = params$cor_estrelas,
        mostrar_cipreste = params$mostrar_cipreste,
        mostrar_vila = params$mostrar_vila,
        mostrar_montanhas = params$mostrar_montanhas,
        estilo_pincelada = params$estilo_pincelada
      )
      
      dev.off()
    }
  )
}

