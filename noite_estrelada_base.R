# ===============================================================================
# REPRODUÇÃO ARTÍSTICA: A NOITE ESTRELADA DE VAN GOGH
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# Data: Agosto 2025
# ===============================================================================

# Carregamento das bibliotecas necessárias
library(ggplot2)      # Biblioteca principal para gráficos
library(dplyr)        # Manipulação de dados
library(grid)         # Controle de elementos gráficos
library(gridExtra)    # Arranjo de múltiplos gráficos
library(scales)       # Escalas e transformações
library(RColorBrewer) # Paletas de cores
library(viridis)      # Paletas de cores científicas

# ===============================================================================
# FUNÇÃO PRINCIPAL: CRIAR A NOITE ESTRELADA
# ===============================================================================

criar_noite_estrelada <- function(largura = 800, altura = 600, 
                                  intensidade_espirais = 50,
                                  num_estrelas = 11,
                                  tamanho_lua = 15) {
  
  # Definição da paleta de cores baseada na obra original
  cores_ceu <- c("#1e3a8a", "#1e40af", "#2563eb", "#3b82f6", "#60a5fa")
  cores_estrelas <- c("#fbbf24", "#f59e0b", "#d97706", "#ffffff")
  cor_lua <- "#fbbf24"
  cor_cipreste <- "#0f172a"
  cor_vila <- c("#374151", "#4b5563", "#6b7280")
  cor_montanhas <- "#1f2937"
  
  # ===============================================================================
  # CRIAÇÃO DO CÉU COM ESPIRAIS (ELEMENTO PRINCIPAL)
  # ===============================================================================
  
  # Função para gerar espirais matemáticas
  gerar_espirais <- function(n_pontos = 1000, n_espirais = 8) {
    espirais_data <- data.frame()
    
    for (i in 1:n_espirais) {
      # Parâmetros únicos para cada espiral
      centro_x <- runif(1, 0.2, 0.8) * largura
      centro_y <- runif(1, 0.6, 0.9) * altura
      raio_max <- runif(1, 50, 120)
      rotacao <- runif(1, 0, 2*pi)
      
      # Geração dos pontos da espiral usando coordenadas polares
      t <- seq(0, 4*pi, length.out = n_pontos/n_espirais)
      raio <- seq(5, raio_max, length.out = length(t))
      
      x <- centro_x + raio * cos(t + rotacao)
      y <- centro_y + raio * sin(t + rotacao)
      
      # Adição de variação orgânica (simulando pinceladas)
      x <- x + rnorm(length(x), 0, 3)
      y <- y + rnorm(length(y), 0, 3)
      
      espiral_temp <- data.frame(
        x = x, y = y, 
        espiral_id = i,
        intensidade = seq(1, 0.3, length.out = length(x))
      )
      
      espirais_data <- rbind(espirais_data, espiral_temp)
    }
    
    return(espirais_data)
  }
  
  # ===============================================================================
  # CRIAÇÃO DAS ESTRELAS E LUA
  # ===============================================================================
  
  # Função para gerar estrelas com halos luminosos
  gerar_estrelas <- function(n_estrelas = num_estrelas) {
    estrelas <- data.frame(
      x = runif(n_estrelas, 0.1*largura, 0.9*largura),
      y = runif(n_estrelas, 0.7*altura, 0.95*altura),
      tamanho = runif(n_estrelas, 8, 20),
      brilho = runif(n_estrelas, 0.7, 1.0)
    )
    
    # Adição da lua (estrela maior)
    lua <- data.frame(
      x = 0.85 * largura,
      y = 0.8 * altura,
      tamanho = tamanho_lua,
      brilho = 1.0,
      tipo = "lua"
    )
    
    estrelas$tipo <- "estrela"
    return(rbind(estrelas, lua))
  }
  
  # ===============================================================================
  # CRIAÇÃO DO CIPRESTE (ELEMENTO VERTICAL DOMINANTE)
  # ===============================================================================
  
  gerar_cipreste <- function() {
    # Base do cipreste
    base_x <- 0.15 * largura
    base_y <- 0.1 * altura
    topo_y <- 0.95 * altura
    
    # Geração da forma orgânica do cipreste usando splines
    n_pontos <- 100
    y_pontos <- seq(base_y, topo_y, length.out = n_pontos)
    
    # Largura variável do cipreste (mais largo na base)
    largura_base <- 40
    largura_topo <- 8
    larguras <- seq(largura_base, largura_topo, length.out = n_pontos)
    
    # Adição de ondulações naturais
    ondulacao <- sin(seq(0, 8*pi, length.out = n_pontos)) * 10
    
    # Lado esquerdo e direito do cipreste
    x_esquerda <- base_x - larguras/2 + ondulacao
    x_direita <- base_x + larguras/2 + ondulacao
    
    cipreste_data <- data.frame(
      x = c(x_esquerda, rev(x_direita)),
      y = c(y_pontos, rev(y_pontos)),
      elemento = "cipreste"
    )
    
    return(cipreste_data)
  }
  
  # ===============================================================================
  # CRIAÇÃO DA VILA E MONTANHAS
  # ===============================================================================
  
  gerar_paisagem <- function() {
    # Montanhas de fundo
    montanhas <- data.frame(
      x = c(0, 0.3*largura, 0.6*largura, largura, largura, 0),
      y = c(0.4*altura, 0.5*altura, 0.45*altura, 0.48*altura, 0, 0),
      elemento = "montanha"
    )
    
    # Vila com casas simples
    casas <- data.frame()
    n_casas <- 8
    
    for (i in 1:n_casas) {
      casa_x <- (0.3 + i * 0.06) * largura
      casa_y <- 0.35 * altura
      casa_largura <- runif(1, 15, 25)
      casa_altura <- runif(1, 20, 35)
      
      # Estrutura básica da casa (retângulo + triângulo do telhado)
      casa_temp <- data.frame(
        x = c(casa_x, casa_x + casa_largura, casa_x + casa_largura, casa_x, casa_x),
        y = c(casa_y, casa_y, casa_y + casa_altura, casa_y + casa_altura, casa_y),
        casa_id = i,
        elemento = "casa"
      )
      
      casas <- rbind(casas, casa_temp)
    }
    
    # Igreja com torre (elemento vertical secundário)
    igreja_x <- 0.6 * largura
    igreja_y <- 0.35 * altura
    
    igreja <- data.frame(
      x = c(igreja_x, igreja_x + 30, igreja_x + 30, igreja_x, igreja_x,
            igreja_x + 10, igreja_x + 20, igreja_x + 20, igreja_x + 10, igreja_x + 10),
      y = c(igreja_y, igreja_y, igreja_y + 40, igreja_y + 40, igreja_y,
            igreja_y + 40, igreja_y + 40, igreja_y + 80, igreja_y + 80, igreja_y + 40),
      elemento = c(rep("igreja_base", 5), rep("igreja_torre", 5))
    )
    
    return(list(montanhas = montanhas, casas = casas, igreja = igreja))
  }
  
  # ===============================================================================
  # MONTAGEM DO GRÁFICO PRINCIPAL
  # ===============================================================================
  
  # Geração de todos os elementos
  espirais_data <- gerar_espirais(intensidade_espirais * 20)
  estrelas_data <- gerar_estrelas()
  cipreste_data <- gerar_cipreste()
  paisagem_data <- gerar_paisagem()
  
  # Criação do gráfico base
  p <- ggplot() +
    # Fundo do céu com gradiente
    annotate("rect", xmin = 0, xmax = largura, ymin = 0.4*altura, ymax = altura,
             fill = "#1e3a8a", alpha = 0.9) +
    
    # Montanhas
    geom_polygon(data = paisagem_data$montanhas, 
                 aes(x = x, y = y), 
                 fill = cor_montanhas, alpha = 0.8) +
    
    # Casas da vila
    geom_polygon(data = paisagem_data$casas, 
                 aes(x = x, y = y, group = casa_id), 
                 fill = cor_vila[1], color = cor_vila[3], size = 0.5) +
    
    # Igreja
    geom_polygon(data = paisagem_data$igreja, 
                 aes(x = x, y = y, group = elemento), 
                 fill = cor_vila[2], color = cor_vila[3], size = 0.5) +
    
    # Espirais do céu (elemento principal de Van Gogh)
    geom_path(data = espirais_data, 
              aes(x = x, y = y, group = espiral_id, alpha = intensidade),
              color = "#60a5fa", size = 2) +
    
    # Cipreste (elemento vertical dominante)
    geom_polygon(data = cipreste_data, 
                 aes(x = x, y = y), 
                 fill = cor_cipreste, alpha = 0.9) +
    
    # Estrelas com efeito de brilho
    geom_point(data = estrelas_data[estrelas_data$tipo == "estrela",], 
               aes(x = x, y = y, size = tamanho, alpha = brilho),
               color = cores_estrelas[1], shape = 8) +
    
    # Lua com halo
    geom_point(data = estrelas_data[estrelas_data$tipo == "lua",], 
               aes(x = x, y = y, size = tamanho),
               color = cor_lua, alpha = 0.9, shape = 16) +
    
    # Configurações do tema
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#0f172a", color = NA),
      panel.background = element_rect(fill = "#0f172a", color = NA),
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0)
    ) +
    
    # Configurações das escalas
    scale_alpha_identity() +
    scale_size_identity() +
    coord_fixed(ratio = 1) +
    xlim(0, largura) +
    ylim(0, altura)
  
  return(p)
}

# ===============================================================================
# FUNÇÃO DE TESTE E VISUALIZAÇÃO
# ===============================================================================

# Função para salvar a obra em alta resolução
salvar_obra <- function(grafico, nome_arquivo = "noite_estrelada_vangogh.png", 
                        largura = 12, altura = 9, dpi = 300) {
  
  ggsave(filename = nome_arquivo, 
         plot = grafico, 
         width = largura, 
         height = altura, 
         dpi = dpi, 
         bg = "#0f172a")
  
  cat("Obra salva como:", nome_arquivo, "\n")
  cat("Dimensões:", largura, "x", altura, "polegadas\n")
  cat("Resolução:", dpi, "DPI\n")
}

# ===============================================================================
# EXECUÇÃO E TESTE
# ===============================================================================

# Criação da obra
cat("Iniciando a criação de 'A Noite Estrelada' de Van Gogh...\n")
obra_arte <- criar_noite_estrelada(largura = 800, altura = 600, 
                                   intensidade_espirais = 60,
                                   num_estrelas = 11,
                                   tamanho_lua = 18)

# Visualização
print(obra_arte)

# Salvamento da obra
salvar_obra(obra_arte, "noite_estrelada_diogo_rego.png")

cat("\n=== REPRODUÇÃO ARTÍSTICA CONCLUÍDA ===\n")
cat("Obra: A Noite Estrelada (Van Gogh, 1889)\n")
cat("Técnica: Programação artística em R\n")
cat("Autor da reprodução: Diogo Rego - Estudante de Estatística UFPB\n")
cat("Projeto: Pixel Poesia R\n")
cat("=======================================\n")

