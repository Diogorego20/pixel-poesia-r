# ===============================================================================
# APLICAÇÃO SHINY INTERATIVA: A NOITE ESTRELADA DE VAN GOGH
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# GitHub: https://github.com/Diogorego20/pixel-poesia-r
# Data: Agosto 2025
# ===============================================================================

# Carregamento das bibliotecas (usando apenas R base para compatibilidade)
# library(shiny)        # Interface web interativa
# library(shinydashboard) # Dashboard profissional

# ===============================================================================
# FUNÇÃO PRINCIPAL DE CRIAÇÃO ARTÍSTICA
# ===============================================================================

criar_noite_estrelada_interativa <- function(largura = 800, altura = 600,
                                             num_estrelas = 11,
                                             intensidade_espirais = 6,
                                             tamanho_lua = 15,
                                             cor_ceu = "#1e3a8a",
                                             cor_estrelas = "#ffeb3b",
                                             mostrar_cipreste = TRUE,
                                             mostrar_vila = TRUE,
                                             estilo_pincelada = "organico") {
  
  # Configuração do dispositivo gráfico temporário
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = largura, height = altura, bg = "#0a0a2e", res = 100)
  
  # Configuração da área de plotagem
  par(mar = c(0, 0, 0, 0), bg = "#0a0a2e")
  plot(0, 0, type = "n", xlim = c(0, largura), ylim = c(0, altura), 
       axes = FALSE, xlab = "", ylab = "")
  
  # ===============================================================================
  # FUNDO DO CÉU COM GRADIENTE CUSTOMIZÁVEL
  # ===============================================================================
  
  # Conversão da cor hexadecimal para RGB
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", hex)
    r <- as.numeric(paste0("0x", substr(hex, 1, 2))) / 255
    g <- as.numeric(paste0("0x", substr(hex, 3, 4))) / 255
    b <- as.numeric(paste0("0x", substr(hex, 5, 6))) / 255
    return(c(r, g, b))
  }
  
  rgb_ceu <- hex_to_rgb(cor_ceu)
  
  # Criação do gradiente do céu
  for (i in 1:100) {
    y_pos <- altura * 0.4 + (altura * 0.6) * (i/100)
    intensidade <- 0.3 + 0.5 * (i/100)
    rect(0, y_pos, largura, y_pos + altura * 0.006, 
         col = rgb(rgb_ceu[1], rgb_ceu[2], rgb_ceu[3] * intensidade, alpha = 0.8), 
         border = NA)
  }
  
  # ===============================================================================
  # MONTANHAS DE FUNDO
  # ===============================================================================
  
  mont_x <- c(0, largura * 0.3, largura * 0.6, largura, largura, 0)
  mont_y <- c(altura * 0.4, altura * 0.5, altura * 0.45, altura * 0.48, 0, 0)
  polygon(mont_x, mont_y, col = "#1a1a3a", border = NA)
  
  # ===============================================================================
  # VILA COM CASAS E IGREJA (OPCIONAL)
  # ===============================================================================
  
  if (mostrar_vila) {
    # Casas da vila
    for (i in 1:8) {
      casa_x <- largura * (0.3 + i * 0.06)
      casa_y <- altura * 0.35
      casa_largura <- 15 + runif(1, 0, 10)
      casa_altura <- 20 + runif(1, 0, 15)
      
      # Corpo da casa
      rect(casa_x, casa_y, casa_x + casa_largura, casa_y + casa_altura, 
           col = "#2d2d4a", border = "#404060")
      
      # Telhado triangular
      telhado_x <- c(casa_x - 2, casa_x + casa_largura/2, casa_x + casa_largura + 2)
      telhado_y <- c(casa_y + casa_altura, casa_y + casa_altura + 10, casa_y + casa_altura)
      polygon(telhado_x, telhado_y, col = "#1a1a2e", border = "#404060")
      
      # Janela com luz (algumas casas)
      if (runif(1) > 0.5) {
        rect(casa_x + casa_largura/3, casa_y + casa_altura/2, 
             casa_x + 2*casa_largura/3, casa_y + 3*casa_altura/4, 
             col = "#ffeb3b", border = "#ffc107")
      }
    }
    
    # Igreja com torre
    igreja_x <- largura * 0.6
    igreja_y <- altura * 0.35
    
    rect(igreja_x, igreja_y, igreja_x + 30, igreja_y + 40, 
         col = "#2d2d4a", border = "#404060")
    rect(igreja_x + 10, igreja_y + 40, igreja_x + 20, igreja_y + 80, 
         col = "#2d2d4a", border = "#404060")
  }
  
  # ===============================================================================
  # CIPRESTE (ELEMENTO VERTICAL DOMINANTE) - OPCIONAL
  # ===============================================================================
  
  if (mostrar_cipreste) {
    cipreste_x <- largura * 0.15
    cipreste_base_y <- altura * 0.1
    cipreste_topo_y <- altura * 0.95
    
    n_pontos <- 50
    y_pontos <- seq(cipreste_base_y, cipreste_topo_y, length.out = n_pontos)
    larguras <- seq(20, 4, length.out = n_pontos)
    
    # Variação do estilo de pincelada
    if (estilo_pincelada == "organico") {
      ondulacao <- sin(seq(0, 8*pi, length.out = n_pontos)) * 5
    } else if (estilo_pincelada == "geometrico") {
      ondulacao <- rep(0, n_pontos)
    } else { # "expressivo"
      ondulacao <- sin(seq(0, 12*pi, length.out = n_pontos)) * 8
    }
    
    x_esquerda <- cipreste_x - larguras/2 + ondulacao
    x_direita <- cipreste_x + larguras/2 + ondulacao
    
    cipreste_x_coords <- c(x_esquerda, rev(x_direita))
    cipreste_y_coords <- c(y_pontos, rev(y_pontos))
    
    polygon(cipreste_x_coords, cipreste_y_coords, col = "#0d1117", border = NA)
  }
  
  # ===============================================================================
  # ESPIRAIS DO CÉU (INTENSIDADE VARIÁVEL)
  # ===============================================================================
  
  desenhar_espiral <- function(centro_x, centro_y, raio_max, rotacao = 0, cor = "#4a90e2") {
    t <- seq(0, 4*pi, length.out = 200)
    raio <- seq(5, raio_max, length.out = length(t))
    
    x <- centro_x + raio * cos(t + rotacao)
    y <- centro_y + raio * sin(t + rotacao)
    
    # Variação baseada no estilo de pincelada
    if (estilo_pincelada == "organico") {
      x <- x + rnorm(length(x), 0, 2)
      y <- y + rnorm(length(y), 0, 2)
    } else if (estilo_pincelada == "expressivo") {
      x <- x + rnorm(length(x), 0, 4)
      y <- y + rnorm(length(y), 0, 4)
    }
    
    # Desenhar a espiral
    for (offset in c(-1, 0, 1)) {
      lines(x + offset, y + offset, col = cor, lwd = 2)
    }
  }
  
  # Número de espirais baseado na intensidade
  num_espirais <- min(intensidade_espirais, 10)
  
  for (i in 1:num_espirais) {
    centro_x <- largura * runif(1, 0.2, 0.8)
    centro_y <- altura * runif(1, 0.65, 0.9)
    raio <- runif(1, 30, 80)
    rotacao <- runif(1, 0, 2*pi)
    
    desenhar_espiral(centro_x, centro_y, raio, rotacao, "#4a90e2")
  }
  
  # ===============================================================================
  # ESTRELAS E LUA (NÚMERO CUSTOMIZÁVEL)
  # ===============================================================================
  
  rgb_estrelas <- hex_to_rgb(cor_estrelas)
  
  desenhar_estrela <- function(x, y, tamanho = 5) {
    # Halo da estrela
    symbols(x, y, circles = tamanho * 2, add = TRUE, 
            fg = NA, bg = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = 0.3), 
            inches = FALSE)
    
    # Centro brilhante da estrela
    symbols(x, y, circles = tamanho, add = TRUE, 
            fg = NA, bg = cor_estrelas, inches = FALSE)
    
    # Raios da estrela
    for (angulo in seq(0, 2*pi, length.out = 8)) {
      x_fim <- x + cos(angulo) * tamanho * 3
      y_fim <- y + sin(angulo) * tamanho * 3
      lines(c(x, x_fim), c(y, y_fim), col = cor_estrelas, lwd = 2)
    }
  }
  
  # Desenhar estrelas (número customizável)
  for (i in 1:num_estrelas) {
    estrela_x <- largura * runif(1, 0.1, 0.9)
    estrela_y <- altura * runif(1, 0.75, 0.95)
    tamanho <- runif(1, 3, 6)
    
    desenhar_estrela(estrela_x, estrela_y, tamanho)
  }
  
  # Lua (tamanho customizável)
  lua_x <- largura * 0.85
  lua_y <- altura * 0.8
  
  # Halo da lua
  symbols(lua_x, lua_y, circles = tamanho_lua * 1.5, add = TRUE, 
          fg = NA, bg = rgb(rgb_estrelas[1], rgb_estrelas[2], rgb_estrelas[3], alpha = 0.4), 
          inches = FALSE)
  
  # Corpo da lua
  symbols(lua_x, lua_y, circles = tamanho_lua, add = TRUE, 
          fg = NA, bg = cor_estrelas, inches = FALSE)
  
  # Raios da lua
  for (angulo in seq(0, 2*pi, length.out = 12)) {
    x_fim <- lua_x + cos(angulo) * tamanho_lua * 2
    y_fim <- lua_y + sin(angulo) * tamanho_lua * 2
    lines(c(lua_x, lua_x + cos(angulo) * tamanho_lua * 1.3), 
          c(lua_y, lua_y + sin(angulo) * tamanho_lua * 1.3), 
          col = cor_estrelas, lwd = 3)
  }
  
  # ===============================================================================
  # ASSINATURA ARTÍSTICA
  # ===============================================================================
  
  text(largura * 0.02, altura * 0.02, 
       "Reprodução Digital: Diogo Rego (UFPB) - Pixel Poesia R", 
       col = "#ffffff", cex = 0.6, adj = 0)
  
  text(largura * 0.02, altura * 0.05, 
       "Obra Original: A Noite Estrelada - Vincent van Gogh (1889)", 
       col = "#cccccc", cex = 0.5, adj = 0)
  
  # Fechar dispositivo e retornar caminho do arquivo
  dev.off()
  return(temp_file)
}

# ===============================================================================
# INTERFACE SHINY (VERSÃO SIMPLIFICADA PARA COMPATIBILIDADE)
# ===============================================================================

# Como não temos acesso aos pacotes Shiny instalados, vou criar uma versão
# que simula a interatividade através de diferentes configurações pré-definidas

cat("=== APLICAÇÃO INTERATIVA: A NOITE ESTRELADA ===\n")
cat("Autor: Diogo Rego - Estudante de Estatística UFPB\n")
cat("Projeto: Pixel Poesia R\n")
cat("GitHub: https://github.com/Diogorego20/pixel-poesia-r\n\n")

# ===============================================================================
# DEMONSTRAÇÃO DE DIFERENTES ESTILOS ARTÍSTICOS
# ===============================================================================

# Estilo 1: Clássico (fiel ao original)
cat("Criando versão CLÁSSICA...\n")
arquivo1 <- criar_noite_estrelada_interativa(
  largura = 800, altura = 600,
  num_estrelas = 11,
  intensidade_espirais = 6,
  tamanho_lua = 15,
  cor_ceu = "#1e3a8a",
  cor_estrelas = "#ffeb3b",
  mostrar_cipreste = TRUE,
  mostrar_vila = TRUE,
  estilo_pincelada = "organico"
)
file.copy(arquivo1, "noite_estrelada_classica.png")

# Estilo 2: Minimalista
cat("Criando versão MINIMALISTA...\n")
arquivo2 <- criar_noite_estrelada_interativa(
  largura = 800, altura = 600,
  num_estrelas = 5,
  intensidade_espirais = 3,
  tamanho_lua = 12,
  cor_ceu = "#2d3748",
  cor_estrelas = "#f7fafc",
  mostrar_cipreste = FALSE,
  mostrar_vila = FALSE,
  estilo_pincelada = "geometrico"
)
file.copy(arquivo2, "noite_estrelada_minimalista.png")

# Estilo 3: Expressivo
cat("Criando versão EXPRESSIVA...\n")
arquivo3 <- criar_noite_estrelada_interativa(
  largura = 800, altura = 600,
  num_estrelas = 15,
  intensidade_espirais = 10,
  tamanho_lua = 20,
  cor_ceu = "#1a202c",
  cor_estrelas = "#fbd38d",
  mostrar_cipreste = TRUE,
  mostrar_vila = TRUE,
  estilo_pincelada = "expressivo"
)
file.copy(arquivo3, "noite_estrelada_expressiva.png")

# Estilo 4: Contemporâneo
cat("Criando versão CONTEMPORÂNEA...\n")
arquivo4 <- criar_noite_estrelada_interativa(
  largura = 800, altura = 600,
  num_estrelas = 8,
  intensidade_espirais = 4,
  tamanho_lua = 18,
  cor_ceu = "#2b6cb0",
  cor_estrelas = "#ed8936",
  mostrar_cipreste = TRUE,
  mostrar_vila = TRUE,
  estilo_pincelada = "organico"
)
file.copy(arquivo4, "noite_estrelada_contemporanea.png")

# Limpeza dos arquivos temporários
unlink(c(arquivo1, arquivo2, arquivo3, arquivo4))

cat("\n=== GALERIA DE ESTILOS CRIADA COM SUCESSO ===\n")
cat("Arquivos gerados:\n")
cat("• noite_estrelada_classica.png - Fiel ao original de Van Gogh\n")
cat("• noite_estrelada_minimalista.png - Versão simplificada e moderna\n")
cat("• noite_estrelada_expressiva.png - Intensificação dos elementos dramáticos\n")
cat("• noite_estrelada_contemporanea.png - Reinterpretação com cores atuais\n")
cat("\nCada versão demonstra como a tecnologia atual permite aos artistas\n")
cat("explorar diferentes interpretações de obras clássicas através da programação.\n")
cat("===============================================\n")

