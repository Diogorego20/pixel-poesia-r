# ===============================================================================
# APLICAÇÃO SHINY PRINCIPAL: A NOITE ESTRELADA DE VAN GOGH
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# GitHub: https://github.com/Diogorego20/pixel-poesia-r
# Data: Agosto 2025
# ===============================================================================

# Este é o arquivo principal que combina a interface (UI) e o servidor (Server)
# para criar uma aplicação web interativa que reproduz "A Noite Estrelada"

# ===============================================================================
# CARREGAMENTO DE BIBLIOTECAS
# ===============================================================================

# Verificar e instalar pacotes necessários
pacotes_necessarios <- c("shiny", "colourpicker")

for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    cat("Instalando pacote:", pacote, "\n")
    install.packages(pacote, repos = "https://cran.rstudio.com/")
    library(pacote, character.only = TRUE)
  }
}

# ===============================================================================
# CARREGAMENTO DOS ARQUIVOS DE INTERFACE E SERVIDOR
# ===============================================================================

# Carregar interface do usuário
if (file.exists("ui.R")) {
  source("ui.R", local = TRUE)
} else {
  stop("Arquivo ui.R não encontrado! Certifique-se de que está no diretório correto.")
}

# Carregar lógica do servidor
if (file.exists("server.R")) {
  source("server.R", local = TRUE)
} else {
  stop("Arquivo server.R não encontrado! Certifique-se de que está no diretório correto.")
}

# ===============================================================================
# INICIALIZAÇÃO DA APLICAÇÃO
# ===============================================================================

# Criar e executar a aplicação Shiny
shinyApp(ui = ui, server = server)

# ===============================================================================
# INFORMAÇÕES SOBRE A APLICAÇÃO
# ===============================================================================

# Esta aplicação permite:
# 1. Ajustar parâmetros visuais da obra (cores, tamanhos, quantidades)
# 2. Escolher diferentes estilos de pincelada (orgânico, geométrico, expressivo)
# 3. Mostrar/ocultar elementos da paisagem (cipreste, vila, montanhas)
# 4. Aplicar presets artísticos pré-configurados
# 5. Visualizar a obra em tempo real
# 6. Fazer download da obra em alta resolução
# 
# A aplicação demonstra como artistas contemporâneos podem usar programação
# para reinterpretar obras clássicas, explorando diferentes possibilidades
# criativas através da tecnologia.
#
# Elementos técnicos implementados:
# - Geração procedural de espirais matemáticas
# - Sistema de gradientes de cor customizáveis
# - Algoritmos de posicionamento orgânico
# - Renderização em tempo real
# - Interface responsiva e intuitiva
# - Sistema de presets para diferentes estilos artísticos
# - Exportação em alta resolução
#
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Transformando código em arte

