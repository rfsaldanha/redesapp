# Packages
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(timetk)
library(tidyr)
library(DBI)
library(duckdb)
library(sf)
library(DT)
library(vchartr)
library(scales)
library(brpop)
library(igraph)
library(visNetwork)
library(glue)

# Connect to AIH database
con <- dbConnect(
  duckdb(),
  dbdir = "data/aih.duckdb",
  read_only = TRUE
)

# Database tables
tb_aih <- tbl(con, "aih")

# Read events data
events <- readRDS("data/events.rds")

# Read municipality seats data
mun_seats <- readRDS("data/mun_seats.rds") |>
  filter(code_muni %in% unique(events$code_muni))

# Municipality list for selector
mun_names <- mun_seats$code_muni
names(mun_names) <- paste(mun_seats$name_muni, "-", mun_seats$abbrev_state)

# Municipality codes and names
ref_mun_names <- mun_seats |>
  st_drop_geometry() |>
  select(code_muni, name_muni, abbrev_state) |>
  mutate(name_muni = paste(name_muni, "-", abbrev_state)) |>
  select(-abbrev_state) |>
  as_tibble()

# Time units for selector
time_unit_list <- c("day", "week", "month")
names(time_unit_list) <- c("Dia", "Semana", "Mês")

# Read procedures table
# proc <- readRDS("data/proc.rds")
proc <- read_csv2(file = "data/proc.csv")

# Interface
ui <- page_navbar(
  title = "FluxSUS",
  theme = bs_theme() |>
    bs_add_rules(
      list(
        sass::sass_file("www/style.scss")
      )
    ),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Sidebar
  sidebar = sidebar(
    # Select municipality
    selectizeInput(
      inputId = "mun",
      label = "Município",
      choices = NULL
    ),

    # Select extreme event
    selectizeInput(
      inputId = "cat_even",
      label = "Categoria do evento",
      choices = NULL
    ),

    selectizeInput(
      inputId = "date_even",
      label = "Data do evento",
      choices = NULL
    ),

    # Select procedure
    selectizeInput(
      inputId = "proc_group",
      label = "Grupo de procedimentos",
      choices = c(
        "Todos" = "12",
        "Trauma- Internação Clínica ou Cirúrgica em todas as idades" = "1",
        "Idoso - Internação Cirúrgica (65 a mais)" = "2",
        "Idoso - Internação Clínica (65 a mais)" = "3",
        "Internação Obstétrica para Parto Cesariano (mulheres de 15 a 49)" = "4",
        "Pediatria - Internação clínica" = "5",
        "Internação ginecológica clínica ou cirúrgica" = "6",
        "Adulto- Internação Clínica (Todos, 15 a 64)" = "7",
        "Internação Obstétrica para Parto Normal (mulheres de 15 a 49)" = "8",
        "Mulher - Internação Obstétrica para Curetagem pós-aborto (mulheres de 15 a 49)" = "9",
        "Pediatria- Internação cirúrgica" = "10",
        "Adulto- Internação Cirúrgica de baixa e media complexidade (Todos, 15 a 64)" = "11"
      ),
      selected = "Todos"
    ),

    # Select unit
    selectizeInput(
      inputId = "time_unit",
      label = "Unidade de tempo",
      choices = time_unit_list,
      selected = "week"
    ),

    # Select horizon
    sliderInput(
      inputId = "horizon",
      label = "Horizonte",
      min = 30,
      max = 180,
      step = 30,
      value = 90
    ),
  ),

  # Home page
  nav_panel(
    title = "Início",
    card(
      card_header("FluxSUS"),
      tags$p(HTML(
        "O FluxSUS é um sistema de informações geográficas que oferece dados, análises e visualizações sobre os impactos de desastres e eventos climáticos extremos na rede de assistência à saúde brasileira. Desenvolvido pela Fiocruz em parceria com instituições nacionais e internacionais, o sistema integra informações do Sistema de Informações Hospitalares do SUS (<a href='https://datasus.saude.gov.br/acesso-a-informacao/producao-hospitalar-sih-sus/'>SIH/SUS</a>) e do Sistema Integrado de Informações sobre Desastres (<a href='https://s2id.mi.gov.br/'>S2iD</a>) para mapear fluxos de pacientes, identificar sobrecargas no sistema de saúde e caracterizar padrões de deslocamento populacional durante emergências."
      )),
      tags$p(HTML(
        "Esta plataforma foi criada para apoiar profissionais de saúde, gestores públicos e pesquisadores na tomada de decisões baseadas em evidências, no planejamento de ações de resposta a emergências e na prevenção de riscos à saúde pública. Por meio de painéis interativos, mapas geoespaciais e análises epidemiológicas, o FluxSUS permite compreender como desastres afetam o acesso aos serviços de saúde, facilitando a otimização da distribuição de recursos e o fortalecimento da capacidade de resposta do sistema de saúde em cenários de crise. Este projeto segue a Classificação e Codificação Brasileira de Desastres (<a href='https://www.gov.br/mdr/pt-br/centrais-de-conteudo/publicacoes/protecao-e-defesa-civil-sedec/DOCU_cobrade2.pdf'>Cobrade</a>), resumida a seguir:"
      )),
      p(
        "Geológico - eventos como movimentação de massa, deslizamentos e erosão entre outros;"
      ),
      p(
        "Hidrológico - eventos como inundações, enxurradas e alagamentos entre outros;"
      ),
      p(
        "Meteorológico - eventos como tempestades e temperaturas extremas entre outros;"
      ),
      p(
        "Climatológico - eventos como seca, estiagem e baixa humidade do ar entre outros."
      ),
      p(
        "Explore nossos painéis e relatórios para acessar informações estratégicas que contribuem para um sistema de saúde mais resiliente e preparado para enfrentar os desafios impostos por eventos climáticos extremos."
      )
    )
  ),

  # Time series page
  nav_panel(
    title = "Série temporal",
    # Cards
    card(
      card_header("Internações do próprio município"),
      card_body(class = "p-0", vchartOutput(outputId = "net_local"))
    ),
    card(
      card_header("Internações enviadas para outros municípios"),
      card_body(class = "p-0", vchartOutput(outputId = "net_out"))
    ),
    card(
      card_header("Internações recebidas de outros municípios"),
      card_body(class = "p-0", vchartOutput(outputId = "net_in"))
    )
  ),

  # Map page
  nav_panel(
    title = "Mapa de fluxos",
    # Cards
    card(
      card_body(class = "p-0", htmlOutput("map_iframe"))
    )
  ),

  # Sankey page
  nav_panel(
    title = "Diagrama de fluxo",

    # Graphs card
    accordion(
      multiple = TRUE,
      accordion_panel(
        "Antes do evento",
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_header("Internações enviadas para outros municípios"),
            vchartOutput(outputId = "sankey_out_1")
          ),
          card(
            card_header("Internações recebidas de outros municípios"),
            vchartOutput(outputId = "sankey_in_1")
          )
        )
      ),
      accordion_panel(
        "Depois do evento",
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_header("Internações enviadas para outros municípios"),
            vchartOutput(outputId = "sankey_out_2")
          ),
          card(
            card_header("Internações recebidas de outros municípios"),
            vchartOutput(outputId = "sankey_in_2")
          )
        )
      )
    )
  ),

  # Graphs page
  nav_panel(
    title = "Grafo",

    # Graphs card
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Antes do evento"),
        visNetworkOutput(outputId = "graph_1")
      ),
      card(
        card_header("Após o evento"),
        visNetworkOutput(outputId = "graph_2")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Fill municipality selector and select from url parameter
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[['codMun']])) {
      updateSelectizeInput(
        session = session,
        server = TRUE,
        inputId = "mun",
        choices = mun_names,
        selected = query[['codMun']]
      )
    } else {
      updateSelectizeInput(
        session = session,
        server = TRUE,
        inputId = "mun",
        choices = mun_names
      )
    }
  })

  # Update categories
  observe({
    categ <- events |>
      filter(code_muni == input$mun) |>
      select(categoria) |>
      filter(!(categoria %in% c("Biológicos", "Tecnológicos"))) |>
      distinct(categoria) |>
      arrange(categoria) |>
      pull(categoria)

    updateSelectizeInput(
      session = session,
      inputId = "cat_even",
      server = FALSE,
      choices = categ
    )
  })

  # Update dates
  observe({
    dates <- events |>
      filter(
        code_muni == input$mun,
        categoria == input$cat_even
      ) |>
      select(date) |>
      distinct(date) |>
      arrange(date) |>
      pull(date)

    updateSelectizeInput(
      session = session,
      inputId = "date_even",
      server = FALSE,
      choices = dates
    )
  })

  # AIH data
  aih_data <- reactive({
    req(input$mun)
    req(input$date_even)
    req(input$proc_group)
    req(input$horizon)

    # Reference dates around event date
    date_even_1 <- as.Date(input$date_even) - input$horizon
    date_even_2 <- as.Date(input$date_even) + input$horizon

    # Query table about municipality and dates
    query <- tbl(con, "aih") |>
      filter(munic_res == input$mun | munic__mov == input$mun) |>
      filter(dt_inter >= date_even_1 & dt_inter <= date_even_2)

    # Filter procedure
    if (input$proc_group != "12") {
      proc_vct <- proc |>
        filter(cod_grupo == input$proc_group) |>
        select(cod) |>
        pull(cod)

      query <- query |>
        filter(proc_rea %in% proc_vct)
    }

    # Modify dates to aggregate
    query <- query |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit))

    # Collect and return
    query |>
      group_by(dt_inter, munic_res, munic__mov) |>
      summarise(freq = n()) |>
      ungroup() |>
      arrange(dt_inter, munic_res, munic__mov) |>
      collect()
  })

  # Flow map iframe
  output$map_iframe <- renderUI({
    req(input$mun)
    req(input$date_even)
    req(input$proc_group)
    req(input$horizon)

    # Iframe variables
    uf <- substr(input$mun, 0, 2)
    anos <- substr(input$date_even, 0, 4)
    evento <- glue("{input$mun}{input$date_even}")

    tipo <- input$proc_group

    if (tipo == 12) {
      tipo <- ""
    }

    horizon <- input$horizon

    iframe_url <- glue(
      "https://homologacao-mapas.icict.fiocruz.br/inova/sincro2.php?uf={uf}&anos={anos}&evento={evento}&tipo={tipo}&horizon={horizon}"
    )

    print(iframe_url)

    tags$iframe(src = iframe_url, width = "100%", height = "800px")
  })

  # Time series graphs
  output$net_local <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res == munic__mov) |>
      complete(
        dt_inter = seq.Date(min_date, max_date, by = "day"),
        fill = list(freq = 0)
      ) |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit)) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date)
  })

  output$net_in <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res != munic__mov) |>
      filter(munic__mov == input$mun) |>
      complete(
        dt_inter = seq.Date(min_date, max_date, by = "day"),
        fill = list(freq = 0)
      ) |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit)) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date)
  })

  output$net_out <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res != munic__mov) |>
      filter(munic_res == input$mun) |>
      complete(
        dt_inter = seq.Date(min_date, max_date, by = "day"),
        fill = list(freq = 0)
      ) |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit)) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date)
  })

  # Render sankeys
  output$sankey_out_1 <- renderVchart({
    aih_data <- aih_data()

    print(aih_data)

    # Sent to others, before
    if (nrow(aih_data) > 0) {
      aih_data |>
        filter(munic_res != munic__mov) |>
        filter(munic_res == input$mun & munic__mov != input$mun) |>
        filter(dt_inter < input$date_even) |>
        group_by(munic_res, munic__mov) |>
        summarise(freq = sum(freq, na.rm = TRUE)) |>
        ungroup() |>
        left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
        select(-munic_res) |>
        rename(munic_res = name_muni) |>
        left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
        select(-munic__mov) |>
        rename(munic__mov = name_muni) |>
        vchart() |>
        v_sankey(aes(munic__mov, munic_res, value = freq))
    } else {
      vchart() %>%
        v_labs(
          title = "Sem dados suficientes para o gráfico."
        )
    }
  })

  output$sankey_out_2 <- renderVchart({
    aih_data <- aih_data()

    if (nrow(aih_data) > 0) {
      aih_data() |>
        filter(munic_res != munic__mov) |>
        filter(munic_res == input$mun & munic__mov != input$mun) |>
        filter(dt_inter >= input$date_even) |>
        group_by(munic_res, munic__mov) |>
        summarise(freq = sum(freq, na.rm = TRUE)) |>
        ungroup() |>
        left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
        select(-munic_res) |>
        rename(munic_res = name_muni) |>
        left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
        select(-munic__mov) |>
        rename(munic__mov = name_muni) |>
        vchart() |>
        v_sankey(aes(munic__mov, munic_res, value = freq))
    } else {
      vchart() %>%
        v_labs(
          title = "Sem dados suficientes para o gráfico."
        )
    }
  })

  output$sankey_in_1 <- renderVchart({
    # Received from others, before
    aih_data <- aih_data()

    if (nrow(aih_data) > 0) {
      aih_data() |>
        filter(munic_res != munic__mov) |>
        filter(munic_res != input$mun & munic__mov == input$mun) |>
        filter(dt_inter < input$date_even) |>
        group_by(munic_res, munic__mov) |>
        summarise(freq = sum(freq, na.rm = TRUE)) |>
        ungroup() |>
        left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
        select(-munic_res) |>
        rename(munic_res = name_muni) |>
        left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
        select(-munic__mov) |>
        rename(munic__mov = name_muni) |>
        vchart() |>
        v_sankey(aes(munic__mov, munic_res, value = freq))
    } else {
      vchart() %>%
        v_labs(
          title = "Sem dados suficientes para o gráfico."
        )
    }
  })

  output$sankey_in_2 <- renderVchart({
    aih_data <- aih_data()

    if (nrow(aih_data) > 0) {
      aih_data() |>
        filter(munic_res != munic__mov) |>
        filter(munic_res != input$mun & munic__mov == input$mun) |>
        filter(dt_inter >= input$date_even) |>
        group_by(munic_res, munic__mov) |>
        summarise(freq = sum(freq, na.rm = TRUE)) |>
        ungroup() |>
        left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
        select(-munic_res) |>
        rename(munic_res = name_muni) |>
        left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
        select(-munic__mov) |>
        rename(munic__mov = name_muni) |>
        vchart() |>
        v_sankey(aes(munic__mov, munic_res, value = freq))
    } else {
      vchart() %>%
        v_labs(
          title = "Sem dados suficientes para o gráfico."
        )
    }
  })

  # AIH data on health region
  aih_data_reg <- reactive({
    req(input$mun)
    req(input$date_even)
    req(input$proc_group)
    req(input$horizon)

    # Reference dates around event date
    date_even_1 <- as.Date(input$date_even) - input$horizon
    date_even_2 <- as.Date(input$date_even) + input$horizon

    # Identify municipality's region
    reg <- mun_reg_saude_449 |>
      filter(code_muni == input$mun) |>
      pull(codi_reg_saude)

    # Municiality list within region
    list_mun <- mun_reg_saude_449 |>
      filter(codi_reg_saude == reg) |>
      pull(code_muni)

    # Query table about municipality list and dates
    query <- tbl(con, "aih") |>
      filter(munic_res %in% list_mun | munic__mov %in% list_mun) |>
      filter(dt_inter >= date_even_1 & dt_inter <= date_even_2)

    # Filter procedure
    if (input$proc_group != "12") {
      proc_vct <- proc |>
        filter(cod_grupo == input$proc_group) |>
        select(cod) |>
        pull(cod)

      query <- query |>
        filter(proc_rea %in% proc_vct)
    }

    query |>
      group_by(dt_inter, munic_res, munic__mov) |>
      summarise(freq = n()) |>
      ungroup() |>
      collect()
  })

  # Render graphs
  output$graph_1 <- renderVisNetwork({
    res <- aih_data_reg() |>
      filter(!munic_res == munic__mov) |>
      filter(dt_inter < input$date_even) |>
      group_by(munic_res, munic__mov) |>
      summarise(freq = sum(freq, na.rm = TRUE)) |>
      ungroup() |>
      left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
      select(-munic_res) |>
      rename(munic_res = name_muni) |>
      left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
      select(-munic__mov) |>
      rename(munic__mov = name_muni, value = freq) |>
      select(munic_res, munic__mov, value)

    if (nrow(res) > 0) {
      i_res <- graph_from_data_frame(res)
      v_res <- toVisNetworkData(i_res)

      visNetwork(
        nodes = v_res$nodes,
        edges = v_res$edges
      ) |>
        visEdges(arrows = 'to', scaling = list(min = 2, max = 2))
    } else {
      visNetwork(
        nodes = list(0),
        edges = list(0),
        main = "Sem dados suficientes para o gráfico."
      )
    }
  })

  output$graph_2 <- renderVisNetwork({
    res <- aih_data_reg() |>
      filter(!munic_res == munic__mov) |>
      filter(dt_inter > input$date_even) |>
      group_by(munic_res, munic__mov) |>
      summarise(freq = sum(freq, na.rm = TRUE)) |>
      ungroup() |>
      left_join(ref_mun_names, by = c("munic_res" = "code_muni")) |>
      select(-munic_res) |>
      rename(munic_res = name_muni) |>
      left_join(ref_mun_names, by = c("munic__mov" = "code_muni")) |>
      select(-munic__mov) |>
      rename(munic__mov = name_muni, value = freq) |>
      select(munic_res, munic__mov, value)

    if (nrow(res) > 0) {
      i_res <- graph_from_data_frame(res)
      v_res <- toVisNetworkData(i_res)

      visNetwork(
        nodes = v_res$nodes,
        edges = v_res$edges
      ) |>
        visEdges(arrows = 'to', scaling = list(min = 2, max = 2))
    } else {
      visNetwork(
        nodes = list(0),
        edges = list(0),
        main = "Sem dados suficientes para o gráfico."
      )
    }
  })
}

shinyApp(ui, server)
