# Packages
library(shiny)
library(bslib)
library(dplyr)
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
  dbdir = "../inova_redes/aih.duckdb",
  read_only = TRUE
)

# Read events data
events <- readRDS("../inova_redes/events.rds")

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
proc <- readRDS("data/proc.rds")

# Interface
ui <- page_navbar(
  title = "EERAS Brasil",
  theme = bs_theme(bootswatch = "shiny"),

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
      choices = NULL
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
      max = 390,
      step = 30,
      value = 90
    ),
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
    title = "Fluxo",

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
    title = "Região de saúde",

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
  ),

  # About page
  nav_panel(
    title = "Sobre o projeto",
    card(
      card_header("Card title"),
      p("Bla bla bla.")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Título A",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título B",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título C",
        p("Bla bla bla.")
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

  # Procedures available on selection
  proc_avail <- reactive({
    req(input$mun)
    req(input$date_even)
    req(input$horizon)

    date_even_1 <- as.Date(input$date_even) - input$horizon
    date_even_2 <- as.Date(input$date_even) + input$horizon

    proc_vct <- tbl(con, "aih") |>
      filter(munic_res == input$mun | munic__mov == input$mun) |>
      filter(dt_inter >= date_even_1 & dt_inter <= date_even_2) |>
      select(proc_rea) |>
      distinct(proc_rea) |>
      collect() |>
      pull(proc_rea)

    proc |>
      filter(cod %in% proc_vct)
  })

  # Update procedure selectors
  observe({
    proc_groups <- proc_avail() |>
      select(grupo) |>
      distinct(grupo) |>
      pull(grupo)

    updateSelectizeInput(
      session = session,
      inputId = "proc_group",
      server = FALSE,
      choices = c("Todos", proc_groups)
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
    if (input$proc_group != "Todos") {
      proc_vct <- proc |>
        filter(grupo == input$proc_group) |>
        select(cod) |>
        pull(cod)

      query <- query |>
        filter(proc_rea %in% proc_vct)
    }

    # Modify dates to aggregate
    query <- query |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit))

    query |>
      group_by(dt_inter, munic_res, munic__mov) |>
      summarise(freq = n()) |>
      ungroup() |>
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
    tipo <- ifelse(
      test = input$proc_group == "Todos",
      yes = "",
      no = input$proc_group
    )
    horizon <- input$horizon

    iframe_url <- glue(
      "http://157.86.68.234/inova/fluxo/sincro2.php?uf={uf}&anos={anos}&evento={evento}&tipo={tipo}&horizon={horizon}"
    )

    tags$iframe(src = iframe_url, width = "100%", height = "800px")
  })

  output$net_local <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      pad_by_time(dt_inter, .pad_value = 0) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date) |>
      v_scale_y_continuous(min = 0)
  })

  output$net_in <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res != input$mun) |>
      filter(!munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      pad_by_time(dt_inter, .pad_value = 0) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date) |>
      v_scale_y_continuous(min = 0)
  })

  output$net_out <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res == input$mun) |>
      filter(!munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      pad_by_time(dt_inter, .pad_value = 0) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date) |>
      v_scale_y_continuous(min = 0)
  })

  # Render sankeys
  output$sankey_out_1 <- renderVchart({
    # Sent to others, before
    aih_data() |>
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
  })

  output$sankey_out_2 <- renderVchart({
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
  })

  output$sankey_in_1 <- renderVchart({
    # Received from others, before
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
  })

  output$sankey_in_2 <- renderVchart({
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
    if (input$proc_group != "Todos") {
      proc_vct <- proc |>
        filter(grupo == input$proc_group) |>
        select(cod) |>
        pull(cod)

      query <- query |>
        filter(proc_rea %in% proc_vct)
    }

    # Modify dates to aggregate
    query <- query |>
      mutate(dt_inter = floor_date(dt_inter, unit = input$time_unit))

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

    i_res <- graph_from_data_frame(res)
    v_res <- toVisNetworkData(i_res)

    visNetwork(
      nodes = v_res$nodes,
      edges = v_res$edges
    )
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

    i_res <- graph_from_data_frame(res)
    v_res <- toVisNetworkData(i_res)

    visNetwork(
      nodes = v_res$nodes,
      edges = v_res$edges
    )
  })
}

shinyApp(ui, server)
