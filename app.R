# Packages
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DBI)
library(duckdb)
library(sf)
library(DT)
library(vchartr)
library(scales)

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

# Read procedures table
proc <- readRDS("data/proc.rds")

# Interface
ui <- page_navbar(
  title = "EERAS Brasil", 
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML('$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });')),
    tags$style(
      HTML('@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}')
    )),

  # Translation
  tags$script(
    HTML("
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
    ")
  ),

  # Map page
  nav_panel(
    title = "Internações",

    # Sidebar
    layout_sidebar(
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

        # Select horizon
        sliderInput(
          inputId = "horizon", 
          label = "Horizonte (dias)",
          min = 30, 
          max = 150,
          step = 30, 
          value = 90
        ),

      ),

      # Cards
      card(
        card_header("Internações do próprio muncípio"),
        card_body(class = "p-0", vchartOutput(outputId = "net_local"))
      ),
      card(
        card_header("Internações enviadas para outros muncípios"),
        card_body(class = "p-0", vchartOutput(outputId = "net_out"))
      ),
      card(
        card_header("Internações recebidas de outros municípios"),
        card_body(class = "p-0", vchartOutput(outputId = "net_in"))
      )

    )
  ),

  # Graphs page
  nav_panel(
    title = "Página B",

    layout_sidebar(
      sidebar = sidebar(
        
      ),

      # Graphs card
      card(
        full_screen = TRUE,
        card_header("Card header"),
        card_body(
          
        )
      )
    )
  ),

  # About page
  nav_panel(
    title = "Página B",
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

    if(!is.null(query[['codMun']])){
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

    date_even_1 <- as.Date(input$date_even) - input$horizon
    date_even_2 <- as.Date(input$date_even) + input$horizon

    query <- tbl(con, "aih") |>
      filter(munic_res == input$mun | munic__mov == input$mun) |>
      filter(dt_inter >= date_even_1 & dt_inter <= date_even_2)

    if(input$proc_group != "Todos"){
      proc_vct <- proc |>
        filter(grupo == input$proc_group) |>
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

  output$net_local <- renderVchart({
    tmp <- aih_data()

    min_date <- as.Date(input$date_even) - input$horizon
    max_date <- as.Date(input$date_even) + input$horizon

    tmp |>
      filter(munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      complete(dt_inter = seq.Date(min_date, max_date, by = "day"), fill = list(freq = 0)) |>
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
      filter(munic_res != input$mun) |>
      filter(!munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      complete(dt_inter = seq.Date(min_date, max_date, by = "day"), fill = list(freq = 0)) |>
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
      filter(munic_res == input$mun) |>
      filter(!munic_res == munic__mov) |>
      summarise(freq = sum(freq), .by = dt_inter) |>
      arrange(dt_inter) |>
      complete(dt_inter = seq.Date(min_date, max_date, by = "day"), fill = list(freq = 0)) |>
      vchart() |>
      v_line(aes(x = dt_inter, y = freq)) |>
      v_smooth(aes(x = dt_inter, y = freq)) |>
      v_mark_vline(
        x = as.Date(input$date_even),
        .label.text = "Evento"
      ) |>
      v_scale_x_date(min = min_date, max = max_date)
  })
}

shinyApp(ui, server)