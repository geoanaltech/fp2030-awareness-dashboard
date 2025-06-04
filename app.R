# ────────────────────────────────────────────────────────────────────────────────
# app.R  –  Shiny Dashboard Part 4: Interactive Demographics + Filters
# ────────────────────────────────────────────────────────────────────────────────

# 1. Load required libraries (including DT for datatables)
library(shiny)
library(tidyverse)
library(DT)

# 2. Load the cleaned data
df_clean <- read_csv("FP2030_cleaned_NE_SE.csv") %>%
  mutate(
    state_of_residence = str_trim(state_of_residence),
    age_group           = str_trim(age_group),
    area_type           = str_trim(area_type),
    sex                 = str_trim(sex),
    fp2030_policy_awarenes = str_trim(fp2030_policy_awarenes),
    barriers_final      = str_trim(barriers_final)
  )

# 3. UI: Sidebar with filters + Main panel with tabs
ui <- fluidPage(
  titlePanel("FP2030 Awareness & Barriers – NE & SE"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "state",
        label    = "Select State:",
        choices  = sort(unique(df_clean$state_of_residence)),
        selected = NULL,
        multiple = TRUE
      ),
      selectInput(
        inputId  = "area",
        label    = "Select Area Type:",
        choices  = c("All", "Urban", "Rural"),
        selected = "All"
      ),
      selectInput(
        inputId  = "age",
        label    = "Select Age Group:",
        choices  = c("All", unique(df_clean$age_group)),
        selected = "All"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Demographics",
          fluidRow(
            column(
              width = 4,
              plotOutput("plot_by_sex", height = "300px")
            ),
            column(
              width = 4,
              plotOutput("plot_by_age", height = "300px")
            ),
            column(
              width = 4,
              plotOutput("plot_by_area", height = "300px")
            )
          )
        ),
        
        tabPanel(
          "Summary",
          h4("Overall Awareness"),
          textOutput("overall_pct"),
          hr(),
          h4("Respondent Counts"),
          DT::dataTableOutput("demog_table"),
          hr(),
          h4("Top Barriers"),
          DT::dataTableOutput("barrier_table")
        ),
        
        tabPanel(
          "Plots",
          plotOutput("awareness_by_state_plot"),
          hr(),
          plotOutput("barrier_by_area_plot")
        )
      )
    )
  )
)

# 4. Server: reactives + renderers
server <- function(input, output, session) {
  # 4.1 Reactive: Filter dataset based on sidebar inputs
  df_reactive <- reactive({
    tmp <- df_clean
    
    # Filter by state if any are selected
    if (!is.null(input$state) && length(input$state) > 0) {
      tmp <- tmp %>% filter(state_of_residence %in% input$state)
    }
    
    # Filter by area_type if not "All"
    if (input$area != "All") {
      tmp <- tmp %>% filter(area_type == input$area)
    }
    
    # Filter by age_group if not "All"
    if (input$age != "All") {
      tmp <- tmp %>% filter(age_group == input$age)
    }
    
    tmp
  })
  
  # 4.2 Tab “Demographics” – Respondent distributions
  
  # 4.2.1 Plot: Distribution by Sex
  output$plot_by_sex <- renderPlot({
    df0 <- df_reactive()
    if (nrow(df0) == 0) return(NULL)
    
    sex_df <- df0 %>%
      count(sex) %>%
      mutate(pct = round(100 * n / sum(n), 1))
    
    ggplot(sex_df, aes(x = sex, y = n, fill = sex)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(n, " (", pct, "%)")),
                vjust = -0.5, size = 3.5) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(
        title = "Respondents by Sex",
        x     = "Sex",
        y     = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # 4.2.2 Plot: Distribution by Age Group
  output$plot_by_age <- renderPlot({
    df0 <- df_reactive()
    if (nrow(df0) == 0) return(NULL)
    
    age_df <- df0 %>%
      count(age_group) %>%
      mutate(pct = round(100 * n / sum(n), 1))
    
    ggplot(age_df, aes(x = reorder(age_group, -n), y = n, fill = age_group)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(n, " (", pct, "%)")),
                vjust = -0.5, size = 3.5) +
      scale_fill_brewer(palette = "Pastel2") +
      labs(
        title = "Respondents by Age Group",
        x     = "Age Group",
        y     = "Count"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # 4.2.3 Plot: Distribution by Area Type
  output$plot_by_area <- renderPlot({
    df0 <- df_reactive()
    if (nrow(df0) == 0) return(NULL)
    
    area_df <- df0 %>%
      count(area_type) %>%
      mutate(pct = round(100 * n / sum(n), 1))
    
    ggplot(area_df, aes(x = area_type, y = n, fill = area_type)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(n, " (", pct, "%)")),
                vjust = -0.5, size = 3.5) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Respondents by Area Type",
        x     = "Area Type",
        y     = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # 4.3 Tab “Summary” – Overall awareness, respondent counts, top barriers
  
  # 4.3.1 Overall awareness (Text)
  output$overall_pct <- renderText({
    df0 <- df_reactive()
    if (nrow(df0) == 0) {
      return("No data to calculate awareness.")
    }
    pct_yes <- round(100 * sum(df0$fp2030_policy_awarenes == "Yes") / nrow(df0), 1)
    paste0("FP2030 Awareness: ", pct_yes, "%  (n=", nrow(df0), ")")
  })
  
  # 4.3.2 Respondent counts (datatable)
  output$demog_table <- DT::renderDataTable({
    df0 <- df_reactive()
    if (nrow(df0) == 0) {
      empty_df <- tibble(
        State     = character(0),
        Area      = character(0),
        AgeGroup  = character(0),
        Sex       = character(0),
        Count     = integer(0)
      )
      return(DT::datatable(empty_df, rownames = FALSE))
    }
    
    demog_counts <- df0 %>%
      count(
        state_of_residence,
        area_type,
        age_group,
        sex,
        name = "Count"
      ) %>%
      rename(
        State    = state_of_residence,
        Area     = area_type,
        AgeGroup = age_group,
        Sex      = sex
      )
    
    DT::datatable(
      demog_counts,
      rownames = FALSE,
      colnames = c("State", "Area", "Age Group", "Sex", "Count"),
      options  = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # 4.3.3 Top barriers (datatable)
  output$barrier_table <- DT::renderDataTable({
    df0 <- df_reactive()
    if (nrow(df0) == 0) {
      return(DT::datatable(tibble(Barrier = character(0), Count = integer(0))))
    }
    top_barriers <- df0 %>%
      filter(!is.na(barriers_final) & barriers_final != "") %>%
      separate_rows(barriers_final, sep = ";") %>%
      mutate(barrier = str_trim(barriers_final)) %>%
      filter(barrier != "") %>%
      count(barrier, sort = TRUE) %>%
      slice(1:5)
    
    DT::datatable(
      top_barriers,
      rownames = FALSE,
      colnames = c("Barrier", "Count"),
      options  = list(pageLength = 5, dom = "t")
    )
  })
  
  # 4.4 Tab “Plots” – Awareness by state; Barriers by area type
  
  # 4.4.1 Awareness rate by state (bar chart)
  output$awareness_by_state_plot <- renderPlot({
    df0 <- df_reactive()
    if (nrow(df0) == 0) return(NULL)
    
    plot_df <- df0 %>%
      group_by(state_of_residence) %>%
      summarize(
        pct_yes = round(100 * sum(fp2030_policy_awarenes == "Yes") / n(), 1),
        .groups = "drop"
      )
    
    ggplot(plot_df, aes(x = reorder(state_of_residence, -pct_yes), y = pct_yes, fill = pct_yes)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "FP2030 Awareness Rate by State",
        x     = "State of Residence",
        y     = "Percent Aware"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # 4.4.2 Top 3 barriers by area type (bar chart)
  output$barrier_by_area_plot <- renderPlot({
    df0 <- df_reactive()
    if (nrow(df0) == 0) return(NULL)
    
    plot_df <- df0 %>%
      filter(!is.na(barriers_final) & barriers_final != "") %>%
      separate_rows(barriers_final, sep = ";") %>%
      mutate(barrier = str_trim(barriers_final)) %>%
      filter(barrier != "") %>%
      group_by(area_type, barrier) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(area_type, desc(count)) %>%
      group_by(area_type) %>%
      slice_max(order_by = count, n = 3) %>%
      ungroup()
    
    ggplot(plot_df, aes(x = reorder(barrier, count), y = count, fill = area_type)) +
      geom_col(position = "dodge") +
      facet_wrap(~ area_type) +
      coord_flip() +
      labs(
        title = "Top 3 FP2030 Implementation Barriers by Area Type",
        x     = "Barrier",
        y     = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# 5. Run the application
shinyApp(ui, server)
