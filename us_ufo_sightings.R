# Load necessary packages
library(tidyverse)
library(shiny)
library(bslib)
library(shinyWidgets)
library(scales)
library(DT)
library(sf)
library(leaflet)
library(maps)
library(plotly)
library(tidyr)

# Load data set
state_name <- state.name
names(state_name) <- state.abb

ufo_df <- read_csv("us_ufo_sightings_cleaned.csv") %>%
  filter(country == "us") %>%
  mutate(
    datetime = mdy(`date posted`),
    Year = year(datetime),
    Month = month(datetime),
    shape = case_when(
      shape %in% c("light", "triangle", "circle", "fireball", "unknown", "sphere", "disk") ~ shape,
      TRUE ~ "other"
    ),
    State_abb = str_to_upper(state),
    State_name = str_to_lower(state_name[State_abb])
  ) %>%
  select(-datetime, -`duration (hours/min)`, -comments) %>%
  rename("Duration" = `duration (seconds)`) %>%
  filter(!is.na(Duration) & Duration < 50000) %>%
  mutate(Duration = Duration / 3600)  # Convert to hours

usa <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

# === Part I: Shiny UI Interface
ui <- page_sidebar(
  # --- App Title
  title = "UFO Sighting Dashboard",
  
  # --- App Theme
  theme = bs_theme(
    bg = "#101010",
    fg = "#FFF",
    primary = "#E69F00",
    secondary = "#0072B2",
    success = "#609ee4",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  
  # --- Sidebar panel: Inputs
  sidebar = sidebar(
    
    # Input #1: Select the year range
    sliderInput(
      "year_range", label = "Select Years:",
      min = 1998, max = 2014, value = c(1998, 2014)
    ),
    
    # Input #2: Select the duration range
    sliderInput(
      "duration_range", label = "Select Duration (Hours):",
      min = 0, max = 3, value = c(0, 3), step = 0.1
    ),
    
    # Input #3: Include longer durations
    checkboxInput(
      "include_long_duration", label = "Include durations longer than 3 hours", value = FALSE
    ),
    
    # Input #4: Select the UFO shape
    checkboxGroupInput(
      "shape", label = "Reported Shape:",
      choices = rev(unique(ufo_df$shape)),
      selected = unique(ufo_df$shape)
    )
  ),
  
  img(src = "data:image/png;base64,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", width = "100%", style = "display: block; margin: 0 auto;"),
  
  # --- Main panel for displaying visualizations
  navset_card_underline(
    title = "Visualization",
    
    tags$head(tags$style('h4 {color:#E69F00;}')),
    
    # --- Panel 1: Spatial Distribution
    nav_panel("Interactive Map",
              fluidRow(
                column(6, leafletOutput("p1_map")),
                column(6, plotOutput("p1_state"))
              ),
              h4("Explanation"),
              p("• The first page displays the distribution of UFO sighting reports in the United States. The left image marks the coordinates of each sighting location with its state, city, latitude and longitude, while the right image shows the number of sighting reports in the United States by state, with darker colors indicating more reports.")
    ),
    
    # --- Panel 2: Time Series Output
    nav_panel("Reported Date",
              fluidRow(
                column(6, plotlyOutput("p2_main")),
                column(6, plotlyOutput("p2_year"))
              ),
              h4("Explanation"),
              p("• The second page displays the number of reports by year and month in the form of a time series. The left figure shows the trend of changes within the selected year range, while the right figure displays the compasiron of numbers of reports differentiated by month.")
    ),
    
    # --- Panel 3: Reported Duration
    nav_panel("Reported Duration",
              fluidRow(
                column(6, plotOutput("p3_main")),
                column(6, plotOutput("p3_violin"))
              ),
              h4("Explanation"),
              p("• The third page displays the distribution of UFO durations in sighting reports. The left image shows the frequency of UFO sightings by their reported duration in hours. The right image uses a violin plot to visualize the distribution of sighting durations, with the shape indicating the density of reports and a boxplot overlay to highlight the central tendency and spread of the data.")
    ),
    
    # --- Panel 4: Shape Distribution
    nav_panel("Reported Shape",
              fluidRow(
                column(12, plotOutput("p4_main"))
              ),
              h4("Explanation"),
              p("• The fourth page displays the distribution of UFO shapes in sighting reports. The Shape in the raw data is subdivided into dozens of types. After cleaning, 7 main categories are selected while the others all in 'other' class. The categories are ordered by their descending numbers.")
    ),
    
    # --- Panel 5: About
    nav_panel("About",
              h4("Data Description"),
              p("This dataset contains over 65,000 reports of UFO sightings over the last century. Since the reports date back to the 20th century, some older data might be obscured. Data contains city, state, time, description, and duration of each sighting. This dataset was scraped, geolocated, and time standardized from NUFORC data by Sigmond Axel"),
              h4("Data Preview"),
              fluidRow(
                column(12, dataTableOutput("p5_preview"))
              ),
              h4("Data Source"),
              p("Data Source: Kaggle UFO Sightings, Reports of unidentified flying object reports in the last century,",
                a("UFO Sightings Dataset on Kaggle", href = "https://www.kaggle.com/datasets/NUFORC/ufo-sightings/data", target = "_blank"))
    )
  )
)

# === Part II: Server Logic ===
server <- function(input, output, session) {
  
  # Reactive function to filter the original dataset based on user selection for year range, shape, and duration
  sel_ufo_df <- reactive({
    data <- ufo_df %>%
      filter(between(Year, input$year_range[1], input$year_range[2])) %>%
      filter(shape %in% input$shape)
    
    # Apply filters based on whether the user includes long-duration sightings
    if (input$include_long_duration) {
      data <- data %>%
        filter(Duration >= input$duration_range[1])  # Filter by minimum duration if longer durations included
    } else {
      data <- data %>%
        filter(between(Duration, input$duration_range[1], input$duration_range[2]))  # Filter within the specified duration range
    }
    
    data
  })
  
  # --- Panel 1: Interactive Map ---
  # Renders an interactive leaflet map showing UFO sightings based on the filtered data
  output$p1_map <- renderLeaflet({
    leaflet(sel_ufo_df()) %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        # Display information in the popup
        popup = ~paste(
          "<strong>State:</strong>", state, "<br>",
          "<strong>City:</strong>", city, "<br>",
          "<strong>Latitude:</strong>", latitude, "<br>",
          "<strong>Longitude:</strong>", longitude
        ),
        color = '#FFD700',      # Marker border color
        fillColor = '#FFD700',  # Marker fill color
        fillOpacity = 0.8,
        clusterOptions = markerClusterOptions()  # Enable marker clustering
      ) %>%
      setView(lat = 39.0119, lng = -98.4842, zoom = 3)  # Set default map view
  })
  
  # --- Panel 1: UFO Sightings by State ---
  # Renders a map of the US showing UFO sightings by state, with color scaling based on the number of reports
  output$p1_state <- renderPlot({
    state_df <- sel_ufo_df() %>%
      count(State_name)  # Count the number of sightings per state
    
    usa %>%
      left_join(state_df, by = c("ID" = "State_name")) %>%
      ggplot() +
      geom_sf(aes(fill = n), color = "#FFD700", lwd = 0.6) + 
      # Define the color scale representing the number of UFO sightings
      scale_fill_gradient2(
        "Reported Count",
        low = "#DAA520",   # Dark brown for low values
        mid = "#8B4513",   # Chestnut color for medium values
        high = "#3E2C1B",  
        midpoint = quantile(state_df$n, probs = 0.5, na.rm = TRUE),  # Adjust color midpoint
        na.value = "grey80",
        space = "Lab"  # Use CIELab color space for smoother transitions
      ) +
      ggtitle("UFO Sightings Reported Count by State in US\n") +
      theme_void() +
      guides(fill = guide_colorbar(barwidth = 20)) +  # Custom color bar for fill scale
      theme(
        text = element_text(size = 11, color = '#E69F00'),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",  
        plot.background = element_rect(fill = '#101010'),  
        panel.background = element_rect(fill = '#101010', colour = '#E69F00')  
      )
  }, bg = "transparent")

  # --- Panel 2: Time Series Output
  output$p2_main <- renderPlotly({
    data <- sel_ufo_df() %>%
      mutate(Date = as.Date(paste0(Year, "-", Month, "-01"))) %>%
      count(Date)
    
    fig <- plot_ly(
      data,
      x = ~Date,
      y = ~n,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = "#609ee4"),
      marker = list(color = "#609ee4", size = 4),
      hovertemplate = 'Date: %{x|%Y-%m-%d}<br>Count: %{y}<extra></extra>'
    )
    
    fig <- fig %>% layout(
      title = list(
        text = '<b>UFO Sightings Reported Time Series</b>',
        x = 0.5,
        xanchor = 'center',
        xref = 'paper',
        font = list(size = 16, color = '#E69F00', family = 'Arial')
      ),
      xaxis = list(
        title = list(
          text = 'Date',
          font = list(size = 13, color = '#E69F00', family = 'Arial')
        ),
        tickfont = list(size = 13, color = '#E69F00', family = 'Arial'),
        rangeselector = list(
          buttons = list(
            list(count = 1, label = '1 year', step = 'year', stepmode = 'backward'),
            list(step = 'all')
          )
        ),
        rangeslider = list(visible = TRUE),
        type = 'date',
        tickformat = '%Y',
        showgrid = FALSE,
        zeroline = FALSE,
        linecolor = '#E69F00',
        linewidth = 1,
        mirror = TRUE,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = 'Reported Count',
          font = list(size = 13, color = '#E69F00', family = 'Arial')
        ),
        tickfont = list(size = 13, color = '#E69F00', family = 'Arial'),
        showgrid = FALSE,
        zeroline = FALSE,
        linecolor = '#E69F00',
        linewidth = 1,
        mirror = TRUE,
        automargin = TRUE
      ),
      plot_bgcolor = '#101010',
      paper_bgcolor = '#101010',
      font = list(size = 11, color = '#E69F00', family = 'Arial'),
      hoverlabel = list(
        font = list(size = 11, color = 'white', family = 'Arial'),
        bgcolor = '#444444'
      ),
      margin = list(l = 30, r = 30, t = 30, b = 30)
    )
    
    fig
  })
  
  output$p2_year <- renderPlotly({
    library(tidyr)
    library(plotly)
    
    # Count the number of UFO reports for each month and ensure all months are represented (filling with 0 if missing)
    data <- sel_ufo_df() %>%
      count(Month) %>%
      complete(Month = 1:12, fill = list(n = 0))
    
    # Convert month numbers to abbreviations and calculate angles for polar plot
    data <- data %>%
      mutate(
        Month_label = factor(Month, levels = 1:12, labels = month.abb),
        theta = (Month - 1) * 30  # Set angle: each month occupies 30 degrees
      )
    
    # Custom color gradient (e.g., different shades of orange)
    colors <- colorRampPalette(c("#E69F00"))(length(data$n))
    
    # Create polar bar chart for UFO reports by month
    fig <- plot_ly(
      type = 'barpolar',
      r = data$n,                    # Heights of the bars representing report counts
      theta = data$theta,            # Positions of the bars in polar coordinates
      text = paste0("Month: ", data$Month_label, "<br>Count: ", data$n),  # Hover text showing month and count
      hoverinfo = 'text',
      marker = list(
        color = colors,             
        line = list(color = '#E69F00', width = 1) 
      )
    )
    
    # Customize the plot layout
    fig <- fig %>%
      layout(
        title = list(
          text = '<b>UFO Sightings Reported by Month</b>',
          font = list(size = 16, color = '#E69F00'),
          x = 0.5,                   # Center the title
          xanchor = 'center',
          y = 0.97,                   # Adjust title's vertical position
          yanchor = 'top'
        ),
        polar = list(
          domain = list(
            x = c(0, 1),
            y = c(0.15, 0.9)          # Adjust the vertical range of the polar plot
          ),
          radialaxis = list(
            visible = TRUE,
            showline = FALSE,
            showticklabels = FALSE,  # Hide labels on radial axis
            showgrid = FALSE,        # Remove gridlines on radial axis
            ticks = '',              # Hide ticks on radial axis
            gridcolor = 'transparent'
          ),
          angularaxis = list(
            tickmode = 'array',
            tickvals = seq(0, 330, by = 30),  # Set tick positions for months
            ticktext = month.abb,             # Display month abbreviations
            tickfont = list(size = 12, color = '#E69F00'),
            rotation = 90,                    # Start January at the top
            direction = 'clockwise',          # Use clockwise rotation
            showline = FALSE,
            showgrid = FALSE,                
            ticks = '',                       
            gridcolor = 'transparent',
            tickpadding = 10                  
          ),
          bgcolor = '#101010' 
        ),
        margin = list(t = 20, b = 20, l = 20, r = 20), 
        height = 450,             
        showlegend = FALSE,       
        paper_bgcolor = '#101010',  
        plot_bgcolor = '#101010',   
        font = list(color = '#E69F00'),  
        hoverlabel = list(
          font = list(size = 12, color = 'white'),  
          bgcolor = '#444444'  
        )
      )
    
    fig  # Render the plot
  })
  
  
  # --- Panel 3: Reported Duration ---
  # 1. Render histogram for UFO sightings duration
  output$p3_main <- renderPlot({
    sel_ufo_df() %>%
      ggplot(aes(x = Duration)) +
      geom_histogram(bins = 30, fill = "#609ee4", color = "white") +
      scale_x_continuous(expand = c(0, 0)) +
      labs(
        x = "Duration (Hours)",
        y = "Frequency",
        title = "Distribution of UFO Sightings Duration"
      ) +
      theme_dark() +
      theme(
        text = element_text(size = 11, color = '#E69F00'),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, color = '#E69F00', face = "bold"),
        axis.text = element_text(size = 13, color = '#E69F00'),
        panel.grid.major.x = element_blank(),  # Remove grid lines for x-axis
        legend.position = 'none',
        plot.background = element_rect(fill = '#101010', colour = '#101010'),
        panel.background = element_rect(fill = '#101010')
      )
  }, bg = "transparent")
  
  # 2. Render violin plot for UFO sightings duration with boxplot overlay
  output$p3_violin <- renderPlot({
    sel_ufo_df() %>%
      ggplot(aes(x = "", y = Duration)) +
      geom_violin(fill = "#609ee4", color = "#E69F00", alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "#E69F00", color = "white", outlier.shape = NA) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      labs(
        x = "",
        y = "Duration (Hours)",
        title = "Violin Plot of UFO Sightings Duration"
      ) +
      theme_dark() +
      theme(
        text = element_text(size = 12, color = '#E69F00'),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 13, color = '#E69F00', face = "bold"),
        axis.text.y = element_text(size = 13, color = '#E69F00'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        legend.position = 'none',
        plot.background = element_rect(fill = '#101010', colour = '#101010'),
        panel.background = element_rect(fill = '#101010')
      )
  }, bg = "transparent")
  
  # Render lollipop plot for UFO sightings by shape
  output$p4_main <- renderPlot({
    shape_counts <- sel_ufo_df() %>%
      count(shape) %>%
      arrange(desc(n))  # Order shapes by the number of reports
    
    # Create the lollipop plot with shape counts
    ggplot(shape_counts, aes(x = reorder(shape, n), y = n)) +
      geom_segment(
        aes(x = reorder(shape, n), xend = reorder(shape, n), y = 0, yend = n),
        color = '#E69F00', size = 1.5
      ) +
      geom_point(color = "#609ee4", size = 6) +
      geom_text(aes(label = n), vjust = 0.5, hjust = -0.3, size = 5, color = '#E69F00') +
      labs(
        x = "Reported Shape",
        y = "Reported Count",
        title = "UFO Sightings Shape Reported"
      ) +
      coord_flip() +  # Flip axes for better readability
      theme_dark() +
      theme(
        text = element_text(size = 12, color = '#E69F00'),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 13, color = '#E69F00', face = "bold"),
        axis.text = element_text(size = 13, color = '#E69F00'),
        legend.position = 'none',
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = '#444444'),
        plot.background = element_rect(fill = '#101010', colour = '#101010'),
        panel.background = element_rect(fill = '#101010', colour = '#E69F00')
      )
  }, bg = "transparent")
  # --- Panel 4: data details ---
  # Render a data table for UFO sightings data preview
  output$p5_preview <- renderDataTable({
    datatable(
      ufo_df,
      selection = 'none', escape = FALSE,  # Disable selection and escaping for HTML
      options = list(
        pageLength = 10,  # Set the number of rows to display per page
        lengthMenu = c(10, 25, 50, 100), 
        scrollX = TRUE  # Enable horizontal scrolling
      )
    )
  })
}

# Run Shiny application
shinyApp(ui, server, options = list(launch.browser = TRUE))
