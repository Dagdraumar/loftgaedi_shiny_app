library(shiny)
library(tidyverse)
library(httr)
library(lubridate)
library(stringr)

# Define UI for application 
ui <- fluidPage(
  dateRangeInput(inputId = "dags", label = "Veldu tímabil",
                 start = Sys.Date()-16, end = Sys.Date()-10,
                 format = "yyyy-mm-dd", min = "2010-01-01", max = Sys.Date(), 
                 weekstart = 1, language = "is", separator = "til"),
  
  plotOutput(outputId = "utsk")
)


# Define server logic required
server <- function(input, output) {
  
  output$utsk <- renderPlot({
    
    #Tökum dagsetningar-inntök og skerum niður í búta
    fors_upph <- str_split(string = input$dags[1], pattern = "-", n = 3, simplify = TRUE)
    fors_end <- str_split(string = input$dags[2], pattern = "-", n = 3, simplify = TRUE)
    
    #API slóðir fyrir Fjölskyldu- og húsdýragarð (Mælistöð 03)
    param_url_NO2  <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/03/12/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    param_url_PM10 <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/03/91/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    
    #Búum til lista sem inniheldur allar slóðirnar sem við viljum nálgast
    param_url <- list(param_url_NO2, param_url_PM10)
    lengd <- length(param_url)
    
    #Notum GET() til að tengjast API og sækja gögnin. Notum lapply() til að gera þetta fyrir allar slóðirnar
    #í param_url listanum
    allt <- lapply(X = param_url, GET)
    
    #Lúppum okkur í gegn til að smíða kassalaga gagnasett, ramma (sem er listaveisla enn sem komið er)
    allt_content_radad <- vector("list", length = lengd)
    for (i in 1:lengd) {
      allt_content <- content(allt[[i]])
      allt_content_radad[[i]] <- do.call(rbind, allt_content) 
    }
    
    #Breytum gögnunum úr listum í gagnaramma
    rammi <- do.call(rbind, allt_content_radad)
    gagnarammi <- as_data_frame(matrix(nrow = nrow(rammi), ncol = ncol(rammi)))
    breidd <- ncol(rammi)
    for (j in 1:breidd) {
      gagnarammi[, j] <- unlist(rammi[, j])
    }
    
    #Gefum dálkum rétt nöfn með lágstöfum
    names(gagnarammi) <- rammi[1,] %>%
      names() %>%
      str_to_lower()
    
    #Gerum dálkinn 'time' að POSIXct (date-time) object
    gagnarammi <- gagnarammi %>% 
      mutate(time = ymd_hms(time),
             Tegund = case_when(parameterid == "12" ~ "Köfnunarefnisdíoxíð",
                                parameterid == "91" ~ "Svifryk"))
    
    gagnarammi %>% 
      ggplot(aes(time, value)) +
      geom_line(aes(color = Tegund), alpha = 0.66) +
      scale_x_datetime() +
      ylim(c(0, 50)) + 
      ylab("Styrkur (µg/m³)") +
      facet_wrap(~Tegund)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

