library(shiny)
library(tidyverse)
library(httr)
library(lubridate)
library(stringr)


# rammasmidur-function ----------------------------------------------------

rammasmidur <- function(stod, timabil) {
  
  #Tökum dagsetningar-inntök og skerum niður í búta
  fors_upph <- str_split(string = timabil[1], pattern = "-", n = 3, simplify = TRUE)
  fors_end <- str_split(string = timabil[2], pattern = "-", n = 3, simplify = TRUE)
  
  
  
  
  
  #Mismunandi skilgreiningar eftir mismunandi vali á stöð
  if (stod == "02") {
    #API slóðir fyrir Grensásveg (Mælistöð 02)
    param_url_NO2  <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/02/12/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    param_url_SO2  <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/02/41/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    param_url_H2S  <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/02/42/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    param_url_PM10 <- paste0("http://loftapi.reykjavik.is/api/v1/stations/data/02/91/",
                             fors_upph[2], "-", fors_upph[3], "-", fors_upph[1],
                             "/0/0/",
                             fors_end[2], "-", fors_end[3], "-", fors_end[1], 
                             "/23/30")
    
    
    #Búum til lista sem inniheldur allar slóðirnar sem við viljum nálgast
    param_url <- list(param_url_NO2, param_url_SO2, param_url_H2S, param_url_PM10)
    
    
  } else if (stod == "03") {
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
    
  }
  
  
  # If setning lokast og við höldum áfram
  
  
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
           Tegund = case_when(parameterid == "12" ~ "Köfnunarefnisdíoxíð (NO2)",
                              parameterid == "41" ~ "Brennisteinsdíoxíð (SO2)",
                              parameterid == "42" ~ "Brennisteinsvetni (H2S)",
                              parameterid == "91" ~ "Svifryk (PM10)"))
  gagnarammi
}

# ui.R ----------------------------------------------------------------------


# Define UI for application 
ui <- fluidPage(
  headerPanel("Loftgæðamælingar í Reykjavík"),
  
  sidebarPanel(
  dateRangeInput(inputId = "dags", label = "Veldu tímabil",
                 start = Sys.Date()-13, end = Sys.Date()-1,
                 format = "yyyy-mm-dd", min = "2009-01-01", max = Sys.Date(), 
                 weekstart = 1, language = "is", separator = "til"),
  
  selectInput(inputId = "stodval", label = "Veldu fastastöð", choices = c("Grensásvegur 15" = "02",
                                                                          "Fjölskyldu- og húsdýragarðurinn" = "03"), 
              selected = "Grensásvegur", multiple = FALSE, selectize = FALSE),
  
  downloadButton(outputId = "dl_gogn", label = "Sækja valin gögn (.csv)")
  ),
  
  mainPanel(
  textOutput(outputId = "valtexti"),
  plotOutput(outputId = "mynd"),
  textOutput(outputId = "utsk")
  )

)


# server.R ----------------------------------------------------------------

server <- function(input, output) {

#Gögn
gogn <- reactive({rammasmidur(stod = input$stodval, timabil = input$dags)})
  
#Render
  output$mynd <- renderPlot({
      gogn() %>% 
      ggplot(aes(time, value)) +
      geom_line(aes(color = Tegund), alpha = 0.66) +
      scale_x_datetime() + 
      ylab("Styrkur (µg/m³)") +
      xlab("Tími") +
      facet_wrap(~Tegund)
  })
  
  output$utsk <- renderText({"Ef óskað er eftir frekari upplýsingum varðandi gögnin er bent á hafa samband við Heilbrigðiseftirlit Reykjavíkur í síma 411 1111. Hafa skal samráð við Heilbrigðiseftirlit Reykjavíkur um birtingu gagna. Vinsamlega athugið að þetta eru rauntímagögn sem ekki er búið að leiðrétta og því geta leynst í þeim villur."})
  
  output$dl_gogn <- downloadHandler(filename = "loftgaedi.csv", 
                                    content = function(file){
                                      write.csv2(x = gogn(), file, row.names = FALSE, fileEncoding = "UTF-8")
                                    })
}


# runApp ------------------------------------------------------------------
# Hnýtum ui og server saman. 
shinyApp(ui = ui, server = server)

