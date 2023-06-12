pacman::p_load(shiny,dplyr,ggplot2,tidyr,plotly,DT,shinythemes,writexl,readxl,ggpubr,Hmisc,stringr,shinyWidgets,rebus)


# Define UI for application that draws a histogram


bla = shinyauthr::loginUI(id = "login",
                          title = "Monitoreo de lectura mensual",
                          user_title = "Usuario",
                          pass_title = "Clave",
                          login_title = "Ingresar")

ui <- fluidPage(
  bla,
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout",label = "salir")), ## agregar en la app heroku
  uiOutput("UI")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  user_base <- tibble::tibble(
    user = c("Cristian", "lectura"),
    password = sapply(c("Astoreca", "mensual"), sodium::password_store),
    permissions = c("admin", "admin"),
    name = c("Cristian", "equipo_1ro")
  )
  
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  timer <- reactiveTimer(1000 * 60 * 0.5) ### este mierda mantiene prendida la app
  observe({
    timer()
    {{"Bla"}}
  })
  
  
  
  
  output$info = renderTable(
    credentials()$info$permissions # Esto es solo para ver qué contiene info
  )
  
  
  
  
  output$UI = renderUI({
    
    req(credentials()$user_auth) # Es raro que esto esté volando
    
    setBackgroundColor(
      color = c("#FFFFFF", "#FEF9CC"),
      gradient = "linear",
      direction = "bottom"
    )
    tagList(titlePanel(h2("Monitoreo de lecturas mensuales de 1ro básico")),
            #titlePanel("Monitoreo de lecturas 1ro básico",windowTitle = "Lectura mensual"),
            sidebarLayout(fluid = FALSE,
                          sidebarPanel(width = 2,
                                       fileInput(inputId = "data_f",label = "Cargar datos",buttonLabel = "Cargar"),
                                       
                                       br(),
                                       br(),
                                       selectInput(inputId = "niveles_desempeño", label = "Selecciona categorías",
                                                   choices = c("E","MB","B","S","I","NL"), multiple = T),
                                       br(),
                                       selectInput(inputId = "tipo_grupo",label = "¿Qué quieres grafica?",
                                                   choices = c("Curso","Colegio"),selected = "Curso")
                                       
                                       
                                       
                                       
                                       ## poner otras weas
                          ),
                          mainPanel(tabsetPanel(type = "tabs",
                                                tabPanel("Datos",div(style = "margin-top:30px"),
                                                         #fluidRow(column(width = 2,tableOutput(outputId = "info"))), esto es para mostrar el elemento de chockoutput()$info
                                                         fluidRow(column(12, DTOutput("datos_lm")))),
                                                
                                                tabPanel("Gráfico tendencia",
                                                         fluidRow(column(width=12,plotlyOutput(outputId = "tendencia",height = "600px"))),
                                                         fluidRow(column(width = 6,selectInput("grupo",
                                                                                               label = "Selecciona grupos",
                                                                                               choices = c("Primero debes cargar datos"),
                                                                                               multiple = T, width = 800)),
                                                                  column(width = 3, textInput(inputId = "titulo_grafico", label = "Título",
                                                                                              value = "Tendencia")),
                                                                  column(width = 3, selectInput("n_lectura_tendencia",label = "Seleccionar lectura", choices = 
                                                                                                  c("Lectura_1", "Lectura_2", "Lectura_3", "Lectura_4",
                                                                                                    "Lectura_5", "Lectura_6","Lectura_7",
                                                                                                    "Lectura_8", "Lectura_9"),multiple = T)))),
                                                
                                                tabPanel("Ordenamiento",
                                                         fluidRow(column(width=12,plotlyOutput(outputId = "ordenamiento",height = "600px"))),
                                                         fluidRow(column(width = 6,selectInput("grupo2",
                                                                                               label = "Selecciona grupos",
                                                                                               choices = c("Primero debes cargar datos"),
                                                                                               multiple = T, width = 800)),
                                                                  column(width = 3, textInput(inputId = "titulo_grafico2", label = "Título",
                                                                                              value = "Ordenamiento")),
                                                                  column(width = 3, selectInput("n_lectura",label = "Seleccionar lectura", choices = 
                                                                                                  c("Lectura_1", "Lectura_2", "Lectura_3", "Lectura_4",
                                                                                                    "Lectura_5", "Lectura_6","Lectura_7",
                                                                                                    "Lectura_8", "Lectura_9"),multiple = F)))),
                                                
                                                
                                                
                                                tabPanel("Comparativo Astoreca",
                                                         fluidRow(column(width=12,plotlyOutput(outputId = "ordenamiento_astoreca",height = "600px"))),
                                                         fluidRow(column(width = 6,selectInput("grupo_astoreca",
                                                                                               label = "Selecciona grupos",
                                                                                               choices = c("Primero debes cargar datos"),
                                                                                               multiple = T, width = 800)),
                                                                  column(width = 3, textInput(inputId = "titulo_grafico_astoreca", label = "Título",
                                                                                              value = "Ordenamiento")),
                                                                  column(width = 3, selectInput("n_lectura_astoreca",label = "Seleccionar lectura", choices = 
                                                                                                  c("Lectura_1", "Lectura_2", "Lectura_3", "Lectura_4",
                                                                                                    "Lectura_5", "Lectura_6","Lectura_7",
                                                                                                    "Lectura_8", "Lectura_9"),multiple = F)))),
                                                
                                                
                                                
                                                tabPanel("Individuales",
                                                         fluidRow(column(width = 12,plotlyOutput(outputId = "ordenamiento_estudiantes",height = "600px"))),
                                                         fluidRow(column(width = 6,selectInput("grupo_estudiantes",
                                                                                               label = "Selecciona grupos",
                                                                                               choices = c("Primero debes cargar datos"),
                                                                                               multiple = T, width = 800)),
                                                                  column(width = 3,selectInput("n_lectura_estudiantes", label = "Selecciona lecturas",
                                                                                               choices = c("Lectura_1", "Lectura_2", "Lectura_3", "Lectura_4",
                                                                                                           "Lectura_5", "Lectura_6","Lectura_7",
                                                                                                           "Lectura_8", "Lectura_9"),multiple = T)))),
                                                
                                                tabPanel("Resumen",div(style = "margin-top:30px"),fluidRow(column(12, DTOutput("datos_resumen")))),
                                                #tabPanel("Resumen2",div(style = "margin-top:30px"),fluidRow(column(12, DTOutput("datos_comparativos")))),
                                                
                          ))
            )
    )
    
  })
  
  
  
  
  
  
  
  
  
  data_file = reactive({ ## 1: crear reactive y poner condiciones de acuerdo al radio buttons
    if(is.null(input$data_f)){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return() ## arrojar ningún output si no hay nada en el input blabla
    } else{
      file_path = input$data_f
      aa = read_excel(path = file_path$datapath)
      aa = aa %>% mutate(across(.cols = 6:ncol(aa), .fns = function(x){
        str_replace_all(x, pattern = SPC, replacement = "")
      }))
      return(aa)}
  })
  
  
  output$datos_lm = renderDT({
    if(is.null(input$data_f)){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return()
    } else{
      data_file()
    }
  }, options = list(scrollX = TRUE))
  
  
  
  
  
  
  
  
  observeEvent(is.null(data_comparativos()==F), {
    updateSelectInput(inputId = "grupo", choices = c(unique(data_comparativos()$grupo),"Astoreca"))
  })
  
  
  observeEvent(is.null(data_comparativos()==F), {
    updateSelectInput(inputId = "grupo2", choices = c(unique(data_comparativos()$grupo),"Astoreca"))
  })
  
  
  observeEvent(is.null(data_comparativos()==F), {
    updateSelectInput(inputId = "grupo_astoreca", choices = c(unique(data_comparativos()$Colegio),"Astoreca"))
  })
  
  
  
  grafico_tendencia = reactive({data_comparativos() %>% filter(grupo %in% input$grupo, evaluacion %in% input$n_lectura_tendencia)})
  
  output$tendencia = renderPlotly(
    
    grafico_tendencia() %>% 
      ggplot(aes(x=evaluacion, y = porcentaje_desempeño, color = grupo, group=grupo,text = paste0("Presentes ",proporcion_presentes, " %"))) + 
      geom_text(aes(label=grupo), alpha=0.8,nudge_x = 0.13, nudge_y = 0.2, size=3.5,show.legend = F) +
      geom_point(aes(size=proporcion_presentes), alpha=0.4,show.legend = F) +
      geom_line(show.legend = F) +
      theme_bw() +
      #geom_text_repel(aes(label=grupo),seed = 123,box.padding = 0.5, max.overlaps = Inf, show.legend = F) +
      theme(legend.position = "bottom") +
      #stat_summary(fun = "mean", geom = "point", color = "black") +
      #stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom = "pointrange", color = "black", aes(group=1), position = position_nudge(x=-0.2)) +
      #stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom = "errorbar", color = "black", aes(group=1), position = position_nudge(x=-0.2),width = 0.1) +
      geom_point(data = grafico_tendencia(), aes(x=evaluacion,y=promedio_general, text = "-"),position = position_nudge(x=-0.2),color="black", shape=2) +
      geom_errorbar(data = grafico_tendencia(), aes(x=evaluacion,y=promedio_general,ymin=promedio_general-desviacion_general, ymax=promedio_general+desviacion_general),position = position_nudge(x=-0.2),color="black",width=0.1) +
      guides(color = "none") +
      labs(y="% de desempeño", x = "Lectura", title = input$titulo_grafico))
  
  
  
  output$ordenamiento = renderPlotly(
    data_comparativos() %>% filter(grupo %in% input$grupo2) %>%  filter(evaluacion == input$n_lectura) %>%
      ggpubr::ggbarplot(x="grupo", y="porcentaje_desempeño",
                        fill="proporcion_presentes",
                        sort.val = "desc",label = F,lab.pos = "in",lab.hjust = 0.5) +
      coord_flip() + labs(fill = "% presentes",y="% desempeño", x = "Orden", title = input$titulo_grafico2) +
      scale_fill_continuous(low = "yellow4", high = "green3") +
      geom_text(aes(label=str_c(round(porcentaje_desempeño,1))), nudge_x = 0, nudge_y = -1.2, size=3)
  )
  
  
  
  
  ### Datos para comparativo con Astoreca y otros colegios:
  
  
  data_comparativos = reactive({
    if(is.null(data_file())){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return() ## arrojar ningún output si no hay nada en el input blabla
    } else {
      
      orden_ev = colnames(data_file()[,6:ncol(data_file())])
      
      if(input$tipo_grupo == "Curso"){
        dat_long = data_file() %>% gather(key=evaluacion, value=valor, Lectura_1:Final) %>% 
          mutate(valor = ifelse(valor %in% c("E", "MB", "B", "S", "I", "NL"), valor, NA)) %>%
          mutate(valor = ifelse(is.na(valor)==T,"Ausente",valor)) %>% 
          filter(valor %in% c("E", "MB", "B", "S", "I", "NL", "Ausente")) %>% 
          mutate(evaluacion = factor(evaluacion, levels = c(orden_ev))) %>% 
          count(Año,Colegio,Letra,evaluacion,valor) %>% 
          group_by(Año,Colegio,Letra,evaluacion) %>% 
          mutate(total=sum(n),
                 prop = round(n/sum(n),3)*100) %>% 
          ungroup()
        
        ## generar la base de ausentes
        
        ausentes = dat_long %>% filter(valor == "Ausente")
        ausentes = ausentes %>% rename(total_ausentes = n, proporcion_ausentes = prop) %>% select(-valor)
        ausentes = ausentes %>% select(-total)
        
        ## Generar la base de lo demás:
        
        dat_long = data_file() %>% gather(key=evaluacion, value=valor, Lectura_1:Final) %>% 
          mutate(valor = ifelse(valor %in% c("E", "MB", "B", "S", "I", "NL"), valor, NA)) %>%
          mutate(valor = ifelse(is.na(valor)==T,"Ausente",valor)) %>% 
          filter(valor %in% c("E", "MB", "B", "S", "I", "NL")) %>% # Aquí mueren los ausentes
          mutate(evaluacion = factor(evaluacion, levels = c(orden_ev)))
        
        
        
        tabla_frecuencias = table(dat_long$Año, dat_long$Colegio,
                                  dat_long$Letra, dat_long$evaluacion,
                                  dat_long$valor)
        
        
        
        categorias_posibles <- expand.grid(unique(dat_long$Año),
                                           unique(dat_long$Colegio),
                                           unique(dat_long$Letra),
                                           unique(dat_long$evaluacion),
                                           unique(dat_long$valor))
        
        
        
        tabla_final <- merge(categorias_posibles, tabla_frecuencias, by = c("Var1", "Var2", "Var3", "Var4", "Var5"), all = TRUE)
        
        colnames(tabla_final) = c("Año","Colegio","Letra","evaluacion","valor","totales")
        
        
        tabla_astoreca = tabla_final %>% filter(Colegio %in% c("SJULFA","SJOLFA","SJRFA")) %>% 
          group_by(evaluacion) %>% mutate(total = sum(totales)) %>% filter(total > 0) %>% 
          ungroup() %>% group_by(evaluacion,valor) %>%  mutate(totales = sum(totales)) %>% 
          ungroup() %>% mutate(id = str_c(evaluacion, valor)) %>% distinct(id, .keep_all = T) %>%
          mutate(prop = round(totales/total,3)*100) %>% 
          select(Año, evaluacion,valor,prop)
        
        tabla_astoreca$Colegio = "Astoreca"
        tabla_astoreca$Letra = "1A"
        tabla_astoreca$proporcion_ausentes = 0
        
        
        tabla_final = tabla_final %>% 
          group_by(Año,Colegio,Letra,evaluacion) %>% 
          mutate(total=sum(totales),
                 prop = round(totales/sum(totales),3)*100) %>% 
          ungroup()
        
        
        tabla_final = tabla_final %>% filter(total > 0)
        tabla_final = tabla_final %>% left_join(ausentes) %>% mutate(total_ausentes = ifelse(is.na(total_ausentes)==T,0,total_ausentes),
                                                                     proporcion_ausentes = ifelse(is.na(proporcion_ausentes)==T,0,proporcion_ausentes))
        
        tabla_final = tabla_final %>% bind_rows(tabla_astoreca)
        
        
        tabla_final = tabla_final %>% mutate(aprobado = ifelse(valor %in% input$niveles_desempeño,"Aprobado","Reprobado")) %>% 
          group_by(Año,Colegio,Letra,evaluacion,aprobado) %>% 
          mutate(porcentaje_desempeño = sum(prop)) %>% 
          filter(aprobado == "Aprobado") %>% 
          mutate(proporcion_presentes = 100-proporcion_ausentes,grupo = str_c(Colegio, "_",Letra)) %>%
          ungroup() %>% 
          mutate(id = str_c(Año,Colegio,Letra,evaluacion)) %>% distinct(id,.keep_all = T) %>%
          select(Año,Colegio,Letra,grupo,evaluacion,porcentaje_desempeño,proporcion_presentes)
        
        tabla_final = tabla_final %>% mutate(grupo = ifelse(grupo == "Astoreca_1A","Astoreca",grupo))
        
        tabla_final = tabla_final %>%
          group_by(Año,evaluacion) %>%
          mutate(promedio_general = round(mean(porcentaje_desempeño),1),
                 desviacion_general = round(sd(porcentaje_desempeño),1),
                 diferencia_general = round(porcentaje_desempeño-promedio_general,1)) %>%
          ungroup()
        
      } else {
        
        dat_long = data_file() %>% gather(key=evaluacion, value=valor, Lectura_1:Final) %>% 
          mutate(valor = ifelse(valor %in% c("E", "MB", "B", "S", "I", "NL"), valor, NA)) %>%
          mutate(valor = ifelse(is.na(valor)==T,"Ausente",valor)) %>% 
          filter(valor %in% c("E", "MB", "B", "S", "I", "NL", "Ausente")) %>% 
          mutate(evaluacion = factor(evaluacion, levels = c(orden_ev))) %>% 
          count(Año,Colegio,evaluacion,valor) %>% 
          group_by(Año,Colegio,evaluacion) %>% 
          mutate(total=sum(n),
                 prop = round(n/sum(n),3)*100) %>% 
          ungroup()
        
        ## generar la base de ausentes
        
        ausentes = dat_long %>% filter(valor == "Ausente")
        ausentes = ausentes %>% rename(total_ausentes = n, proporcion_ausentes = prop) %>% select(-valor)
        ausentes = ausentes %>% select(-total)
        
        ## Generar la base de lo demás:
        
        dat_long = data_file() %>% gather(key=evaluacion, value=valor, Lectura_1:Final) %>% 
          mutate(valor = ifelse(valor %in% c("E", "MB", "B", "S", "I", "NL"), valor, NA)) %>%
          mutate(valor = ifelse(is.na(valor)==T,"Ausente",valor)) %>% 
          filter(valor %in% c("E", "MB", "B", "S", "I", "NL")) %>% # Aquí mueren los ausentes
          mutate(evaluacion = factor(evaluacion, levels = c(orden_ev)))
        
        
        
        tabla_frecuencias = table(dat_long$Año, dat_long$Colegio,
                                  dat_long$evaluacion,
                                  dat_long$valor)
        
        
        
        categorias_posibles <- expand.grid(unique(dat_long$Año),
                                           unique(dat_long$Colegio),
                                           unique(dat_long$evaluacion),
                                           unique(dat_long$valor))
        
        
        
        tabla_final <- merge(categorias_posibles, tabla_frecuencias, by = c("Var1", "Var2", "Var3", "Var4"), all = TRUE)
        
        colnames(tabla_final) = c("Año","Colegio","evaluacion","valor","totales")
        
        
        
        
        tabla_astoreca = tabla_final %>% filter(Colegio %in% c("SJULFA","SJOLFA","SJRFA")) %>% 
          group_by(evaluacion) %>% mutate(total = sum(totales)) %>% filter(total > 0) %>% 
          ungroup() %>% group_by(evaluacion,valor) %>%  mutate(totales = sum(totales)) %>% 
          ungroup() %>% mutate(id = str_c(evaluacion, valor)) %>% distinct(id, .keep_all = T) %>%
          mutate(prop = round(totales/total,3)*100) %>% 
          select(Año, evaluacion,valor,prop)
        
        tabla_astoreca$Colegio = "Astoreca"
        tabla_astoreca$proporcion_ausentes = 0
        
        
        tabla_final = tabla_final %>% 
          group_by(Año,Colegio,evaluacion) %>% 
          mutate(total=sum(totales),
                 prop = round(totales/sum(totales),3)*100) %>% 
          ungroup()
        
        
        tabla_final = tabla_final %>% filter(total > 0)
        tabla_final = tabla_final %>% left_join(ausentes) %>% mutate(total_ausentes = ifelse(is.na(total_ausentes)==T,0,total_ausentes),
                                                                     proporcion_ausentes = ifelse(is.na(proporcion_ausentes)==T,0,proporcion_ausentes))
        
        tabla_final = tabla_final %>% bind_rows(tabla_astoreca)
        
        tabla_final = tabla_final %>% mutate(aprobado = ifelse(valor %in% input$niveles_desempeño,"Aprobado","Reprobado")) %>% 
          group_by(Año,Colegio,evaluacion,aprobado) %>% 
          mutate(porcentaje_desempeño = sum(prop)) %>% 
          filter(aprobado == "Aprobado") %>% 
          mutate(proporcion_presentes = 100-proporcion_ausentes,grupo = str_c(Colegio)) %>%
          ungroup() %>% 
          mutate(id = str_c(Año,Colegio,evaluacion)) %>% distinct(id,.keep_all = T) %>%
          select(Año,Colegio,grupo,evaluacion,porcentaje_desempeño,proporcion_presentes)
        
        tabla_final = tabla_final %>%
          group_by(Año,evaluacion) %>%
          mutate(promedio_general = round(mean(porcentaje_desempeño),1),
                 desviacion_general = round(sd(porcentaje_desempeño),1),
                 diferencia_general = round(porcentaje_desempeño-promedio_general,1)) %>%
          ungroup()
        
      }
    }
    
  })
  
  
  
  
  # El desafío es además, hacer que uno defina en cuál quiere centrarse y que el resto aparezca anónimo
  
  
  
  
  
  output$ordenamiento_astoreca = renderPlotly(
    
    
    data_comparativos() %>%
      
      mutate(grupo2 = case_when(
        Colegio %in% c(input$grupo_astoreca)~grupo,
        Colegio == "Astoreca"~"Astoreca",
        !Colegio %in% c("Astoreca", "SJULFA","SJOLFA","SJRFA",input$grupo_astoreca)~"Otro"),
        
        Colegio = case_when(
          Colegio %in% c(input$grupo_astoreca)~Colegio,
          Colegio == "Astoreca"~"Astoreca",
          !Colegio %in% c("Astoreca",input$grupo_astoreca)~"Otro")) %>% 
      filter(Colegio %in% c(input$grupo_astoreca, "Otro")) %>%
      filter(evaluacion == input$n_lectura_astoreca) %>%
      filter(is.na(grupo2)==F) %>% 
      group_by(Colegio, grupo2) %>% summarise(porcentaje_desempeño = mean(porcentaje_desempeño,na.rm=T),
                                              porcentaje_presentes = mean(proporcion_presentes, na.rm=T)) %>% 
      
      ggpubr::ggbarplot(x="grupo2", y="porcentaje_desempeño",
                        fill="Colegio",sort.by.groups = F,
                        sort.val = "desc",label = F,lab.pos = "in",lab.hjust = 0.5) +
      coord_flip() +
      labs(fill = "Colegio",
           y="% desempeño",
           x = "Orden",
           title = input$titulo_grafico_astoreca) +
      geom_text(aes(label=str_c(round(porcentaje_desempeño,1))), nudge_x = 0, nudge_y = -1.2, size=3))
  
  
  
  
  
  data_individuales = reactive({
    if(is.null(data_file())){ ## Hay que poner esto, de lo contrario genera un error todo el tiempo
      return() ## arrojar ningún output si no hay nada en el input blabla
    } else {
      data = data_file() %>% 
        gather(key=evaluacion, value=valor, Lectura_1:Final) %>% 
        filter(valor %in% c("E", "MB", "B", "S", "I", "NL")) %>% 
        mutate(grupo = str_c(Colegio, "_",Letra))
    }
  })
  
  
  
  observeEvent(is.null(data_file()==F), {
    updateSelectInput(inputId = "grupo_estudiantes", choices = c(unique(data_individuales()$grupo)))
  })
  
  output$ordenamiento_estudiantes = renderPlotly(
    
    
    data_individuales() %>% 
      filter(grupo %in% c(input$grupo_estudiantes)) %>%
      filter(evaluacion %in% input$n_lectura_estudiantes) %>%
      mutate(ID = str_c(Colegio, Nombre)) %>% 
      mutate(valor_n = case_when(
        valor == "E"~6,
        valor == "MB"~5,
        valor == "B"~4,
        valor == "S"~3,
        valor == "I"~2,
        valor == "NL"~1
      )) %>% 
      
      group_by(ID,Nombre) %>%
      summarise(promedio = mean(valor_n,na.rm=T),
                lecturas = n()) %>%
      ungroup() %>% 
      mutate(lecturas = factor(lecturas)) %>% 
      
      ggpubr::ggbarplot(x="Nombre",
                        y="promedio",
                        sort.by.groups = F,
                        sort.val = "desc",
                        fill = "lecturas",
                        label = F,
                        lab.pos = "in",
                        lab.hjust = 0.5) +
      coord_flip() +
      labs(y="Promedio",x = "Orden", fill = "Cantidad de lecturas") +
      scale_y_continuous(breaks = c(0,1,2,3,4,5,6), labels = c("","1\n(NL)","2\n(I)",
                                                               "3\n(S)","4\n(B)","5\n(MB)","6\n(E)"))
  )
  
  
  
  
  
  
  
  
  
  
  
  output$datos_resumen = renderDT({
    data_comparativos()
  },options = list(scrollX = TRUE))
  
  
  
  #  output$datos_comparativos = renderDT({
  #    data_comparativos()
  #  },options = list(scrollX = TRUE))
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
