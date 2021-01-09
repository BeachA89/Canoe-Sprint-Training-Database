server <- function(input, output) {
  
  #### Read data into a list ####
  # filesInfo <- drop_dir("CanoeRaceProfileData")
  # filePaths <- filesInfo$path_display
  # table1 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE,skip = 1, header=TRUE)
  
  #table1 <-  isolate(lapply(input$file1$datapath, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE))
  
  
  
  tab_WBT <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea) %>% select(WBT, WBTsec,WBTSpeed)
    
  })
  tab_Prog <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    a <- Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea) %>% select(ProgTime100, ProgTimesec100, ProgSpeed100, 
                                                                                                             ProgTime97, ProgTimesec97, ProgSpeed97, 
                                                                                                             ProgTime95, ProgTimesec95, ProgSpeed95, 
                                                                                                             ProgTime93, ProgTimesec93, ProgSpeed93)
    Prog100 = a[,1:3]
    Prog97 <-  a[,4:6]
    Prog95 <- a[,7:9]
    Prog93 <- a[,10:12]
    
    colnames(Prog100)=colnames(Prog97)=colnames(Prog95)=colnames(Prog93) = c("Prognostic Time", "in seconds", "Avg Speed (km/h)")
    b <- rbind(Prog100, Prog97, Prog95, Prog93)
    
    return(b)
    
  })
  
  
  
  output$table_WBT <- renderDataTable({ 
    WBTdata <-  tab_WBT()
    input$goButton
    isolate(
      datatable(WBTdata,  rownames = NULL, colnames = c("World's Best Time", "in seconds", "Avg Speed (km/h)"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))
    )
  })
  output$table_Prog <- renderDataTable({ 
    Progdata <-  tab_Prog()
    input$goButton
    isolate(datatable(Progdata,  rownames = c("100%", "97% (Open)", "95% (U23)", "93% (U18)"), colnames = c("Prognostic Time", "in seconds", "Avg Speed (km/h)"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))
    )
  })
  
  
  
  
  
  #### Filtering based on dropdowns - Time ####
  
  
  
  tab_Time_R1 <-  reactive({
    
    get(paste0(input$distance,"_time"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
      filter(Competition == input$Competition) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  
  # test <- Labelled_data_500_time %>%
  # melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
  # filter(Class == "MK1") %>% 
  # filter_all(any_vars(str_detect(., pattern = "Murray Stewart")))%>% 
  # filter(Competition == "NationalChampionships-2020") %>% 
  # filter(Phase == "F") %>%
  # mutate(variable = as.numeric(as.character(variable)))
  
  
  
  tab2_Time <-  reactive({
    get(paste0(input$distance,"_time")) %>% 
      filter(Class == input$Class) %>%
      arrange_at(ncol(.)) %>%
      head(10) %>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
  })
  
  
  
  
  output$select_Class <-  renderUI({
    
    selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(get(paste0(input$distance,"_time"))$Class)))  
  }) 
  
  output$select_Name <-  renderUI({
    inputClass = as.character(input$Class)
    choice_Name <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        select(contains("Name")) %>% as.list() %>%
        unlist() %>% as.character() %>% na.omit() 
      
    })
    
    
    
    
    selectizeInput('Name', 'Select Name', choices = c("select" = "", choice_Name()))  
  })         
  
  
  
  output$select_Competition <-  renderUI({
    inputClass = as.character(input$Class)
    inputName = as.character(input$Name)
    choice_Competition <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Competition', 'Select Competition', choices = c("select" = "", choice_Competition()))  
  })
  
  output$select_Phase <-  renderUI({
    inputClass = as.character(input$Class)
    inputName = as.character(input$Name)
    inputcompetition = as.character(input$Competition)
    choice_Phase <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
        filter(Competition == inputcompetition) %>% 
        pull(Phase) %>% 
        as.character()
      
      
    })
    
    
    selectizeInput('Phase', 'Select Phase', choices = c("select" = "", choice_Phase()))  
  })     
  
  # output$table2 <- renderDataTable({ 
  #   
  #   tab_Time_R1()
  # })
  
  
  
  #### Filtering based on dropdowns - Split Race 1 ####
  
  
  
  tab_Split_R1 <-  reactive({
    
    get(paste0(input$distance,"_split"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
      filter(Competition == input$Competition) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  tab2_Split <-  reactive({
    get(paste0(input$distance,"_split")) %>% 
      filter(Class == input$Class) %>%
      head(10) %>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
  })
  
  
  ###################
  tab_Vel_R1 <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_vel"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
      filter(Competition == input$Competition) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(ProgSpeed = (ClassProgSpeed/value)*100)
  })  
  
  
  
  
  
  
  tab2_Vel <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_vel")) %>% 
      filter(Class == input$Class) %>%
      head(10) %>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))%>%
      mutate(ProgSpeedUpperLimit = (ClassProgSpeed/UpperLimit)*100) %>%
      mutate(ProgSpeedAverage = (ClassProgSpeed/Average)*100)%>%
      mutate(ProgSpeedLowerLimit = (ClassProgSpeed/LowerLimit)*100)
    
  })
  
  tab_SR_R1 <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_SR"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
      filter(Competition == input$Competition) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  
  
  
  
  
  tab2_SR <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_SR")) %>% 
      filter(Class == input$Class) %>%
      head(10) %>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2))
    
    
  })
  
  
  #### Filtering based on dropdowns - Time Race 2 ####
  
  tab_Time_R2 <-  reactive({
    
    get(paste0(input$distance,"_time"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      filter(Competition == input$Competition2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  
  output$select_Class <-  renderUI({
    
    
    selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(get(paste0(input$distance,"_time"))$Class)))  
    
  }) 
  
  output$select_Name2 <-  renderUI({
    inputClass = as.character(input$Class)
    choice_Name2 <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        select(contains("Name")) %>% as.list() %>%
        unlist() %>% as.character() %>% na.omit() 
      
    })
    
    
    
    if (input$Report_Type == "Two Races"){
      selectizeInput('Name2', 'Select Name 2', choices = c("select" = "", choice_Name2()))  
    }
  })         
  
  
  
  output$select_Competition2 <-  renderUI({
    inputClass = as.character(input$Class)
    inputName2 = as.character(input$Name2)
    choice_Competition2 <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Races"){
      selectizeInput('Competition2', 'Select Competition 2', choices = c("select" = "", choice_Competition2()))  
    }
  })
  
  output$select_Phase2 <-  renderUI({
    inputClass = as.character(input$Class)
    inputName2 = as.character(input$Name2)
    inputcompetition2 = as.character(input$Competition2)
    choice_Phase2 <- reactive({
      get(paste0(input$distance,"_time")) %>% 
        filter(Class == inputClass) %>% 
        filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
        filter(Competition == inputcompetition2) %>% 
        pull(Phase) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Races"){
      selectizeInput('Phase2', 'Select Phase 2', choices = c("select" = "", choice_Phase2()))  
    }
  })     
  
  # output$table2 <- renderDataTable({ 
  #   
  #   tab_Time_R1()
  # })
  
  
  
  #### Filtering based on dropdowns - Split Race 1 ####
  
  
  
  tab_Split_R2 <-  reactive({
    
    get(paste0(input$distance,"_split"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      filter(Competition == input$Competition2) %>% 
      filter(Phase == input$Phase) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  
  
  
  ###################
  tab_Vel_R2 <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_vel"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      filter(Competition == input$Competition2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable))) %>%
      mutate(ProgSpeed = (ClassProgSpeed/value)*100)
  })  
  
  
  
  tab_SR_R2 <-  reactive({
    if (input$distance == "Labelled_data_200"){
      distancea = 200
    }  else if (input$distance == "Labelled_data_500"){
      distancea = 500
    }else if (input$distance == "Labelled_data_1000"){
      distancea = 1000
    }
    
    ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
    ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
    
    get(paste0(input$distance,"_SR"))%>%
      melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
      filter(Class == input$Class) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      filter(Competition == input$Competition2) %>% 
      filter(Phase == input$Phase2) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })  
  
  
  output$ggplotvel <-  renderPlotly({
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      Label_Race1 = paste(input$Name, input$Competition, input$Phase)
      
      if (input$Report_Type == "Single Race"){    
        
        # p <-ggplot() +   xlab("Split (m)") + ylab("Vel (km/h)") + geom_line(data=tab_Vel_R1(), aes(variable,value, color = "red")) + 
        #   theme(legend.title = element_blank()) +
        #   geom_line(data=tab_SR_R1(), aes(variable,value/7,color = "blue")) +
        #   scale_color_discrete(name = "Y series", labels = c("Vel", "SR"))+
        #   scale_y_continuous(sec.axis = sec_axis(~.*7, name = "Stroke Rate (spm)"))
        # ggplotly(p)
        # 
        
        
        
        fig <- plot_ly()
        fig <- fig %>% add_lines(x = tab_Vel_R1()$variable, y = tab_Vel_R1()$value, name = "Velocity", line = list(color = "orange"))
        fig <- fig %>% add_lines(x = tab_SR_R1()$variable, y = tab_SR_R1()$value, name = "Stroke Rate", yaxis = "y2", line = list(color = "blue"))
        fig <- fig %>% layout(margin = list(l = 10, r = 50, b = 10, t = 10),
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Split (m)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Velocity (km/h)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis2 = list(title = "Stroke Rate (spm)",
                                            overlaying = "y",
                                            side = "right",
                                            gridcolor = 'rgb(255,255,255)',
                                            showgrid = TRUE,
                                            showline = FALSE,
                                            showticklabels = TRUE,
                                            tickcolor = 'rgb(127,127,127)',
                                            ticks = 'outside',
                                            zeroline = FALSE),
                              legend = list(x = 0.5, y = 0.9)
                              
        ) %>%
          config(displayModeBar = F)
        
        fig
        
        
        
        
      }else if (input$Report_Type == "Two Races"){
        Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
        
        
        
        # p <-  ggplot(tab_Vel_R1()) + geom_line(aes(variable, value, color = Label_Race1), group=1) +   xlab("Split (m)") + 
        #   ylab("Vel (km/h)") + geom_line(data=tab_Vel_R2(), aes(variable,value, color = Label_Race2)) + theme(legend.title = element_blank(), legend.position="bottom")
        # ggplotly(p)
        
        
        fig <- plot_ly()
        fig <- fig %>% add_lines(x = tab_Vel_R1()$variable, y = tab_Vel_R1()$value, name = Label_Race1, line = list(color = "orange"))
        fig <- fig %>% add_lines(x = tab_Vel_R2()$variable, y = tab_Vel_R2()$value, name = Label_Race2, line = list(color = "blue"))
        fig <- fig %>% layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Split (m)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Velocity (km/h)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              legend = list(x = 0.5, y = 0.9)) %>%
          config(displayModeBar = F)
        
        fig
        
        
      }else if(input$Report_Type == "vs Top 10"){
        
        # ytitle <- list(
        #   title = "Velocity (km/h)"
        # )
        # 
        # p <-  ggplot(tab2_Vel()) + geom_line(aes(variable, Average, color = 'Top 10'), group=1) +   xlab("Split (m)") + 
        #   ylab("Vel (km/h)") +
        #   geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab_Vel_R1(), aes(variable,value, color = Label_Race1)) + theme(legend.title = element_blank(), legend.position="bottom")
        # 
        # fig <- ggplotly() %>%
        #   config(displayModeBar = F)%>% layout(margin = list(l = 10, r = 50, b = 10, t = 10),
        #                                        title = "", yaxis = ytitle,
        #                                        xaxis = list(title="Split (m)"), legend = list(x = 0.5, y = 0.9))
        # fig
        
        
        
        fig <- plot_ly(x = tab2_Vel()$variable, y = tab2_Vel()$UpperLimit, type = 'scatter', mode = 'lines',
                       line = list(color = 'rgba(0,0,0,0)'),
                       showlegend = FALSE, name = 'Upperlimit')
        fig <- fig %>% add_trace(y = tab2_Vel()$LowerLimit, type = 'scatter', mode = 'lines',
                                 fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                                 showlegend = FALSE, name = 'Lowerlimit')
        fig <- fig %>% add_trace(y = tab2_Vel()$Average, type = 'scatter', mode = 'lines', showlegend = TRUE, name = 'Top10', line = list(color = "green"))
        fig <- fig %>% add_trace(y = tab_Vel_R1()$value, type = 'scatter', mode = 'lines', showlegend = TRUE, name = Label_Race1, line = list(color = "orange"))
        fig <- fig %>% layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Split (m)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Velocity (km/h)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              legend = list(x = 0.5, y = 0.9))
        
        fig
        
        
      }
    })
  })
  
  output$ggplotSR <-  renderPlotly({
    if (input$goButton == 0)
      return()
    Label_Race1 = paste(input$Name, input$Competition, input$Phase)
    input$goButton
    isolate({
      if (input$Report_Type == "Two Races"){
        ytitle <- list(
          title = "Stroke Rate (spm)"
        )
        Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
        
        
        # 
        # p <- ggplot(tab_SR_R1()) + geom_line(aes(variable, value, color = Label_Race1), group=1) +   xlab("Split (m)") + 
        #   ylab("SR (spm)") + geom_line(data=tab_SR_R2(), aes(variable,value, color = Label_Race2)) + theme(legend.title = element_blank(), legend.position="bottom")
        # ggplotly(p)%>%
        #   config(displayModeBar = F)%>% layout(margin = list(l = 10, r = 50, b = 10, t = 10),
        #                                        title = "", yaxis = ytitle,
        #                                        xaxis = list(title="Split (m)"), legend = list(x = 0.5, y = 0.9))
        
        
        fig <- plot_ly()
        fig <- fig %>% add_lines(x = tab_SR_R1()$variable, y = tab_SR_R1()$value, name = Label_Race1,line = list(color = "orange"))
        fig <- fig %>% add_lines(x = tab_SR_R2()$variable, y = tab_SR_R2()$value, name = Label_Race2,line = list(color = "blue"))
        fig <- fig %>% layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Split (m)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Stroke Rate (spm)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              legend = list(x = 0.5, y = 0.9)) %>%
          config(displayModeBar = F)
        fig
        
        
      }else if(input$Report_Type == "vs Top 10"){
        
        # ytitle <- list(
        #   title = "Stroke Rate (spm)"
        # )
        # p <-  ggplot(tab2_SR()) + geom_line(aes(variable, Average, color = 'Top 10'), group=1) +   xlab("Split (m)") + 
        #   ylab("Vel (km/h)") +
        #   geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab_SR_R1(), aes(variable,value, color = Label_Race1)) + theme(legend.title = element_blank(), legend.position="bottom")
        # 
        # ggplotly(p)%>%
        #   config(displayModeBar = F)%>% layout(margin = list(l = 10, r = 50, b = 10, t = 10),
        #                                        title = "", yaxis = ytitle,
        #                                        xaxis = list(title="Split (m)"), legend = list(x = 0.5, y = 0.9))
        
        fig <- plot_ly(x = tab2_SR()$variable, y = tab2_SR()$UpperLimit, type = 'scatter', mode = 'lines',
                       line = list(color = 'rgba(0,0,0,0)'),
                       showlegend = FALSE, name = 'Upperlimit')
        fig <- fig %>% add_trace(y = tab2_SR()$LowerLimit, type = 'scatter', mode = 'lines',
                                 fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                                 showlegend = FALSE, name = 'Lowerlimit')
        fig <- fig %>% add_trace(y = tab2_SR()$Average, type = 'scatter', mode = 'lines', showlegend = TRUE, name = 'Top10', line = list(color = 'green'))
        fig <- fig %>% add_trace(y = tab_SR_R1()$value, type = 'scatter', mode = 'lines', showlegend = TRUE, name = Label_Race1, line = list(color = 'orange'))
        fig <- fig %>% layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              xaxis = list(title = "Split (m)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              yaxis = list(title = "Velocity (km/h)",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE),
                              legend = list(x = 0.5, y = 0.9))
        
        fig
        
      }
      
    })
  })
  
  
  
  
  
  #### Race 1 ####
  Race1 <- reactive({
    ### Splits Time calculations ###
    Filtered_Time_data <-  tab_Time_R1() %>% select(variable, value)
    # Filtered_Time_data <-  test %>% select(variable, value)
    
    Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
    Filtered_Time_data$value <- as.numeric(Filtered_Time_data$value)
    
    
    #50 splits
    Split50_Time_50 <- Filtered_Time_data["50",]
    Split50_Time_100 <- Filtered_Time_data["100",]-Filtered_Time_data["50",]
    Split50_Time_150 <- Filtered_Time_data["150",]-Filtered_Time_data["100",]
    Split50_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["150",]
    Split50_Time_250 <- Filtered_Time_data["250",]-Filtered_Time_data["200",]
    Split50_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["250",]
    Split50_Time_350 <- Filtered_Time_data["350",]-Filtered_Time_data["300",]
    Split50_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["350",]
    Split50_Time_450 <- Filtered_Time_data["450",]-Filtered_Time_data["400",]
    Split50_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["450",]
    Split50_Time_550 <- Filtered_Time_data["550",]-Filtered_Time_data["500",]
    Split50_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["550",]
    Split50_Time_650 <- Filtered_Time_data["650",]-Filtered_Time_data["600",]
    Split50_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["650",]
    Split50_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["700",]
    Split50_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["750",]
    Split50_Time_850 <- Filtered_Time_data["850",]-Filtered_Time_data["800",]
    Split50_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["850",]
    Split50_Time_950 <- Filtered_Time_data["950",]-Filtered_Time_data["900",]
    Split50_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["950",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500, Split50_Time_550, Split50_Time_600,
                                       Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800,
                                       Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500)
      
    }else if(input$distance== "Labelled_data_200"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200)
      
      
    }
    Split50_Time_Avg <- sec_to_ms(mean(t(Split50_Time_Data)))
    # Split50_Time_Max1 <- max(t(Split50_Time_Data))
    # Split50_Time_Min1 <- min(t(Split50_Time_Data))
    # Split50_Time_DO <-  round((Split50_Time_Max1-Split50_Time_Min1)/Split50_Time_Max1*100,2)
    # Split50_Time_Max <-  sec_to_ms(Split50_Time_Max1)
    # Split50_Time_Min <-  sec_to_ms(Split50_Time_Min1)
    
    #100 splits
    Split100_Time_100 <- Filtered_Time_data["100",]
    Split100_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["100",]
    Split100_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["200",]
    Split100_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["300",]
    Split100_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["400",]
    Split100_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["500",]
    Split100_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["600",]
    Split100_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["700",]
    Split100_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["800",]
    Split100_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["900",]
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500, Split100_Time_600,
                                        Split100_Time_700, Split100_Time_800,
                                        Split100_Time_900, Split100_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200)
      
    }
    
    
    Split100_Time_Avg <- sec_to_ms(mean(t(Split100_Time_Data)))
    # Split100_Time_Max1 <- max(t(Split100_Time_Data))
    # Split100_Time_Min1 <- min(t(Split100_Time_Data))
    # Split100_Time_DO <-  round((Split100_Time_Max1-Split100_Time_Min1)/Split100_Time_Max1*100,2)
    # Split100_Time_Max <-  sec_to_ms(Split100_Time_Max1)
    # Split100_Time_Min <-  sec_to_ms(Split100_Time_Min1)
    
    #250 splits
    
    Split250_Time_250 <- Filtered_Time_data["250",]
    Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
    Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
    Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                        Split250_Time_750,  Split250_Time_1000)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }
    
    # Split250_Time_Max1 <- max(t(Split250_Time_Data))
    # Split250_Time_Min1 <- min(t(Split250_Time_Data))
    # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
    # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
    # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
    
    #500 Splits
    Split500_Time_500 <- Filtered_Time_data["500",]
    Split500_Time_1000 <- Filtered_Time_data["1000",] - Filtered_Time_data["500",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Time_Data <-  data.table(Split500_Time_500,
                                        Split500_Time_1000)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }else if(input$distance== "Labelled_data_500"){
      Split500_Time_Data <-  data.table(Split500_Time_500)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }
    
    # Split500_Time_Max1 <- max(t(Split500_Time_Data))
    # Split500_Time_Min1 <- min(t(Split500_Time_Data))
    # Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
    # Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
    # Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
    
    
    ########################
    #AvVel 50 splits
    Split50_AvVel_50 <-  round(((50/Split50_Time_50)*3.6),2)
    Split50_AvVel_100 <-  round(((50/Split50_Time_100)*3.6),2)
    Split50_AvVel_150 <-  round(((50/Split50_Time_150)*3.6),2)
    Split50_AvVel_200 <-  round(((50/Split50_Time_200)*3.6),2)
    Split50_AvVel_250 <-  round(((50/Split50_Time_250)*3.6),2)
    Split50_AvVel_300 <-  round(((50/Split50_Time_300)*3.6),2)
    Split50_AvVel_350 <-  round(((50/Split50_Time_350)*3.6),2)
    Split50_AvVel_400 <-  round(((50/Split50_Time_400)*3.6),2)
    Split50_AvVel_450 <-  round(((50/Split50_Time_450)*3.6),2)
    Split50_AvVel_500 <-  round(((50/Split50_Time_500)*3.6),2)
    Split50_AvVel_550 <-  round(((50/Split50_Time_550)*3.6),2)
    Split50_AvVel_600 <-  round(((50/Split50_Time_600)*3.6),2)
    Split50_AvVel_650 <-  round(((50/Split50_Time_650)*3.6),2)
    Split50_AvVel_700 <-  round(((50/Split50_Time_700)*3.6),2)
    Split50_AvVel_750 <-  round(((50/Split50_Time_750)*3.6),2)
    Split50_AvVel_800 <-  round(((50/Split50_Time_800)*3.6),2)
    Split50_AvVel_850 <-  round(((50/Split50_Time_850)*3.6),2)
    Split50_AvVel_900 <-  round(((50/Split50_Time_900)*3.6),2)
    Split50_AvVel_950 <-  round(((50/Split50_Time_950)*3.6),2)
    Split50_AvVel_1000 <-  round(((50/Split50_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500, Split50_AvVel_550, Split50_AvVel_600,
                                        Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800,
                                        Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200)
      
    }
    Split50_AvVel_Data <-  as.numeric(Split50_AvVel_Data)
    Split50_AvVel_Avg <- round(mean(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Max <- round(max(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Min <- round(min(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_DO <-  round((Split50_AvVel_Max-Split50_AvVel_Min)/Split50_AvVel_Max*100,2)
    # AvVel 100 splits
    Split100_AvVel_100 <-  round(((100/Split100_Time_100)*3.6),2)
    Split100_AvVel_200 <-  round(((100/Split100_Time_200)*3.6),2)
    Split100_AvVel_300 <-  round(((100/Split100_Time_300)*3.6),2)
    Split100_AvVel_400 <-  round(((100/Split100_Time_400)*3.6),2)
    Split100_AvVel_500 <-  round(((100/Split100_Time_500)*3.6),2)
    Split100_AvVel_600 <-  round(((100/Split100_Time_600)*3.6),2)
    Split100_AvVel_700 <-  round(((100/Split100_Time_700)*3.6),2)
    Split100_AvVel_800 <-  round(((100/Split100_Time_800)*3.6),2)
    Split100_AvVel_900 <-  round(((100/Split100_Time_900)*3.6),2)
    Split100_AvVel_1000 <-  round(((100/Split100_Time_100)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500, Split100_AvVel_600,
                                         Split100_AvVel_700, Split100_AvVel_800,
                                         Split100_AvVel_900, Split100_AvVel_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200)
      
    }
    Split100_AvVel_Data <-  as.numeric(Split100_AvVel_Data)
    Split100_AvVel_Avg <- round(mean(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Max <- round(max(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Min <- round(min(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_DO <-  round((Split100_AvVel_Max-Split100_AvVel_Min)/Split100_AvVel_Max*100,2)
    
    
    # AvVel 250 splits
    Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
    Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
    Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
    Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                         Split250_AvVel_750,  Split250_AvVel_1000)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2) 
      
    }
    
    
    
    # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
    
    # AvVel 500 splits
    Split500_AvVel_500 <-  round(((500/Split500_Time_500)*3.6),2)
    Split500_AvVel_1000 <-  round(((500/Split500_Time_1000)*3.6),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500,
                                         Split500_AvVel_1000)
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500)
      
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }
    
    # Split500_AvVel_Max <- round(max(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_Min <- round(min(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_DO <-  round((Split500_AvVel_Max-Split500_AvVel_Min)/Split500_AvVel_Max*100,2)
    
    ##############
    
    Filtered_SR_data <-  tab_SR_R1() %>% select(variable, value)
    Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
    Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
    #50 splits
    Split50_SR_50 <- round(((Filtered_SR_data["25",]+Filtered_SR_data["50",])/2),2)
    Split50_SR_100 <- round(((Filtered_SR_data["75",]+Filtered_SR_data["100",])/2),2)
    Split50_SR_150 <- round(((Filtered_SR_data["125",]+Filtered_SR_data["150",])/2),2)
    Split50_SR_200 <- round(((Filtered_SR_data["175",]+Filtered_SR_data["200",])/2),2)
    Split50_SR_250 <- round(((Filtered_SR_data["225",]+Filtered_SR_data["250",])/2),2)
    Split50_SR_300 <- round(((Filtered_SR_data["275",]+Filtered_SR_data["300",])/2),2)
    Split50_SR_350 <- round(((Filtered_SR_data["325",]+Filtered_SR_data["350",])/2),2)
    Split50_SR_400 <- round(((Filtered_SR_data["375",]+Filtered_SR_data["400",])/2),2)
    Split50_SR_450 <- round(((Filtered_SR_data["425",]+Filtered_SR_data["450",])/2),2)
    Split50_SR_500 <- round(((Filtered_SR_data["475",]+Filtered_SR_data["500",])/2),2)
    Split50_SR_550 <- round(((Filtered_SR_data["525",]+Filtered_SR_data["550",])/2),2)
    Split50_SR_600 <- round(((Filtered_SR_data["575",]+Filtered_SR_data["600",])/2),2)
    Split50_SR_650 <- round(((Filtered_SR_data["625",]+Filtered_SR_data["650",])/2),2)
    Split50_SR_700 <- round(((Filtered_SR_data["675",]+Filtered_SR_data["700",])/2),2)
    Split50_SR_750 <- round(((Filtered_SR_data["725",]+Filtered_SR_data["750",])/2),2)
    Split50_SR_800 <- round(((Filtered_SR_data["775",]+Filtered_SR_data["800",])/2),2)
    Split50_SR_850 <- round(((Filtered_SR_data["825",]+Filtered_SR_data["850",])/2),2)
    Split50_SR_900 <- round(((Filtered_SR_data["875",]+Filtered_SR_data["900",])/2),2)
    Split50_SR_950 <- round(((Filtered_SR_data["925",]+Filtered_SR_data["950",])/2),2)
    Split50_SR_1000 <- round(((Filtered_SR_data["975",]+Filtered_SR_data["1000",])/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500, Split50_SR_550, Split50_SR_600,
                                     Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800,
                                     Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200)
      
      
    }
    Split50_SR_Avg <- mean(t(Split50_SR_Data))
    # Split50_SR_Max1 <- max(t(Split50_SR_Data))
    # Split50_SR_Min1 <- min(t(Split50_SR_Data))
    # Split50_SR_DO <-  round((Split50_SR_Max1-Split50_SR_Min1)/Split50_SR_Max1*100,2)
    # Split50_SR_Max <-  sec_to_ms(Split50_SR_Max1)
    # Split50_SR_Min <-  sec_to_ms(Split50_SR_Min1)
    
    #100 splits
    Split100_SR_100 <- round(((Split50_SR_50+Split50_SR_100)/2),2)
    Split100_SR_200 <- round(((Split50_SR_150+Split50_SR_200)/2),2)
    Split100_SR_300 <- round(((Split50_SR_250+Split50_SR_300)/2),2)
    Split100_SR_400 <- round(((Split50_SR_350+Split50_SR_400)/2),2)
    Split100_SR_500 <- round(((Split50_SR_450+Split50_SR_500)/2),2)
    Split100_SR_600 <- round(((Split50_SR_550+Split50_SR_600)/2),2)
    Split100_SR_700 <- round(((Split50_SR_650+Split50_SR_700)/2),2)
    Split100_SR_800 <- round(((Split50_SR_750+Split50_SR_800)/2),2)
    Split100_SR_900 <- round(((Split50_SR_850+Split50_SR_900)/2),2)
    Split100_SR_1000 <- round(((Split50_SR_50+Split50_SR_1000)/2),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500, Split100_SR_600,
                                      Split100_SR_700, Split100_SR_800,
                                      Split100_SR_900, Split100_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200)
      
    }
    
    
    Split100_SR_Avg <- mean(t(Split100_SR_Data))
    # Split100_SR_Max1 <- max(t(Split100_SR_Data))
    # Split100_SR_Min1 <- min(t(Split100_SR_Data))
    # Split100_SR_DO <-  round((Split100_SR_Max1-Split100_SR_Min1)/Split100_SR_Max1*100,2)
    # Split100_SR_Max <-  sec_to_ms(Split100_SR_Max1)
    # Split100_SR_Min <-  sec_to_ms(Split100_SR_Min1)
    
    #250 splits
    
    Split250_SR_250 <- round(((Split50_SR_50+Split50_SR_100+Split50_SR_150+Split50_SR_200+Split50_SR_250)/5),2)
    Split250_SR_500 <- round(((Split50_SR_300+Split50_SR_350+Split50_SR_400+Split50_SR_450+Split50_SR_500)/5),2)
    Split250_SR_750 <- round(((Split50_SR_550+Split50_SR_600+Split50_SR_650+Split50_SR_700+Split50_SR_750)/5),2)
    Split250_SR_1000 <- round(((Split50_SR_800+Split50_SR_850+Split50_SR_900+Split50_SR_950+Split50_SR_1000)/5),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                      Split250_SR_750,  Split250_SR_1000)
      Split250_SR_Avg <- mean(t(Split250_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500)
      Split250_SR_Avg <- mean(t(Split250_SR_Data)) 
    }
    
    # Split250_SR_Max1 <- max(t(Split250_SR_Data))
    # Split250_SR_Min1 <- min(t(Split250_SR_Data))
    # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
    # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
    # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
    
    #500 Splits
    Split500_SR_500 <- round(((Split250_SR_250+Split250_SR_500)/2),2)
    Split500_SR_1000 <- round(((Split250_SR_750+Split250_SR_1000)/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_SR_Data <-  data.table(Split500_SR_500,
                                      Split500_SR_1000)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      Split500_SR_Data <-  data.table(Split500_SR_500)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
      
      
    }
    
    
    
    ########################
    
    #AvVel 50 splits
    
    Split50_Pace500_50 <-  Split50_Time_50*10
    Split50_Pace500_100 <-  Split50_Time_100*10
    Split50_Pace500_150 <-  Split50_Time_150*10
    Split50_Pace500_200 <-  Split50_Time_200*10
    Split50_Pace500_250 <-  Split50_Time_250*10
    Split50_Pace500_300 <-  Split50_Time_300*10
    Split50_Pace500_350 <-  Split50_Time_350*10
    Split50_Pace500_400 <-  Split50_Time_400*10
    Split50_Pace500_450 <-  Split50_Time_450*10
    Split50_Pace500_500 <-  Split50_Time_500*10
    Split50_Pace500_550 <-  Split50_Time_550*10
    Split50_Pace500_600 <-  Split50_Time_600*10
    Split50_Pace500_650 <-  Split50_Time_650*10
    Split50_Pace500_700 <-  Split50_Time_700*10
    Split50_Pace500_750 <-  Split50_Time_750*10
    Split50_Pace500_800 <-  Split50_Time_800*10
    Split50_Pace500_850 <-  Split50_Time_850*10
    Split50_Pace500_900 <-  Split50_Time_900*10
    Split50_Pace500_950 <-  Split50_Time_950*10
    Split50_Pace500_1000 <-  Split50_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500, Split50_Pace500_550, Split50_Pace500_600,
                                          Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800,
                                          Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200)
      
    }
    Split50_Pace500_Data <-  as.numeric(Split50_Pace500_Data)
    Split50_Pace500_Avg <- sec_to_ms(round(mean(t(Split50_Pace500_Data)),2))
    # Split50_Pace500_Max <- round(max(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_Min <- round(min(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_DO <-  round((Split50_Pace500_Max-Split50_Pace500_Min)/Split50_Pace500_Max*100,2)
    # Pace 100 splits
    Split100_Pace500_100 <-  Split100_Time_100*5
    Split100_Pace500_200 <-  Split100_Time_200*5
    Split100_Pace500_300 <-  Split100_Time_300*5
    Split100_Pace500_400 <-  Split100_Time_400*5
    Split100_Pace500_500 <-  Split100_Time_500*5
    Split100_Pace500_600 <-  Split100_Time_600*5
    Split100_Pace500_700 <-  Split100_Time_700*5
    Split100_Pace500_800 <-  Split100_Time_800*5
    Split100_Pace500_900 <-  Split100_Time_900*5
    Split100_Pace500_1000 <-  Split100_Time_1000*5
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500, Split100_Pace500_600,
                                           Split100_Pace500_700, Split100_Pace500_800,
                                           Split100_Pace500_900, Split100_Pace500_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200)
    }
    Split100_Pace500_Data <-  as.numeric(Split100_Pace500_Data)
    Split100_Pace500_Avg <- sec_to_ms(round(mean(t(Split100_Pace500_Data)),2))
    # Split100_Pace500_Max <- round(max(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_Min <- round(min(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_DO <-  round((Split100_Pace500_Max-Split100_Pace500_Min)/Split100_Pace500_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace500_250 <-  Split250_Time_250*2
    Split250_Pace500_500 <-  Split250_Time_500*2
    Split250_Pace500_750 <-  Split250_Time_750*2
    Split250_Pace500_1000 <-  Split250_Time_1000*2
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                           Split250_Pace500_750,  Split250_Pace500_1000)
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500)
      
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }
    
    
    
    # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace500_500 <-  Split500_Time_500*1
    Split500_Pace500_1000 <-  Split500_Time_1000*1
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500,
                                           Split500_Pace500_1000)
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500)
      
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }
    
    # Split500_Pace500_Max <- round(max(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_Min <- round(min(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_DO <-  round((Split500_Pace500_Max-Split500_Pace500_Min)/Split500_Pace500_Max*100,2)
    
    
    #######################
    
    
    #AvVel 50 splits
    
    Split50_Pace1000_50 <-  Split50_Time_50*20
    Split50_Pace1000_100 <-  Split50_Time_100*20
    Split50_Pace1000_150 <-  Split50_Time_150*20
    Split50_Pace1000_200 <-  Split50_Time_200*20
    Split50_Pace1000_250 <-  Split50_Time_250*20
    Split50_Pace1000_300 <-  Split50_Time_300*20
    Split50_Pace1000_350 <-  Split50_Time_350*20
    Split50_Pace1000_400 <-  Split50_Time_400*20
    Split50_Pace1000_450 <-  Split50_Time_450*20
    Split50_Pace1000_500 <-  Split50_Time_500*20
    Split50_Pace1000_550 <-  Split50_Time_550*20
    Split50_Pace1000_600 <-  Split50_Time_600*20
    Split50_Pace1000_650 <-  Split50_Time_650*20
    Split50_Pace1000_700 <-  Split50_Time_700*20
    Split50_Pace1000_750 <-  Split50_Time_750*20
    Split50_Pace1000_800 <-  Split50_Time_800*20
    Split50_Pace1000_850 <-  Split50_Time_850*20
    Split50_Pace1000_900 <-  Split50_Time_900*20
    Split50_Pace1000_950 <-  Split50_Time_950*20
    Split50_Pace1000_1000 <-  Split50_Time_1000*20
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500, Split50_Pace1000_550, Split50_Pace1000_600,
                                           Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800,
                                           Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200)
      
    }
    Split50_Pace1000_Data <-  as.numeric(Split50_Pace1000_Data)
    Split50_Pace1000_Avg <- sec_to_ms(round(mean(t(Split50_Pace1000_Data)),2))
    # Split50_Pace1000_Max <- round(max(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_Min <- round(min(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_DO <-  round((Split50_Pace1000_Max-Split50_Pace1000_Min)/Split50_Pace1000_Max*100,2)
    # Pace 100 splits
    Split100_Pace1000_100 <-  Split100_Time_100*10
    Split100_Pace1000_200 <-  Split100_Time_200*10
    Split100_Pace1000_300 <-  Split100_Time_300*10
    Split100_Pace1000_400 <-  Split100_Time_400*10
    Split100_Pace1000_500 <-  Split100_Time_500*10
    Split100_Pace1000_600 <-  Split100_Time_600*10
    Split100_Pace1000_700 <-  Split100_Time_700*10
    Split100_Pace1000_800 <-  Split100_Time_800*10
    Split100_Pace1000_900 <- Split100_Time_900*10
    Split100_Pace1000_1000 <-  Split100_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500, Split100_Pace1000_600,
                                            Split100_Pace1000_700, Split100_Pace1000_800,
                                            Split100_Pace1000_900, Split100_Pace1000_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500)
      
      
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200)
      
    }
    Split100_Pace1000_Data <-  as.numeric(Split100_Pace1000_Data)
    Split100_Pace1000_Avg <- sec_to_ms(round(mean(t(Split100_Pace1000_Data)),2))
    # Split100_Pace1000_Max <- round(max(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_Min <- round(min(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_DO <-  round((Split100_Pace1000_Max-Split100_Pace1000_Min)/Split100_Pace1000_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace1000_250 <-  Split250_Time_250*8
    Split250_Pace1000_500 <-  Split250_Time_500*8
    Split250_Pace1000_750 <-  Split250_Time_750*8
    Split250_Pace1000_1000 <-  Split250_Time_1000*8
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                            Split250_Pace1000_750,  Split250_Pace1000_1000)
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500)
      
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }
    
    
    
    # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace1000_500 <-  Split500_Time_500*2
    Split500_Pace1000_1000 <-  Split500_Time_1000*2
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500,
                                            Split500_Pace1000_1000)
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500)
      
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
      
    }
    
    # Split500_Pace1000_Max <- round(max(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_Min <- round(min(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_DO <-  round((Split500_Pace1000_Max-Split500_Pace1000_Min)/Split500_Pace1000_Max*100,2)
    
    
    #### Convert to ms in table
    Split50_Time_50 <- sec_to_ms(Split50_Time_50)
    Split50_Time_100 <- sec_to_ms(Split50_Time_100)
    Split50_Time_150 <- sec_to_ms(Split50_Time_150)
    Split50_Time_200 <- sec_to_ms(Split50_Time_200)
    Split50_Time_250 <- sec_to_ms(Split50_Time_250)
    Split50_Time_300 <- sec_to_ms(Split50_Time_300)
    Split50_Time_350 <- sec_to_ms(Split50_Time_350)
    Split50_Time_400 <- sec_to_ms(Split50_Time_400)
    Split50_Time_450 <- sec_to_ms(Split50_Time_450)
    Split50_Time_500 <- sec_to_ms(Split50_Time_500)
    Split50_Time_550 <- sec_to_ms(Split50_Time_550)
    Split50_Time_600 <- sec_to_ms(Split50_Time_600)
    Split50_Time_650 <- sec_to_ms(Split50_Time_650)
    Split50_Time_700 <- sec_to_ms(Split50_Time_700)
    Split50_Time_750 <- sec_to_ms(Split50_Time_750)
    Split50_Time_800 <- sec_to_ms(Split50_Time_800)
    Split50_Time_850 <- sec_to_ms(Split50_Time_850)
    Split50_Time_900 <- sec_to_ms(Split50_Time_900)
    Split50_Time_950 <- sec_to_ms(Split50_Time_950)
    Split50_Time_1000 <- sec_to_ms(Split50_Time_1000)
    
    
    #100 splits
    Split100_Time_100 <- sec_to_ms(Split100_Time_100)
    Split100_Time_200 <- sec_to_ms(Split100_Time_200)
    Split100_Time_300 <- sec_to_ms(Split100_Time_300)
    Split100_Time_400 <- sec_to_ms(Split100_Time_400)
    Split100_Time_500 <- sec_to_ms(Split100_Time_500)
    Split100_Time_600 <- sec_to_ms(Split100_Time_600)
    Split100_Time_700 <- sec_to_ms(Split100_Time_700)
    Split100_Time_800 <- sec_to_ms(Split100_Time_800)
    Split100_Time_900 <- sec_to_ms(Split100_Time_900)
    Split100_Time_1000 <- sec_to_ms(Split100_Time_1000)
    
    
    #250 splits
    
    Split250_Time_250 <- sec_to_ms(Split250_Time_250)
    Split250_Time_500 <- sec_to_ms(Split250_Time_500)
    Split250_Time_750 <- sec_to_ms(Split250_Time_750)
    Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
    
    
    
    #500 Splits
    Split500_Time_500 <- sec_to_ms(Split500_Time_500)
    Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
    
    ###################
    #### Convert to ms in table
    Split50_Pace500_50 <- sec_to_ms(Split50_Pace500_50)
    Split50_Pace500_100 <- sec_to_ms(Split50_Pace500_100)
    Split50_Pace500_150 <- sec_to_ms(Split50_Pace500_150)
    Split50_Pace500_200 <- sec_to_ms(Split50_Pace500_200)
    Split50_Pace500_250 <- sec_to_ms(Split50_Pace500_250)
    Split50_Pace500_300 <- sec_to_ms(Split50_Pace500_300)
    Split50_Pace500_350 <- sec_to_ms(Split50_Pace500_350)
    Split50_Pace500_400 <- sec_to_ms(Split50_Pace500_400)
    Split50_Pace500_450 <- sec_to_ms(Split50_Pace500_450)
    Split50_Pace500_500 <- sec_to_ms(Split50_Pace500_500)
    Split50_Pace500_550 <- sec_to_ms(Split50_Pace500_550)
    Split50_Pace500_600 <- sec_to_ms(Split50_Pace500_600)
    Split50_Pace500_650 <- sec_to_ms(Split50_Pace500_650)
    Split50_Pace500_700 <- sec_to_ms(Split50_Pace500_700)
    Split50_Pace500_750 <- sec_to_ms(Split50_Pace500_750)
    Split50_Pace500_800 <- sec_to_ms(Split50_Pace500_800)
    Split50_Pace500_850 <- sec_to_ms(Split50_Pace500_850)
    Split50_Pace500_900 <- sec_to_ms(Split50_Pace500_900)
    Split50_Pace500_950 <- sec_to_ms(Split50_Pace500_950)
    Split50_Pace500_1000 <- sec_to_ms(Split50_Pace500_1000)
    
    
    #100 splits
    Split100_Pace500_100 <- sec_to_ms(Split100_Pace500_100)
    Split100_Pace500_200 <- sec_to_ms(Split100_Pace500_200)
    Split100_Pace500_300 <- sec_to_ms(Split100_Pace500_300)
    Split100_Pace500_400 <- sec_to_ms(Split100_Pace500_400)
    Split100_Pace500_500 <- sec_to_ms(Split100_Pace500_500)
    Split100_Pace500_600 <- sec_to_ms(Split100_Pace500_600)
    Split100_Pace500_700 <- sec_to_ms(Split100_Pace500_700)
    Split100_Pace500_800 <- sec_to_ms(Split100_Pace500_800)
    Split100_Pace500_900 <- sec_to_ms(Split100_Pace500_900)
    Split100_Pace500_1000 <- sec_to_ms(Split100_Pace500_1000)
    
    
    #250 splits
    
    Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
    Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
    Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
    Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
    
    
    
    #500 Splits
    Split500_Pace500_500 <- sec_to_ms(Split500_Pace500_500)
    Split500_Pace500_1000 <- sec_to_ms(Split500_Pace500_1000)
    
    ################### 
    
    Split50_Pace1000_50 <- sec_to_ms(Split50_Pace1000_50)
    Split50_Pace1000_100 <- sec_to_ms(Split50_Pace1000_100)
    Split50_Pace1000_150 <- sec_to_ms(Split50_Pace1000_150)
    Split50_Pace1000_200 <- sec_to_ms(Split50_Pace1000_200)
    Split50_Pace1000_250 <- sec_to_ms(Split50_Pace1000_250)
    Split50_Pace1000_300 <- sec_to_ms(Split50_Pace1000_300)
    Split50_Pace1000_350 <- sec_to_ms(Split50_Pace1000_350)
    Split50_Pace1000_400 <- sec_to_ms(Split50_Pace1000_400)
    Split50_Pace1000_450 <- sec_to_ms(Split50_Pace1000_450)
    Split50_Pace1000_500 <- sec_to_ms(Split50_Pace1000_500)
    Split50_Pace1000_550 <- sec_to_ms(Split50_Pace1000_550)
    Split50_Pace1000_600 <- sec_to_ms(Split50_Pace1000_600)
    Split50_Pace1000_650 <- sec_to_ms(Split50_Pace1000_650)
    Split50_Pace1000_700 <- sec_to_ms(Split50_Pace1000_700)
    Split50_Pace1000_750 <- sec_to_ms(Split50_Pace1000_750)
    Split50_Pace1000_800 <- sec_to_ms(Split50_Pace1000_800)
    Split50_Pace1000_850 <- sec_to_ms(Split50_Pace1000_850)
    Split50_Pace1000_900 <- sec_to_ms(Split50_Pace1000_900)
    Split50_Pace1000_950 <- sec_to_ms(Split50_Pace1000_950)
    Split50_Pace1000_1000 <- sec_to_ms(Split50_Pace1000_1000)
    
    #100 splits
    Split100_Pace1000_100 <- sec_to_ms(Split100_Pace1000_100)
    Split100_Pace1000_200 <- sec_to_ms(Split100_Pace1000_200)
    Split100_Pace1000_300 <- sec_to_ms(Split100_Pace1000_300)
    Split100_Pace1000_400 <- sec_to_ms(Split100_Pace1000_400)
    Split100_Pace1000_500 <- sec_to_ms(Split100_Pace1000_500)
    Split100_Pace1000_600 <- sec_to_ms(Split100_Pace1000_600)
    Split100_Pace1000_700 <- sec_to_ms(Split100_Pace1000_700)
    Split100_Pace1000_800 <- sec_to_ms(Split100_Pace1000_800)
    Split100_Pace1000_900 <- sec_to_ms(Split100_Pace1000_900)
    Split100_Pace1000_1000 <- sec_to_ms(Split100_Pace1000_1000)
    
    
    #250 splits
    
    Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
    Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
    Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
    Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
    
    
    
    #500 Splits
    Split500_Pace1000_500 <- sec_to_ms(Split500_Pace1000_500)
    Split500_Pace1000_1000 <- sec_to_ms(Split500_Pace1000_1000)
    
    ################### 
    
    
    
    
    
    
    if (input$distance== "Labelled_data_1000"){
      
      
      Race1 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   Split50_Time_550, Split50_Time_600, Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800, Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500, "",
                                                    Split100_Time_600, "", Split100_Time_700, "", Split100_Time_800, "", Split100_Time_900, "", Split100_Time_1000,
                                                    "", Split100_Time_Avg),
                           
                           
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500, "", "", "", "", Split250_Time_750, "", "", "", "", Split250_Time_1000,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500, "", "", "", "", "", "", "", "", "", Split500_Time_1000,
                                                    "", Split500_Time_Avg),
                           
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               Split50_Pace500_550, Split50_Pace500_600, Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800, Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500, "",
                                                Split100_Pace500_600, "", Split100_Pace500_700, "", Split100_Pace500_800, "", Split100_Pace500_900, "", Split100_Pace500_1000,
                                                "", Split100_Pace500_Avg),
                           
                           
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500, "", "", "", "", Split250_Pace500_750, "", "", "", "", Split250_Pace500_1000,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500, "", "", "", "", "", "", "", "", "", Split500_Pace500_1000,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                Split50_Pace1000_550, Split50_Pace1000_600, Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800, Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500, "",
                                                 Split100_Pace1000_600, "", Split100_Pace1000_700, "", Split100_Pace1000_800, "", Split100_Pace1000_900, "", Split100_Pace1000_1000,
                                                 "", Split100_Pace1000_Avg),
                           
                           
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500, "", "", "", "", Split250_Pace1000_750, "", "", "", "", Split250_Pace1000_1000,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500, "", "", "", "", "", "", "", "", "", Split500_Pace1000_1000,
                                                 "", Split500_Pace1000_Avg),
                           
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                Split50_AvVel_550, Split50_AvVel_600, Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800, Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000,
                                                "", Split50_AvVel_Avg),
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500, "",
                                                 Split100_AvVel_600, "", Split100_AvVel_700, "", Split100_AvVel_800, "", Split100_AvVel_900, "", Split100_AvVel_1000,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500, "", "", "", "", Split250_AvVel_750, "", "", "", "", Split250_AvVel_1000,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500, "", "", "", "", "", "", "", "", "", Split500_AvVel_1000,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              Split50_SR_550, Split50_SR_600, Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800, Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000,
                                              "", Split50_SR_Avg),
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500, "",
                                               Split100_SR_600, "", Split100_SR_700, "", Split100_SR_800, "", Split100_SR_900, "", Split100_SR_1000,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500, "", "", "", "", Split250_SR_750, "", "", "", "", Split250_SR_1000,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500, "", "", "", "", "", "", "", "", "", Split500_SR_1000,
                                               "", Split500_SR_Avg)
      )
      
    }else if(input$distance== "Labelled_data_500"){
      
      
      
      Race1 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500,
                                                    "", Split100_Time_Avg),
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500,
                                                    "", Split500_Time_Avg),
                           
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500,
                                                "", Split100_Pace500_Avg),
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500,
                                                 "", Split100_Pace1000_Avg),
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500,
                                                 "", Split500_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500,
                                               "", Split500_SR_Avg))
      
    }else if(input$distance== "Labelled_data_200"){
      Race1 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200,
                                                    "", Split100_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200,
                                                "", Split100_Pace500_Avg),
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200,
                                                 "", Split100_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200,
                                                 "", Split100_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200,
                                               "", Split100_SR_Avg))
      
    }
    return(Race1)
    
    
    
  })
  
  #select
  
  #### Race 2 ####
  Race2 <- reactive({
    ### Splits Time calculations ###
    Filtered_Time_data <-  tab_Time_R2() %>% select(variable, value)
    Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
    Filtered_Time_data$value <- as.numeric(Filtered_Time_data$value)
    #50 splits
    Split50_Time_50 <- Filtered_Time_data["50",]
    Split50_Time_100 <- Filtered_Time_data["100",]-Filtered_Time_data["50",]
    Split50_Time_150 <- Filtered_Time_data["150",]-Filtered_Time_data["100",]
    Split50_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["150",]
    Split50_Time_250 <- Filtered_Time_data["250",]-Filtered_Time_data["200",]
    Split50_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["250",]
    Split50_Time_350 <- Filtered_Time_data["350",]-Filtered_Time_data["300",]
    Split50_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["350",]
    Split50_Time_450 <- Filtered_Time_data["450",]-Filtered_Time_data["400",]
    Split50_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["450",]
    Split50_Time_550 <- Filtered_Time_data["550",]-Filtered_Time_data["500",]
    Split50_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["550",]
    Split50_Time_650 <- Filtered_Time_data["650",]-Filtered_Time_data["600",]
    Split50_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["650",]
    Split50_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["700",]
    Split50_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["750",]
    Split50_Time_850 <- Filtered_Time_data["850",]-Filtered_Time_data["800",]
    Split50_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["850",]
    Split50_Time_950 <- Filtered_Time_data["950",]-Filtered_Time_data["900",]
    Split50_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["950",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500, Split50_Time_550, Split50_Time_600,
                                       Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800,
                                       Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500)
      
    }else if(input$distance== "Labelled_data_200"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200)
      
      
    }
    Split50_Time_Avg <- sec_to_ms(mean(t(Split50_Time_Data)))
    # Split50_Time_Max1 <- max(t(Split50_Time_Data))
    # Split50_Time_Min1 <- min(t(Split50_Time_Data))
    # Split50_Time_DO <-  round((Split50_Time_Max1-Split50_Time_Min1)/Split50_Time_Max1*100,2)
    # Split50_Time_Max <-  sec_to_ms(Split50_Time_Max1)
    # Split50_Time_Min <-  sec_to_ms(Split50_Time_Min1)
    
    #100 splits
    Split100_Time_100 <- Filtered_Time_data["100",]
    Split100_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["100",]
    Split100_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["200",]
    Split100_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["300",]
    Split100_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["400",]
    Split100_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["500",]
    Split100_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["600",]
    Split100_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["700",]
    Split100_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["800",]
    Split100_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["900",]
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500, Split100_Time_600,
                                        Split100_Time_700, Split100_Time_800,
                                        Split100_Time_900, Split100_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200)
      
    }
    
    
    Split100_Time_Avg <- sec_to_ms(mean(t(Split100_Time_Data)))
    # Split100_Time_Max1 <- max(t(Split100_Time_Data))
    # Split100_Time_Min1 <- min(t(Split100_Time_Data))
    # Split100_Time_DO <-  round((Split100_Time_Max1-Split100_Time_Min1)/Split100_Time_Max1*100,2)
    # Split100_Time_Max <-  sec_to_ms(Split100_Time_Max1)
    # Split100_Time_Min <-  sec_to_ms(Split100_Time_Min1)
    
    #250 splits
    
    Split250_Time_250 <- Filtered_Time_data["250",]
    Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
    Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
    Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                        Split250_Time_750,  Split250_Time_1000)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }
    
    # Split250_Time_Max1 <- max(t(Split250_Time_Data))
    # Split250_Time_Min1 <- min(t(Split250_Time_Data))
    # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
    # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
    # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
    
    #500 Splits
    Split500_Time_500 <- Filtered_Time_data["500",]
    Split500_Time_1000 <- Filtered_Time_data["1000",] - Filtered_Time_data["500",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Time_Data <-  data.table(Split500_Time_500,
                                        Split500_Time_1000)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }else if(input$distance== "Labelled_data_500"){
      Split500_Time_Data <-  data.table(Split500_Time_500)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }
    
    # Split500_Time_Max1 <- max(t(Split500_Time_Data))
    # Split500_Time_Min1 <- min(t(Split500_Time_Data))
    # Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
    # Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
    # Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
    
    
    ########################
    #AvVel 50 splits
    Split50_AvVel_50 <-  round(((50/Split50_Time_50)*3.6),2)
    Split50_AvVel_100 <-  round(((50/Split50_Time_100)*3.6),2)
    Split50_AvVel_150 <-  round(((50/Split50_Time_150)*3.6),2)
    Split50_AvVel_200 <-  round(((50/Split50_Time_200)*3.6),2)
    Split50_AvVel_250 <-  round(((50/Split50_Time_250)*3.6),2)
    Split50_AvVel_300 <-  round(((50/Split50_Time_300)*3.6),2)
    Split50_AvVel_350 <-  round(((50/Split50_Time_350)*3.6),2)
    Split50_AvVel_400 <-  round(((50/Split50_Time_400)*3.6),2)
    Split50_AvVel_450 <-  round(((50/Split50_Time_450)*3.6),2)
    Split50_AvVel_500 <-  round(((50/Split50_Time_500)*3.6),2)
    Split50_AvVel_550 <-  round(((50/Split50_Time_550)*3.6),2)
    Split50_AvVel_600 <-  round(((50/Split50_Time_600)*3.6),2)
    Split50_AvVel_650 <-  round(((50/Split50_Time_650)*3.6),2)
    Split50_AvVel_700 <-  round(((50/Split50_Time_700)*3.6),2)
    Split50_AvVel_750 <-  round(((50/Split50_Time_750)*3.6),2)
    Split50_AvVel_800 <-  round(((50/Split50_Time_800)*3.6),2)
    Split50_AvVel_850 <-  round(((50/Split50_Time_850)*3.6),2)
    Split50_AvVel_900 <-  round(((50/Split50_Time_900)*3.6),2)
    Split50_AvVel_950 <-  round(((50/Split50_Time_950)*3.6),2)
    Split50_AvVel_1000 <-  round(((50/Split50_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500, Split50_AvVel_550, Split50_AvVel_600,
                                        Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800,
                                        Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200)
      
    }
    Split50_AvVel_Data <-  as.numeric(Split50_AvVel_Data)
    Split50_AvVel_Avg <- round(mean(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Max <- round(max(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Min <- round(min(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_DO <-  round((Split50_AvVel_Max-Split50_AvVel_Min)/Split50_AvVel_Max*100,2)
    # AvVel 100 splits
    Split100_AvVel_100 <-  round(((100/Split100_Time_100)*3.6),2)
    Split100_AvVel_200 <-  round(((100/Split100_Time_200)*3.6),2)
    Split100_AvVel_300 <-  round(((100/Split100_Time_300)*3.6),2)
    Split100_AvVel_400 <-  round(((100/Split100_Time_400)*3.6),2)
    Split100_AvVel_500 <-  round(((100/Split100_Time_500)*3.6),2)
    Split100_AvVel_600 <-  round(((100/Split100_Time_600)*3.6),2)
    Split100_AvVel_700 <-  round(((100/Split100_Time_700)*3.6),2)
    Split100_AvVel_800 <-  round(((100/Split100_Time_800)*3.6),2)
    Split100_AvVel_900 <-  round(((100/Split100_Time_900)*3.6),2)
    Split100_AvVel_1000 <-  round(((100/Split100_Time_100)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500, Split100_AvVel_600,
                                         Split100_AvVel_700, Split100_AvVel_800,
                                         Split100_AvVel_900, Split100_AvVel_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200)
      
    }
    Split100_AvVel_Data <-  as.numeric(Split100_AvVel_Data)
    Split100_AvVel_Avg <- round(mean(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Max <- round(max(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Min <- round(min(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_DO <-  round((Split100_AvVel_Max-Split100_AvVel_Min)/Split100_AvVel_Max*100,2)
    
    
    # AvVel 250 splits
    Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
    Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
    Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
    Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                         Split250_AvVel_750,  Split250_AvVel_1000)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2) 
      
    }
    
    
    
    # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
    
    # AvVel 500 splits
    Split500_AvVel_500 <-  round(((500/Split500_Time_500)*3.6),2)
    Split500_AvVel_1000 <-  round(((500/Split500_Time_1000)*3.6),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500,
                                         Split500_AvVel_1000)
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500)
      
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }
    
    # Split500_AvVel_Max <- round(max(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_Min <- round(min(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_DO <-  round((Split500_AvVel_Max-Split500_AvVel_Min)/Split500_AvVel_Max*100,2)
    
    ##############
    
    Filtered_SR_data <-  tab_SR_R1() %>% select(variable, value)
    Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
    Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
    #50 splits
    Split50_SR_50 <- round(((Filtered_SR_data["25",]+Filtered_SR_data["50",])/2),2)
    Split50_SR_100 <- round(((Filtered_SR_data["75",]+Filtered_SR_data["100",])/2),2)
    Split50_SR_150 <- round(((Filtered_SR_data["125",]+Filtered_SR_data["150",])/2),2)
    Split50_SR_200 <- round(((Filtered_SR_data["175",]+Filtered_SR_data["200",])/2),2)
    Split50_SR_250 <- round(((Filtered_SR_data["225",]+Filtered_SR_data["250",])/2),2)
    Split50_SR_300 <- round(((Filtered_SR_data["275",]+Filtered_SR_data["300",])/2),2)
    Split50_SR_350 <- round(((Filtered_SR_data["325",]+Filtered_SR_data["350",])/2),2)
    Split50_SR_400 <- round(((Filtered_SR_data["375",]+Filtered_SR_data["400",])/2),2)
    Split50_SR_450 <- round(((Filtered_SR_data["425",]+Filtered_SR_data["450",])/2),2)
    Split50_SR_500 <- round(((Filtered_SR_data["475",]+Filtered_SR_data["500",])/2),2)
    Split50_SR_550 <- round(((Filtered_SR_data["525",]+Filtered_SR_data["550",])/2),2)
    Split50_SR_600 <- round(((Filtered_SR_data["575",]+Filtered_SR_data["600",])/2),2)
    Split50_SR_650 <- round(((Filtered_SR_data["625",]+Filtered_SR_data["650",])/2),2)
    Split50_SR_700 <- round(((Filtered_SR_data["675",]+Filtered_SR_data["700",])/2),2)
    Split50_SR_750 <- round(((Filtered_SR_data["725",]+Filtered_SR_data["750",])/2),2)
    Split50_SR_800 <- round(((Filtered_SR_data["775",]+Filtered_SR_data["800",])/2),2)
    Split50_SR_850 <- round(((Filtered_SR_data["825",]+Filtered_SR_data["850",])/2),2)
    Split50_SR_900 <- round(((Filtered_SR_data["875",]+Filtered_SR_data["900",])/2),2)
    Split50_SR_950 <- round(((Filtered_SR_data["925",]+Filtered_SR_data["950",])/2),2)
    Split50_SR_1000 <- round(((Filtered_SR_data["975",]+Filtered_SR_data["1000",])/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500, Split50_SR_550, Split50_SR_600,
                                     Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800,
                                     Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200)
      
      
    }
    Split50_SR_Avg <- mean(t(Split50_SR_Data))
    # Split50_SR_Max1 <- max(t(Split50_SR_Data))
    # Split50_SR_Min1 <- min(t(Split50_SR_Data))
    # Split50_SR_DO <-  round((Split50_SR_Max1-Split50_SR_Min1)/Split50_SR_Max1*100,2)
    # Split50_SR_Max <-  sec_to_ms(Split50_SR_Max1)
    # Split50_SR_Min <-  sec_to_ms(Split50_SR_Min1)
    
    #100 splits
    Split100_SR_100 <- round(((Split50_SR_50+Split50_SR_100)/2),2)
    Split100_SR_200 <- round(((Split50_SR_150+Split50_SR_200)/2),2)
    Split100_SR_300 <- round(((Split50_SR_250+Split50_SR_300)/2),2)
    Split100_SR_400 <- round(((Split50_SR_350+Split50_SR_400)/2),2)
    Split100_SR_500 <- round(((Split50_SR_450+Split50_SR_500)/2),2)
    Split100_SR_600 <- round(((Split50_SR_550+Split50_SR_600)/2),2)
    Split100_SR_700 <- round(((Split50_SR_650+Split50_SR_700)/2),2)
    Split100_SR_800 <- round(((Split50_SR_750+Split50_SR_800)/2),2)
    Split100_SR_900 <- round(((Split50_SR_850+Split50_SR_900)/2),2)
    Split100_SR_1000 <- round(((Split50_SR_50+Split50_SR_1000)/2),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500, Split100_SR_600,
                                      Split100_SR_700, Split100_SR_800,
                                      Split100_SR_900, Split100_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200)
      
    }
    
    
    Split100_SR_Avg <- mean(t(Split100_SR_Data))
    # Split100_SR_Max1 <- max(t(Split100_SR_Data))
    # Split100_SR_Min1 <- min(t(Split100_SR_Data))
    # Split100_SR_DO <-  round((Split100_SR_Max1-Split100_SR_Min1)/Split100_SR_Max1*100,2)
    # Split100_SR_Max <-  sec_to_ms(Split100_SR_Max1)
    # Split100_SR_Min <-  sec_to_ms(Split100_SR_Min1)
    
    #250 splits
    
    Split250_SR_250 <- round(((Split50_SR_50+Split50_SR_100+Split50_SR_150+Split50_SR_200+Split50_SR_250)/5),2)
    Split250_SR_500 <- round(((Split50_SR_300+Split50_SR_350+Split50_SR_400+Split50_SR_450+Split50_SR_500)/5),2)
    Split250_SR_750 <- round(((Split50_SR_550+Split50_SR_600+Split50_SR_650+Split50_SR_700+Split50_SR_750)/5),2)
    Split250_SR_1000 <- round(((Split50_SR_800+Split50_SR_850+Split50_SR_900+Split50_SR_950+Split50_SR_1000)/5),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                      Split250_SR_750,  Split250_SR_1000)
      Split250_SR_Avg <- mean(t(Split250_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500)
      Split250_SR_Avg <- mean(t(Split250_SR_Data)) 
    }
    
    # Split250_SR_Max1 <- max(t(Split250_SR_Data))
    # Split250_SR_Min1 <- min(t(Split250_SR_Data))
    # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
    # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
    # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
    
    #500 Splits
    Split500_SR_500 <- round(((Split250_SR_250+Split250_SR_500)/2),2)
    Split500_SR_1000 <- round(((Split250_SR_750+Split250_SR_1000)/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_SR_Data <-  data.table(Split500_SR_500,
                                      Split500_SR_1000)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      Split500_SR_Data <-  data.table(Split500_SR_500)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
      
      
    }
    
    
    
    ########################
    
    #AvVel 50 splits
    
    Split50_Pace500_50 <-  Split50_Time_50*10
    Split50_Pace500_100 <-  Split50_Time_100*10
    Split50_Pace500_150 <-  Split50_Time_150*10
    Split50_Pace500_200 <-  Split50_Time_200*10
    Split50_Pace500_250 <-  Split50_Time_250*10
    Split50_Pace500_300 <-  Split50_Time_300*10
    Split50_Pace500_350 <-  Split50_Time_350*10
    Split50_Pace500_400 <-  Split50_Time_400*10
    Split50_Pace500_450 <-  Split50_Time_450*10
    Split50_Pace500_500 <-  Split50_Time_500*10
    Split50_Pace500_550 <-  Split50_Time_550*10
    Split50_Pace500_600 <-  Split50_Time_600*10
    Split50_Pace500_650 <-  Split50_Time_650*10
    Split50_Pace500_700 <-  Split50_Time_700*10
    Split50_Pace500_750 <-  Split50_Time_750*10
    Split50_Pace500_800 <-  Split50_Time_800*10
    Split50_Pace500_850 <-  Split50_Time_850*10
    Split50_Pace500_900 <-  Split50_Time_900*10
    Split50_Pace500_950 <-  Split50_Time_950*10
    Split50_Pace500_1000 <-  Split50_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500, Split50_Pace500_550, Split50_Pace500_600,
                                          Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800,
                                          Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200)
      
    }
    Split50_Pace500_Data <-  as.numeric(Split50_Pace500_Data)
    Split50_Pace500_Avg <- sec_to_ms(round(mean(t(Split50_Pace500_Data)),2))
    # Split50_Pace500_Max <- round(max(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_Min <- round(min(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_DO <-  round((Split50_Pace500_Max-Split50_Pace500_Min)/Split50_Pace500_Max*100,2)
    # Pace 100 splits
    Split100_Pace500_100 <-  Split100_Time_100*5
    Split100_Pace500_200 <-  Split100_Time_200*5
    Split100_Pace500_300 <-  Split100_Time_300*5
    Split100_Pace500_400 <-  Split100_Time_400*5
    Split100_Pace500_500 <-  Split100_Time_500*5
    Split100_Pace500_600 <-  Split100_Time_600*5
    Split100_Pace500_700 <-  Split100_Time_700*5
    Split100_Pace500_800 <-  Split100_Time_800*5
    Split100_Pace500_900 <-  Split100_Time_900*5
    Split100_Pace500_1000 <-  Split100_Time_1000*5
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500, Split100_Pace500_600,
                                           Split100_Pace500_700, Split100_Pace500_800,
                                           Split100_Pace500_900, Split100_Pace500_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200)
    }
    Split100_Pace500_Data <-  as.numeric(Split100_Pace500_Data)
    Split100_Pace500_Avg <- sec_to_ms(round(mean(t(Split100_Pace500_Data)),2))
    # Split100_Pace500_Max <- round(max(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_Min <- round(min(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_DO <-  round((Split100_Pace500_Max-Split100_Pace500_Min)/Split100_Pace500_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace500_250 <-  Split250_Time_250*2
    Split250_Pace500_500 <-  Split250_Time_500*2
    Split250_Pace500_750 <-  Split250_Time_750*2
    Split250_Pace500_1000 <-  Split250_Time_1000*2
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                           Split250_Pace500_750,  Split250_Pace500_1000)
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500)
      
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }
    
    
    
    # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace500_500 <-  Split500_Time_500*1
    Split500_Pace500_1000 <-  Split500_Time_1000*1
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500,
                                           Split500_Pace500_1000)
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500)
      
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }
    
    # Split500_Pace500_Max <- round(max(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_Min <- round(min(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_DO <-  round((Split500_Pace500_Max-Split500_Pace500_Min)/Split500_Pace500_Max*100,2)
    
    
    #######################
    
    
    #AvVel 50 splits
    
    Split50_Pace1000_50 <-  Split50_Time_50*20
    Split50_Pace1000_100 <-  Split50_Time_100*20
    Split50_Pace1000_150 <-  Split50_Time_150*20
    Split50_Pace1000_200 <-  Split50_Time_200*20
    Split50_Pace1000_250 <-  Split50_Time_250*20
    Split50_Pace1000_300 <-  Split50_Time_300*20
    Split50_Pace1000_350 <-  Split50_Time_350*20
    Split50_Pace1000_400 <-  Split50_Time_400*20
    Split50_Pace1000_450 <-  Split50_Time_450*20
    Split50_Pace1000_500 <-  Split50_Time_500*20
    Split50_Pace1000_550 <-  Split50_Time_550*20
    Split50_Pace1000_600 <-  Split50_Time_600*20
    Split50_Pace1000_650 <-  Split50_Time_650*20
    Split50_Pace1000_700 <-  Split50_Time_700*20
    Split50_Pace1000_750 <-  Split50_Time_750*20
    Split50_Pace1000_800 <-  Split50_Time_800*20
    Split50_Pace1000_850 <-  Split50_Time_850*20
    Split50_Pace1000_900 <-  Split50_Time_900*20
    Split50_Pace1000_950 <-  Split50_Time_950*20
    Split50_Pace1000_1000 <-  Split50_Time_1000*20
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500, Split50_Pace1000_550, Split50_Pace1000_600,
                                           Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800,
                                           Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200)
      
    }
    Split50_Pace1000_Data <-  as.numeric(Split50_Pace1000_Data)
    Split50_Pace1000_Avg <- sec_to_ms(round(mean(t(Split50_Pace1000_Data)),2))
    # Split50_Pace1000_Max <- round(max(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_Min <- round(min(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_DO <-  round((Split50_Pace1000_Max-Split50_Pace1000_Min)/Split50_Pace1000_Max*100,2)
    # Pace 100 splits
    Split100_Pace1000_100 <-  Split100_Time_100*10
    Split100_Pace1000_200 <-  Split100_Time_200*10
    Split100_Pace1000_300 <-  Split100_Time_300*10
    Split100_Pace1000_400 <-  Split100_Time_400*10
    Split100_Pace1000_500 <-  Split100_Time_500*10
    Split100_Pace1000_600 <-  Split100_Time_600*10
    Split100_Pace1000_700 <-  Split100_Time_700*10
    Split100_Pace1000_800 <-  Split100_Time_800*10
    Split100_Pace1000_900 <- Split100_Time_900*10
    Split100_Pace1000_1000 <-  Split100_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500, Split100_Pace1000_600,
                                            Split100_Pace1000_700, Split100_Pace1000_800,
                                            Split100_Pace1000_900, Split100_Pace1000_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500)
      
      
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200)
      
    }
    Split100_Pace1000_Data <-  as.numeric(Split100_Pace1000_Data)
    Split100_Pace1000_Avg <- sec_to_ms(round(mean(t(Split100_Pace1000_Data)),2))
    # Split100_Pace1000_Max <- round(max(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_Min <- round(min(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_DO <-  round((Split100_Pace1000_Max-Split100_Pace1000_Min)/Split100_Pace1000_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace1000_250 <-  Split250_Time_250*8
    Split250_Pace1000_500 <-  Split250_Time_500*8
    Split250_Pace1000_750 <-  Split250_Time_750*8
    Split250_Pace1000_1000 <-  Split250_Time_1000*8
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                            Split250_Pace1000_750,  Split250_Pace1000_1000)
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500)
      
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }
    
    
    
    # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace1000_500 <-  Split500_Time_500*2
    Split500_Pace1000_1000 <-  Split500_Time_1000*2
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500,
                                            Split500_Pace1000_1000)
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500)
      
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
      
    }
    
    # Split500_Pace1000_Max <- round(max(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_Min <- round(min(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_DO <-  round((Split500_Pace1000_Max-Split500_Pace1000_Min)/Split500_Pace1000_Max*100,2)
    
    
    #### Convert to ms in table
    Split50_Time_50 <- sec_to_ms(Split50_Time_50)
    Split50_Time_100 <- sec_to_ms(Split50_Time_100)
    Split50_Time_150 <- sec_to_ms(Split50_Time_150)
    Split50_Time_200 <- sec_to_ms(Split50_Time_200)
    Split50_Time_250 <- sec_to_ms(Split50_Time_250)
    Split50_Time_300 <- sec_to_ms(Split50_Time_300)
    Split50_Time_350 <- sec_to_ms(Split50_Time_350)
    Split50_Time_400 <- sec_to_ms(Split50_Time_400)
    Split50_Time_450 <- sec_to_ms(Split50_Time_450)
    Split50_Time_500 <- sec_to_ms(Split50_Time_500)
    Split50_Time_550 <- sec_to_ms(Split50_Time_550)
    Split50_Time_600 <- sec_to_ms(Split50_Time_600)
    Split50_Time_650 <- sec_to_ms(Split50_Time_650)
    Split50_Time_700 <- sec_to_ms(Split50_Time_700)
    Split50_Time_750 <- sec_to_ms(Split50_Time_750)
    Split50_Time_800 <- sec_to_ms(Split50_Time_800)
    Split50_Time_850 <- sec_to_ms(Split50_Time_850)
    Split50_Time_900 <- sec_to_ms(Split50_Time_900)
    Split50_Time_950 <- sec_to_ms(Split50_Time_950)
    Split50_Time_1000 <- sec_to_ms(Split50_Time_1000)
    
    
    #100 splits
    Split100_Time_100 <- sec_to_ms(Split100_Time_100)
    Split100_Time_200 <- sec_to_ms(Split100_Time_200)
    Split100_Time_300 <- sec_to_ms(Split100_Time_300)
    Split100_Time_400 <- sec_to_ms(Split100_Time_400)
    Split100_Time_500 <- sec_to_ms(Split100_Time_500)
    Split100_Time_600 <- sec_to_ms(Split100_Time_600)
    Split100_Time_700 <- sec_to_ms(Split100_Time_700)
    Split100_Time_800 <- sec_to_ms(Split100_Time_800)
    Split100_Time_900 <- sec_to_ms(Split100_Time_900)
    Split100_Time_1000 <- sec_to_ms(Split100_Time_1000)
    
    
    #250 splits
    
    Split250_Time_250 <- sec_to_ms(Split250_Time_250)
    Split250_Time_500 <- sec_to_ms(Split250_Time_500)
    Split250_Time_750 <- sec_to_ms(Split250_Time_750)
    Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
    
    
    
    #500 Splits
    Split500_Time_500 <- sec_to_ms(Split500_Time_500)
    Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
    
    ###################
    #### Convert to ms in table
    Split50_Pace500_50 <- sec_to_ms(Split50_Pace500_50)
    Split50_Pace500_100 <- sec_to_ms(Split50_Pace500_100)
    Split50_Pace500_150 <- sec_to_ms(Split50_Pace500_150)
    Split50_Pace500_200 <- sec_to_ms(Split50_Pace500_200)
    Split50_Pace500_250 <- sec_to_ms(Split50_Pace500_250)
    Split50_Pace500_300 <- sec_to_ms(Split50_Pace500_300)
    Split50_Pace500_350 <- sec_to_ms(Split50_Pace500_350)
    Split50_Pace500_400 <- sec_to_ms(Split50_Pace500_400)
    Split50_Pace500_450 <- sec_to_ms(Split50_Pace500_450)
    Split50_Pace500_500 <- sec_to_ms(Split50_Pace500_500)
    Split50_Pace500_550 <- sec_to_ms(Split50_Pace500_550)
    Split50_Pace500_600 <- sec_to_ms(Split50_Pace500_600)
    Split50_Pace500_650 <- sec_to_ms(Split50_Pace500_650)
    Split50_Pace500_700 <- sec_to_ms(Split50_Pace500_700)
    Split50_Pace500_750 <- sec_to_ms(Split50_Pace500_750)
    Split50_Pace500_800 <- sec_to_ms(Split50_Pace500_800)
    Split50_Pace500_850 <- sec_to_ms(Split50_Pace500_850)
    Split50_Pace500_900 <- sec_to_ms(Split50_Pace500_900)
    Split50_Pace500_950 <- sec_to_ms(Split50_Pace500_950)
    Split50_Pace500_1000 <- sec_to_ms(Split50_Pace500_1000)
    
    
    #100 splits
    Split100_Pace500_100 <- sec_to_ms(Split100_Pace500_100)
    Split100_Pace500_200 <- sec_to_ms(Split100_Pace500_200)
    Split100_Pace500_300 <- sec_to_ms(Split100_Pace500_300)
    Split100_Pace500_400 <- sec_to_ms(Split100_Pace500_400)
    Split100_Pace500_500 <- sec_to_ms(Split100_Pace500_500)
    Split100_Pace500_600 <- sec_to_ms(Split100_Pace500_600)
    Split100_Pace500_700 <- sec_to_ms(Split100_Pace500_700)
    Split100_Pace500_800 <- sec_to_ms(Split100_Pace500_800)
    Split100_Pace500_900 <- sec_to_ms(Split100_Pace500_900)
    Split100_Pace500_1000 <- sec_to_ms(Split100_Pace500_1000)
    
    
    #250 splits
    
    Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
    Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
    Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
    Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
    
    
    
    #500 Splits
    Split500_Pace500_500 <- sec_to_ms(Split500_Pace500_500)
    Split500_Pace500_1000 <- sec_to_ms(Split500_Pace500_1000)
    
    ################### 
    
    Split50_Pace1000_50 <- sec_to_ms(Split50_Pace1000_50)
    Split50_Pace1000_100 <- sec_to_ms(Split50_Pace1000_100)
    Split50_Pace1000_150 <- sec_to_ms(Split50_Pace1000_150)
    Split50_Pace1000_200 <- sec_to_ms(Split50_Pace1000_200)
    Split50_Pace1000_250 <- sec_to_ms(Split50_Pace1000_250)
    Split50_Pace1000_300 <- sec_to_ms(Split50_Pace1000_300)
    Split50_Pace1000_350 <- sec_to_ms(Split50_Pace1000_350)
    Split50_Pace1000_400 <- sec_to_ms(Split50_Pace1000_400)
    Split50_Pace1000_450 <- sec_to_ms(Split50_Pace1000_450)
    Split50_Pace1000_500 <- sec_to_ms(Split50_Pace1000_500)
    Split50_Pace1000_550 <- sec_to_ms(Split50_Pace1000_550)
    Split50_Pace1000_600 <- sec_to_ms(Split50_Pace1000_600)
    Split50_Pace1000_650 <- sec_to_ms(Split50_Pace1000_650)
    Split50_Pace1000_700 <- sec_to_ms(Split50_Pace1000_700)
    Split50_Pace1000_750 <- sec_to_ms(Split50_Pace1000_750)
    Split50_Pace1000_800 <- sec_to_ms(Split50_Pace1000_800)
    Split50_Pace1000_850 <- sec_to_ms(Split50_Pace1000_850)
    Split50_Pace1000_900 <- sec_to_ms(Split50_Pace1000_900)
    Split50_Pace1000_950 <- sec_to_ms(Split50_Pace1000_950)
    Split50_Pace1000_1000 <- sec_to_ms(Split50_Pace1000_1000)
    
    #100 splits
    Split100_Pace1000_100 <- sec_to_ms(Split100_Pace1000_100)
    Split100_Pace1000_200 <- sec_to_ms(Split100_Pace1000_200)
    Split100_Pace1000_300 <- sec_to_ms(Split100_Pace1000_300)
    Split100_Pace1000_400 <- sec_to_ms(Split100_Pace1000_400)
    Split100_Pace1000_500 <- sec_to_ms(Split100_Pace1000_500)
    Split100_Pace1000_600 <- sec_to_ms(Split100_Pace1000_600)
    Split100_Pace1000_700 <- sec_to_ms(Split100_Pace1000_700)
    Split100_Pace1000_800 <- sec_to_ms(Split100_Pace1000_800)
    Split100_Pace1000_900 <- sec_to_ms(Split100_Pace1000_900)
    Split100_Pace1000_1000 <- sec_to_ms(Split100_Pace1000_1000)
    
    
    #250 splits
    
    Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
    Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
    Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
    Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
    
    
    
    #500 Splits
    Split500_Pace1000_500 <- sec_to_ms(Split500_Pace1000_500)
    Split500_Pace1000_1000 <- sec_to_ms(Split500_Pace1000_1000)
    
    ################### 
    
    
    
    
    if (input$distance== "Labelled_data_1000"){
      
      
      Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   Split50_Time_550, Split50_Time_600, Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800, Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500, "",
                                                    Split100_Time_600, "", Split100_Time_700, "", Split100_Time_800, "", Split100_Time_900, "", Split100_Time_1000,
                                                    "", Split100_Time_Avg),
                           
                           
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500, "", "", "", "", Split250_Time_750, "", "", "", "", Split250_Time_1000,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500, "", "", "", "", "", "", "", "", "", Split500_Time_1000,
                                                    "", Split500_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               Split50_Pace500_550, Split50_Pace500_600, Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800, Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500, "",
                                                Split100_Pace500_600, "", Split100_Pace500_700, "", Split100_Pace500_800, "", Split100_Pace500_900, "", Split100_Pace500_1000,
                                                "", Split100_Pace500_Avg),
                           
                           
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500, "", "", "", "", Split250_Pace500_750, "", "", "", "", Split250_Pace500_1000,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500, "", "", "", "", "", "", "", "", "", Split500_Pace500_1000,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                Split50_Pace1000_550, Split50_Pace1000_600, Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800, Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500, "",
                                                 Split100_Pace1000_600, "", Split100_Pace1000_700, "", Split100_Pace1000_800, "", Split100_Pace1000_900, "", Split100_Pace1000_1000,
                                                 "", Split100_Pace1000_Avg),
                           
                           
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500, "", "", "", "", Split250_Pace1000_750, "", "", "", "", Split250_Pace1000_1000,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500, "", "", "", "", "", "", "", "", "", Split500_Pace1000_1000,
                                                 "", Split500_Pace1000_Avg),
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                Split50_AvVel_550, Split50_AvVel_600, Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800, Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000,
                                                "", Split50_AvVel_Avg),
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500, "",
                                                 Split100_AvVel_600, "", Split100_AvVel_700, "", Split100_AvVel_800, "", Split100_AvVel_900, "", Split100_AvVel_1000,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500, "", "", "", "", Split250_AvVel_750, "", "", "", "", Split250_AvVel_1000,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500, "", "", "", "", "", "", "", "", "", Split500_AvVel_1000,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              Split50_SR_550, Split50_SR_600, Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800, Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000,
                                              "", Split50_SR_Avg),
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500, "",
                                               Split100_SR_600, "", Split100_SR_700, "", Split100_SR_800, "", Split100_SR_900, "", Split100_SR_1000,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500, "", "", "", "", Split250_SR_750, "", "", "", "", Split250_SR_1000,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500, "", "", "", "", "", "", "", "", "", Split500_SR_1000,
                                               "", Split500_SR_Avg))
      
    }else if(input$distance== "Labelled_data_500"){
      
      
      
      Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500,
                                                    "", Split100_Time_Avg),
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500,
                                                    "", Split500_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500,
                                                "", Split100_Pace500_Avg),
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500,
                                                 "", Split100_Pace1000_Avg),
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500,
                                                 "", Split500_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500,
                                               "", Split500_SR_Avg))
      
    }else if(input$distance== "Labelled_data_200"){
      
      Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200,
                                                    "", Split100_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200,
                                                "", Split100_Pace500_Avg),
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200,
                                                 "", Split100_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200,
                                                 "", Split100_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200,
                                               "", Split100_SR_Avg))
    }
    return(Race2)
    
    
    
  })
  
  #select
  #### Top10 ####
  Top10 <- reactive({
    ### Splits Time calculations ####
    
    
    
    Filtered_Time_data <- tab2_Time() %>% select(variable, Average)
    Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
    #Filtered_Time_data <-  rename(Filtered_Time_data,c(Average = "value"))
    Filtered_Time_data$Average <- as.numeric(Filtered_Time_data$Average)
    #50 splits
    Split50_Time_50 <- Filtered_Time_data["50",]
    Split50_Time_100 <- Filtered_Time_data["100",]-Filtered_Time_data["50",]
    Split50_Time_150 <- Filtered_Time_data["150",]-Filtered_Time_data["100",]
    Split50_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["150",]
    Split50_Time_250 <- Filtered_Time_data["250",]-Filtered_Time_data["200",]
    Split50_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["250",]
    Split50_Time_350 <- Filtered_Time_data["350",]-Filtered_Time_data["300",]
    Split50_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["350",]
    Split50_Time_450 <- Filtered_Time_data["450",]-Filtered_Time_data["400",]
    Split50_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["450",]
    Split50_Time_550 <- Filtered_Time_data["550",]-Filtered_Time_data["500",]
    Split50_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["550",]
    Split50_Time_650 <- Filtered_Time_data["650",]-Filtered_Time_data["600",]
    Split50_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["650",]
    Split50_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["700",]
    Split50_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["750",]
    Split50_Time_850 <- Filtered_Time_data["850",]-Filtered_Time_data["800",]
    Split50_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["850",]
    Split50_Time_950 <- Filtered_Time_data["950",]-Filtered_Time_data["900",]
    Split50_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["950",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500, Split50_Time_550, Split50_Time_600,
                                       Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800,
                                       Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                       Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                       Split50_Time_450, Split50_Time_500)
      
    }else if(input$distance== "Labelled_data_200"){
      
      Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200)
      
      
    }
    Split50_Time_Avg <- sec_to_ms(mean(t(Split50_Time_Data)))
    # Split50_Time_Max1 <- max(t(Split50_Time_Data))
    # Split50_Time_Min1 <- min(t(Split50_Time_Data))
    # Split50_Time_DO <-  round((Split50_Time_Max1-Split50_Time_Min1)/Split50_Time_Max1*100,2)
    # Split50_Time_Max <-  sec_to_ms(Split50_Time_Max1)
    # Split50_Time_Min <-  sec_to_ms(Split50_Time_Min1)
    
    #100 splits
    Split100_Time_100 <- Filtered_Time_data["100",]
    Split100_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["100",]
    Split100_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["200",]
    Split100_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["300",]
    Split100_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["400",]
    Split100_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["500",]
    Split100_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["600",]
    Split100_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["700",]
    Split100_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["800",]
    Split100_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["900",]
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500, Split100_Time_600,
                                        Split100_Time_700, Split100_Time_800,
                                        Split100_Time_900, Split100_Time_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                        Split100_Time_300,  Split100_Time_400,
                                        Split100_Time_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200)
      
    }
    
    
    Split100_Time_Avg <- sec_to_ms(mean(t(Split100_Time_Data)))
    # Split100_Time_Max1 <- max(t(Split100_Time_Data))
    # Split100_Time_Min1 <- min(t(Split100_Time_Data))
    # Split100_Time_DO <-  round((Split100_Time_Max1-Split100_Time_Min1)/Split100_Time_Max1*100,2)
    # Split100_Time_Max <-  sec_to_ms(Split100_Time_Max1)
    # Split100_Time_Min <-  sec_to_ms(Split100_Time_Min1)
    
    #250 splits
    
    Split250_Time_250 <- Filtered_Time_data["250",]
    Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
    Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
    Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                        Split250_Time_750,  Split250_Time_1000)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500)
      Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
    }
    
    # Split250_Time_Max1 <- max(t(Split250_Time_Data))
    # Split250_Time_Min1 <- min(t(Split250_Time_Data))
    # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
    # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
    # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
    
    #500 Splits
    Split500_Time_500 <- Filtered_Time_data["500",]
    Split500_Time_1000 <- Filtered_Time_data["1000",] - Filtered_Time_data["500",]
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Time_Data <-  data.table(Split500_Time_500,
                                        Split500_Time_1000)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }else if(input$distance== "Labelled_data_500"){
      Split500_Time_Data <-  data.table(Split500_Time_500)
      Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
      
    }
    
    # Split500_Time_Max1 <- max(t(Split500_Time_Data))
    # Split500_Time_Min1 <- min(t(Split500_Time_Data))
    # Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
    # Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
    # Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
    
    
    ########################
    #AvVel 50 splits
    Split50_AvVel_50 <-  round(((50/Split50_Time_50)*3.6),2)
    Split50_AvVel_100 <-  round(((50/Split50_Time_100)*3.6),2)
    Split50_AvVel_150 <-  round(((50/Split50_Time_150)*3.6),2)
    Split50_AvVel_200 <-  round(((50/Split50_Time_200)*3.6),2)
    Split50_AvVel_250 <-  round(((50/Split50_Time_250)*3.6),2)
    Split50_AvVel_300 <-  round(((50/Split50_Time_300)*3.6),2)
    Split50_AvVel_350 <-  round(((50/Split50_Time_350)*3.6),2)
    Split50_AvVel_400 <-  round(((50/Split50_Time_400)*3.6),2)
    Split50_AvVel_450 <-  round(((50/Split50_Time_450)*3.6),2)
    Split50_AvVel_500 <-  round(((50/Split50_Time_500)*3.6),2)
    Split50_AvVel_550 <-  round(((50/Split50_Time_550)*3.6),2)
    Split50_AvVel_600 <-  round(((50/Split50_Time_600)*3.6),2)
    Split50_AvVel_650 <-  round(((50/Split50_Time_650)*3.6),2)
    Split50_AvVel_700 <-  round(((50/Split50_Time_700)*3.6),2)
    Split50_AvVel_750 <-  round(((50/Split50_Time_750)*3.6),2)
    Split50_AvVel_800 <-  round(((50/Split50_Time_800)*3.6),2)
    Split50_AvVel_850 <-  round(((50/Split50_Time_850)*3.6),2)
    Split50_AvVel_900 <-  round(((50/Split50_Time_900)*3.6),2)
    Split50_AvVel_950 <-  round(((50/Split50_Time_950)*3.6),2)
    Split50_AvVel_1000 <-  round(((50/Split50_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500, Split50_AvVel_550, Split50_AvVel_600,
                                        Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800,
                                        Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                        Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                        Split50_AvVel_450, Split50_AvVel_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200)
      
    }
    Split50_AvVel_Data <-  as.numeric(Split50_AvVel_Data)
    Split50_AvVel_Avg <- round(mean(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Max <- round(max(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_Min <- round(min(t(Split50_AvVel_Data)),2)
    # Split50_AvVel_DO <-  round((Split50_AvVel_Max-Split50_AvVel_Min)/Split50_AvVel_Max*100,2)
    # AvVel 100 splits
    Split100_AvVel_100 <-  round(((100/Split100_Time_100)*3.6),2)
    Split100_AvVel_200 <-  round(((100/Split100_Time_200)*3.6),2)
    Split100_AvVel_300 <-  round(((100/Split100_Time_300)*3.6),2)
    Split100_AvVel_400 <-  round(((100/Split100_Time_400)*3.6),2)
    Split100_AvVel_500 <-  round(((100/Split100_Time_500)*3.6),2)
    Split100_AvVel_600 <-  round(((100/Split100_Time_600)*3.6),2)
    Split100_AvVel_700 <-  round(((100/Split100_Time_700)*3.6),2)
    Split100_AvVel_800 <-  round(((100/Split100_Time_800)*3.6),2)
    Split100_AvVel_900 <-  round(((100/Split100_Time_900)*3.6),2)
    Split100_AvVel_1000 <-  round(((100/Split100_Time_100)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500, Split100_AvVel_600,
                                         Split100_AvVel_700, Split100_AvVel_800,
                                         Split100_AvVel_900, Split100_AvVel_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                         Split100_AvVel_300,  Split100_AvVel_400,
                                         Split100_AvVel_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200)
      
    }
    Split100_AvVel_Data <-  as.numeric(Split100_AvVel_Data)
    Split100_AvVel_Avg <- round(mean(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Max <- round(max(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_Min <- round(min(t(Split100_AvVel_Data)),2)
    # Split100_AvVel_DO <-  round((Split100_AvVel_Max-Split100_AvVel_Min)/Split100_AvVel_Max*100,2)
    
    
    # AvVel 250 splits
    Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
    Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
    Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
    Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                         Split250_AvVel_750,  Split250_AvVel_1000)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500)
      Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
      Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2) 
      
    }
    
    
    
    # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
    # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
    
    # AvVel 500 splits
    Split500_AvVel_500 <-  round(((500/Split500_Time_500)*3.6),2)
    Split500_AvVel_1000 <-  round(((500/Split500_Time_1000)*3.6),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500,
                                         Split500_AvVel_1000)
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }else if(input$distance== "Labelled_data_500"){
      Split500_AvVel_Data <-  data.table(Split500_AvVel_500)
      
      Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
      Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
    }
    
    # Split500_AvVel_Max <- round(max(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_Min <- round(min(t(Split500_AvVel_Data)),2)
    # Split500_AvVel_DO <-  round((Split500_AvVel_Max-Split500_AvVel_Min)/Split500_AvVel_Max*100,2)
    
    ##############
    
    Filtered_SR_data <-  tab_SR_R1() %>% select(variable, value)
    Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
    Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
    #50 splits
    Split50_SR_50 <- round(((Filtered_SR_data["25",]+Filtered_SR_data["50",])/2),2)
    Split50_SR_100 <- round(((Filtered_SR_data["75",]+Filtered_SR_data["100",])/2),2)
    Split50_SR_150 <- round(((Filtered_SR_data["125",]+Filtered_SR_data["150",])/2),2)
    Split50_SR_200 <- round(((Filtered_SR_data["175",]+Filtered_SR_data["200",])/2),2)
    Split50_SR_250 <- round(((Filtered_SR_data["225",]+Filtered_SR_data["250",])/2),2)
    Split50_SR_300 <- round(((Filtered_SR_data["275",]+Filtered_SR_data["300",])/2),2)
    Split50_SR_350 <- round(((Filtered_SR_data["325",]+Filtered_SR_data["350",])/2),2)
    Split50_SR_400 <- round(((Filtered_SR_data["375",]+Filtered_SR_data["400",])/2),2)
    Split50_SR_450 <- round(((Filtered_SR_data["425",]+Filtered_SR_data["450",])/2),2)
    Split50_SR_500 <- round(((Filtered_SR_data["475",]+Filtered_SR_data["500",])/2),2)
    Split50_SR_550 <- round(((Filtered_SR_data["525",]+Filtered_SR_data["550",])/2),2)
    Split50_SR_600 <- round(((Filtered_SR_data["575",]+Filtered_SR_data["600",])/2),2)
    Split50_SR_650 <- round(((Filtered_SR_data["625",]+Filtered_SR_data["650",])/2),2)
    Split50_SR_700 <- round(((Filtered_SR_data["675",]+Filtered_SR_data["700",])/2),2)
    Split50_SR_750 <- round(((Filtered_SR_data["725",]+Filtered_SR_data["750",])/2),2)
    Split50_SR_800 <- round(((Filtered_SR_data["775",]+Filtered_SR_data["800",])/2),2)
    Split50_SR_850 <- round(((Filtered_SR_data["825",]+Filtered_SR_data["850",])/2),2)
    Split50_SR_900 <- round(((Filtered_SR_data["875",]+Filtered_SR_data["900",])/2),2)
    Split50_SR_950 <- round(((Filtered_SR_data["925",]+Filtered_SR_data["950",])/2),2)
    Split50_SR_1000 <- round(((Filtered_SR_data["975",]+Filtered_SR_data["1000",])/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500, Split50_SR_550, Split50_SR_600,
                                     Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800,
                                     Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                     Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                     Split50_SR_450, Split50_SR_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200)
      
      
    }
    Split50_SR_Avg <- mean(t(Split50_SR_Data))
    # Split50_SR_Max1 <- max(t(Split50_SR_Data))
    # Split50_SR_Min1 <- min(t(Split50_SR_Data))
    # Split50_SR_DO <-  round((Split50_SR_Max1-Split50_SR_Min1)/Split50_SR_Max1*100,2)
    # Split50_SR_Max <-  sec_to_ms(Split50_SR_Max1)
    # Split50_SR_Min <-  sec_to_ms(Split50_SR_Min1)
    
    #100 splits
    Split100_SR_100 <- round(((Split50_SR_50+Split50_SR_100)/2),2)
    Split100_SR_200 <- round(((Split50_SR_150+Split50_SR_200)/2),2)
    Split100_SR_300 <- round(((Split50_SR_250+Split50_SR_300)/2),2)
    Split100_SR_400 <- round(((Split50_SR_350+Split50_SR_400)/2),2)
    Split100_SR_500 <- round(((Split50_SR_450+Split50_SR_500)/2),2)
    Split100_SR_600 <- round(((Split50_SR_550+Split50_SR_600)/2),2)
    Split100_SR_700 <- round(((Split50_SR_650+Split50_SR_700)/2),2)
    Split100_SR_800 <- round(((Split50_SR_750+Split50_SR_800)/2),2)
    Split100_SR_900 <- round(((Split50_SR_850+Split50_SR_900)/2),2)
    Split100_SR_1000 <- round(((Split50_SR_50+Split50_SR_1000)/2),2)
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500, Split100_SR_600,
                                      Split100_SR_700, Split100_SR_800,
                                      Split100_SR_900, Split100_SR_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                      Split100_SR_300,  Split100_SR_400,
                                      Split100_SR_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200)
      
    }
    
    
    Split100_SR_Avg <- mean(t(Split100_SR_Data))
    # Split100_SR_Max1 <- max(t(Split100_SR_Data))
    # Split100_SR_Min1 <- min(t(Split100_SR_Data))
    # Split100_SR_DO <-  round((Split100_SR_Max1-Split100_SR_Min1)/Split100_SR_Max1*100,2)
    # Split100_SR_Max <-  sec_to_ms(Split100_SR_Max1)
    # Split100_SR_Min <-  sec_to_ms(Split100_SR_Min1)
    
    #250 splits
    
    Split250_SR_250 <- round(((Split50_SR_50+Split50_SR_100+Split50_SR_150+Split50_SR_200+Split50_SR_250)/5),2)
    Split250_SR_500 <- round(((Split50_SR_300+Split50_SR_350+Split50_SR_400+Split50_SR_450+Split50_SR_500)/5),2)
    Split250_SR_750 <- round(((Split50_SR_550+Split50_SR_600+Split50_SR_650+Split50_SR_700+Split50_SR_750)/5),2)
    Split250_SR_1000 <- round(((Split50_SR_800+Split50_SR_850+Split50_SR_900+Split50_SR_950+Split50_SR_1000)/5),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                      Split250_SR_750,  Split250_SR_1000)
      Split250_SR_Avg <- mean(t(Split250_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      
      Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500)
      Split250_SR_Avg <- mean(t(Split250_SR_Data)) 
    }
    
    # Split250_SR_Max1 <- max(t(Split250_SR_Data))
    # Split250_SR_Min1 <- min(t(Split250_SR_Data))
    # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
    # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
    # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
    
    #500 Splits
    Split500_SR_500 <- round(((Split250_SR_250+Split250_SR_500)/2),2)
    Split500_SR_1000 <- round(((Split250_SR_750+Split250_SR_1000)/2),2)
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_SR_Data <-  data.table(Split500_SR_500,
                                      Split500_SR_1000)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
    }else if(input$distance== "Labelled_data_500"){
      Split500_SR_Data <-  data.table(Split500_SR_500)
      Split500_SR_Avg <- mean(t(Split500_SR_Data))
      
      
    }
    
    
    
    ########################
    
    #AvVel 50 splits
    
    Split50_Pace500_50 <-  Split50_Time_50*10
    Split50_Pace500_100 <-  Split50_Time_100*10
    Split50_Pace500_150 <-  Split50_Time_150*10
    Split50_Pace500_200 <-  Split50_Time_200*10
    Split50_Pace500_250 <-  Split50_Time_250*10
    Split50_Pace500_300 <-  Split50_Time_300*10
    Split50_Pace500_350 <-  Split50_Time_350*10
    Split50_Pace500_400 <-  Split50_Time_400*10
    Split50_Pace500_450 <-  Split50_Time_450*10
    Split50_Pace500_500 <-  Split50_Time_500*10
    Split50_Pace500_550 <-  Split50_Time_550*10
    Split50_Pace500_600 <-  Split50_Time_600*10
    Split50_Pace500_650 <-  Split50_Time_650*10
    Split50_Pace500_700 <-  Split50_Time_700*10
    Split50_Pace500_750 <-  Split50_Time_750*10
    Split50_Pace500_800 <-  Split50_Time_800*10
    Split50_Pace500_850 <-  Split50_Time_850*10
    Split50_Pace500_900 <-  Split50_Time_900*10
    Split50_Pace500_950 <-  Split50_Time_950*10
    Split50_Pace500_1000 <-  Split50_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500, Split50_Pace500_550, Split50_Pace500_600,
                                          Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800,
                                          Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                          Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                          Split50_Pace500_450, Split50_Pace500_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200)
      
    }
    Split50_Pace500_Data <-  as.numeric(Split50_Pace500_Data)
    Split50_Pace500_Avg <- sec_to_ms(round(mean(t(Split50_Pace500_Data)),2))
    # Split50_Pace500_Max <- round(max(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_Min <- round(min(t(Split50_Pace500_Data)),2)
    # Split50_Pace500_DO <-  round((Split50_Pace500_Max-Split50_Pace500_Min)/Split50_Pace500_Max*100,2)
    # Pace 100 splits
    Split100_Pace500_100 <-  Split100_Time_100*5
    Split100_Pace500_200 <-  Split100_Time_200*5
    Split100_Pace500_300 <-  Split100_Time_300*5
    Split100_Pace500_400 <-  Split100_Time_400*5
    Split100_Pace500_500 <-  Split100_Time_500*5
    Split100_Pace500_600 <-  Split100_Time_600*5
    Split100_Pace500_700 <-  Split100_Time_700*5
    Split100_Pace500_800 <-  Split100_Time_800*5
    Split100_Pace500_900 <-  Split100_Time_900*5
    Split100_Pace500_1000 <-  Split100_Time_1000*5
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500, Split100_Pace500_600,
                                           Split100_Pace500_700, Split100_Pace500_800,
                                           Split100_Pace500_900, Split100_Pace500_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                           Split100_Pace500_300,  Split100_Pace500_400,
                                           Split100_Pace500_500)
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200)
    }
    Split100_Pace500_Data <-  as.numeric(Split100_Pace500_Data)
    Split100_Pace500_Avg <- sec_to_ms(round(mean(t(Split100_Pace500_Data)),2))
    # Split100_Pace500_Max <- round(max(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_Min <- round(min(t(Split100_Pace500_Data)),2)
    # Split100_Pace500_DO <-  round((Split100_Pace500_Max-Split100_Pace500_Min)/Split100_Pace500_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace500_250 <-  Split250_Time_250*2
    Split250_Pace500_500 <-  Split250_Time_500*2
    Split250_Pace500_750 <-  Split250_Time_750*2
    Split250_Pace500_1000 <-  Split250_Time_1000*2
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                           Split250_Pace500_750,  Split250_Pace500_1000)
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500)
      
      Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
      Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
    }
    
    
    
    # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
    # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace500_500 <-  Split500_Time_500*1
    Split500_Pace500_1000 <-  Split500_Time_1000*1
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500,
                                           Split500_Pace500_1000)
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace500_Data <-  data.table(Split500_Pace500_500)
      
      Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
      Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
    }
    
    # Split500_Pace500_Max <- round(max(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_Min <- round(min(t(Split500_Pace500_Data)),2)
    # Split500_Pace500_DO <-  round((Split500_Pace500_Max-Split500_Pace500_Min)/Split500_Pace500_Max*100,2)
    
    
    #######################
    
    
    #AvVel 50 splits
    
    Split50_Pace1000_50 <-  Split50_Time_50*20
    Split50_Pace1000_100 <-  Split50_Time_100*20
    Split50_Pace1000_150 <-  Split50_Time_150*20
    Split50_Pace1000_200 <-  Split50_Time_200*20
    Split50_Pace1000_250 <-  Split50_Time_250*20
    Split50_Pace1000_300 <-  Split50_Time_300*20
    Split50_Pace1000_350 <-  Split50_Time_350*20
    Split50_Pace1000_400 <-  Split50_Time_400*20
    Split50_Pace1000_450 <-  Split50_Time_450*20
    Split50_Pace1000_500 <-  Split50_Time_500*20
    Split50_Pace1000_550 <-  Split50_Time_550*20
    Split50_Pace1000_600 <-  Split50_Time_600*20
    Split50_Pace1000_650 <-  Split50_Time_650*20
    Split50_Pace1000_700 <-  Split50_Time_700*20
    Split50_Pace1000_750 <-  Split50_Time_750*20
    Split50_Pace1000_800 <-  Split50_Time_800*20
    Split50_Pace1000_850 <-  Split50_Time_850*20
    Split50_Pace1000_900 <-  Split50_Time_900*20
    Split50_Pace1000_950 <-  Split50_Time_950*20
    Split50_Pace1000_1000 <-  Split50_Time_1000*20
    
    if (input$distance== "Labelled_data_1000"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500, Split50_Pace1000_550, Split50_Pace1000_600,
                                           Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800,
                                           Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000)
      
    }else if(input$distance== "Labelled_data_500"){
      
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                           Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                           Split50_Pace1000_450, Split50_Pace1000_500)
      
    }else if(input$distance== "Labelled_data_200"){
      Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200)
      
    }
    Split50_Pace1000_Data <-  as.numeric(Split50_Pace1000_Data)
    Split50_Pace1000_Avg <- sec_to_ms(round(mean(t(Split50_Pace1000_Data)),2))
    # Split50_Pace1000_Max <- round(max(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_Min <- round(min(t(Split50_Pace1000_Data)),2)
    # Split50_Pace1000_DO <-  round((Split50_Pace1000_Max-Split50_Pace1000_Min)/Split50_Pace1000_Max*100,2)
    # Pace 100 splits
    Split100_Pace1000_100 <-  Split100_Time_100*10
    Split100_Pace1000_200 <-  Split100_Time_200*10
    Split100_Pace1000_300 <-  Split100_Time_300*10
    Split100_Pace1000_400 <-  Split100_Time_400*10
    Split100_Pace1000_500 <-  Split100_Time_500*10
    Split100_Pace1000_600 <-  Split100_Time_600*10
    Split100_Pace1000_700 <-  Split100_Time_700*10
    Split100_Pace1000_800 <-  Split100_Time_800*10
    Split100_Pace1000_900 <- Split100_Time_900*10
    Split100_Pace1000_1000 <-  Split100_Time_1000*10
    
    if (input$distance== "Labelled_data_1000"){
      
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500, Split100_Pace1000_600,
                                            Split100_Pace1000_700, Split100_Pace1000_800,
                                            Split100_Pace1000_900, Split100_Pace1000_1000)
    }else if(input$distance== "Labelled_data_500"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                            Split100_Pace1000_300,  Split100_Pace1000_400,
                                            Split100_Pace1000_500)
      
      
    }else if(input$distance== "Labelled_data_200"){
      Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200)
      
    }
    Split100_Pace1000_Data <-  as.numeric(Split100_Pace1000_Data)
    Split100_Pace1000_Avg <- sec_to_ms(round(mean(t(Split100_Pace1000_Data)),2))
    # Split100_Pace1000_Max <- round(max(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_Min <- round(min(t(Split100_Pace1000_Data)),2)
    # Split100_Pace1000_DO <-  round((Split100_Pace1000_Max-Split100_Pace1000_Min)/Split100_Pace1000_Max*100,2)
    
    
    # Pace 250 splits
    Split250_Pace1000_250 <-  Split250_Time_250*8
    Split250_Pace1000_500 <-  Split250_Time_500*8
    Split250_Pace1000_750 <-  Split250_Time_750*8
    Split250_Pace1000_1000 <-  Split250_Time_1000*8
    
    if (input$distance== "Labelled_data_1000"){
      
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                            Split250_Pace1000_750,  Split250_Pace1000_1000)
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500)
      
      Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
      Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
    }
    
    
    
    # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
    # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
    
    # Pace /500 splits
    Split500_Pace1000_500 <-  Split500_Time_500*2
    Split500_Pace1000_1000 <-  Split500_Time_1000*2
    
    
    if (input$distance== "Labelled_data_1000"){
      
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500,
                                            Split500_Pace1000_1000)
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
    }else if(input$distance== "Labelled_data_500"){
      Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500)
      
      Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
      Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
      
    }
    
    # Split500_Pace1000_Max <- round(max(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_Min <- round(min(t(Split500_Pace1000_Data)),2)
    # Split500_Pace1000_DO <-  round((Split500_Pace1000_Max-Split500_Pace1000_Min)/Split500_Pace1000_Max*100,2)
    
    
    #### Convert to ms in table
    Split50_Time_50 <- sec_to_ms(Split50_Time_50)
    Split50_Time_100 <- sec_to_ms(Split50_Time_100)
    Split50_Time_150 <- sec_to_ms(Split50_Time_150)
    Split50_Time_200 <- sec_to_ms(Split50_Time_200)
    Split50_Time_250 <- sec_to_ms(Split50_Time_250)
    Split50_Time_300 <- sec_to_ms(Split50_Time_300)
    Split50_Time_350 <- sec_to_ms(Split50_Time_350)
    Split50_Time_400 <- sec_to_ms(Split50_Time_400)
    Split50_Time_450 <- sec_to_ms(Split50_Time_450)
    Split50_Time_500 <- sec_to_ms(Split50_Time_500)
    Split50_Time_550 <- sec_to_ms(Split50_Time_550)
    Split50_Time_600 <- sec_to_ms(Split50_Time_600)
    Split50_Time_650 <- sec_to_ms(Split50_Time_650)
    Split50_Time_700 <- sec_to_ms(Split50_Time_700)
    Split50_Time_750 <- sec_to_ms(Split50_Time_750)
    Split50_Time_800 <- sec_to_ms(Split50_Time_800)
    Split50_Time_850 <- sec_to_ms(Split50_Time_850)
    Split50_Time_900 <- sec_to_ms(Split50_Time_900)
    Split50_Time_950 <- sec_to_ms(Split50_Time_950)
    Split50_Time_1000 <- sec_to_ms(Split50_Time_1000)
    
    
    #100 splits
    Split100_Time_100 <- sec_to_ms(Split100_Time_100)
    Split100_Time_200 <- sec_to_ms(Split100_Time_200)
    Split100_Time_300 <- sec_to_ms(Split100_Time_300)
    Split100_Time_400 <- sec_to_ms(Split100_Time_400)
    Split100_Time_500 <- sec_to_ms(Split100_Time_500)
    Split100_Time_600 <- sec_to_ms(Split100_Time_600)
    Split100_Time_700 <- sec_to_ms(Split100_Time_700)
    Split100_Time_800 <- sec_to_ms(Split100_Time_800)
    Split100_Time_900 <- sec_to_ms(Split100_Time_900)
    Split100_Time_1000 <- sec_to_ms(Split100_Time_1000)
    
    
    #250 splits
    
    Split250_Time_250 <- sec_to_ms(Split250_Time_250)
    Split250_Time_500 <- sec_to_ms(Split250_Time_500)
    Split250_Time_750 <- sec_to_ms(Split250_Time_750)
    Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
    
    
    
    #500 Splits
    Split500_Time_500 <- sec_to_ms(Split500_Time_500)
    Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
    
    ###################
    #### Convert to ms in table
    Split50_Pace500_50 <- sec_to_ms(Split50_Pace500_50)
    Split50_Pace500_100 <- sec_to_ms(Split50_Pace500_100)
    Split50_Pace500_150 <- sec_to_ms(Split50_Pace500_150)
    Split50_Pace500_200 <- sec_to_ms(Split50_Pace500_200)
    Split50_Pace500_250 <- sec_to_ms(Split50_Pace500_250)
    Split50_Pace500_300 <- sec_to_ms(Split50_Pace500_300)
    Split50_Pace500_350 <- sec_to_ms(Split50_Pace500_350)
    Split50_Pace500_400 <- sec_to_ms(Split50_Pace500_400)
    Split50_Pace500_450 <- sec_to_ms(Split50_Pace500_450)
    Split50_Pace500_500 <- sec_to_ms(Split50_Pace500_500)
    Split50_Pace500_550 <- sec_to_ms(Split50_Pace500_550)
    Split50_Pace500_600 <- sec_to_ms(Split50_Pace500_600)
    Split50_Pace500_650 <- sec_to_ms(Split50_Pace500_650)
    Split50_Pace500_700 <- sec_to_ms(Split50_Pace500_700)
    Split50_Pace500_750 <- sec_to_ms(Split50_Pace500_750)
    Split50_Pace500_800 <- sec_to_ms(Split50_Pace500_800)
    Split50_Pace500_850 <- sec_to_ms(Split50_Pace500_850)
    Split50_Pace500_900 <- sec_to_ms(Split50_Pace500_900)
    Split50_Pace500_950 <- sec_to_ms(Split50_Pace500_950)
    Split50_Pace500_1000 <- sec_to_ms(Split50_Pace500_1000)
    
    
    #100 splits
    Split100_Pace500_100 <- sec_to_ms(Split100_Pace500_100)
    Split100_Pace500_200 <- sec_to_ms(Split100_Pace500_200)
    Split100_Pace500_300 <- sec_to_ms(Split100_Pace500_300)
    Split100_Pace500_400 <- sec_to_ms(Split100_Pace500_400)
    Split100_Pace500_500 <- sec_to_ms(Split100_Pace500_500)
    Split100_Pace500_600 <- sec_to_ms(Split100_Pace500_600)
    Split100_Pace500_700 <- sec_to_ms(Split100_Pace500_700)
    Split100_Pace500_800 <- sec_to_ms(Split100_Pace500_800)
    Split100_Pace500_900 <- sec_to_ms(Split100_Pace500_900)
    Split100_Pace500_1000 <- sec_to_ms(Split100_Pace500_1000)
    
    
    #250 splits
    
    Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
    Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
    Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
    Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
    
    
    
    #500 Splits
    Split500_Pace500_500 <- sec_to_ms(Split500_Pace500_500)
    Split500_Pace500_1000 <- sec_to_ms(Split500_Pace500_1000)
    
    ################### 
    
    Split50_Pace1000_50 <- sec_to_ms(Split50_Pace1000_50)
    Split50_Pace1000_100 <- sec_to_ms(Split50_Pace1000_100)
    Split50_Pace1000_150 <- sec_to_ms(Split50_Pace1000_150)
    Split50_Pace1000_200 <- sec_to_ms(Split50_Pace1000_200)
    Split50_Pace1000_250 <- sec_to_ms(Split50_Pace1000_250)
    Split50_Pace1000_300 <- sec_to_ms(Split50_Pace1000_300)
    Split50_Pace1000_350 <- sec_to_ms(Split50_Pace1000_350)
    Split50_Pace1000_400 <- sec_to_ms(Split50_Pace1000_400)
    Split50_Pace1000_450 <- sec_to_ms(Split50_Pace1000_450)
    Split50_Pace1000_500 <- sec_to_ms(Split50_Pace1000_500)
    Split50_Pace1000_550 <- sec_to_ms(Split50_Pace1000_550)
    Split50_Pace1000_600 <- sec_to_ms(Split50_Pace1000_600)
    Split50_Pace1000_650 <- sec_to_ms(Split50_Pace1000_650)
    Split50_Pace1000_700 <- sec_to_ms(Split50_Pace1000_700)
    Split50_Pace1000_750 <- sec_to_ms(Split50_Pace1000_750)
    Split50_Pace1000_800 <- sec_to_ms(Split50_Pace1000_800)
    Split50_Pace1000_850 <- sec_to_ms(Split50_Pace1000_850)
    Split50_Pace1000_900 <- sec_to_ms(Split50_Pace1000_900)
    Split50_Pace1000_950 <- sec_to_ms(Split50_Pace1000_950)
    Split50_Pace1000_1000 <- sec_to_ms(Split50_Pace1000_1000)
    
    #100 splits
    Split100_Pace1000_100 <- sec_to_ms(Split100_Pace1000_100)
    Split100_Pace1000_200 <- sec_to_ms(Split100_Pace1000_200)
    Split100_Pace1000_300 <- sec_to_ms(Split100_Pace1000_300)
    Split100_Pace1000_400 <- sec_to_ms(Split100_Pace1000_400)
    Split100_Pace1000_500 <- sec_to_ms(Split100_Pace1000_500)
    Split100_Pace1000_600 <- sec_to_ms(Split100_Pace1000_600)
    Split100_Pace1000_700 <- sec_to_ms(Split100_Pace1000_700)
    Split100_Pace1000_800 <- sec_to_ms(Split100_Pace1000_800)
    Split100_Pace1000_900 <- sec_to_ms(Split100_Pace1000_900)
    Split100_Pace1000_1000 <- sec_to_ms(Split100_Pace1000_1000)
    
    
    #250 splits
    
    Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
    Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
    Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
    Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
    
    
    
    #500 Splits
    Split500_Pace1000_500 <- sec_to_ms(Split500_Pace1000_500)
    Split500_Pace1000_1000 <- sec_to_ms(Split500_Pace1000_1000)
    
    ################### 
    
    
    
    
    
    if (input$distance== "Labelled_data_1000"){
      
      
      Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   Split50_Time_550, Split50_Time_600, Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800, Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500, "",
                                                    Split100_Time_600, "", Split100_Time_700, "", Split100_Time_800, "", Split100_Time_900, "", Split100_Time_1000,
                                                    "", Split100_Time_Avg),
                           
                           
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500, "", "", "", "", Split250_Time_750, "", "", "", "", Split250_Time_1000,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500, "", "", "", "", "", "", "", "", "", Split500_Time_1000,
                                                    "", Split500_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               Split50_Pace500_550, Split50_Pace500_600, Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800, Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500, "",
                                                Split100_Pace500_600, "", Split100_Pace500_700, "", Split100_Pace500_800, "", Split100_Pace500_900, "", Split100_Pace500_1000,
                                                "", Split100_Pace500_Avg),
                           
                           
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500, "", "", "", "", Split250_Pace500_750, "", "", "", "", Split250_Pace500_1000,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500, "", "", "", "", "", "", "", "", "", Split500_Pace500_1000,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                Split50_Pace1000_550, Split50_Pace1000_600, Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800, Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500, "",
                                                 Split100_Pace1000_600, "", Split100_Pace1000_700, "", Split100_Pace1000_800, "", Split100_Pace1000_900, "", Split100_Pace1000_1000,
                                                 "", Split100_Pace1000_Avg),
                           
                           
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500, "", "", "", "", Split250_Pace1000_750, "", "", "", "", Split250_Pace1000_1000,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500, "", "", "", "", "", "", "", "", "", Split500_Pace1000_1000,
                                                 "", Split500_Pace1000_Avg),
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                Split50_AvVel_550, Split50_AvVel_600, Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800, Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000,
                                                "", Split50_AvVel_Avg),
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500, "",
                                                 Split100_AvVel_600, "", Split100_AvVel_700, "", Split100_AvVel_800, "", Split100_AvVel_900, "", Split100_AvVel_1000,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500, "", "", "", "", Split250_AvVel_750, "", "", "", "", Split250_AvVel_1000,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500, "", "", "", "", "", "", "", "", "", Split500_AvVel_1000,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              Split50_SR_550, Split50_SR_600, Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800, Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000,
                                              "", Split50_SR_Avg),
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500, "",
                                               Split100_SR_600, "", Split100_SR_700, "", Split100_SR_800, "", Split100_SR_900, "", Split100_SR_1000,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500, "", "", "", "", Split250_SR_750, "", "", "", "", Split250_SR_1000,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500, "", "", "", "", "", "", "", "", "", Split500_SR_1000,
                                               "", Split500_SR_Avg))
      
    }else if(input$distance== "Labelled_data_500"){
      
      
      
      Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500,
                                                    "", Split100_Time_Avg),
                           "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500,
                                                    "", Split250_Time_Avg),
                           "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500,
                                                    "", Split500_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500,
                                                "", Split100_Pace500_Avg),
                           "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500,
                                                "", Split250_Pace500_Avg),
                           "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500,
                                                "", Split500_Pace500_Avg),
                           
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500,
                                                 "", Split100_Pace1000_Avg),
                           "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500,
                                                 "", Split250_Pace1000_Avg),
                           "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500,
                                                 "", Split500_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500,
                                                 "", Split100_AvVel_Avg),
                           "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500,
                                                 "", Split250_AvVel_Avg),
                           "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500,
                                                 "", Split500_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500,
                                               "", Split100_SR_Avg),
                           "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500,
                                               "", Split250_SR_Avg),
                           "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500,
                                               "", Split500_SR_Avg))
      
      
    }else if(input$distance== "Labelled_data_200"){
      
      Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "", "Average"),
                           "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                                   "", Split50_Time_Avg),
                           "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200,
                                                    "", Split100_Time_Avg),
                           "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                               "", Split50_Pace500_Avg),
                           "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200,
                                                "", Split100_Pace500_Avg),
                           "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                                "", Split50_Pace1000_Avg),
                           "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200,
                                                 "", Split100_Pace1000_Avg),
                           
                           "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                                "", Split50_AvVel_Avg),
                           
                           "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200,
                                                 "", Split100_AvVel_Avg),
                           "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                              "", Split50_SR_Avg),
                           
                           "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200,
                                               "", Split100_SR_Avg))
    }
    return(Top10)
    
    
    
  })
  
  # ### Splits Stroke calculations ###
  # Filtered_AvStkRate_data <- <-  tab_AvStkRate() %>% select(variable, value)
  # Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
  # #250 splits
  # 
  # Split250_AvStkRate_250 <- format(round((mean(Filtered_AvStkRate_data[2:6,])),2), nsmall = 2)
  # Split250_AvStkRate_500 <- format(round((mean(Filtered_AvStkRate_data[7:11,])),2), nsmall = 2)
  # Split250_AvStkRate_750 <- format(round((mean(Filtered_AvStkRate_data[12:16,])),2), nsmall = 2)
  # Split250_AvStkRate_1000 <- format(round((mean(Filtered_AvStkRate_data[17:21,])),2), nsmall = 2)
  # Split250_AvStkRate_1250 <- format(round((mean(Filtered_AvStkRate_data[22:26,])),2), nsmall = 2)
  # Split250_AvStkRate_1500 <- format(round((mean(Filtered_AvStkRate_data[27:31,])),2), nsmall = 2)
  # Split250_AvStkRate_1750 <- format(round((mean(Filtered_AvStkRate_data[32:36,])),2), nsmall = 2)
  # Split250_AvStkRate_2000 <- format(round((mean(Filtered_AvStkRate_data[37:41,])),2), nsmall = 2)
  # 
  
  # Filtered_AvProgSpeed_data <-  tab_ProgSpeed() %>% select(variable, value)
  # Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')
  # ### Splits Prog Speed calculations ####      
  # # 250 splits prog speed
  # Split250_AvProgSpeed_250 <- format(round((mean(Filtered_AvProgSpeed_data[2:6,])),2), nsmall = 2)
  # Split250_AvProgSpeed_500 <- format(round((mean(Filtered_AvProgSpeed_data[7:11,])),2), nsmall = 2)
  # Split250_AvProgSpeed_750 <- format(round((mean(Filtered_AvProgSpeed_data[12:16,])),2), nsmall = 2)
  # Split250_AvProgSpeed_1000 <- format(round((mean(Filtered_AvProgSpeed_data[17:21,])),2), nsmall = 2)
  # Split250_AvProgSpeed_1250 <- format(round((mean(Filtered_AvProgSpeed_data[22:26,])),2), nsmall = 2)
  # Split250_AvProgSpeed_1500 <- format(round((mean(Filtered_AvProgSpeed_data[27:31,])),2), nsmall = 2)
  # Split250_AvProgSpeed_1750 <- format(round((mean(Filtered_AvProgSpeed_data[32:36,])),2), nsmall = 2)
  # Split250_AvProgSpeed_2000 <- format(round((mean(Filtered_AvProgSpeed_data[37:41,])),2), nsmall = 2)
  # 
  # 
  
  #### Data table ####
  
  
  
  
  output$SummaryTable <-  DT::renderDataTable({ 
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      #"50m velocities (km/h)"
      #"50m Stroke Rate (spm)"
      if (input$distance== "Labelled_data_200"){
        if (input$Report_Type == "Single Race"){ 
          
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Race2" = Race2()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"], "Race2" = Race2()[,"100m splits (secs)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m splits (secs)","Race2.100m splits (secs)"), new = c("Race 2", "Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Ideal" = Top10()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"], "Ideal" = Top10()[,"100m splits (secs)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m splits (secs)","Ideal.100m splits (secs)"), new = c("Top 10", "Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
        
        
        
        
      } else {
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"], 
                                          Race1()[,"250m splits (secs)"], 
                                          Race1()[,"500m splits (secs)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Race2" = Race2()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"], "Race2" = Race2()[,"100m splits (secs)"],
                                          Race1()[,"250m splits (secs)"], "Race2" = Race2()[,"250m splits (secs)"],
                                          Race1()[,"500m splits (secs)"], "Race2" = Race2()[,"500m splits (secs)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m splits (secs)","Race2.100m splits (secs)","Race2.250m splits (secs)","Race2.500m splits (secs)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Ideal" = Top10()[,"50m splits (secs)"], 
                                          Race1()[,"100m splits (secs)"], "Ideal" = Top10()[,"100m splits (secs)"],
                                          Race1()[,"250m splits (secs)"], "Ideal" = Top10()[,"250m splits (secs)"],
                                          Race1()[,"500m splits (secs)"], "Ideal" = Top10()[,"500m splits (secs)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m splits (secs)","Ideal.100m splits (secs)","Ideal.250m splits (secs)","Ideal.500m splits (secs)"), new = c("Top 10", "Top 10","Top 10","Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
        
      }  
    })
    
  })
  
  output$SummaryTablePace <-  DT::renderDataTable({ 
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      #"50m velocities (km/h)"
      #"50m Stroke Rate (spm)"
      if (input$distance== "Labelled_data_200"){
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                          Race1()[,"50m Pace /500"], "Race2" = Race2()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"], "Race2" = Race2()[,"100m Pace /500"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m Pace /500","Race2.100m Pace /500"), new = c("Race 2", "Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m Pace /500"], "Ideal" = Top10()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"], "Ideal" = Top10()[,"100m Pace /500"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m Pace /500","Ideal.100m Pace /500"), new = c("Top 10", "Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
      } else {
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"], 
                                          Race1()[,"250m Pace /500"], 
                                          Race1()[,"500m Pace /500"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                          Race1()[,"50m Pace /500"], "Race2" = Race2()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"], "Race2" = Race2()[,"100m Pace /500"],
                                          Race1()[,"250m Pace /500"], "Race2" = Race2()[,"250m Pace /500"],
                                          Race1()[,"500m Pace /500"], "Race2" = Race2()[,"500m Pace /500"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m Pace /500","Race2.100m Pace /500","Race2.250m Pace /500","Race2.500m Pace /500"), new = c("Race 2", "Race 2","Race 2","Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m Pace /500"], "Ideal" = Top10()[,"50m Pace /500"], 
                                          Race1()[,"100m Pace /500"], "Ideal" = Top10()[,"100m Pace /500"],
                                          Race1()[,"250m Pace /500"], "Ideal" = Top10()[,"250m Pace /500"],
                                          Race1()[,"500m Pace /500"], "Ideal" = Top10()[,"500m Pace /500"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m Pace /500","Ideal.100m Pace /500","Ideal.250m Pace /500","Ideal.500m Pace /500"), new = c("Top 10", "Top 10","Top 10","Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
        
      }
      
    })
    
  })
  
  # output$SummaryTable2 <-  DT::renderDataTable({ 
  #   
  #   #"50m velocities (km/h)"
  #   #"50m Stroke Rate (spm)"
  #   
  #   if (input$Report_Type == "Single Race"){ 
  #     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
  #                                     Race1()[,"100m vel (km/h)"], 
  #                                     Race1()[,"250m vel (km/h)"], 
  #                                     Race1()[,"500m vel (km/h)"],
  #                                     Race1()[,"50m SR (spm)"], 
  #                                     Race1()[,"100m SR (spm)"], 
  #                                     Race1()[,"250m SR (spm)"], 
  #                                     Race1()[,"500m SR (spm)"])
  #     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
  #         formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
  #         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
  #                     `border-top` = styleEqual('Average', "solid 3px")) %>%
  #         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
  #       
  #     })
  #     
  #   } else if (input$Report_Type == "Two Races"){ 
  #     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
  #                                     Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"],
  #                                     Race1()[,"250m vel (km/h)"], "Race2" = Race2()[,"250m vel (km/h)"],
  #                                     Race1()[,"500m vel (km/h)"], "Race2" = Race2()[,"500m vel (km/h)"],
  #                                     Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
  #                                     Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"],
  #                                     Race1()[,"250m SR (spm)"], "Race2" = Race2()[,"250m SR (spm)"],
  #                                     Race1()[,"500m SR (spm)"], "Race2" = Race2()[,"500m SR (spm)"])
  #     
  #     Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)","Race2.250m vel (km/h)","Race2.500m vel (km/h)", 
  #                                                             "Race2.50m SR (spm)","Race2.100m SR (spm)","Race2.250m SR (spm)","Race2.500m SR (spm)"), new = c("Race 2", "Race 2","Race 2","Race 2", "Race 2", "Race 2","Race 2","Race 2"))
  #     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
  #         formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
  #         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
  #                     `border-top` = styleEqual('Average', "solid 3px")) %>%
  #         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
  #       
  #     })
  #     
  #   } else if (input$Report_Type == "vs Top 10"){
  #     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
  #                                     Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"],
  #                                     Race1()[,"250m vel (km/h)"], "Ideal" = Top10()[,"250m vel (km/h)"],
  #                                     Race1()[,"500m vel (km/h)"], "Ideal" = Top10()[,"500m vel (km/h)"],
  #                                     Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
  #                                     Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"],
  #                                     Race1()[,"250m SR (spm)"], "Ideal" = Top10()[,"250m SR (spm)"],
  #                                     Race1()[,"500m SR (spm)"], "Ideal" = Top10()[,"500m SR (spm)"])
  #     
  #     Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)","Ideal.250m vel (km/h)","Ideal.500m vel (km/h)", 
  #                                                             "Ideal.50m SR (spm)","Ideal.100m SR (spm)","Ideal.250m SR (spm)","Ideal.500m SR (spm)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
  #     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
  #         formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
  #         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
  #                     `border-top` = styleEqual('Average', "solid 3px")) %>%
  #         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
  #       
  #       
  #     })
  #   }
  #   
  #   
  #   
  #   
  # })
  
  output$SummaryTableVel <-  DT::renderDataTable({ 
    
    #"50m velocities (km/h)"
    #"50m Stroke Rate (spm)"
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      
      if (input$distance== "Labelled_data_200"){
        
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)"), new = c("Race 2", "Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)"),  new = c("Top 10", "Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
      } else {
        
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"], 
                                          Race1()[,"250m vel (km/h)"], 
                                          Race1()[,"500m vel (km/h)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"],
                                          Race1()[,"250m vel (km/h)"], "Race2" = Race2()[,"250m vel (km/h)"],
                                          Race1()[,"500m vel (km/h)"], "Race2" = Race2()[,"500m vel (km/h)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)","Race2.250m vel (km/h)","Race2.500m vel (km/h)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
                                          Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"],
                                          Race1()[,"250m vel (km/h)"], "Ideal" = Top10()[,"250m vel (km/h)"],
                                          Race1()[,"500m vel (km/h)"], "Ideal" = Top10()[,"500m vel (km/h)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)","Ideal.250m vel (km/h)","Ideal.500m vel (km/h)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
        
        
      }
      
    })
  })
  
  
  output$SummaryTableSR <-  DT::renderDataTable({ 
    if (input$goButton == 0)
      return()
    #"50m velocities (km/h)"
    #"50m Stroke Rate (spm)"
    input$goButton
    isolate({
      
      if (input$distance== "Labelled_data_200"){
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m SR (spm)"], 
                                          Race1()[,"100m SR (spm)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                          Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
                                          Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m SR (spm)","Race2.100m SR (spm)"), new = c("Race 2", "Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable<-  data.table(Race1()[,"Distance (m)"],
                                         Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
                                         Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m SR (spm)","Ideal.100m SR (spm)"),  new = c("Top 10", "Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
      }else {
        if (input$Report_Type == "Single Race"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                          Race1()[,"50m SR (spm)"], 
                                          Race1()[,"100m SR (spm)"], 
                                          Race1()[,"250m SR (spm)"], 
                                          Race1()[,"500m SR (spm)"])
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "Two Races"){ 
          Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                          Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
                                          Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"],
                                          Race1()[,"250m SR (spm)"], "Race2" = Race2()[,"250m SR (spm)"],
                                          Race1()[,"500m SR (spm)"], "Race2" = Race2()[,"500m SR (spm)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m SR (spm)","Race2.100m SR (spm)","Race2.250m SR (spm)","Race2.500m SR (spm)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
          })
          
        } else if (input$Report_Type == "vs Top 10"){
          Summarydatatable<-  data.table(Race1()[,"Distance (m)"],
                                         Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
                                         Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"],
                                         Race1()[,"250m SR (spm)"], "Ideal" = Top10()[,"250m SR (spm)"],
                                         Race1()[,"500m SR (spm)"], "Ideal" = Top10()[,"500m SR (spm)"])
          
          Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m SR (spm)","Ideal.100m SR (spm)","Ideal.250m SR (spm)","Ideal.500m SR (spm)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
          ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
              formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
              formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                          `border-top` = styleEqual('Average', "solid 3px")) %>%
              formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
            
            
          })
        }
      }
      
      
    })
    
  })
  
  #### Output headers ####
  output$Summaryhead <- renderText({
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      Label_Race1 = paste(input$Name, input$Competition, input$Phase)
      if (input$Report_Type == "Single Race"){ 
        Summaryhead = Label_Race1
      }else if(input$Report_Type == "Two Races"){ 
        Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
        Summaryhead = paste(Label_Race1, "vs", Label_Race2) 
        
      }else if (input$Report_Type == "vs Top 10"){ 
        Summaryhead = paste(Label_Race1, "vs Top 10 Average") 
        
      }
      
      {Summaryhead}
    })
  })
  output$Timehead <- renderText({
    if (input$goButton == 0)
      return()
    input$goButton
    isolate({
      Label_Race1 = paste(input$Name, input$Competition, input$Phase)
      if (input$Report_Type == "Single Race"){ 
        Timehead = paste("25m Splits -", Label_Race1)
      }else if (input$Report_Type == "Two Races"){ 
        Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
        
        Timehead = paste("25m Splits -", Label_Race1, "vs", Label_Race2) 
        
      }else if (input$Report_Type == "vs Top 10"){ 
        Timehead = paste("25m Splits -", Label_Race1, "vs Top 10 Average") 
        
      }
      
      {Timehead}
    })
  })
  
  output$Summaryheadgap <- renderText({
    paste(" ")
  })
  output$Splithead <- renderText({paste0("Splits vs Top 10 average")
  })
  
  output$BoxTitleWBT <- renderText({
    paste(input$Event, "World Best")
  })
  output$BoxTitleProg <- renderText({
    paste(input$Event, "Prognostic")
  })
  
  output$Plothead1 <- renderText({
    if (input$Report_Type == "Single Race"){ 
      Plothead1 = paste("Velocity and Stroke Rate")
    }else if(input$Report_Type == "Two Races"){ 
      Plothead1 = paste("Velocity") 
      
    }else if (input$Report_Type == "vs Top 10"){ 
      Plothead1 = paste("Velocity") 
      
    }
    
    {Plothead1}
  })
  
  output$Plothead2 <- renderText({
    if(input$Report_Type == "Two Races"){ 
      Plothead2 = paste("Stroke Rate") 
      
    }else if (input$Report_Type == "vs Top 10"){ 
      Plothead2 = paste("Stroke Rate") 
      
    }
    
    {Plothead2}
  })
  
  
  
  output$RaceSummary <-  DT::renderDataTable({
    input$goButton
    isolate(
      if (input$Report_Type == "Single Race"){
        #Race 1
        FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
        
        
        if (input$distance == "Labelled_data_200"){
          distancea = 200
        }else if (input$distance == "Labelled_data_500"){
          distancea = 500
        }else if (input$distance == "Labelled_data_1000"){
          distancea = 1000
        }
        ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
        
        #ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass=="MK1") %>% filter(Distance=="500")
        
        ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
        PrognosticTime <- ClassProg[1,'ProgTimesec100']
        
        Difference <-  round((PrognosticTime/FinishTime)*100,2)
        AvgVel = round(distancea/FinishTime*3.6,2)
        FinishTime <- sec_to_ms(FinishTime)
        PrognosticTime <- sec_to_ms(PrognosticTime)
        
        ClassProgSpeed2 = round(ClassProgSpeed,2)
        
        
        Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
        colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
        
        
        ({datatable(Race_1, rownames = c("Race 1"), options = list(dom='t', ordering=F, scrollX = TRUE))})
        
      } else if (input$Report_Type == "Two Races"){
        #Race 1
        FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
        
        if (input$distance == "Labelled_data_200"){
          distancea = 200
        }else if (input$distance == "Labelled_data_500"){
          distancea = 500
        }else if (input$distance == "Labelled_data_1000"){
          distancea = 1000
        }
        
        ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
        ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
        PrognosticTime <- ClassProg[1,'ProgTimesec100']
        
        Difference <-  round((PrognosticTime/FinishTime)*100,2)
        AvgVel = round(distancea/FinishTime*3.6,2)
        FinishTime <- sec_to_ms(FinishTime)
        PrognosticTime <- sec_to_ms(PrognosticTime)
        ClassProgSpeed2 = round(ClassProgSpeed,2)
        
        #Race 2
        FinishTime_R2 <-  tab_Time_R2() %>% select(value) %>% last() %>% unlist()
        
        if (input$distance == "Labelled_data_200"){
          distancea = 200
        }else if (input$distance == "Labelled_data_500"){
          distancea = 500
        }else if (input$distance == "Labelled_data_1000"){
          distancea = 1000
        }
        
        ClassProgSpeed_R2 <- ClassProg[1,'ProgSpeed100']
        PrognosticTime_R2 <- ClassProg[1,'ProgTimesec100']
        
        Difference_R2 <-  round(PrognosticTime_R2/FinishTime_R2*100,2)
        AvgVel_R2 = round(distancea/FinishTime_R2*3.6,2)
        FinishTime_R2 <- sec_to_ms(FinishTime_R2)
        PrognosticTime_R2 <- sec_to_ms(PrognosticTime_R2)
        ClassProgSpeed2_R2 = round(ClassProgSpeed_R2,2)
        
        
        
        Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
        colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
        
        Race_2 = data.frame(FinishTime_R2, PrognosticTime_R2, Difference_R2, AvgVel_R2, ClassProgSpeed2_R2)
        colnames(Race_2) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
        
        RaceSummary <<-  rbind(Race_1, Race_2)
        ({datatable(RaceSummary, rownames = c("Race 1", "Race 2"), options = list(dom='t', ordering=F, scrollX = TRUE))})
        
      }  else if (input$Report_Type == "vs Top 10"){
        
        #Race 1
        FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
        
        if (input$distance == "Labelled_data_200"){
          distancea = 200
        }else if (input$distance == "Labelled_data_500"){
          distancea = 500
        }else if (input$distance == "Labelled_data_1000"){
          distancea = 1000
        }
        ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
        
        #ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass=="MK1") %>% filter(Distance=="500")
        
        ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
        PrognosticTime <- ClassProg[1,'ProgTimesec100']
        
        Difference <-  round((PrognosticTime/FinishTime)*100,2)
        AvgVel = round(distancea/FinishTime*3.6,2)
        FinishTime <- sec_to_ms(FinishTime)
        PrognosticTime <- sec_to_ms(PrognosticTime)
        
        ClassProgSpeed2 = round(ClassProgSpeed,2)
        
        #Race 2
        FinishTime_Top10 <-  tab2_Time() %>% select(Average) %>% last() %>% unlist()
        
        if (input$distance == "Labelled_data_200"){
          distancea = 200
        }else if (input$distance == "Labelled_data_500"){
          distancea = 500
        }else if (input$distance == "Labelled_data_1000"){
          distancea = 1000
        }
        ClassProgSpeed_Top10 <- ClassProg[1,'ProgSpeed100']
        PrognosticTime_Top10 <- ClassProg[1,'ProgTimesec100']
        
        Difference_Top10 <-  round(PrognosticTime_Top10/FinishTime_Top10*100,2)
        AvgVel_Top10 <- round(distancea/FinishTime_Top10*3.6,2)
        FinishTime_Top10 <- sec_to_ms(FinishTime_Top10)
        PrognosticTime_Top10 <- sec_to_ms(PrognosticTime_Top10)
        ClassProgSpeed2_Top10 = round(ClassProgSpeed_Top10,2)
        
        
        
        
        Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
        colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
        
        Race_2 = data.frame(FinishTime_Top10, PrognosticTime_Top10, Difference_Top10, AvgVel_Top10, ClassProgSpeed2_Top10)
        colnames(Race_2) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
        
        RaceSummary <-  rbind(Race_1, Race_2)
        ({datatable(RaceSummary, rownames = c("Race 1", "Top 10"), options = list(dom='t', ordering=F, scrollX = TRUE))})
        
        
      }
    )
  })
  
  
  
}

