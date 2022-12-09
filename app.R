#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(shinydashboard)
data=read.csv("2020-2021.csv")
newdat=as.data.frame(data)




ui <- fluidPage(
  
  # App title ----
  titlePanel("Shots Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("bins", "No of Bins",
                  min = 0, max = 20,
                  value = 20),
      radioButtons("choice","Make your selection",choices=c("Home Team Shots","Away Team Shots"),selected="Home Team Shots")
      
      
      
    ),
    mainPanel(
      plotOutput("distPlot")
      
    )
    
  )
)





ui=dashboardPage(
  dashboardHeader(title="English Premier League-Interactive dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName="ram"),
      menuItem("Shots Analysis ",tabName = "sam"),
      menuItem("Goal Scoring Ability",tabName="dam"),
      menuItem("Overall Performance",tabName="jam"),
      menuItem("Conclusion",tabName = "lam")
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("ram",
              tabsetPanel(
                tabPanel(h2("EPL"),
                         fluidPage(
                           textOutput("tex")
                           
                           
                         )
                )
              )),
      
      tabItem("sam",
              tabsetPanel(
                tabPanel("HS-AS Analysis",h2("Home Team vs Away Team Shot"),
                         fluidPage(
                           actionButton("HS","Home Team Shots"),actionButton("AS","Away Team Shots"),
                           
                           
                           plotOutput("graphs1")
                         )
                         
                ),
                tabPanel("HST-AST Analysis",h2("Home Team shots on target vs Away Team shots on target"),
                         fluidPage(
                           actionButton("HS_target","Home Team Shots on target"),actionButton("AS_target","Away team shots on Target"),
                           plotOutput("graphs2")
                         )
                         
                ),
                
                tabPanel("Goals(home) vs Goals(Away)",h2("Home Team Goals vs Away team goals"),
                         fluidPage(
                           actionButton("HomeGoals","Home Team Goals"),actionButton('AwayGoals',"Away Team Goals"),
                           plotOutput("graphs3")
                         )
                         
                )
              )),
      tabItem("dam",
              tabsetPanel(
                tabPanel("Corner vs goals",h2("Corners recieved Vs Total goals Scored"),
                         fluidPage(
                           actionButton("CNGHome","Corner to goal(Home)"),actionButton("CNGAway","Corner to goal(Away)"),actionButton("TCNG","Corner to Goals(Overall)"),
                           
                           
                           
                           plotOutput("graphs4")
                         )
                         
                ),
                tabPanel("Shots vs goals",h2("Total Shots Vs Total goals Scored"),
                         fluidPage(
                           actionButton("STGHome","Shots to goal(Home)"),actionButton("STGAway","Shots to goal(Away)"),actionButton("TSTG","Shots to Goals(Overall)"),
                           
                           
                           
                           plotOutput("graphs5")
                         )
                         
                )
                
              )),
      tabItem("jam",
              tabsetPanel(
                tabPanel("goal Difference Analysis",h2("Total Goals Scored Vs Total goals Conceded"),
                         fluidPage(
                           actionButton("TPC","Team performances Chart"),
                           
                           
                           
                           plotOutput("graphs6")
                         )
                         
                ),
                tabPanel("Situational Analysis",h2("Home Team Reactions to different Situations"),
                         fluidPage(
                           actionButton("SAHT","Home team Situation Analysis"),
                           
                           
                           
                           plotOutput("graphs7")
                         )
                         
                ),
                tabPanel("Foul Analysis",h2("Most aggressive Vs Least aggressive "),
                         fluidPage(
                           actionButton("MHT"," Most Aggresive Home Team"),actionButton("MAT","Most Aggresive Away Team"),
                           
                           
                           
                           plotOutput("graphs8")
                         )
                         
                )
                
                
                
                
                
              )),
      tabItem("lam",
              tabsetPanel(
                tabPanel(h2("Conclusion"),
                         fluidPage(
                           textOutput("tex1"),
                           
                           
                         )
                )
              ))
      
    )
    
    
  )
  
  
)



server<-function(input,output){
  output$tex = renderText({
    print("Sports betting is a 500 billion dollar market (Sydney Herald)
          Football is played by 250 million players in over 200 countries (most popular sport globally).
          The English Premier League is the most popular domestic team in the world.
          It contains 20 years of EPL matches dataset. The Analysis consists of several match statistics
          generated over the 380 games played.
          
          Over the different graphs provided throughout the analysis i have tried to boil down the effect 
          of playing at home and thus help in bringing insights to the current betting system in place.
          Performing well at home creates a ripple effect as it also invariably related to the trust that
          supporters put in and help push the billion dollar betting market. This acts as an incentive for
          both parties( Players and Supporters).
")
  })
  
  observeEvent(input$HS,(output$graphs1=renderPlot({
    c_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(ts=sum(HS))
    ggplot(data=c_1,aes(HomeTeam,ts))+
      
      geom_bar(stat="identity",fill=rainbow(nrow(c_1)))+
      xlab("Teams")+ylab("Shots")+
      coord_flip()
    
    
  })))
  observeEvent(input$AS,(output$graphs1=renderPlot({
    c_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(ts=sum(AS))
    ggplot(data=c_2,aes(AwayTeam,ts))+
      geom_bar(stat="identity",fill=rainbow(nrow(c_2)))+
      xlab("Teams")+ylab("Shots")+
      coord_flip()
    
  })))
  observeEvent(input$HS_target,(output$graphs2=renderPlot({
    d_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(tst=sum(HST))
    ggplot(data=d_1,aes(HomeTeam,tst))+
      geom_bar(stat="identity",fill=rainbow(nrow(d_1)))+
      ylab("Shots On Target")+xlab("Teams")+
      coord_flip()
    
  })))
  observeEvent(input$AS_target,(output$graphs2=renderPlot({
    d_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(tst=sum(AST))
    ggplot(data=d_2,aes(AwayTeam,tst))+
      geom_bar(stat="identity",fill=rainbow(nrow(d_2)))+
      ylab("Shots on target")+xlab("Teams")+
      coord_flip()
    
  })))
  observeEvent(input$HomeGoals,(output$graphs3=renderPlot({
    
    table1=as.data.frame(table(newdat$FTHG))
    ggplot(data=table1,aes(x=Var1,y=Freq))+
      xlab("goals")+ylab("frequency")+
      geom_bar(stat="identity",fill=rainbow(nrow(table1)))
    
  })))
  
  observeEvent(input$AwayGoals,(output$graphs3=renderPlot({
    
    table2=as.data.frame(table(newdat$FTAG))
    ggplot(data=table2,aes(x=Var1,y=Freq))+
      geom_bar(stat="identity",fill=rainbow(nrow(table2)))+
      xlab("Goals")+ylab("frequency")
    
  })))
  
  observeEvent(input$CNGHome,(output$graphs4=renderPlot({
    
    
    
    a_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(tc=sum(HC),tgs=sum(FTHG))
    
    
    ggplot(a_1, aes(tc, tgs, label = c(HomeTeam)))+
      geom_point()+
      xlab("Corners")+ylab("Goals Scored")+
      geom_text(aes(label = c(HomeTeam)),
                size = 3.5)
    
    
    
    
    
  })))
  observeEvent(input$CNGAway,(output$graphs4=renderPlot({
    
    a_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(tc=sum(AC),tgs=sum(FTAG))
    
    
    ggplot(a_2, aes(tc, tgs, label = c(AwayTeam)))+
      geom_point()+
      xlab("Corners")+ylab("Goals Scored")+
      geom_text(aes(label = c(AwayTeam)),
                size = 3.5)
    
    
    
    
  })))
  observeEvent(input$TCNG,(output$graphs4=renderPlot({
    
    
    
    a_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(tc=sum(HC),tgs=sum(FTHG))
    
    a_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(tc=sum(AC),tgs=sum(FTAG))
    
    
    Corner_Goal_tally=data.frame(Team=c(a_1$HomeTeam),
                                 Total_Corners=c(a_1$tc+a_2$tc),
                                 Total_goal_Scored=c(a_1$tgs+a_2$tgs))
    
    ggplot(Corner_Goal_tally, aes(Total_Corners,Total_goal_Scored, label = c(Team)))+
      geom_point()+
      xlab("Corners")+ylab("Goals Scored")+
      geom_text(aes(label = c(Team)),
                size = 3.5)
    
    
  })))
  observeEvent(input$STGHome,(output$graphs5=renderPlot({
    b_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(ts=sum(HS),tgs=sum(FTHG))
    
    
    ggplot(b_1, aes(ts, tgs, label = c(HomeTeam)))+
      geom_point()+
      xlab("Shots")+ylab("Goals Scored")+
      geom_text(aes(label = c(HomeTeam)),
                size = 3.5)
    
    
    
    
    
  })))
  observeEvent(input$STGAway,(output$graphs5=renderPlot({
    
    b_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(ts=sum(AS),tgs=sum(FTAG))
    
    
    ggplot(b_2, aes(ts, tgs, label = c(AwayTeam)))+
      geom_point()+
      xlab("Shots")+ylab("Goals Scored")+
      geom_text(aes(label = c(AwayTeam)),
                size = 3.5)
    
    
  })))
  observeEvent(input$TSTG,(output$graphs5=renderPlot({
    b_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(ts=sum(HS),tgs=sum(FTHG))
    b_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(ts=sum(AS),tgs=sum(FTAG))
    
    Shots_Goal_tally=data.frame(Team=c(b_1$HomeTeam),
                                Total_Shots=c(b_1$ts+b_2$ts),
                                Total_goal_Scored=c(b_1$tgs+b_2$tgs))
    
    ggplot(Shots_Goal_tally, aes(Total_Shots,Total_goal_Scored, label = c(Team)))+
      geom_point()+
      xlab("Shots")+ylab("Goals Scored")+
      geom_text(aes(label = c(Team)),
                size = 3.5)
    
    
    
  })))
  observeEvent(input$TPC,(output$graphs6=renderPlot({
    
    b=newdat %>%
      group_by(HomeTeam)%>%
      summarise(tgs=sum(FTHG),tgc=sum(FTAG))
    
    c=newdat %>%
      group_by(AwayTeam)%>%
      summarise(tgs=sum(FTAG),tgc=sum(FTHG))
    
    Goals_tally=data.frame(Team=c(b$HomeTeam),
                           Total_Goal_Scored=c(b$tgs+c$tgs),
                           Total_goal_Conceded=c(b$tgc+c$tgc))
    
    Goals_tally$Goal_Diff=c(Goals_tally$Total_Goal_Scored-Goals_tally$Total_goal_Conceded)
    
    ggplot(Goals_tally,aes(Total_Goal_Scored,Total_goal_Conceded,label=c(Team)))+
      geom_point()+
      xlab("Total Goals Scored")+ylab("Total Goals concded")+
      geom_text(aes(label=c(Team)),
                size=2)
    
  })))
  observeEvent(input$SAHT,(output$graphs7=renderPlot({
    
    d=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="H",FTR=="H"))
    
    ## dominance by away team
    e=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="A",FTR=="A"))
    ## bottled by home team
    f=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="H",FTR=="A"))
    ## comeback by home team
    g=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="A",FTR=="H"))
    
    # draw matches
    h=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(FTR=="D"))
    
    ## Stronger second half by Home team
    i=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="D",FTR=="H"))
    
    ## Stronger second half by away team
    
    j=nrow(newdat%>%
             group_by(HomeTeam)%>%
             filter(HTR=="D",FTR=="A"))
    analysis=data.frame(x=c("Home Team Win","Dominated by away team",
                            "Bottled by Home Team","Comeback by HomeTeam",
                            "Draw Matches","Stronger second half by Home Team",
                            "Stronger second half by Away team"),y=c(d,e,f,g,h,i,j))
    
    ggplot(analysis, aes(x = "", y = y, fill = x)) +
      geom_col(color="black")+
      geom_text(aes(label=y),
                position=position_stack(vjust=0.5))+
      coord_polar(theta = "y")+
      xlab("")+ylab("")
    
  })))
  observeEvent(input$MHT,(output$graphs8=renderPlot({
    e_1=newdat %>%
      group_by(HomeTeam)%>%
      summarise(ty=sum(HY))
    
    ggplot(e_1, aes(x=HomeTeam, y=ty)) + 
      geom_point(size=3) + 
      geom_segment(aes(x=HomeTeam, 
                       xend=HomeTeam, 
                       y=0, 
                       yend=ty)) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      xlab("Team")+ylab("Yellow cards")
  })))
  observeEvent(input$MAT,(output$graphs8=renderPlot({
    e_2=newdat %>%
      group_by(AwayTeam)%>%
      summarise(ty=sum(AY))
    
    ggplot(e_2, aes(x=AwayTeam, y=ty)) + 
      geom_point(size=3) + 
      geom_segment(aes(x=AwayTeam, 
                       xend=AwayTeam, 
                       y=0, 
                       yend=ty)) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      xlab("Team")+ylab("Yellow cards")
  })))
  output$tex1 = renderText({
    print("Through The various visual analytics i have tried to shown the inmatch, situation wise and overall performance of 
    all the teams participating. This kinds of visualization helps customers chose their betting preferences and maximize profits.
    However there will always be outliers and ambiguity when it comes to sports.Understanding the nature of the game and analysis of 
    previous games helps us mitigate the risks undertaken over the entire season.")
  })
  
}



shinyApp(ui = ui, server = server)






