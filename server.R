# NPBの一軍と二軍の個人成績を表示するshiny app
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
# geom_ggrepelからgeom_text_repel(family = "HiraKakuPro-W3") に変更。多少早くなったかも。

# githubに保存したファイルを使用する
batUrl <- "https://raw.githubusercontent.com/SNiN-17/farm_stats/master/NPB_batting_since2012.csv"
bat <- read_csv(batUrl, 
                locale=locale(encoding="CP932"))
pitUrl <- "https://raw.githubusercontent.com/SNiN-17/farm_stats/master/NPB_pitching_since2012.csv"
pit <- read_csv(pitUrl, 
                locale=locale(encoding="CP932"))
# 前処理
# 通算打撃成績
career_bat <- dplyr::summarise(group_by(bat, Player_name, T, Level),
                               N = sum(PA),
                               HRpct = weighted.mean(HRpct, PA),
                               Kpct = weighted.mean(Kpct, PA),
                               BBpct = weighted.mean(BBpct, PA),
                               BABIP =weighted.mean(BABIP, BABIP_denom),
                               AVG = weighted.mean(AVG, AB),
                               wOBA = weighted.mean(wOBA, wOBA_denom)) %>%
  mutate(K_BB = Kpct -BBpct)
career_bat$ID <- paste(career_bat$Player_name, career_bat$T)
names(career_bat)[10] <- "overall"
# 通算投球成績
career_pit <- dplyr::summarise(group_by(pit, Player_name, T, Level),
                               N = sum(TBF),
                               HR_9 = weighted.mean(HR_9, TBF),
                               Kpct = weighted.mean(Kpct, TBF),
                               BBpct = weighted.mean(BBpct, TBF),
                               DER =weighted.mean(DER, DER_denom),
                               ERA = weighted.mean(ERA, IP),
                               FIP = weighted.mean(FIP, IP))%>%
  mutate(K_BB = Kpct -BBpct)
career_pit$ID <- paste(career_pit$Player_name, career_pit$T)
names(career_pit) <- c("Player_name", "T","Level","N","HRpct","Kpct","BBpct","BABIP","AVG","overall","K_BB","ID")

# Define server logic ----
server <- function(input, output) {
  
  
  # 与えられた条件に従ってフィルター
  Hitter_Input2 <- eventReactive(input$update, {
    df <- career_bat
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range_hit1) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range_hit2) 
    inner_join(top, farm, by = "ID")
  }, ignoreNULL = FALSE)
  
  Pitcher_Input2 <- eventReactive(input$update2, {
    df <- career_pit
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range_pit1) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range_pit2) 
    inner_join(top, farm, by = "ID")
  }, ignoreNULL = FALSE)
  # 打者ラベル用インプット
  Hitter_Input3 <- eventReactive(input$update3, {
    df <- career_bat
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range_hit1_L) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range_hit2_L) 
    inner_join(top, farm, by = "ID")
  }, ignoreNULL = FALSE)
  
  # 投手ラベル用インプット
  Pitcher_Input3 <- eventReactive(input$update4, {
    df <- career_pit
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range_pit1_L) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range_pit2_L) 
    inner_join(top, farm, by = "ID")
  }, ignoreNULL = FALSE)
  
  # wOBAプロットラベルなし ----
  output$plot_wOBA <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = overall.y, 
                     y = overall.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      
      labs(title = "2軍wOBA vs 1軍wOBA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍wOBA", y = "1軍wOBA", color = "二軍でのPA") 
    gg
  })
  # FIPプロットラベルなし ----
  output$plot_FIP <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = overall.y, 
                     y = overall.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      
      labs(title = "2軍FIP vs 1軍FIP",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍FIP", y = "1軍FIP", color = "二軍でのTBF") 
    gg
  })
  #AVGラベルなし ----
  output$plot_AVG <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = AVG.y, 
                     y = AVG.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍AVG vs 1軍AVG",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍AVG", y = "1軍AVG", color = "二軍でのPA") 
    gg
  })
  #ERAラベルなし ----
  output$plot_ERA <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = AVG.y, 
                     y = AVG.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍ERA vs 1軍ERA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍ERA ", y = "1軍ERA", color = "二軍でのTBF") 
    gg
  })
  # HRpctラベルなし ----
  output$plot_HRpct <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = HRpct.y, 
                     y = HRpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍HRpct vs 1軍HRpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍HRpct", y = "1軍HRpct",color = "二軍でのPA") 
    gg
  })
  # HRpctラベルなし ----
  output$plot_HR9<- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = HRpct.y, 
                     y = HRpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍HRpct or HR/9 vs 1軍HRpct or HR/9",
           subtitle = "赤の破線はy = x; 打者はHRpct, 投手はHR/9イニングを表示.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍HRpct or HR/9", y = "1軍HRpct or HR/9",color = "二軍でのPA/TBF") 
    gg
  })
  # K ラベルなし (打者) ----
  output$plot_Kpct_hit <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = Kpct.y, 
                     y = Kpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍Kpct vs 1軍Kpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍Kpct", y = "1軍Kpct", color = "二軍でのPA") 
    gg
  })
  # K pct 投手　ラベルなし ----
  output$plot_Kpct_pit <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = Kpct.y, 
                     y = Kpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍Kpct vs 1軍Kpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍Kpct", y = "1軍Kpct", color = "二軍でのTBF") 
    gg
  })
  # BBラベルなし　打者 ----
  output$plot_BBpct_hit <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, 
                 aes(x = BBpct.y, 
                     y = BBpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BBpct vs 1軍BBpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BBpct", y = "1軍BBpct", color = "2軍でのPA") 
    gg
  })
  # BBラベルなし 投手 ----
  output$plot_BBpct_pit <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, 
                 aes(x = BBpct.y, 
                     y = BBpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BBpct vs 1軍BBpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BBpct", y = "1軍BBpct", color = "2軍でのTBF") 
    gg
  })

  # K-BBラベルなし 打者 ----
  output$plot_K_BB_hit <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = K_BB.y, 
                     y = K_BB.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍 (Kpct - BBpct) vs 1軍 (Kpct - BB pct)",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍 (Kpct - BBpct)", y = "1軍 (Kpct - BB pct)", color = "2軍でのPA") 
    gg
  })
  # K-BBラベルなし 投手 ----
  output$plot_K_BB_pit <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, 
                 aes(x = K_BB.y, 
                     y = K_BB.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍 (Kpct - BBpct) vs 1軍 (Kpct - BB pct)",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍 (Kpct - BBpct)", y = "1軍 (Kpct - BB pct)", color = "2軍でのTBF") 
    gg
  })
  # BABIPラベルなし 打者 ----
  output$plot_BABIP <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = BABIP.y, 
                     y = BABIP.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BABIP vs 1軍BABIP",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BABIP", y = "1軍BABIP", color = "二軍でのPA") 
    gg
  })
  # DERラベルなし ----
  output$plot_DER <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = BABIP.y, 
                     y = BABIP.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍DER vs 1軍DER",
           subtitle = "赤の破線はy = x; 簡易版DER (失策などを考慮していない).",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍DER", y = "1軍DER", color = "二軍でのTBF") 
    gg
  })
  # PAラベルなし 打者 ----
  output$plot_PA <- renderPlot({
    all <- Hitter_Input2()
    gg <- ggplot(all, # data
                 aes(x = N.y, 
                     y = N.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍PA vs 1軍PA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍PA", y = "1軍PA", color = "二軍でのPA") 
    gg
  })
  # PAラベルなし 投手 ----
  output$plot_TBF <- renderPlot({
    all <- Pitcher_Input2()
    gg <- ggplot(all, # data
                 aes(x = N.y, 
                     y = N.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍TBF vs 1軍TBF",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍TBF", y = "1軍TBF", color = "二軍でのTBF") 
    gg
  })

  # ラベルあり系 ----
  # wOBAプロットラベルあり
  output$plot_wOBA_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, 
                 aes(x = overall.y, 
                     y = overall.x, 
                     label = Player_name.x,
                     color = N.y)) +
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_point() +
      geom_text_repel(family = "HiraKakuPro-W3") + 
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 

      labs(title = "2軍wOBA vs 1軍wOBA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍wOBA", y = "1軍wOBA", color = "二軍でのPA") 
    gg
  })
  # FIPプロットラベルあり
  output$plot_FIP_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = overall.y, 
                     y = overall.x, 
                     label = Player_name.x,
                     color = N.y)) +
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_point() +
      geom_text_repel(family = "HiraKakuPro-W3") + 
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍FIP vs 1軍FIP",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍FIP", y = "1軍FIP", color = "二軍でのTBF") 
    gg
  })
  #AVGラベルあり
  output$plot_AVG_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = AVG.y, 
                     y = AVG.x, 
                     label = Player_name.x,
                     color = N.y)) +
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_point() +
      geom_text_repel(family = "HiraKakuPro-W3") + 
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍AVG vs 1軍AVG",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍AVG", y = "1軍AVG", color = "二軍でのPA") 
    gg
  })
  #ERAラベルあり
  output$plot_ERA_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = AVG.y, 
                     y = AVG.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍ERA vs 1軍ERA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍ERA ", y = "1軍ERA", color = "二軍でのTBF") 
    gg
  })
  # HRpctラベルあり
  output$plot_HRpct_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = HRpct.y, 
                     y = HRpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + # size = 0.5 etcで小さくできる aes()の外
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍HRpct vs 1軍HRpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍HRpct", y = "1軍HRpct",color = "二軍でのPA") 
    gg
  })
  # 投手HR9ラベルあり
  output$plot_HR9_L<- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = HRpct.y, 
                     y = HRpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍HR/9 vs 1軍HR/9",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍HR/9", y = "1軍HR/9",color = "二軍でのBF") 
    gg
  })
  # K ラベルあり (打者)
  output$plot_Kpct_hit_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = Kpct.y, 
                     y = Kpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍Kpct vs 1軍Kpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍Kpct", y = "1軍Kpct", color = "二軍でのPA") 
    gg
  })
  # K pct 投手　ラベルあり
  output$plot_Kpct_pit_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = Kpct.y, 
                     y = Kpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍Kpct vs 1軍Kpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍Kpct", y = "1軍Kpct", color = "二軍でのTBF") 
    gg
  })
  # BBラベルあり　打者
  output$plot_BBpct_hit_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, 
                 aes(x = BBpct.y, 
                     y = BBpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +

      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BBpct vs 1軍BBpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BBpct", y = "1軍BBpct", color = "2軍でのPA") 
    gg
  })
  # BBラベルあり 投手
  output$plot_BBpct_pit_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, 
                 aes(x = BBpct.y, 
                     y = BBpct.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BBpct vs 1軍BBpct",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BBpct", y = "1軍BBpct", color = "2軍でのTBF") 
    gg
  })
  
  # K-BBラベルあり 打者
  output$plot_K_BB_hit_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = K_BB.y, 
                     y = K_BB.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + #
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 

      labs(title = "2軍 (Kpct - BBpct) vs 1軍 (Kpct - BB pct)",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍 (Kpct - BBpct)", y = "1軍 (Kpct - BB pct)", color = "2軍でのPA") 
    gg
  })
  # K-BBラベルあり 投手
  output$plot_K_BB_pit_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, 
                 aes(x = K_BB.y, 
                     y = K_BB.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍 (Kpct - BBpct) vs 1軍 (Kpct - BB pct)",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍 (Kpct - BBpct)", y = "1軍 (Kpct - BB pct)", color = "2軍でのTBF") 
    gg
  })
  # BABIPラベルあり 打者
  output$plot_BABIP_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = BABIP.y, 
                     y = BABIP.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍BABIP vs 1軍BABIP",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BABIP", y = "1軍BABIP", color = "二軍でのPA") 
    gg
  })
  # DERラベルあり
  output$plot_DER_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = BABIP.y, 
                     y = BABIP.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍DER vs 1軍DER",
           subtitle = "赤の破線はy = x; 簡易版DER (失策などを考慮していない).",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍DER", y = "1軍DER", color = "二軍でのTBF") 
    gg
  })
  # PAラベルあり 打者
  output$plot_PA_L <- renderPlot({
    all <- Hitter_Input3()
    gg <- ggplot(all, # data
                 aes(x = N.y, 
                     y = N.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍PA vs 1軍PA",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍PA", y = "1軍PA", color = "二軍でのPA") 
    gg
  })
  # PAラベルあり 投手
  output$plot_TBF_L <- renderPlot({
    all <- Pitcher_Input3()
    gg <- ggplot(all, # data
                 aes(x = N.y, 
                     y = N.x, 
                     label = Player_name.x,
                     color = N.y)) +
      geom_point() + 
      geom_text_repel(family = "HiraKakuPro-W3") + 
      theme_gray(base_family = "HiraKakuPro-W3") +
      geom_smooth()+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title = element_text(size = 12)) +
      geom_abline(intercept = 0, 
                  slope = 1,
                  colour = "red",
                  size = 1,               
                  linetype = 2,           
                  alpha = 0.5) + 
      labs(title = "2軍TBF vs 1軍TBF",
           subtitle = "赤の破線はy = x.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍TBF", y = "1軍TBF", color = "二軍でのTBF") 
    gg
  })

  

  # 参考文献等 ----
  output$refs <- renderUI({
    str1 <- "[1] NPB公式サイト http://npb.jp"
    str2 <- "[2] wOBA係数@deltagraphs http://1point02.jp/op/gnav/glossary/discription/dis_bs_woba.html"
    str3 <- "[3] FIPの計算@fangraphs http://www.fangraphs.com/library/pitching/fip/"
    str4 <- "[4] Shiny全般について ほくそえむ様によるチュートリアル翻訳版目次 http://d.hatena.ne.jp/hoxo_m/20151222/p1"
    str5 <- "[5] Rを使った野球統計全般 Marchi and Albert, Analyzing Baseball Data with R (2013; CRC press)."
    str6 <- "[6] DER, wOBAの計算に関する補足 https://sleepnowinthenumbers.blogspot.jp/2017/09/blog-post_23.html"
    str7 <- "[7] 中の人 https://twitter.com/sleep_in_nmbrs"
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
  })
}