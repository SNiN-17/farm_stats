# NPBの一軍と二軍の個人成績を表示するshiny app
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
# githubに保存したファイルを使用する
batUrl <- "https://raw.githubusercontent.com/SiN-17/farm_stats/master/NPB_batting_2012_onward.csv"
bat <- read_csv(batUrl, 
                locale=locale(encoding="CP932"))
pitUrl <- "https://raw.githubusercontent.com/SiN-17/farm_stats/master/NPB_pitching_2012_onward.csv"
pit <- read_csv(pitUrl, 
                locale=locale(encoding="CP932"))
# 通算打撃成績
career_bat <- dplyr::summarise(group_by(bat, Player_name, T, Level),
                               N = sum(PA),
                               HRpct = sum(HR)/N,
                               Kpct = weighted.mean(Kpct, PA),
                               BBpct = weighted.mean(BBpct, PA),
                               BABIP =weighted.mean(BABIP, PA),
                               AVG = weighted.mean(AVG, AB),
                               wOBA = weighted.mean(wOBA, PA)) %>%
  mutate(K_BB = Kpct -BBpct)
career_bat$ID <- paste(career_bat$Player_name, career_bat$T)
names(career_bat)[10] <- "overall"
# 通算投球成績
career_pit <- dplyr::summarise(group_by(pit, Player_name, T, Level),
                               N = sum(TBF),
                               HR_9 = weighted.mean(HR_9, TBF),
                               Kpct = weighted.mean(Kpct, TBF),
                               BBpct = weighted.mean(BBpct, TBF),
                               DER =weighted.mean(DER, TBF),
                               ERA = weighted.mean(ERA, IP),
                               FIP = weighted.mean(FIP, IP))%>%
  mutate(K_BB = Kpct -BBpct)
career_pit$ID <- paste(career_pit$Player_name, career_pit$T)
names(career_pit) <- c("Player_name", "T","Level","N","HRpct","Kpct","BBpct","BABIP","AVG","overall","K_BB","ID")


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("プロ野球個人成績 1, 2軍比較 with Shiny"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    position = ("left"),
    fluid = TRUE,
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("データはNPB公式から. 2012年以後の1, 2軍での個人成績比較. 曲線は局所的な移動平均."),
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "データタイプ:",
                  choices = c("Batting", "Pitching")),
      br(),
      
      # 選手名で検索するためのtextInputとその説明
      helpText("特定の選手のみを見たい場合、名前を入力. 要完全一致 (スペースは全角で). 正しい名前を入力して表示されない時は下のスライドバーを左にすると見えるようになるかも."),
      
      textInput("symb", "名前:", ""),
      br(),
      helpText("データ選択 or 名前入力後に, 表示を更新するには下をポチる."),
      
      # 選手名を入れた時にactiveだとすぐに何も表示されなくなるのを回避するため、更新ボタン
      actionButton("update", "Update View"),
      br(),
      br(),
      
      #スライドバーでサンプルの閾値を設定できるようにする 
      sliderInput("range",
                  "1軍での最低PA/TBF:",
                  min = 1,
                  max =2000,
                  value = 700),
      sliderInput("range2",
                  "2軍での最低PA/TBF:",
                  min = 1,
                  max =2000,
                  value = 700),

      
      p("上のスライドバーで解析に含めるサンプルサイズの閾値を変更して遊ぼう! BattingではPA, PitchingではTBFを使用. "),
      p("2軍でのサンプルサイズが大きいほど(干されている or 期待されている, ほど)ラベルやプロットの色が薄くなるぞ! (図の右のゲージ参照)"),
      p("上のタブでstatsの種類を選べる."),
      helpText("statsの性格上サンプリングバイアスのかたまりなので注意 (一軍で運が悪い → 2軍でのサンプル数が多い、など)."),
      helpText("成績は名前+チームごとに合算している. これは主に、同一の登録名の別人の選手の成績を合計することを回避するため (外国人選手にありがち).
        このため移籍すると別人扱いになる. また登録名を変更した場合も、別人として扱っている."),
      helpText("wOBAスケールは合わせていない (delta HPで示されている係数を利用した)."),
      helpText("FIPはFangraphs方式で定数を計算して算出. 各年度, 1, 2軍ごとに定数を計算して使用している. "),
      helpText("DERは失策を考慮に入れていない簡易版. 正確ではないので注意. チームDERの影響も排除していない."),
      width = 4
      ),
    # 画面に表示する
    mainPanel(
      # タブをつくって、そこにプロットと参考文献を配置する
      tabsetPanel(type = "tabs",
                  tabPanel("wOBA/FIP", plotOutput("plot")),
                  tabPanel("AVG/ERA", plotOutput("plot7")),
                  tabPanel("HR", plotOutput("plot2")),
                  tabPanel("K", plotOutput("plot3")),
                  tabPanel("BB", plotOutput("plot4")),
                  tabPanel("K-BB", plotOutput("plot5")),
                  tabPanel("BABIP", plotOutput("plot6")),
                  tabPanel("Ref", htmlOutput("refs"))
      ))
  ))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "Batting" = career_bat,
           "Pitching" = career_pit) 
  }, ignoreNULL = FALSE)

  # 入力された名前を拾う
  nameInput <- eventReactive(input$update, {
    nput$symb
  }, ignoreNULL = FALSE)
  
  # 名前が入力されている場合、filterでその名前の選手だけをsubsetする
  finalInput <- eventReactive(input$update, {
    if(input$symb == ""){
      datasetInput()
    }else{
      df <- datasetInput() 
      df <- df %>% 
        filter(Player_name == input$symb)
    }
  }, ignoreNULL = FALSE)

  # wOBA/FIPプロット
  output$plot <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍wOBA/FIP vs 1軍wOBA/FIP",
           subtitle = "赤の破線はx = y; 打者はwOBA, 投手はFIPを表示.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍wOBA/FIP", y = "1軍wOBA/FIP", color = "二軍でのPA/TBF") 
    gg
  })
  #AVG/ERA
  output$plot7 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍AVG/ERA vs 1軍AVG/ERA",
           subtitle = "赤の破線はx = y; 打者はAVG, 投手はERAを表示.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍AVG/ERA ", y = "1軍AVG/ERA", color = "二軍でのPA/TBF") 
    gg
  })
  # プロット2
  output$plot2 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍HR/PA or HR/9 vs 1軍HR/PA or HR/9",
           subtitle = "赤の破線はx = y; 打者はHR/PA, 投手はHR/9イニングを表示.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍HR/PA or HR/9", y = "1軍HR/PA or HR/9",color = "二軍でのPA/TBF") 
    gg
  })
  # プロット3
  output$plot3 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍Kpct vs 1軍Kpct",
           subtitle = "赤の破線はx = y; 分母は野手はPA, 投手はTBF.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍Kpct", y = "1軍Kpct", color = "二軍でのPA/TBF") 
    gg
  })
  # プロット4
  output$plot4 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
    gg <- ggplot(all, # data
                 aes(x = BBpct.y, 
                     y = BBpct.x, 
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍BBpct vs 1軍BBpct",
           subtitle = "赤の破線はx = y; 分母は野手はPA, 投手はTBF.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BBpct", y = "1軍BBpct", color = "2軍でのPA/TBF") 
    gg
  })
  # プロット5
  output$plot5 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍 (Kpct - BBpct) vs 1軍 (Kpct - BB pct)",
           subtitle = "赤の破線はx = y; 分母は野手はPA, 投手はTBF.",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍 (Kpct - BBpct)", y = "1軍 (Kpct - BB pct)", color = "2軍でのPA/TBF") 
    gg
  })
  # プロット6
  output$plot6 <- renderPlot({
    df <- finalInput()
    top <- df %>% filter(Level == "Top") %>% 
      filter(N >= input$range) 
    farm <- df %>% filter(Level == "Farm") %>% 
      filter(N >= input$range2) 
    all <- inner_join(top, farm, by = "ID")
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
      geom_label_repel(size = 4, family = "HiraKakuPro-W3",
                       box.padding = unit(1.5,"lines"),
                       point.padding = unit(0.1,"lines"),
                       segment.color='grey70') +
      labs(title = "2軍BABIP/DER vs 1軍BABIP/DER",
           subtitle = "赤の破線はx = y; 野手はBABIP, 投手は簡易版DER (失策を考慮していない).",
           caption = "Source: NPB公式 (2012年以後)", 
           x = "2軍BABIP/DER", y = "1軍BABIP/DER", color = "二軍でのPA/TBF") 
    gg
  })
  # 参考文献等
  output$refs <- renderUI({
    str1 <- "[1] NPB公式サイト http://npb.jp"
    str2 <- "[2] wOBA係数@deltagraphs http://1point02.jp/op/gnav/glossary/discription/dis_bs_woba.html"
    str3 <- "[3] FIPの計算@fangraphs http://www.fangraphs.com/library/pitching/fip/"
    str4 <- "[4] Shiny全般について ほくそえむ様によるチュートリアル翻訳版目次 http://d.hatena.ne.jp/hoxo_m/20151222/p1"
    str5 <- "[5] Rを使った野球統計全般 Marchi and Albert, Analyzing Baseball Data with R (2013; CRC press)."
    HTML(paste(str1, str2, str3, str4, str5,sep = '<br/>'))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)