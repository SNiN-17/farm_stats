library(shiny)
# install.packages("shinydashboard")
library(shinydashboard)

# header ----
dashHeader <- dashboardHeader(title='')

# sidebar ----
dashSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Home', # 画面に表示される名前
             tabName='Hometab',
             icon=icon('dashboad')
    ),
    menuItem('打者', # 画面に表示される
             tabName='HitterTab',
             icon=icon('bar-chart-o')
    ),
    menuItem('打者 (ラベルあり)', # 画面に表示される
             tabName='HitterTab_L',
             icon=icon('bar-chart-o')
    ),
    menuItem('投手', # 画面に表示される
             tabName='PitcherTab',
             icon=icon('bar-chart-o')
    ),
    menuItem('投手 (ラベルあり)', # 画面に表示される
             tabName='PitcherTab_L',
             icon=icon('bar-chart-o')
    )
  )
)

# body ----
dashBody <- dashboardBody(
  tabItems(
    # home ----
    tabItem(tabName='Hometab',
            h1('プロ野球個人成績 1, 2軍比較 with Shiny'),
            p("データはNPB公式から. 2012年以後の1, 2軍での個人成績比較. データは18' 10/31時点で更新されていたもの. "),
            p("2012年以後の個人の1軍通算成績と通算成績2軍を散布図でプロットする. 曲線は局所的に重み付けされた回帰."),
            p("左から打者か投手を選ぶ. 「ラベルあり」では散布図に名前ラベルをつける. ラベルをつけるときはサンプルサイズを大きめ (500以上など) にしておかないと, ラベルに埋め尽くされるので注意. またラベルありでは描画にかなり時間がかかるので注意."),
            
            p("dataの性格上サンプリングバイアスのかたまりなので注意 (一軍で運が悪い → 2軍でのサンプル数が多い、など)."),
            p("成績は名前+チームごとに合算している. これは主に、NPB公式で同一名で表示される別人の選手の成績を合計することを回避するため (外国人選手にありがち).
                     このため移籍すると別人扱いになる. また表示名が変更された場合も, 別人として扱っている."),
            p("wOBAスケールは合わせていない (1.02 HPで示されている係数を利用した)."),
            p("FIPはFangraphs方式で定数を計算して算出. 各年度, 1, 2軍ごとに定数を計算して使用している. "),
            p("DERは失策などを考慮に入れていない簡易版. 正確ではないので注意. チームDERの影響も排除していない."),
            htmlOutput("refs")
    ),
    
    # hitter ----
    tabItem(tabName='HitterTab',
            h1('打撃成績'),
            p("下のスライドバーで解析に含めるサンプルサイズの閾値を変更できます. 上の ≡ を押すと左のデータ選択部分を隠せます."),
            fluidRow(
              
              column(4,sliderInput("range_hit1",
                                   "1軍での最低PA:",
                                   min = 1,
                                   max =2000,
                                   value =200)),
              
              column(4, sliderInput("range_hit2",
                                    "2軍での最低PA:",
                                    min = 1,
                                    max =2000,
                                    value = 200))
            ),

            fluidRow(
              column(5, p("数値入力後に, 表示を更新するには右をポチる.")),
              column(4, actionButton("update", "Update View"))
              
            ),
            
            p("2軍でのサンプルサイズが大きいほど (干されている or 期待されている, ほど) ラベルやプロットの色が薄くなるぞ! (図の右のゲージ参照)"),

            fluidRow(
              column(6, plotOutput("plot_wOBA")),
              column(6, plotOutput("plot_AVG"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_HRpct")),
              column(6, plotOutput("plot_K_BB_hit"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_Kpct_hit")),
              column(6, plotOutput("plot_BBpct_hit"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_BABIP")),
              column(6, plotOutput("plot_PA"))
            )
    ),
    
    # hitter with labels ----
    tabItem(tabName='HitterTab_L',
            h1('打撃成績 (名前ラベルあり)'),
            p("下のスライドバーで解析に含めるサンプルサイズの閾値を変更できます. 上の ≡ を押すと左のデータ選択部分を隠せます."),
            p('描画には結構時間がかかります。含まれている選手数が多いと、特に時間がかかりますので注意。'),
            fluidRow(
              column(4,sliderInput("range_hit1_L",
                                   "1軍での最低PA:",
                                   min = 1,
                                   max =2000,
                                   value =650)),

              column(4, sliderInput("range_hit2_L",
                                    "2軍での最低PA:",
                                    min = 1,
                                    max =2000,
                                    value = 650))
              
            ),

            fluidRow(
              column(5, p("数値入力後に, 表示を更新するには右をポチる.")),
              column(4, actionButton("update3", "Update View"))
              
            ),
            
            p("2軍でのサンプルサイズが大きいほど(干されている or 期待されている, ほど) ラベルやプロットの色が薄くなるぞ! (図の右のゲージ参照)"),
            
            plotOutput("plot_wOBA_L"),
            plotOutput("plot_AVG_L"),
            plotOutput("plot_HRpct_L"),
            plotOutput("plot_K_BB_hit_L"),
            plotOutput("plot_Kpct_hit_L"),
            plotOutput("plot_BBpct_hit_L"),
            plotOutput("plot_BABIP_L"),
            plotOutput("plot_PA_L")
          
    ),
    
    # pitcher ----
    tabItem(tabName='PitcherTab',
            h1('投手成績'),
            p("下のスライドバーで解析に含めるサンプルサイズの閾値を変更できます. 上の ≡ を押すと左のデータ選択部分を隠せます."),
            fluidRow(

              column(4,sliderInput("range_pit1",
                                   "1軍での最低TBF:",
                                   min = 1,
                                   max =2000,
                                   value =200)),

              column(4, sliderInput("range_pit2",
                                    "2軍での最低TBF:",
                                    min = 1,
                                    max =2000,
                                    value = 200))
              
            ),
            fluidRow(
              column(5, p("数値入力後に, 表示を更新するには右をポチる.")),
              column(4, actionButton("update2", "Update View"))
              
            ),
            
            p("2軍でのサンプルサイズが大きいほど(干されている or 期待されている, ほど) ラベルやプロットの色が薄くなるぞ! (図の右のゲージ参照)"),
            
            fluidRow(
              column(6, plotOutput("plot_FIP")),
              column(6, plotOutput("plot_ERA"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_HR9")),
              column(6, plotOutput("plot_K_BB_pit"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_Kpct_pit")),
              column(6, plotOutput("plot_BBpct_pit"))
              
            ),
            fluidRow(
              column(6, plotOutput("plot_DER")),
              column(6, plotOutput("plot_TBF"))
            )),
    
    # pitcher with labels
    tabItem(tabName='PitcherTab_L',
            h1('投手成績 (名前ラベルあり)'),
            p("下のスライドバーで解析に含めるサンプルサイズの閾値を変更できます. 上の ≡ を押すと左のデータ選択部分を隠せます."),
            p('描画には結構時間がかかります。含まれている選手数が多いと、特に時間がかかりますので注意。'),
            fluidRow(

              column(4,sliderInput("range_pit1_L",
                                   "1軍での最低TBF:",
                                   min = 1,
                                   max =2000,
                                   value =800)),

              column(4, sliderInput("range_pit2_L",
                                    "2軍での最低TBF:",
                                    min = 1,
                                    max =2000,
                                    value = 800))
              
            ),
            fluidRow(
              column(4, p("数値入力後に, 表示を更新するには右をポチる.")),
              column(4, actionButton("update4", "Update View"))
              
            ),
            
            p("2軍でのサンプルサイズが大きいほど (干されている or 期待されている, ほど) ラベルやプロットの色が薄くなるぞ! (図の右のゲージ参照)"),
            
            plotOutput("plot_FIP_L"),
            plotOutput("plot_ERA_L"),
            plotOutput("plot_HR9_L"),
            plotOutput("plot_K_BB_pit_L"),
            plotOutput("plot_Kpct_pit_L"),
            plotOutput("plot_BBpct_pit_L"),
            plotOutput("plot_DER_L"),
            plotOutput("plot_TBF_L")
)
  )
)


dashboardPage(
  header=dashHeader,
  sidebar=dashSidebar,
  body=dashBody,
  title='1, 2軍成績比較'
)