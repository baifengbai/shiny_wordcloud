
library(shiny)
library(stringr)
library(jiebaR)
library(wordcloud2)

server<-shinyServer(
  
  function(input, output) {
    
    output$plot <- renderWordcloud2({
      
      cutter<-worker()
      content <-as.character(input$selection) # 将数据字符串化
      segWords <- segment(content,cutter)
      segWords <- gsub("[0-9a-zA-Z]+?","",segWords)
      segWords<-gsub("[的|和|了|来|与|到|由|等|从|以|一|为|在|上|各|去|对|侧|多|并|千|万|年|更|向|这是]","",segWords)
      segWords <- str_trim(segWords)
      segWords <- segWords[nchar(segWords)>1]
      seg <- unlist(segWords)      # 将segwords向量化
      tableWord <- table(seg)     # 生成为列联表
      Wordresult<- tableWord[order(-tableWord)] # 将tableword降序排列，从多到少排序
      wordcloud2(Wordresult[1:input$selection2],shape = input$selection3)
      
    })
  }
)


ui<- fluidPage(
  # 应用名
  titlePanel("中文文本词云"),
  
  # 侧边输入栏
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("selection",label = "请输入你的文本",value = '中国特色社会主义进入新时代，宣传工作必须立足新方位、找准新坐标，抓住历史机遇，应对风险挑战，把统一思想、凝聚力量作为宣传思想工作的中心环节。

去年，全国宣传思想工作会议在北京召开，习近平总书记在会上强调，要强化教育引导实践养成、制度保障，把社会主义核心价值观融入社会发展各方面，引导全体人民自觉践行。

广大农村是检验宣传思想工作成效的重要阵地，需要大力发挥宣传思想工作的“乘数效应”。提振群众精神，统一认识，筑牢理想信念，形成合力，是乡村振兴的应有之义，也是宣传思想工作的出发点和落脚点。

发挥“乘数效应”，需狠抓队伍建设，紧牵“牛鼻子”。要着力培养打造政治过硬、本领高强、求实创新、能打胜仗的乡村宣传思想工作队伍。这支工作队伍，要注重实地取“真经”，放下架子、扑下身子、摸清实情，善在田间地头问情于民；要善于练就“火眼金睛”，勤学善思，深刻把握当前农村宣传思想工作存在的问题和困难，理清工作脉络，提升观察力和思辨能力；要掌握“笔杆子”，勤于动笔，牢牢把握宣传思想工作重要的“武器”，文风忌“假大空”，力求生动、形象、接地气。

发挥“乘数效应”，需着力平台搭建，抓准“衣领子”。要拓宽宣传渠道，充分利用农村广播、农民夜校、农家书屋、文艺演出等形式，大力宣传弘扬社会主义核心价值观，破除陈规陋习，倡导文明乡风。要创新宣传方式，开展“星级文明户”“文明好家风”等评选活动，树立群众身边的榜样典型，让社会主义核心价值观深入人心。要与时俱进，紧跟农村发展步伐，善用正面思想舆论不断“更新”农民思想观念，牢牢掌握农村意识形态工作话语权、领导权、主动权。'),
      textInput("selection2",label = "要展示排名前多少的词",value = 50),
      selectInput("selection3",label = "选择词云的形状",list('circle','cardioid','star','diamond','triangle','pentagon'),selected = 'circle')
    ),
    #展示
    mainPanel(
      wordcloud2Output("plot")
    )
  )
)
shinyApp(ui,server)





