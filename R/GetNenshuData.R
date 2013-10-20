GetNenshuData <- function(code){

  CellName2Seireki <- function(CellName_node){
    CellName <- xmlValue(CellName_node)
    CellName <- strsplit(CellName, " ")[[1]][2]
    if(substr(CellName, start=1, stop=2) == "\xe5\xb9\xb3\xe6\x88\x90"){
      as.integer(substr(CellName, start=3, stop=4)) - 12 - 1 + 2000
    }else{
      NA
    }
  }

  CellBrownRed2numeric <- function(CellBrownRed_node){
    CellBrownRed <- xmlValue(CellBrownRed_node)
    CellBrownRed <- gsub(",", "", CellBrownRed)
    as.numeric(CellBrownRed)
  }

  CellBrown2numeric <- function(Rinji_node){
    tmp <- gsub(",", "", xmlValue(Rinji_node))
    tmp <- ifelse(tmp == "\xef\xbc\x8d", NA, tmp)
    tmp <- ifelse(tmp == "\xe2\x88\x92", NA, tmp)
    as.numeric(tmp)
  }

  CellKessan2numeric <- function(Kessan_node){
    xmlValue(Kessan_node)
  }

  link <- paste("http://www.nenshu.jp/code/", as.character(code), ".htm", sep="")
  doc <- htmlParse(link)
  doc2 <- xpathSApply(doc, "//table")[[3]]
  
  year <- c()
  for( i in xpathSApply(doc2, "//td[@class=\"CellName\"]//text()")){
    year <- c(year, CellName2Seireki(i))
  }

  period <- c()
  for( i in xpathSApply(doc2, "//td[@class=\"CellName\"]//text()")){
    period <- c(period, xmlValue(i))
  }

  employee <- c()
  for( i in xpathSApply(doc2, "//td[@class=\"CellBrownRed\"]//text()")){
    employee <- c(employee, CellBrownRed2numeric(i))
  }
  
  CellBrown <- xpathSApply(doc2, "//td[@class=\"CellBrown\"]//text()")
  dim(CellBrown) <- c(3, length(CellBrown)/3)
  
  temporary <- c()
  for(i in CellBrown[1,]){
    temporary <- c(temporary, CellBrown2numeric(i))  
  }
  
  age <- c()
  for(i in CellBrown[2,]){
    age <- c(age, CellBrown2numeric(i))
  }
  
  seniority <- c()
  for(i in CellBrown[3,]){
    seniority <- c(seniority, CellBrown2numeric(i))
  }
  
  income <- c()
  for(i in xpathSApply(doc2, "//td[@class=\"CellDouble\"]//text()")){
    income <- c(income , CellBrown2numeric(i))
  }
  
  release <- c()
  for(i in xpathSApply(doc2, "//td[@class=\"CellKessan\"]//text()")){
    release <- c(release, CellKessan2numeric(i))
  }
  
  data.frame(code = rep(code, length(year)),
             name = rep(xmlValue(xpathSApply(doc, "//h1//a")[[2]]), length(year)),
             year = year,
             period = period,
             employee = employee,
             temporary = temporary,
             age = age,
             seniority = seniority,
             income = income,
             release = release
             )
}

