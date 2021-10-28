#ライブラリ
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

setwd("~/Desktop/osakacity_shape/")

#シェープファイル読込
#eStatから取得 .dbf .prj .shxも合わせて作業ディレクトリ下部のshapeフォルダに入れておくべし。
shape <- st_read(dsn = "~/Desktop/osakacity_shape/", layer = "osakacity")

#住民基本台帳csv読込
data1 <- read_csv("jyuki202106.csv")

#Shift-JIS版
#data1 <- read_csv("./住基データ/02-20_jyu-ki(suminoe)----H30_3.csv", locale=locale(encoding="SJIS"))

#男女別が「計」のデータだけ抽出
data2 <- data1 %>% filter(data1$男女別=="計")

#shapeファイルとcsvを結合
data <- left_join(shape, data2, by=c("CITYNAME"="区名"))

#カラム名取得
column = colnames(data)
kuname="大阪市"

#######################################################
#総数　ファイルに書き出し 総数は9
for(i in 9:9){
  quartz(type="png", file=sprintf("大阪市全域住基R0306_総数.png"), dpi=144, bg="white")
	par(new=TRUE, mex="0.5", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
  col_km <- data[[column[i]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(9999,20000,40000,60000,80000,100000,120000,140000,160000,180000,max(.))) %>% findColours(.,pal=brewer.pal(10,"RdYlGn"))
  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, "　住民基本台帳人口　", column[i], " (令和3年6月末現在)", sep=""))
  #背景が濃い場合、テキストの視認性が落ちるため２色をシャドーとして描画
  #区名
  text(st_coordinates(shape %>% st_centroid)[,1]-0.0005, st_coordinates(shape %>% st_centroid)[,2]+0.0022, labels=shape$CITYNAME, cex=0.8, col="gray99")
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.002, labels=shape$CITYNAME, cex=0.8, col="gray14")
  #人口
  text(st_coordinates(shape %>% st_centroid)[,1]-0.00045, st_coordinates(shape %>% st_centroid)[,2]-0.0051, labels=data[[column[i]]], cex=0.75, col="gray99")
  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.005, labels=data[[column[i]]], cex=0.75, col="gray14")
  dev.off()
}



#######################################################
#総数　0歳～100歳以上まで各年齢別にファイルに書き出し
#for(i in 42:142){
#  quartz(type="pdf", file=sprintf("住之江区住基H3003_%d歳.pdf",i-42))
#	par(new=TRUE, mex="0.2", family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
#  col_km <- data[[column[i]]] %>% classIntervals(., 10, style="fixed", fixedBreaks=c(9999,20000,40000,60000,80000,100000,120000,140000,160000,180000,max(.))) %>% findColours(.,pal=brewer.pal(10,"YlGnBu"))
#  plot(st_geometry(shape[7]), col=col_km, main=paste(kuname, " ", column[i], "  (平成30年3月末現在)", sep=""))
#  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.001, labels=shape$MOJI, cex=0.3)
#  text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=data[[column[i]]], cex=0.4)
#  dev.off()
#}

