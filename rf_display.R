#library(mvpart)
library(rpart)
library(rpart.plot)

ALLDATA <- iris
CLASS_NAME <- "Species"
CLASSES <- unique(ALLDATA[[CLASS_NAME]])
M <- 50  #決定木の数
feature_num <- ncol(ALLDATA)-1  #説明変数の数
use_feature_num <- as.integer(feature_num/2)  #学習に使用する説明変数の数
train_num <- nrow(ALLDATA)*2/3  #学習データの全データ数
test_num <- nrow(ALLDATA)*1/3  #テストデータの全データ数
rate_of_usedata <- 2/3  #学習データをランダム抽出する際の比率
#学習・テストデータをランダムに選ぶ
findex <- sample(nrow(ALLDATA),train_num)
data.train <- ALLDATA[findex, ] #全学習データ
data.test <- ALLDATA[-findex,] #全テストデータ
# 決定木モデル、学習データ、使用した説明変数をメンバ変数に持つクラスを宣言
setClass("decisionTree", representation( model = "list", data.train = "data.frame", feature = "vector"))

trees <- list()
sid <- 1
split.screen(c(3,3))
for (i in 1:M){
  #学習に使用するデータをランダムに抽出
  index <- sample(nrow(data.train), nrow(data.train)*rate_of_usedata)
  traindata_a_tree <- data.train[index,]
  #説明変数をランダムに選出
  dec <- sample(ncol(ALLDATA)-1, use_feature_num)
  features <- c(1:ncol(ALLDATA))
  features <- features[-dec]	
  #選出した説明変数で学習データを作成
  tree <- new("decisionTree")
  tree@data.train  <- traindata_a_tree[features]
  tree@feature  <- features
  #選出した説明変数と学習データで学習
  treeModel <- rpart(paste(CLASS_NAME, "~.", sep=""), data = tree@data.train, method = "class")
  tree@model  <- list(treeModel)  #rpartはlistを返すが、なぜかdecisionTreeにセットできないのでlist$に格納
  #decisionTreeクラスをリストへ格納
  trees <- c(trees, list(tree))
  if(i%%9==0){
    sid <- 9
  }else{
    sid <- i%%9
  }
  if(sid == 1){
    plot.new();
  }
  screen(sid); rpart.plot(tree@model[[1]]);
}

#ランダムフォレスト用prediction関数
rf_predict <- function(trees, data, classes=CLASSES){
  class_num = nrow(as.array(classes));
  tree_num = nrow(as.array(trees));
  predicted_rets = list();
  for(i in 1:nrow(data)){
    #テストデータを1個体取り出す
    target_data = data[i,];
    #結果カウント用リスト
    results_count = list();
    for(j in 1:class_num){
      results_count[[ as.character(classes[j]) ]] = 0;
    }
    #M個の決定木に対し、多数決で決定する
    for(j in 1:tree_num){
      tree = trees[[j]]@model[[1]];
      feature = trees[[j]]@feature;
      result <- predict(tree, newdata = target_data[feature], type = "class")
      results_count[[ as.character(result) ]] <- results_count[[ as.character(result) ]] + 1
    }
    res_names <- names(results_count);
    cnt_max = 0;
    predicted_class = "";
    for(j in 1:nrow(as.array(results_count))){
      if(cnt_max < results_count[[j]]){
        cnt_max = results_count[[j]];
        predicted_class <- as.character(res_names[j]);
      }
    }
    predicted_ret = list(target_data, predicted_class);
    predicted_rets = c(predicted_rets, list(predicted_ret));
    print(results_count);
    print("=======================");
  }
  return(predicted_rets);
}

# 予測実行
rf.res <- rf_predict(trees, data.test);

# クロス集計
rf.evl = data.frame()
for(i in 1:nrow(as.array(rf.res))){
  pred_class = rf.res[[i]][2];
  ins <- data.frame(Species=c(pred_class[[1]]))
  rf.evl <- rbind(rf.evl, ins)
}

print(table(rf.evl[,1],data.test[,5]))


