<?php
class decisionTree{
  // 対象データ
  private $data = [
                    ["a1"=>"肉食", "a2"=>"卵生" , "a3"=>"恒温" , "c"=>"鳥類" ], #スズメ
                    ["a1"=>"草食", "a2"=>"卵生" , "a3"=>"恒温" , "c"=>"鳥類" ], #ダチョウ
                    ["a1"=>"肉食", "a2"=>"胎生" , "a3"=>"恒温" , "c"=>"ほ乳類" ], #ネコ
                    ["a1"=>"草食", "a2"=>"胎生" , "a3"=>"恒温" , "c"=>"ほ乳類" ], #ウシ
                    ["a1"=>"草食", "a2"=>"卵生" , "a3"=>"変温" , "c"=>"は虫類" ], #トカゲ
                    ["a1"=>"草食", "a2"=>"卵生" , "a3"=>"変温" , "c"=>"は虫類" ] #カメ
                  ];


  private $cat_clm_name = "c";
  private $firstE = 0;
  private $train_result = null;
  private $predict_result = null;

  private function countHash($d, $clm_name){
    $res = array();
    for($i=0; $i<count($d); $i++){
      if(array_key_exists($d[$i][$clm_name],$res)){
        array_push ($res[$d[$i][$clm_name]], $d[$i]);
      }else{
        $res[$d[$i][$clm_name]] = [$d[$i]];
      }
    }
    return $res;
  }

  private function getEntropy($d){
    $fhash = $this->countHash($d,$this->cat_clm_name);
    $entlopy = 0;
    foreach($fhash as $k => $v){
      $prob = count($v)/count($d);
      $entlopy += - $prob * log($prob)/log(2);
    }
    return $entlopy;
  }

  // 各質問に対する得点のリストを返す
  private function getScores($d,$qlist){
    $scores = [];
    foreach($qlist as $k1 => $v1){
      $score = 0;
      $tmp_h = $this->countHash($d,$v1);
      foreach($tmp_h as $k2 => $v2){
        $score += count($v2)/count($d) * $this->getEntropy($v2);
      }
      $scores[$v1] = $this->firstE - $score;
    }
    arsort($scores, SORT_NUMERIC);
   return $scores;
  }

  // 決定木を作成
  private function train($d,$qlist) {
    $tmp_h = $this->countHash($d, $this->cat_clm_name);

    if(count($tmp_h)==1){
      $tmp_h_keys = array_keys($tmp_h);
#var_dump($tmp_h_keys);
      return [$tmp_h_keys[0]];
    }
    if(count($qlist)==1){
      $qlist_keys = array_keys($qlist);

      $tmp_h = $this->countHash($d,$qlist[$qlist_keys[0]]);
      $ret = [$qlist[$qlist_keys[0]],[]];
var_dump($ret);
      foreach($tmp_h as $k => $v){
        $ret[1][$k] = $this->countHash($v,$this->cat_clm_name);
      }
/*
var_dump($qlist);
print("\n=====\n");
var_dump($tmp_h);
print("\n=======================================================\n\n");
/**/

      return $ret;
    }
    $scores = $this->getScores($d,$qlist);
    $score_keys = array_keys($scores);

    $hiScoreQuestion = $score_keys[0];
    $hiScoreGroup = $this->countHash($d,$hiScoreQuestion);
    $ret = [$hiScoreQuestion, []];
    $next_qlist = array_diff($qlist,[$hiScoreQuestion]);

    foreach($hiScoreGroup as $k => $v){
      $ret[1][$k] = $this->train($v, $next_qlist);
    }
    return $ret;
  }

  public function predict($treeObj=null, $test=null){
    try{
      //作成した決定木をもとに、与えられたデータをもとに、各データがどのクラスになるかを予想
      if($treeObj===null){
        throw new Exception("ERROR: Decision tree object must be obtained.");
      }
      if($test===null){
        throw new Exception("ERROR: Test data must be obtained.");
      }
      if($this->train_result===null){
        throw new Exception("ERROR: Execute train method first.");
      }

      if(count($treeObj) == 1){
        return $treeObj[0];
      }else{
        $question = $treeObj[0];
        foreach($treeObj[1] as $key => $nextTree){
          if($test[$question] === $key){
            return $this->predict($nextTree, $test);
          }
        }
      }
    }catch(Exception $e){
      throw $e;
    }
  }

  // 実行
  public function exec($test=null , $data=null) {
    try{
      if($test===null){
        throw new Exception("Test data must be obtained.");
      }
      if($data===null){
        $data = $this->data;
      }
      $this->firstE = $this->getEntropy($data);
      $this->train_result = $this->train($data,["a1","a2","a3"]);

      var_dump($this->predict($this->train_result, $test));
      return $this->train_result;
    }catch(Exception $e){
      echo $e->getMessage()."\n";
    }
  }
}

$tree = new decisionTree();
$test_data = ["a1"=>"肉食", "a2"=>"胎生" , "a3"=>"恒温" , "c"=>"ほ乳類" ];
echo "Input data: \n";
print_r($test_data);
echo "\n=====================================\n\n";

echo "Predicted class: \n";
$a = $tree->exec($test_data);
echo "\n=====================================\n\n";
echo "Dump decisionTree : \n";
var_dump($a);

?>

