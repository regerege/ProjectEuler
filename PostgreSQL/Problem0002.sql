/*
フィボナッチ数列の項は前の2つの項の和である.
最初の2項を 1, 2 とすれば, 最初の10項は以下の通りである.
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
数列の項の値が400万より小さい, 偶数値の項の総和を求めよ.
Note:この問題は最近更新されました. お使いのパラメータが正しいかどうか確認してください.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%202
*/

WITH RECURSIVE temp(a,b) AS (
  SELECT 1,1
 UNION ALL
  SELECT b,a+b
  FROM temp
  WHERE a < 4000000
)
SELECT SUM(a)
FROM temp t
WHERE
  a < 4000000
  AND a % 2 = 0
