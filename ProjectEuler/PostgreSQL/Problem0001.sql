/*
10未満の自然数のうち,3 もしくは 5 の倍数になっているものは 3, 5, 6, 9 の4つがあり,
これらの合計は 23 になる.
同じようにして, 1000 未満の 3 か 5 の倍数になっている数字の合計を求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%201
*/

WITH RECURSIVE temp(n) AS (
  SELECT 1
 UNION ALL
  SELECT n + 1
  FROM temp
  WHERE n < 1000-1
)
SELECT SUM(n)
FROM temp
WHERE n % 3 = 0 OR n % 5 = 0