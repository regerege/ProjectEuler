/*
最初の10個の自然数について, その二乗の和は,
12 + 22 + ... + 102 = 385
最初の10個の自然数について, その和の二乗は,
(1 + 2 + ... + 10)2 = 3025
これらの数の差は 3025 - 385 = 2640 となる.
同様にして, 最初の100個の自然数について二乗の和と和の二乗の差を求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%206
*/
WITH RECURSIVE temp(n) AS (
  SELECT 1
UNION ALL
  SELECT n+1
  FROM temp
  WHERE n < 100
)
SELECT MAX(a.m) - SUM(a.n)
FROM
  (
    SELECT n*n n, (SUM(n) OVER() * SUM(n) OVER()) m
    FROM temp
  ) a
