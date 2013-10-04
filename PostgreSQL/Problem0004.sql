/*
左右どちらから読んでも同じ値になる数を回文数という.
2桁の数の積で表される回文数のうち, 最大のものは 9009 = 91 × 99 である.
では, 3桁の数の積で表される回文数の最大値を求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%204
*/

WITH RECURSIVE temp(n) AS (
  SELECT 100
UNION ALL
  SELECT n+1
  FROM temp
  WHERE n < 999
)
SELECT DISTINCT n
FROM
  (SELECT n,MAX(n) OVER() m
   FROM
     (SELECT ((a.n * b.n)|| '') a, reverse((a.n * b.n)|| '') b, a.n * b.n n
      FROM temp a,temp b) t
   WHERE a = b) t
WHERE n = m