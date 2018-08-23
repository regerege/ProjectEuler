/*
ピタゴラス数(ピタゴラスの定理を満たす自然数)とは a < b < c で以下の式を満たす数の組である.
a + b + c = 1000 となるピタゴラスの三つ組が一つだけ存在する.
これらの積 abc を計算しなさい.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%209
*/

WITH RECURSIVE s(n) AS (
  SELECT 1
 UNION ALL
  SELECT n + 1
  FROM s
  WHERE n < 500
)
SELECT a.n,b.n,c.n
FROM
  s a
    INNER JOIN
      s b
        ON a.n < b.n
    INNER JOIN
      s c
        ON b.n < c.n
WHERE
  a.n + b.n + c.n = 1000
  AND a.n * a.n + b.n * b.n = c.n * c.n
