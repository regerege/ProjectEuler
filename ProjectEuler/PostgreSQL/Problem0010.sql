/*
10以下の素数の和は 2 + 3 + 5 + 7 = 17 である.
200万以下の全ての素数の和を求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2010
*/
WITH RECURSIVE temp(n,ps) AS (
  SELECT 2,ARRAY[2]::int[]
UNION ALL
  SELECT n+1
    , (CASE
         WHEN
           NOT EXISTS(
             SELECT *
             FROM (SELECT unnest(ps)) a
             WHERE (n+1) % a.unnest = 0
           )
         THEN ps || (n+1)
         ELSE ps
       END)
  FROM temp
  WHERE n < 2000000
), prim(p) AS (
SELECT unnest(ps)
FROM temp a
WHERE a.n = (SELECT MAX(b.n) FROM temp b)
)
SELECT SUM(p)
FROM prim
