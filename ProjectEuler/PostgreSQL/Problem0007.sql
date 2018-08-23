/*
generate_series
素数を小さい方から6つ並べると 2, 3, 5, 7, 11, 13 であり,
6番目の素数は 13 である.
10001 番目の素数を求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%207
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
  WHERE array_length(ps,1) < 10001
)
SELECT ps[array_length(ps,1)]
FROM temp a
WHERE a.n = (SELECT MAX(b.n) FROM temp b)
