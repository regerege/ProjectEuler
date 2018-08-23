/*
13195 の素因数は 5, 7, 13, 29 である.
600851475143 の素因数のうち最大のものを求めよ.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%203
*/

WITH RECURSIVE temp(n,p,l) AS (
  SELECT 600851475143,2,ARRAY[]::int[]
UNION ALL
  SELECT
    (CASE WHEN n%p=0 THEN n/p ELSE n END)
    , (CASE WHEN p = 2 THEN 3 ELSE p+2 END)
    , (CASE WHEN n%p=0 THEN array_append(l,p) ELSE l END)
  FROM temp
  WHERE p <= n
)
SELECT MAX(p)
FROM (
SELECT unnest(l) p
FROM temp a
WHERE EXISTS(SELECT 1 FROM temp b HAVING MIN(b.n) = a.n)
) t