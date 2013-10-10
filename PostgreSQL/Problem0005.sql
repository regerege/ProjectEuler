/*
2520 は 1 から 10 の数字の全ての整数で割り切れる数字であり,
そのような数字の中では最小の値である.
では, 1 から 20 までの整数全てで割り切れる数字の中で最小の正の数はいくらになるか.
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%205
*/
WITH RECURSIVE temp(n) AS (
  SELECT 1
UNION ALL
  SELECT n+1
  FROM temp
  WHERE n < 10000000
)
SELECT n
FROM temp
WHERE
  (
    n%1=0
    AND n%2=0
    AND n%3=0
    AND n%4=0
    AND n%5=0
    AND n%6=0
    AND n%7=0
    AND n%8=0
    AND n%9=0
    AND n%10=0
    AND n%11=0
    AND n%12=0
    AND n%13=0
    AND n%14=0
    AND n%15=0
    AND n%16=0
    AND n%17=0
    AND n%18=0
    AND n%19=0
    AND n%20=0
  )
