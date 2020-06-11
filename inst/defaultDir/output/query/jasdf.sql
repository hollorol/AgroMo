/*asdgadfsgh*/



SELECT plotid,AVG(maxYield*10000) FROM (SELECT MAX(grainDM) AS maxYield, plotid, year FROM carpatclim WHERE year >= 2001 AND year <= 2010 GROUP BY year, plotid) GROUP BY plotid;
