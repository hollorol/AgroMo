SELECT AVG(maxYield*10000)
FROM (SELECT MAX(grainDM) AS maxYield, plotid, year 
      FROM agromo 
	  WHERE year >= 1981 AND year <= 2010
	  GROUP BY year, plotid)
GROUP BY plotid;