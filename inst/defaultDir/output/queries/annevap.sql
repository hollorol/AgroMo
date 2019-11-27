SELECT AVG(maxCumEvap) 
FROM (SELECT MAX(cumevap) AS maxCumEvap, plotid, year 
      FROM agromo
	  WHERE year >= 1981 AND year <= 2010 
	  GROUP BY year, plotid)
GROUP BY plotid;
