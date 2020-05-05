SELECT AVG(maxLai) 
FROM (SELECT MAX(lai) AS maxLai, plotid 
      FROM agromo 
	  WHERE year >= 1981 AND year <= 2010 
	  GROUP BY year, plotid) 
GROUP BY plotid;