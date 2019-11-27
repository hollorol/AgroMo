SELECT AVG(maxCumTrans) 
FROM (SELECT MAX(cumtrans) AS maxCumTrans, plotid, year 
      FROM agromo 
	  WHERE year >= 1981 AND year <= 2010
	  GROUP BY year, plotid) 
GROUP BY plotid;