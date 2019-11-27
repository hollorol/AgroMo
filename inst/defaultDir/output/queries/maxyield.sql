SELECT MAX(maxYield*10000) 
FROM (SELECT MAX(grainDM) AS maxYield, plotid 
      FROM agromo
	  GROUP BY year, plotid) 
GROUP BY plotid;