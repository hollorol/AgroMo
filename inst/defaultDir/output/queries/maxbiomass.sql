SELECT MAX(maxBiomass) 
FROM (SELECT MAX(agbiomassC) AS maxBiomass, plotid, year 
      FROM agromo
	  WHERE year >= 1981 AND year <= 2010 
	  GROUP BY year, plotid) 
GROUP BY plotid;