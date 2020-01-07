SELECT AVG(plantResp) 
FROM (SELECT AVG(dmresp+dgresp) AS plantResp, plotid, year 
      FROM agromo
	  WHERE year >= 1981 AND year <= 2010 
	  GROUP BY year, plotid)
GROUP BY plotid;
