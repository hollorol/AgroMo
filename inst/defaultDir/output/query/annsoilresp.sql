SELECT AVG(avgYearSoilResp) 
FROM (SELECT plotid, AVG(dsoilresp) AS avgYearSoilResp 
      FROM agromo 
	  WHERE year >= 1981 AND year <= 2000 
	  GROUP BY year,plotid)
GROUP BY plotid;