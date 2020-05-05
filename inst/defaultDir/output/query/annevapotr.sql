SELECT AVG(avgYearEvapotr) 
FROM (SELECT plotid, MAX(cumevtr) AS avgYearEvapotr 
      FROM agromo 
	  WHERE year >= 1981 AND year <= 2010 
	  GROUP BY year, plotid) 
GROUP BY plotid;
