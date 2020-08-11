/*carpforsee*/



SELECT simOne.plotid,simOne.avggrain-simTwo.avggrain AS diff FROM (SELECT plotid, AVG(yearmax) AS avggrain FROM (SELECT plotid, MAX(grainDM)*10000 AS yearmax FROM carpatclim WHERE year >= 2001 AND year <= 2010 GROUP BY year, plotid) GROUP BY plotid) AS simOne INNER JOIN (SELECT plotid, AVG(yearmax) AS avggrain FROM (SELECT plotid, MAX(grainDM)*10000 AS yearmax FROM forsee WHERE year >= 2001 AND year <= 2010 GROUP BY year, plotid) GROUP BY plotid) AS simTwo on simOne.plotid=simTwo.plotid;
