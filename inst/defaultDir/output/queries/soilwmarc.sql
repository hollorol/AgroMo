SELECT soilWaterCont030
FROM (SELECT plotid, AVG((3*swc1+7*swc2+20*swc3)/30) AS soilWaterCont030
      FROM agromo
      WHERE month = 3 AND year >= 1981 AND year <= 2010
      GROUP BY year, plotid)
GROUP BY plotid;
