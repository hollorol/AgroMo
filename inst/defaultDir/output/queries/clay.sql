SELECT clay0to30 
FROM soil.soildata 
INNER JOIN soil.griddata10km ON soil.griddata10km.id100m = soil.soildata.id100m 
ORDER BY id10km;