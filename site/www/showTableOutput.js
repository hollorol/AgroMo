var musoVariablesToPlot =
    [
  {
    "VARIABLE": "leaf dry matter",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "leaf dry matter in litter",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "fine root dry matter",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "yield",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "soft stem dry matter",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "projected lai",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "cum. evapotranspiration",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "cum_trans",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "rooting_depth",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "daily_gpp",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "daily_tr",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "daily_nee",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_0",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_1",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_2",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_3",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_4",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_5",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil_6",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_0",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_1",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_2",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_3",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "Variable names": "vwc_4",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_5",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc_6",
    "TIME STEP": "day",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "tsoil-profil",
    "TIME STEP": "none",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  },
  {
    "VARIABLE": "vwc-profil",
    "TIME STEP": "none",
    "FUNCTION": "identity",
    "PLOT TYPE": "line"
  }
]

putObjectAsTable(musoVariablesToPlot,"#showdiv-table-output_container","showdiv-table-output","showdiv-table-output-header","#showdiv-table-header_container");

var columnOptions = [["day","month","year","decade"],
              ["identity","var","min","max","mean","median","modus"],
               ["bar","line","scatter"]];

DT("#showdiv-table-output", "selected-rows_showdiv_table_output", columnOptions, "#showdiv-table-header_container th:nth-child(1)");
