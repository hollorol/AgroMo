var musoVariablesToPlot =
    [
  {
    "Variable names": "leaf dry matter",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "leaf dry matter in litter",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "fine root dry matter",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "yield",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "soft stem dry matter",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "projected lai",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "cum. evapotranspiration",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "cum_trans",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "rooting_depth",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "daily_gpp",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "daily_tr",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "daily_nee",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_0",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_1",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_2",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_3",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_4",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_5",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil_6",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_0",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_1",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_2",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_3",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_4",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_5",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc_6",
    "Time frame": "day",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "tsoil-profil",
    "Time frame": "none",
    "Grouping function": "identity",
    "Plot type": "line"
  },
  {
    "Variable names": "vwc-profil",
    "Time frame": "none",
    "Grouping function": "identity",
    "Plot type": "line"
  }
]

putObjectAsTable(musoVariablesToPlot,"#showdiv-table-output_container","showdiv-table-output");

var columnOptions = [["day","month","year","decade"],
              ["identity","var","min","max","mean","median","modus"],
               ["bar","line","scatter"]];

DT("#showdiv-table-output", "selected-rows_showdiv_table_output", columnOptions);
