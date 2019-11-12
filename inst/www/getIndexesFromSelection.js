function getIndexesForSelection(selectionClass){
    //Wrong base implementation td has to be tr
  var a = [];
  $(selectionClass).each(function(){
    a.push($("td").index(this));
  })
  return a.map(x => x+1);
}
// function getIndexesForSelection(selectionClass){
//     //The good implementation
//   var a = [];
//   $(selectionClass).each(function(){
//     a.push($("tr").index(this));
//   })
//   return a.map(x => x+1);
// }
