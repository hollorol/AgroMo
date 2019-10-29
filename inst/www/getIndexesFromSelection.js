function getIndexesForSelection(selectionClass){
  var a = [];
  $(selectionClass).each(function(){
    a.push($("td").index(this));
  })
  return a.map(x => x+1);
}
