function getIndexesForSelection(){
  var a = [];
  $(".selected").each(function(){
    a.push($("tr").index(this));
  })
  return a;
}
