function DT(tableID, classID, columnOptions){
  $(tableID + ' td').on('click', function() {
    // circular menu implementation with click events
    currName = $(this).text();
    colIndex = $(this).index()-1;
    if(colIndex >= 0){
        $(this).text(cicleArrays(columnOptions,colIndex,currName,1));
    } else {
        $(this).closest('tr').toggleClass(classID);
      Shiny.onInputChange("dataTable", getJSONFromDataTable());
    }
});
  $(tableID + " td").bind("wheel",function(e){
    // circular menu implementation with wheel events
    currName = $(this).text();
    colIndex = $(this).index()-1;
    if(colIndex >= 0){
      if(getWheelDelta(e) > 0){
          $(this).text(cicleArrays(columnOptions,colIndex,currName,1));
      } else {
          $(this).text(cicleArrays(columnOptions,colIndex,currName,-1));

      }
      e.preventDefault();
     }

});
  $(tableID + " th:nth-child(1)").on('click',function(){
    // Select all elements or none by clicking to the first element in the header
    if($(".selected").length == 0){
        $("tr :not(th)").addClass(classID);   
    } else {
        $(".selected").each(function(){
            $(this).removeClass(classID);
        });
    }
}); 
}
function getWheelDelta(event) {
  //Retuns with a mousewheel-delta. browser indipendently
    return event.wheelDelta || -event.detail || event.originalEvent.wheelDelta || -event.originalEvent.detail || -(event.originalEvent.deltaY) || null;
}
function cicleArrays(argArrays,rowIndex,choosenOne,direction){

    var numberOfOptions = argArrays[rowIndex].length;
    if(direction==1){
        return argArrays[rowIndex][(argArrays[rowIndex].indexOf(choosenOne) + 1) % numberOfOptions];
    } else {
        return argArrays[rowIndex][((argArrays[rowIndex].indexOf(choosenOne) - 1) % numberOfOptions +
                numberOfOptions) % numberOfOptions]; 
    }
}
function putObjectAsTable(tableObject, containerID, tableID){
    // Creating The main table
    var colNames = Object.keys(tableObject[0]);
    var nCols = colNames.length;
    var nRows = tableObject.length;
    $(containerID).append("<table id = " + "\"" + tableID + "\">" + "</table>");
    //Making header
    $("#" + tableID).append("<thead></thead>");
    $("#" + tableID + " thead").append("<tr></tr>");
    for(i=0;i<nCols;i++){
        $("#" + tableID + " thead tr").append("<th>"+colNames[i]+"</th>");
    }
    //Making body
    $("#" + tableID).append("<tbody></tbody>");
    for(i=0;i<nRows;i++){
        var rowClass = i + "-rowHR";
        $("#" + tableID + " tbody").append("<tr class=\""+rowClass+"\"></tr>");
        for(j=0;j<nCols;j++){
            var colClass = j + "-colHR";
            $("#" + tableID + " ."+rowClass).append("<td \""+colClass+"\">"+tableObject[i][colNames[j]]+"</td>");
        }
    }

}
function getJSONFromDataTable(){

    Json = [];
    attributes = [];
    $('th').each(function(){
        attributes.push($(this).html());
    });
    $(".selected").each(function(){
        tempObject = {};
        $(this).find("td").each(function(index,value){
            tempObject[attributes[index]] = $(this).html();
        });
        Json.push(tempObject);
    });
    return Json;//JSON.stringify(Json);
}
