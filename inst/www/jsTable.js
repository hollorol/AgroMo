function DT(tableID, classID, columnOptions, selectAllTriggerElementID){
  $(tableID + ' td').on('click', function() {
    // circular menu implementation with click events
    currName = $(this).text();
    colIndex = $(this).index()-1;
    if(colIndex >= 0){
        $(this).text(cicleArrays(columnOptions,colIndex,currName,1));
    } else {
        $(this).closest('tr').toggleClass(classID);
      Shiny.onInputChange("dataTable", getJSONFromDataTable());
      // Shiny.onInputChange("showdiv-selection-list", getIndexesForSelection());
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
      e.preventDefault ? e.preventDefault() : (e.returnValue = false); // The later option is for safari and IE
     }

});
  $(selectAllTriggerElementID).on('click',function(e){
      //Select all elements or none by clicking to the first element in the header
      if($("." + classID).length === 0){
        $(tableID + " tr:not(th)").addClass(classID);   
    } else {
        console.log("Te marha!");
        $("." + classID).each(function(){
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
function putObjectAsTable(tableObject, containerID, tableID, headerID, headerContainerID){
    // Creating The main table
    const colNames = Object.keys(tableObject[0]);
    const nCols = colNames.length;
    const nRows = tableObject.length;
    $(headerContainerID).append("<table id = " + "\"" + headerID + "\">" + "</table>");
    //Making header
    $("#" + headerID).append("<thead></thead>");
    $("#" + headerID + " thead").append("<tr></tr>");
    for(let i=0;i<nCols;i++){
        $("#" + headerID + " thead tr").append("<th>"+colNames[i]+"</th>");
    }

    $(containerID).append("<table id = " + "\"" + tableID + "\">" + "</table>");
    //Making body
    $("#" + tableID).append("<tbody></tbody>");
    for(let i=0;i<nRows;i++){
        var rowClass = i + "-rowHR";
        $("#" + tableID + " tbody").append("<tr class=\""+rowClass+"\"></tr>");
        for(j=0;j<nCols;j++){
            var colClass = j + "-colHR";
            $("#" + tableID + " ."+rowClass).append("<td \""+colClass+"\">"+tableObject[i][colNames[j]]+"</td>");
        }
    }

}

function getJSONFromDataTable(){

    const Json = [];
    const attributes = ["variable","time_step","function","plot_type"];
    // $('th').each(function(){
    //     attributes.push($(this).html());
    // });

    $(".selected-rows_showdiv_table_output").each(function(){
        tempObject = {};
        $(this).find("td").each(function(index,value){
            tempObject[attributes[index]] = $(this).html();
        });
        Json.push(tempObject);
    });
    return Json;//JSON.stringify(Json);
}
