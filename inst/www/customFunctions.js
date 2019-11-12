Shiny.addCustomMessageHandler("showHide",function(input){
    var nyamla = input.split(" ");
    // var x = document.getElementsByClassName(nyamla[0]);
    // for(i = 0; i < x.length; ++i){
    // 	i.style.display = "none";
    // }
    console.log(nyamla[1]);
    var x = document.getElementById(nyamla[1]);
    x.style.display = "block";    
})
