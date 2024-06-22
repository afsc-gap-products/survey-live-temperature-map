


############JAVA SCRIPT##############
#https://stackoverflow.com/questions/50313540/r-shiny-conditionally-change-numericinput-background-colour/50314686
jsCode1 <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

###########################3#######

