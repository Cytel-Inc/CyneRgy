// Import the fs (File System) module for reading files  
const fs = require('fs');  
  
// Define the function  
function parseRFunctions(file, functionName) {  
    // Read the file  
    const rScript = fs.readFileSync(file, 'utf8');  
      
    // Split script into lines  
    var lines = rScript.split('\n');  
    var functions = [];  
    var functionExists = false;  
    var functionParametersMatch = false;  
    var braceCount = 0;  
  
    // Iterate through each line  
    for (var i = 0; i < lines.length; i++) {  
        // Update brace count  
        braceCount += (lines[i].match(/{/g) || []).length;  
        braceCount -= (lines[i].match(/}/g) || []).length;  
  
        // Use regular expressions to find function signatures  
        var match = lines[i].match(/^\s*([a-zA-Z0-9._]+)\s*<-?\s*function\s*\((.*?)\)/);  
        if (match && braceCount === 1) {  
            var func = {  
                name: match[1],  
                parameters: match[2].split(',').map(function(param) {  
                    return param.replace(/\s+/g, '');  
                })  
            };  
            functions.push(func);  
  
            // Check if the function name exists and parameters match  
            if (func.name === functionName) {  
                functionExists = true;  
                if (func.parameters.includes('SimData') && func.parameters.includes('DesignParam')   
                    && func.parameters.includes('LookInfo') && func.parameters.includes('UserParam=NULL')) {  
                    functionParametersMatch = true;  
                }  
            }  
        }  
    }  
  
    // Return if the function name is valid and parameters match  
    if (functionExists && functionParametersMatch) {  
        console.log('Success! The function "' + functionName + '" is valid and parameters match.\n');  
    } else {  
        console.log('Error! The function "' + functionName + '" is either not valid or parameters do not match.\n');  
    }  
  
    // Now 'functions' array contains the function names and parameters  
    functions.forEach(function(func) {  
        console.log('Function Name: ' + func.name);  
        console.log('Function Parameters: ' + func.parameters.join(', ') + '\n');  
    });  
}  
  
// Sample tests  
parseRFunctions('test1.R', 'MyFunction');  
parseRFunctions('test2.R', 'AnotherFunction');  
