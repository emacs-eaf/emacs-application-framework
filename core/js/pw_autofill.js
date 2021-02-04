function retrievePasswordFromPage() {
    let password = "";
    let formData = {};
    let inputList = document.getElementsByTagName("input");
    for(let i=0;i<inputList.length && inputList[i];i++){
        if(inputList[i].type === "password" && inputList[i].value != ""){
            password = inputList[i].value;
        }
        else if(inputList[i].type != "hidden" && inputList[i].value != ""){
            if(inputList[i].id != ""){
                formData[inputList[i].id] = inputList[i].value;
            } else if (inputList[i].name != "") {
                formData[inputList[i].name] = inputList[i].value;
            }
        }
    }
    return [password, formData];
}

function autofillPassword(password){
    let formData = %1
    let inputList = document.getElementsByTagName("input");
    for(let i=0;i<inputList.length && inputList[i];i++){
        if(inputList[i].type === "password"){
            inputList[i].value = password;
        }
        else if(inputList[i].type != "hidden"){
            if (inputList[i].id != "" && formData[inputList[i].id] != undefined){
                inputList[i].value = formData[inputList[i].id];
            } else if (inputList[i].name != "" && formData[inputList[i].name] != undefined){
                inputList[i].value = formData[inputList[i].name];
            }
        }
    }
}
