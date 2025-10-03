const fs = require("fs")
const data = fs.readFileSync("data.json", "utf-8") 
const data_json = JSON.parse(data)

data_json.forEach((element) => {
    console.log(element)
});

const data2_json = JSON.parse(fs.readFileSync("data2.json", "utf-8"))
console.log(data2_json.nome)

//console.log(data_json)
