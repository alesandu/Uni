const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("data.json", "utf-8"))

app.get("/items", (req, res) => {
    res.status(200).json(data)
});

app.get("/items-complete", (req, res) => {
    lista = []
    data.forEach(el => {
        if(el.completato == true){
            lista.push(el)
        }
    });
    res.status(200).json(lista)
});

app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server started on port 3000")
    }
});
