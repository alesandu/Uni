const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("dati.json", "utf-8"))


app.get("/articoli", (req, res) => {
    res.status(200).json(data)
});

app.get("/autori", (req, res) => {
    const array = []
    data.forEach(element => {
        array.push(element.autore)
    });
    res.status(200).json(array)
});


app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server partito al 3000")
    }
});