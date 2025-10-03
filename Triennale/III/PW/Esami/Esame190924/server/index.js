const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("dati.json", "utf-8"))

app.get("/task", (req, res) => {
    res.status(200).json(data)
});

app.post("/task/complete/:id", (req, res) => {
    const id = parseInt(req.params.id)
    data.forEach(el => {
        if(el.id === id){
            el.completed = true
        }
    });
    res.status(200).json(data)
    });

app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server partito al 3000")
    }
});