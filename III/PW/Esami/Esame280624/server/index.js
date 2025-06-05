const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("dati.json", "utf-8"))

app.get("/persone", (req, res) => {
    res.status(200).json(data)
});


app.get("/persone/:id", (req, res) => {
    const id = parseInt(req.params.id)
    const perso = data.find((el) => el.id === id)
    if (perso) {
        res.status(200).json(perso)
    }
    else {
        res.status(404).json({ message: "Persona not found" })
    }
});

app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server started on port 3000")
    }
});