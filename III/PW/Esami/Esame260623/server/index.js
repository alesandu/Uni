const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("data.json", "utf-8"))
listaid = []
data.forEach(element => {
    listaid.push(element.id)
});

app.get("", (req, res) => {
    res.status(404).json({
        status: "fail",
        message: "sbagliato url"
    })
});

app.get("/list", (req, res) => {
    res.status(200).json(listaid)
});


app.get("/pics/:id", (req, res) => {
    const id = parseInt(req.params.id)
    const perso = data.find((el) => el.id === id)
    if (perso) {
        res.status(200).json(perso)
    }
    else {
        res.status(404).json({ 
            status: "fail",
            message: "Persona not found" })
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