const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("dati.json", "utf-8"))

app.get("/citations", (req, res) => {
    res.status(200).json(data)
});


app.use((req, res) => {
	res.status(404).json({ 
        status: "error", 
        msg: "API not implemented" });
});

app.get("/citations/:id", (req, res) => {
    const id = parseInt(req.params.id)
    const trovato = data.find((el) => el.ID == id)
    if (trovato) {
        res.status(200).json(trovato)
    }
    else {
        res.status(404).json({ 
            status: "fail",
            message: "not found"})
    }
});

app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server partito al 3000")
    }
});