const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const data = JSON.parse(fs.readFileSync("dati.json", "utf-8"))

app.get("/articles", (req, res) => {
    res.status(200).json(data)
});


app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server started on port 3000")
    }
});