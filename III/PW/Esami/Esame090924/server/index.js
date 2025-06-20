const express = require("express")
const fs = require("fs");
const path = require("path");
const cors = require("cors");

const app = express()

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

const counter = {"counter": 0}
const colors = {"background": "#882200", "text": "#44DDAA"}

app.get("/counter", (req, res) => {
    res.status(200).json(counter)
});

app.post("/increase", (req, res) => {
    counter.counter++;
    res.status(200).json(counter)
});

app.post("/decrease", (req, res) => {
    counter.counter--;
    res.status(200).json(counter)
});

app.get("/colors", (req, res) => {
    res.status(200).json(colors)
});

app.listen(3000, (err) => {
    if(err){
        console.error(err)
    }
    else{
        console.log("Server partito al 3000")
    }
});