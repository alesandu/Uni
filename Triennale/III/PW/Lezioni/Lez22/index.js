const express = require("express")
const fs = require("fs");
const { parse } = require("path");

const app = express()



const data = JSON.parse(fs.readFileSync("prof.json", "utf-8"))
app.get("/api/v1/profs", (req, res) => {
    res.status(200).json(data)
});


app.get("/api/v1/profs/:id", (req, res) => {
    const id = parseInt(req.params.id)
    const prof = data.find((el) => el.id === id)
    if (prof) {
        res.status(200).json(prof)
    }
    else {
        res.status(404).json({ message: "Prof not found" })
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