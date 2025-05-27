const express = require("express");
const app = express();
const path = require("path");


app.set("view engine", "ejs");
app.set("views", path.join(__dirname, "views"));

app.get("/", (req, res) => {
    res.render("home", {messaggio: "essi eh"})
});

app.get("/prodotti", (req, res) => {
    const listap = ["pasta", "riso", "pane", "latte"];
    res.render("prodotti", {listap: listap, messaggio: "oggi sconti"});
});


const PORT = 9999
app.listen(PORT, (err) => {
    if(err){
        console.error(err,message);
    }
    else{
        console.log(`Server started on port ${PORT}`);
    }
});