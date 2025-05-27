const express = require("express");
const app = express();

const morgan = require("morgan");

app.use(morgan("dev"));


const cors = require("cors");

app.use(cors());

app.use(express.static("public"));

const myMiddleware = (req, res, next) => {
    console.log("Middleware executed");
    next();
}

app.get("/", (req, res) => {
    res.send("<h1>Prova</h1>");
});

const checkAuth = function(req, res, next) {
    const authHeader = req.headers["authorization"];
    if (authHeader == "Bearer 123456") {
        next();    
    }
    else{
        res.status(401).send("Unauthorized");
    }
    console.log("Check auth middleware executed");
}

const sendData = function(req, res, next) {
    res.json({
        message: "Hello World"
    });
}

app.get("/secure", checkAuth, sendData);


app.use(myMiddleware);

const PORT = 9090
app.listen(PORT, (err) => {
    if(err){
        console.error(err,message);
    }
    else{
        console.log(`Server started on port ${PORT}`);
    }
});