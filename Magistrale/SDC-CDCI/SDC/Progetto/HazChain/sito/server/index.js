require("dotenv").config();
const express = require("express"); // framework per creare il server
const path = require("path"); // per prendere il percorso delle pagine statiche
const cors = require("cors"); // per permettere le richieste cross-origin

// Route modulari
const authRoutes = require("./routes/auth");
const operationRoutes = require("./routes/operations");
const queryRoutes = require("./routes/queries");

const app = express();

app.use(express.json());
app.use(cors());
app.use(express.static(path.join(__dirname, "../public")));

// --- ROUTES ---
app.use("/api", authRoutes);               // /api/login, /api/register
app.use("/api/operations", operationRoutes); // /api/operations/*
app.use("/api/query", queryRoutes);          // /api/query/*

// --- SERVER ---
app.listen(3000, (err) => {
    if (err) {
        console.error(err);
    } else {
        console.log("Server started on port 3000");
    }
});