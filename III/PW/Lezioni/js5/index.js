const express = require('express')
const app = express()



app.get("/", (req, res)=>{
    res.send("Hello World")
})

const PORT = 8080
app.listen(PORT, (err)=>{
    console.log(`Server in ascolto sulla porta ${PORT}`)
})