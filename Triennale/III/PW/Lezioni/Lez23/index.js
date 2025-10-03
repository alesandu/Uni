const express = require('express');
const app = express();
const bodyparser = require('body-parser');

app.use(express.static('public'));
app.use(bodyparser.urlencoded());

app.get('/elabData', (req, res) => {
    const data = req.query;
    let {email,nome} = req.query;
    console.log(data);
    res.send(`<h1>email ricevuti ${email}</h1>`);
} 
);

app.post('/elabData', (req, res) => {
    const data = req.query;
    let {email,nome} = req.query;
    console.log(data);
    res.send(`<h1>POST</h1>`);
} 
);


app.listen(8080, () => {
    console.log('Server is running on http://localhost:8080');
    }
);