const express = require('express');
const products = require('./products');


const app = express();

app.use(express.json()); // Middleware per il parsing del JSON

app.use('/API/v1/products', products);

app.listen(3000, () => {
    console.log('Server is running on http://localhost:3000');
}
); 



