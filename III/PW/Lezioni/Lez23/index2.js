const express = require('express');
const app = express();
const cors = require('cors');


app.use(express.json());
app.use(cors());

app.post('/data', (req, res) => {
    res.json({
        status: 'success',
        message: 'Data received successfully',
        data: req.body
    });
} 
);

app.listen(3000, () => {
    console.log('Server is running on http://127.0.0.1:3000');
    }
);