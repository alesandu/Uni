const express = require('express');
const router = express.Router();
const fs = require('fs');
const prodotti = JSON.parse(fs.readFileSync('data/products.json', 'utf8'));


router.get('/', (req, res) => {
    res.json({
        status: 'success',
        products: prodotti
    });
}
);

router.get('/:id', (req, res) => {
    const id = req.params.id;
    const wine = prodotti.find(el => el.id == id); //con === si fa deve fare il parse dell int

    if (!wine) {
        return res.status(404).json({
            status: 'fail',
            message: 'id non trovato'
        });
    }
    else {
        return res.json({
            status: 'success',
            product: wine
        });
    }
}
);

router.post('/', (req, res) => {
    const data = req.body;
    console.log(data);

    if (!data.name || !data.price) {
        return res.status(422).json({
            status: 'fail',
            message: 'name e price sono obbligatori'
        });
    }
    else{
        const newId = (prodotti[prodotti.length]-1).id + 1;
        data.id = newId;
        prodotti.push(data);
    }
    
    res.json({
        status: 'success',
        products: data
    });
}
);


module.exports = router;