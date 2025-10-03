"use script"
// Cart
let products = [
    { name: "produt1", price: 10 },
    { name: "product2", price: 20 },
    { name: "product3", price: 30 },
];
let cart = {
    items: [],
    total: 0,
    addItem: function(product)[
        this.items.push(product);
        this.total += product.price;
    ],
};