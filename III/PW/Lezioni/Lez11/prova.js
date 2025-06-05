let a = {"name": "pippo"};
a.saluta = function() {alert("Ciao pippo")};
a.saluta();

a.name = "pluto";
a.saluta = function() {alert("Ciao " + this.name)};
a.saluta();

(function() {
    let a = b = 5;
})();
console.log(b);