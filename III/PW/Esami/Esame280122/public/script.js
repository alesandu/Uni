"use strict"
window.onload = async function(){
    
    async function fetchBlocco() {
        try {
            const res = await fetch("http://localhost:3000/data")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    function converti(data) {
        const main = document.getElementById("spesa")
        const ul = document.createElement("ul")
        main.innerHTML = ""
        data.forEach(element => {
            const li = document.createElement("li")
            li.innerHTML = element.prodotto + " " + element.desc
            ul.appendChild(li)
        });
        main.appendChild(ul)
    }

    async function addBlocchi() {
        const data = await fetchBlocco()
        const bottone = document.getElementById("bottone")
        const div = document.getElementById("lista")
        bottone.addEventListener("click", () => converti(data))     
        const pre = document.createElement("pre")
        pre.innerHTML = JSON.stringify(data, null, 2)
        div.appendChild(pre)
    }

    addBlocchi()

}