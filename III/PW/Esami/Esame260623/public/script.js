"use strict"

async function fetchBlocco() {
    try {
        const res = await fetch("http://localhost:3000/list")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function showBanner(el) {
    const res = await fetch("http://localhost:3000/pics/" + el)
    const data = await res.json()
    const div = document.createElement("div")
    div.innerHTML = data.desc
    const body = document.querySelector("body")    
    div.className = "banner"
    body.appendChild(div)    
}

async function showBlocco(n) {
    const blocco = await fetchBlocco()
    const ul = document.getElementById("blocchi")

    for (let index = n; index < n+4; index++) {
        const li = document.createElement("li")
        const a = document.createElement("a")
        a.addEventListener("click", () => showBanner(blocco[index]))
        a.innerHTML = blocco[index]
        li.appendChild(a)
        ul.appendChild(li)
    }
}

const updateBlocchi = document.getElementById("bottone")
updateBlocchi.addEventListener("click", () => {
    showBlocco(4)
})

showBlocco(0)