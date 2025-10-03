"use strict"

async function fetchCounter() {
    try {
        const res = await fetch("http://localhost:3000/counter")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function fetchColor() {
    try {
        const res = await fetch("http://localhost:3000/colors")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function piu(){
    const res = await fetch("http://localhost:3000/increase", {
        method: "POST"
    })
    const data = await res.json()
    const p = document.getElementById("numero")
    p.innerHTML = data.counter
}

async function meno(){
    const res = await fetch("http://localhost:3000/decrease", {
        method: "POST"
    })
    const data = await res.json()
    const p = document.getElementById("numero")
    p.innerHTML = data.counter
}

async function cambio(){
    const data = await fetchColor()
    const header = document.querySelector("header")
    const footer = document.querySelector("footer")
    header.style.backgroundColor = data.background
    footer.style.backgroundColor = data.background
    header.style.color = data.text
    footer.style.color = data.text
}

async function addBlocchi() {
    const numero = await fetchCounter()
    const p = document.getElementById("numero")
    p.innerHTML = numero.counter
    const piua = document.getElementById("piu")
    const menoa = document.getElementById("meno")
    const cambioa = document.getElementById("cambio")
    piua.addEventListener("click", () => piu())
    menoa.addEventListener("click", () => meno())     
    cambioa.addEventListener("click", () => cambio())
}

addBlocchi()
