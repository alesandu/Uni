"use strict"

async function fetchPeople() {
    try {
        const res = await fetch("http://localhost:3000/persone")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

function showAlert(person) {
    const text = "Nome: " + person.nome + " " + person.cognome + " Età: " + person.età
    alert(text)
}
async function showPeople() {
    const people = await fetchPeople()
    const ul = document.getElementById("persone")
    people.forEach(el => {
        const li = document.createElement("li")
        li.addEventListener("click", () => showAlert(el))
        li.innerHTML = el.nome + " " + el.cognome
        ul.appendChild(li)
    });
}

showPeople()

const updateColors = document.getElementById("bottone")
updateColors.addEventListener("click", () => {

    const body = document.querySelector("body")
    body.style.backgroundColor = "#313131"

})

const toggle = document.getElementById("hambu")
toggle.addEventListener("click", () => {
    const nav = document.getElementById("navbar")
    nav.classList.toggle("active")
})
