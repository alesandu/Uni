"use strict"

async function fetchBlocco() {
    try {
        const res = await fetch("http://localhost:3000/articles")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

function shuffle(array) {
    array.sort(() => Math.random() - 0.5);
    const ul = document.getElementById("arti")
    ul.innerHTML = ""
    array.forEach((element) => {
        const section = document.createElement("section")
        section.innerHTML = "<h4>" + element.titolo + "</h4><p>" + element.contenuto + "</p>";
        ul.appendChild(section)
    },
);
}

async function addBlocchi() {
    const articoli = await fetchBlocco()
    const scritta = document.getElementById("articoli")
    const ul = document.getElementById("arti")
    scritta.addEventListener("click", () => shuffle(articoli))     
    articoli.forEach((element) => {
        const section = document.createElement("section")
        section.innerHTML = "<h4>" + element.titolo + "</h4><p>" + element.contenuto + "</p>";
        ul.appendChild(section)
    },
);
}

function menu() {
	const scritta = document.getElementById("pmenu");
	const menu = document.getElementById("lmenu");

	scritta.addEventListener("click", () => {
		if (window.innerWidth < 1000) {
			menu.classList.toggle("active");

			if (scritta.style.color == "black") {
				scritta.style.color = "red";
			} else {
				scritta.style.color = "black";
			}
		}
	});
}

addBlocchi()
menu()