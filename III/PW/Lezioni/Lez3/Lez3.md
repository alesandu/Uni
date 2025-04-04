Lo scopo dell'HTML5 è quello di aggiungere significato e struttura al
contenuto
– Non è pensato per fornire istruzioni su come il contenuto deve essere
presentato
• Markup semantico: Scegliere l'elemento HTML che fornisce la
miglior descrizione del contenuto
– Es. il titolo più importante all'inizio del documento deve essere circondato
con tag h1, senza preoccuparsi di come viene visualizzato di default
• Tag obsoleti erano stati pensati in assenza di CSS per formattare il
documento

Esigenza: raggruppare logicamente
degli elementi
• Prima di HTML5:
– block division: <div>
– inline division: <span>
• HTML5
– header: <header>
– footer: <footer>
– main: <main>
– sezioni: <section>
– articoli: <article>
– navigazione: <nav>
– contenuto a parte: <aside>

![alt text](image.png)

Il tag <main> specifica il contenuto principale di un
documento.
– Il contenuto all'interno dell'elemento <main> dovrebbe essere univoco
per il documento.
– Ci può essere un solo elemento <main> in un documento.
– NON deve essere un discendente di un elemento <article>, <aside>,
<footer>, <header> o <nav>.

<aside>...</aside>
• Sono elementi correlati ma "marginali" rispetto al
contenuto
– Non li potevano chiamare sidebar perché "side" si riferisce alla
presentazione
• Uso tipico
– sondaggi
– citazioni
– informazioni aggiuntive
– annunci
– link
• non ha un aspetto di default

<nav> navigaton
• Sezione che permette la
navigazione nel sito
– tipicamente si mettono delle liste
nel <nav>
Nei siti responsive spesso viene
compressa per i dispositivi piccoli

Elemento <header>
• Si usa per materiale
introduttivo
• Due usi comuni
– header della pagina
– header di una sezione o articolo
• Se usato in un articolo
tipicamente contiene titolo,
autore, data, etc.

Elemento <footer>
• Contiene le info che vanno alla
fine di sezione logica
– autore, copyright
– documenti correlati
– link
• Usi comuni:
– fondo pagina
– fine article o section

Elemento <section>
• Sezioni tematiche
– possono contenere heading,
paragrafi
– tutti gli elementi che abbia senso
raggruppare
• Può essere usato in diversi
contesti
– da dividere le sezioni della pagina
– a dividere le sezioni di un articolo

Elemento <article>
• Porzioni di pagina auto-consistenti
– teoricamente riusabili in contesti differenti
– e.g. syndacation: porzioni a disposizione di altri siti
• Contesti operativi:
– blog
– giornali

Elemento <figure>
• Un'immagine e una didascalia possono essere
raggruppate in un elemento <figura>.
– L'elemento <img> definisce l'immagine, l'elemento <figcaption>
definisce la didascalia.

<figure>
  <img src="pic_trulli.jpg" alt="Trulli">
  <figcaption>Fig1. - Trulli, Puglia, Italy.</figcaption>
</figure>

HTML Headings
• Sei livelli di "intestazione" o titolo
– h1, h2 , h3, h4, h5 , h6
• Creano l'indice della pagina (outline)
– come in word
• Sono cercati dagli algoritmi dei motori di ricerca nelle
pagine
• Best practice:
– mantenere un ordine logico come negli indici

head vs header vs h1,h2,..., h6
• <header>
– elemento strutturale che identifica un segmento di pagina
– posto all'interno del body o di u suo sotto-elemento
• <head>
– direttamente incluso nel tag html
– non mostrato dal browser
– contiene i metadati
• <h1>,<h2>,...,<h6>
– identificano i titoli testuali della pagina

Paragrafi <p></p>
• Servono a identificare i paragrafi
• Possono contente al loro interno:
– testo,
– immagini,
– inline elements (phrasing content)
• Se non chiudiamo il tag il browser lo può chiudere in
automatico

Elemento <pre>
    • Testo preformattato – Mostrato con gli spazi e gli accapo presenti
    – Sempre il carattere monospace

Elemento <blockquote>
    • Citazione

<hr>
• paragraph-level thematic break
– è un modo per dividere i paragrafi
• Normalmente visualizzato come una riga con un po' di
margine sopra e sotto
– non dovrebbe essere usato per fare righe ma per il suo
significato semantico

Liste
Tre tipi di liste:
• Unordered lists
– liste non ordinate
• Ordered lists
– liste ordinate
• Liste descrittive
– coppie nome-descrizione
La lista nel suo complesso ed i suoi elementi sono mostrati
(di default) come block element

la unordered list è contenuta tra <ul>...</ul>
• le voci della lista (list item) sono contenute tra <li>...</li>
– in automatico viene aggiunto un pallino
– è normale cambiare l'aspetto delle liste con il css per fare menu

la ordered list è contenuta tra <ol>...</ol>
• le voci della lista (list item) sono contenute tra <li>...</li>
– il browser le numera
– è possibile cambiare il tipo di numerazione
• lettere maiuscole/minuscole, numeri romani
– è possibile specificare il numero iniziale
• <ol start="50">...</ol>

Liste annidate
All'interno di una voce della lista
posso inserire un'altra lista
• Per ogni livello di annidamento delle
liste non ordinate viene cambiato lo
stile della lista

Liste descrittive
la description list è contenuta tra <dl>...</dl>
le voci della lista sono composte da due elementi
• elemento "termine" tra <dt>...</dt>
• elemento "descrizione" tra <dd>...</dd>
• Nell'elemento <dl> ci possono essere solo <dt> e <dd>
• Nell'elemento <dt> non ci possono essere gruoping elements (h1, h2 ..., p, etc)
• Nell'elemento <dd> ci può essere qualunque cosa

Elementi <em>, <strong>
• em: testo enfatizzato
– normalmente mostrato in corsivo
• strong: testo fortemente enfatizzato
– normalmente mostrato in grassetto

Line Breaks <br>
• interruzione di linea nel mezzo di un elemento Nota: non usare più di due <br> consecutivi

Elemento <q>
• testo citato alcuni browser inseriscono gli apici

Elemento <code>
• codice di un programma non è preformattato
– è mostrato in monospace

Spazi e new-line
• Non possiamo variare l'output aggiungendo spazi o
accapo
– il browser li rimuove
• Ogni numero di spazi o di accapo valgono come uno
spazio

Block and inline elements
• Block element
– Dopo alcuni elementi il browser inserisce un accapo e spazio
intorno
– <h1></h1>, <h2></h2>, <p></p>
• Inline element
– elementi che non iniziano in una nuova linea ma rimangono nel
flusso del paragrafo
– em, strong, etc.

Commenti <!-- … -->
• testo non mostrato all'utente e non interpretato dal
browser

Caratteri Unicode
• Alcuni caratteri devono essere sostituiti
– perché non ASCII o perché usati nei tag HTML
• La sequenza di escape inizia con & e si chiude con un ;
– Es: &copy; &#169;
• Sono definiti per ogni carattere un identificatore oppure
un numero

Immagini <img>
• <img src="happyface.gif" alt="happy">
• Due attributi
– src: la URL dell'immagina
– alt: testo alternativo

Elementi ancora
<a href="link">testo</a>
• Esempio
• Il contenuto dell'elemento diventa attivo
– il browser cambia il cursore

Attributo href
• Specifica la URL di un documento nel web
– Va scritto fra doppi apici
Due modi per specificare le URL
• URL assolute
– iniziano con http://
– href="http://www.oreilly.com/"
• URL relative
– alla URL del documento corrente
– href="recipes/index.html"
– href="spoon.gif" 

Indirizzi delle pagine web: URL
• Ogni pagina web e ogni risorsa ha un indirizzo
• Questo si chiama URL (Uniform Resource Locator)

le URL sono fatte di tre parti – Il protocollo
– Il nome del server
– Il path della risorsa

File di Default
• alcune URL non includono il nome del file
– indicano solo una directory
– www.uniroma2.it/
– ppl.eln.uniroma2.it/pw/
• Se una richiesta non presenta esplicitamente il file il server web cerca il file
index.html
– www.uniroma2.it/index.html
– ppl.eln.uniroma2.it/pw/index.html
• Il nome del file di default può essere configurato e può variare da server a server
– index.html
– index.htm
– default.html
– index.php
– index.asp

Link esterni
Riferimento: server della pagina corrente
• La risorsa è in un server web diverso Devo usare URL assolute
– è su un server web diverso (normalmente su un a macchina diversa)

Link interni
• Link a una risorsa nello stesso server web
– spesso dello stesso sito
• Posso omettere il protocollo ed il nome del server
• Devo indicare il path del file
– Il path segue la convenzione unix con cartelle separate da /
• Due tipi di path
– Path relativi al path corrente
– Path assoluti
• relativi alla root del server
• quello che comparirebbe nella url assoluta dopo il nome del server

W3C HTML Validator
• validator.w3.org
• controlla il codice html e ne valida la sintassi
• il browser maschera molti errori