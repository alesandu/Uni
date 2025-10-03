Avvio del Server Web
Avviare il server con: npx http-server
• Il server sarà accessibile su http://localhost:8080

http-server
• Root
– Cartella dei file
• Pagina di default
– index.html
• CORS
• Cache

URL Assolute:
– Contengono il dominio completo e path.
<a href="http://www.miosito.com/about.html">Pagina About</a>
• URL Relative alla Pagina:
– Definite rispetto alla posizione del file attuale
<a href="about.html">Pagina About</a>
<a href="privacy/ privacy.html">Pagina About</a>
• URL Relative alla root:
– Partono dalla root del server e iniziano con /
<a href="/about.html">Pagina About</a>
<a href="/privacy/ privacy.html">Pagina About</a>