(add-to-load-path "./")
;;
;;(set! %load-compiled-path (cons "./" %load-compiled-path))
;;

(use-modules
 (mtfa error-handler)
 (mtfa utils)
 (mtfa serializer)
 (mtfa unordered-set) ;;unordered set con chiavi (stringhe, numeri o sumimboli: tutto convertito in stringa). Persistente
 (mtfa unordered-map) ;;unordered map con chiavi (stringhe) e valori (qualsiasi cosa). persistente
 (mtfa star-map) ;;Inserisce stringhe con o senza jolly, la stringa che definisce il jolly e il valore. Cerca le stringhe che matchano!
 (mtfa simple_db)
 (mtfa eis)
 (mtfa va)
 (mtfa extset)
 (mtfa umset)
 (mtfa web)
 (mtfa brg)
 ;;(mtfa nn)
 (mtfa avl)
 (mtfa eqt)
 ;;(mtfa opencv)
 ;;
 (pfds sets)
 (gnutls)
 ;;
 ;;La libreria guile lib
 (scheme kwargs)
 (search basic)
 (math primes)
 (match-bind)
 (graph topological-sort)
 ;;
 ;;i moduli di guile
 ;;((rnrs records syntactic) #:prefix rnrs::)
 (rnrs bytevectors)
 (rnrs arithmetic bitwise)
 (rnrs enums)
 ((rnrs io ports)
  #:prefix ioports::)
 ;;((rnrs) :version (6))
 ;;
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-11)
 ((srfi srfi-18)
  #:prefix srfi-18::) ;;thread e mutex
 ;; date & time rinomina per avere un current time che non si sovrappone
 (srfi srfi-19)
 (srfi srfi-26)
 ;;(srfi srfi-28)
 (srfi srfi-41) ;;streams
 (srfi srfi-42) ;;Eager Comprehensions
 (srfi srfi-43)
 (srfi srfi-45)
 (srfi srfi-60)
 (srfi srfi-111) ;;Boxes
 (srfi srfi-171)
 ;;
 (web uri)
 ;;
 (ice-9 format)
 (ice-9 ftw)
 (ice-9 rdelim)
 (ice-9 pretty-print)
 (ice-9 regex)
 (ice-9 iconv)
 (ice-9 string-fun)
 (ice-9 peg)
 (ice-9 peg string-peg)
 (ice-9 vlist)
 (ice-9 q)
 (ice-9 binary-ports)
 (ice-9 textual-ports)
 (ice-9 threads)
 (ice-9 hash-table)
 (ice-9 control)
 (ice-9 match)
 (ice-9 receive)
 (ice-9 eval-string)
 (ice-9 textual-ports)
 (ice-9 arrays)
 (ice-9 popen)
 (ice-9 exceptions)
 (ice-9 optargs)
 (ice-9 string-fun)

 ;;
 (oop goops)
 (oop goops describe)
 ;; (sxml simple)
 ;; (sxml ssax)
 ;; (sxml xpath)
 (json)
 (system syntax)
 (system foreign)
 (system foreign-library)

 ;;
 ;;(fibers web server)
 (web server)
 (web request)
 (web response)
 (web uri)
 ;;
 (web client)
 ;;
 (newarch-utils)
;; (newarch-schema-manager)
 (client-lisp client)
;; (postgresql)
;; (mtfa-jwt)
 )
;;
;;Recursively find a key and changes its value
;;i tipi json sono
;;
;; string string
;; number	number
;; object	alist
;; array	vector
;; true	#t
;; false	#f
;; null	'null
;;
;;Per imparare!!!
;; (define (BrowseJsonData json)
;;   (cond
;;    ((number? json) json)
;;    ((string? json) json)
;;    ((boolean? json) json)
;;    ((eqv? 'null json) json)
;;    ((vector? json) (vector-map (lambda (i v)
;; 				 (BrowseJsonData v)) json))
;;    ((pair? json) (map (lambda (x) (cons (car x) (BrowseJsonData (cdr x)))) json))))
;;
; (define* (BrowseJsonDataAndUpdate json fun key . keys)
;   (let loop ((k (cons key keys)) (found '()) (json json))
;     (cond
;      ((or (eqv? 'null json) (boolean? json) (string? json) (number? json))
;       (if (equal? k found) (fun json) json))
;      ((vector? json) (vector-map (lambda (i v)
; 				   (loop k found v)) json))
;      ((pair? json)
;       (map (lambda (x) (cons (car x) (loop k (append found `(,(car x))) (cdr x)))) json)))))
;;
;;collegamento alla blockchain, eseguito a tempo di call delle API
(define-public ConnectedToTheBlockchain #f)
(define-public (ConnectToTheBlockchain)
  (mtfa-noerr
   #f
   (if ConnectedToTheBlockchain
       #t
       (let ((connected (json-string->scm (mtfa-bc::Connect cfg::requester cfg::org-to-connect cfg::channel-to-connect cfg::domain-to-connect))))
	 (when (assoc-ref connected "result")
	   (set! ConnectedToTheBlockchain #t))
	 ConnectedToTheBlockchain))))
;;
;;
;;Esportiamo le api basiche
;;per prima cosa scriviamo il corpo delle funzioni e poi le indicizziamo
(define (AddKV json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "AddKV")
	   (ifnot authorized
		  (JSON-ANSWER::error "Not authorized")
		  (let* ((class (assoc-ref json "class"))
			 (key (assoc-ref json "key"))
			 (value (assoc-ref json "value"))
			 (answer (mtfa-bc::AddKV username class key value)))
		    (JSON-ANSWER::success "Ok" `("answer" . ,answer)))))))
;;
(define (GetKV json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetKV")
	   (ifnot authorized
		  (JSON-ANSWER::error "Not authorized")
		  (let* ((class (assoc-ref json "class"))
			 (key (assoc-ref json "key"))
			 (answer (mtfa-bc::GetKV username class key)))
		    (JSON-ANSWER::success "Ok" `("answer" . ,answer)))))))
;;
(define (GetKeyHistory json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetKeyHistory")
	   (ifnot authorized
		  (JSON-ANSWER::error "Not authorized")
		  (let* ((class (assoc-ref json "class"))
			 (key (assoc-ref json "key"))
			 (answer (mtfa-bc::GetKeyHistory username class key)))
		    (JSON-ANSWER::success "Ok" `("answer" . ,answer)))))))
;;
(define (DelKV json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "DelKV")
	     (ifnot authorized
		    (JSON-ANSWER::error "Not authorized")
		    (let* ((class (assoc-ref json "class"))
			   (key (assoc-ref json "key"))
			   (answer (json-string->scm (mtfa-bc::DelKV username class key))))
		      (JSON-ANSWER::success "Ok" `("answer" . ,answer)))))))
;;
(define (GetClasses json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetClasses")
	     (ifnot authorized
		    (JSON-ANSWER::error "Not authorized")
		    (JSON-ANSWER::success "Ok" `("answer" . ,(json-string->scm (mtfa-bc::GetClasses username))))))))
;;
(define (GetNumKeys json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetNumKeys")
	     (ifnot authorized
		    (JSON-ANSWER::error "Not authorized")
		    (let* ((class (assoc-ref json "class"))
			   (key (assoc-ref json "key"))
			   (answer (json-string->scm (mtfa-bc::GetNumKeys username class key))))
		      (JSON-ANSWER::success "Ok" `("answer" . ,answer)))))))
;;
(define* (GetKeys json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetKeys")
	     (ifnot authorized
		    (JSON-ANSWER::error "Not authorized")
		    (let ((class (assoc-ref json "class"))
			  (key (assoc-ref json "key"))
			  (howmany (assoc-ref json "howmany"))
			  (value (assoc-ref json "value")))
		      (unless howmany (set! howmany 10000))
		      (unless value (set! value ""))
		      (JSON-ANSWER::success "Ok" `("answer" . ,(mtfa-bc::GetKeys username class key howmany value))))))))
;;
(define (GetTxData json pbuf actionl auxiliary-info)
  (ifnot (ConnectToTheBlockchain)
	 (JSON-ANSWER::error "Not connected to the blockchain")
	 (receive (authorized username) (CheckJwtToken pbuf cfg::FUNUF::secret "GetTxData")
	     (ifnot authorized
		    (JSON-ANSWER::error "Not authorized")
		    (let ((txid (assoc-ref json "txid")))
		      (JSON-ANSWER::success "Ok" `("answer" . ,(json-string->scm (mtfa-bc::GetTxData username txid cfg::channel-to-connect)))))))))
;;
;;e qui la struttura di ricerca
(mtfa-fs3-add getFunction "AddKV"         AddKV)
(mtfa-fs3-add getFunction "GetKV"         GetKV)
(mtfa-fs3-add getFunction "GetKeyHistory" GetKeyHistory)
(mtfa-fs3-add getFunction "DelKV"         DelKV)
(mtfa-fs3-add getFunction "GetClasses"    GetClasses)
(mtfa-fs3-add getFunction "GetNumKeys"    GetNumKeys)
(mtfa-fs3-add getFunction "GetKeys"       GetKeys)
(mtfa-fs3-add getFunction "GetTxData"     GetTxData)
;;
;;Sono le singole funzioni a verificare l'autorizzazione!!!
;;Dovrebbe essere il livello ARS ma mi manca il JWT a livello ARS
(define (GestioneAPIBackEnd json-body pbuf actionl auxiliary-info)
  ;;(Show! "Got: " json-body "=>" (json-string->scm json-body))
  (let* ((json-body (json-string->scm json-body)) ;;Converte in json
	 (cmd (assoc-ref json-body "cmd"))
	 (fun (mtfa-fs3-get getFunction cmd)))
    ;;(Show! "Fun: " fun)
    (if (null? fun)
	(JSON-ANSWER::error (string-append "Command: " cmd " not found"))
	(let ((ans (fun json-body pbuf actionl auxiliary-info)))
	  ans))))

(define api-manager (mtfa-web-make-general-api-manager "API" #:default-manager GestioneAPIBackEnd))
;;
;;E qui inizializziamo il sistema
;;Ma lo faccio solo se sono nell'ambiente target (dentro docker)
;;quindi se la cartella corrente è /vapps e se il nome del processo è va
;;se command line non è ("guile")!!
;; (when (and (not (null? (command-line))) (not (equal? (car (command-line)) "guile")))
(Show! "Connecting...")
(ConnectToTheBlockchain)
(Show! "Connected!")
;;by default per ora inserisco nella blockchain utenti e pwd accettati
;;Se non ci sono e se abilitato, li inserisce
;;Elenco dei servizi basici erogati
'("AddKV" "GetKV" "GetKeyHistory" "DelKV" "GetClasses" "GetNumKeys" "GetKeys" "GetTxData")
;;Elenco dei servizi di repository erogti
'("GeneralizedSelect" "GetValidation" "GetDocument" "AddSchema" "DropSchema" "GetSchema" "ListSchemi" "DoQuery" "RegQuery" schema-name ":" tname ":" "insert"
 )
