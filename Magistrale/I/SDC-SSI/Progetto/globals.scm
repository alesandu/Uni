(add-to-load-path "./")

(define-module (globals)
  #:use-module  (mtfa error-handler)
  #:use-module  (mtfa utils)
  #:use-module  (mtfa serializer)
  #:use-module (mtfa unordered-set) ;;unordered set con chiavi (stringhe, numeri o sumimboli: tutto convertito in stringa). Persistente
  #:use-module (mtfa unordered-map) ;;unordered map con chiavi (stringhe) e valori (qualsiasi cosa). persistente
  #:use-module (mtfa star-map)      ;;Inserisce stringhe con o senza jolly, la stringa che definisce il jolly e il valore. Cerca le stringhe che matchano!
  #:use-module (mtfa simple_db)
  #:use-module (mtfa eis)
  ;;#:use-module (mtfa fsm)
  #:use-module (mtfa va)
  #:use-module (mtfa extset)  ;;gestisce insiemi i cui elementi sono stringhe! consente operazioni di clone, set, check, get all.... Definisce una macro che consente di creare "al volo" una sottoclasse le cui istanze condividono gli stessi elementi.
  #:use-module (mtfa umset)   ;;è una unordered map (non persistente) che ha stringhe come chiavi e ha insiemi di stringhe come valori. Ogni insert aggiunge all'insieme corrispondente. Definisce inoltre la mtfa-umap-list che consente di mappare liste come chiavi e qualsiasi valore come valore
  #:use-module (mtfa web)
  #:use-module (mtfa brg)
  #:use-module (mtfa avl)
  #:use-module (mtfa eqt)
  ;;
  #:use-module (pfds sets)
  ;;
  #:use-module (gnutls)
  ;;
  ;;La libreria guile lib
  #:use-module (scheme kwargs)
  #:use-module (search basic)
  #:use-module (math primes)
  #:use-module (match-bind)
  #:use-module (graph topological-sort)
  ;;
  ;;i moduli di guile
  ;;((rnrs records syntactic) #:prefix rnrs::)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  ;; ((rnrs io ports)
  ;;  #:select (string->bytevector bytevector->string)
  ;;  #:prefix ioports::)
  ;;
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module ((srfi srfi-18) #:prefix srfi-18::) ;;thread e mutex
  ;; date & time rinomina per avere un current time che non si sovrappone
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  ;;(srfi srfi-28)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-60)
  #:use-module (srfi srfi-111)  ;;Boxes
  #:use-module (srfi srfi-171)
  #:use-module (web uri)
  #:use-module (system foreign)
  ;;
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg string-peg)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 q)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 exceptions)
  ;;
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  ;; (sxml simple)
  ;; (sxml ssax)
  ;; (sxml xpath)
  #:use-module (json)
  #:use-module (system syntax)
  #:use-module (system foreign)
  ;;
  ;;
  #:use-module (web client)
  ;;
  )
;;
(define-syntax !  ;;esegue una serie di espressioni all'interno di un gestore degli errori. in caso di errore torna #f
  (syntax-rules ()
    ((_ exp ...)
     (mtfa-noerr #f exp ...))))
(export !)
;;
(define-public (mtfa-make-ymdhms)
  (let ((now (current-date)))
    (string-append
     (format #f "~4,'0d" (date-year now))
     (format #f "~2,'0d" (date-month now))
     (format #f "~2,'0d" (date-day now))
     (format #f "~2,'0d" (date-hour now))
     (format #f "~2,'0d" (date-minute now))
     (format #f "~2,'0d" (date-second now))
     )))
;;
(define-public (please-reset) (kill 0 SIGABRT))
;;

;;Legge tutte le variabili dal file di configurazione
(eval-when (expand load compile eval)
  (mtfa-define-symbol-value-pairs-from-json "cfg" "generatore.json"))
;;
(define-public (config-force-reload)
  (mtfa-define-symbol-value-pairs-from-json "cfg" "generatore.json"))
;;
;;All'avvio avvia il log
;; (mtfa-logging-run cfg::log-file-name)
;;Da mandare in uscita!! (mtfa-logging-shutdown)
;;
;;(mtfa-logging-shutdown)

;;Init del generatore di numeri casuali
(mtfa-rand-seed (mtfa-micros))
