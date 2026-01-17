(add-to-load-path "./")
;;(set! %load-compiled-path (cons "./" %load-compiled-path))

(define-module (newarch-utils)
  #:use-module (mtfa error-handler)
  #:use-module (mtfa utils)
  #:use-module (mtfa serializer)
  #:use-module (mtfa unordered-set)
  #:use-module (mtfa unordered-map)
  #:use-module (mtfa star-map)
  #:use-module (mtfa simple_db)
  #:use-module (mtfa eis)
  #:use-module (mtfa va)
  #:use-module (mtfa extset)
  #:use-module (mtfa umset)
  #:use-module (mtfa web)
  #:use-module (mtfa brg)
;;  #:use-module (mtfa nn)
  #:use-module (mtfa avl)
  #:use-module (mtfa eqt)
;;  #:use-module (mtfa opencv)
  #:use-module (pfds sets)
  #:use-module (gnutls)
  #:use-module (scheme kwargs)
  #:use-module (search basic)
  #:use-module (math primes)
  #:use-module (match-bind)
  #:use-module (graph topological-sort)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (rnrs enums)
  #:use-module ((rnrs io ports) #:prefix ioports::)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module ((srfi srfi-18) #:prefix srfi-18::) ;;thread e mutex
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41) ;;streams
  #:use-module (srfi srfi-42) ;;Eager Comprehensions
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-45)
  #:use-module (srfi srfi-60)
  #:use-module (srfi srfi-111) ;;Boxes
  #:use-module (srfi srfi-171)
  #:use-module (web uri)
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
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 string-fun)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  ;; (sxml simple)
  ;; (sxml ssax)
  ;; (sxml xpath)
  #:use-module (json)
  #:use-module (system syntax)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web client)
  ;;
  ;;
  ;; #:use-module (mtfa-jwt)
  #:use-module (client-lisp client)

  )

;;Init del generatore di numeri casuali
(mtfa-rand-seed (mtfa-micros))
;;
;;Le variabili di configurazione!!!
(eval-when (expand load compile eval)
  (mtfa-define-symbol-value-pairs-from-json "cfg" "back-end.json"))
;;
(mtfa-start-logger)
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
(define-syntax-public mtfa-make-unix-time
  (lambda (x)
    (syntax-case x (usec nsec sec)
      ((_ nsec)
       #'(let ((t (current-time)))
	 (+ (time-nanosecond t) (* 1000000000 (time-second t)))))
      ((_ usec)
       #'(let ((t (current-time)))
	 (+ (/ (time-nanosecond t) 1000) (* 1000000 (time-second t)))))
      ((_ sec)
       #'(time-second (current-time)))
      ((_)
       #'(time-second (current-time))))))
;;
(define-public glb::pattern${} (mtfa-compile-pattern "\\$\\{[^}]*\\}"))
;;
(define-public (GENERAL-JSON-ANSWER::error msg)
  `((success . #f)(message . ,msg)))
;;
(define-public (GENERAL-JSON-ANSWER::success msg data_name data)
  `((success . #t)(message . ,msg) (data . ((,data_name . ,data)))))
;;
;;va-key è la root delle API
(define*-public (mtfa-web-make-general-api-manager va-key #:key (default-manager '()) (format-error GENERAL-JSON-ANSWER::error) (auxiliary-info '()))
  ;;funzioni interne di gestione
  (define kv (mtfa-fs3-make)) ;;per la registrazione delle funzioni e per la loro chiamata
  (define (ManageRequest k v pbuf set-names)
    (mtfa-noerr #f
		(let ((evaluator (mtfa-fs3-get kv k)))
		  (if (nil? evaluator)
		      (begin
			(eis::GiveHTTPJSONAnswer (format-error (string-append "command " k " not found!"))))
		      (evaluator k v pbuf set-names auxiliary-info)))))
  (define (ManageRequests jparsed pbuf set-names)
    (let ((result #nil) (continue #t))
      (for-each
       (lambda (it)
	 (if continue
	     (begin
	       (set! result (ManageRequest (car it) (cdr it) pbuf set-names))
	       (when (nil? result) (set! continue #f)))
	     #f))
       jparsed)
      result))
  (define (Manage actionl pbuf)
    (mtfa-noerr
     (eis::GiveHTTPJSONAnswer(format-error "Expected a JSON request"))
     (let (
           (body #nil)
           (jparsed #nil)
           (current-method (mtfa-eis-get-current-method pbuf)))
       (cond
        ((string-ci=? "options" current-method)
         (eis::GiveAnswerAndCloseAll
          (string-append
           "HTTP/1.1 200 OK\r\n"
           "Access-Control-Allow-Origin: "
           (if (nil? (mtfa-eis-get-value-current_headers pbuf "Origin"))
               "*"
               (mtfa-eis-get-value-current_headers pbuf "Origin")) "\r\n"
	   "Access-Control-Allow-Methods: " "GET, POST\r\n"
	   "Access-Control-Allow-Headers: " "accept, content-type\r\n"
	   "\r\n")))
        ;;
        ;;POST, con json nei dati
        ((string-ci=? "post" current-method)
         (set! body  (mtfa-eis-get-current-body pbuf #t))
         (if (StringIsEmptyOrNil body)
             (eis::BaseLib::NeedMoreData)
             (begin
	       (if (null? default-manager)
		   (begin
		     (set! jparsed (json-string->scm body))
		     (if jparsed
			 (ManageRequests jparsed pbuf actionl)
			 (eis::GiveHTTPJSONAnswer (format-error "Expected a JSON request"))))
		   (default-manager body pbuf actionl auxiliary-info)))))
        ;;
        ;;GET (default), dati nella url come json
        (#t
         (set! body (mtfa-eis-get-current-pars pbuf))
	 (if (null? default-manager)
	     (begin
               (set! jparsed (json-string->scm (uri-decode body)))
               (if jparsed
		   (ManageRequests jparsed pbuf actionl)
		   (eis::GiveHTTPJSONAnswer (format-error "Expected a JSON request"))))
	     (default-manager body pbuf actionl auxiliary-info)))))))
  ;;
  ;;istanzia la chiamata e formula la funzione di gestione
  (eis::function-pointer-add va-key Manage)
  (lambda (cmd . params)
    (cond
     ((eqv? 'add-handler cmd)
      (mtfa-fs3-add kv (first params) (second params)))
     ((eqv? 'del-handler cmd)
      (mtfa-fs3-add kv (first params) #nil))
     ((eqv? 'list)
      (mtfa-fs3-get-all kv)
      )
     ;;per ora altri comandi non previsti!
     (#t (Show! "Command: " cmd " not handled!")))))
;;; ;
;; ;;La gestione dell'HMAC
;; ;;
;; ;; Function to generate HMAC for a given message
;; (define (hmac-sha256 key message)
;;   (mtfa-hmac-hs 3 key message))
;; ;;
;; (define (generate-hmac message secret-key)
;;   (call-with-input-string message
;;     (lambda (port)
;;       (let* ((hmac-key (string->utf8 secret-key))
;;              (message-bytes (get-bytevector-all port))
;;              (hmac (hmac-sha256 hmac-key message-bytes)))
;; 	hmac))))
;; ;;
;; ;; Function to verify HMAC
;; (define (verify-hmac message received-hmac secret-key))
;;   (string=? received-hmac (generate-hmac message secret-key)))

;; ;; Define a simple web server handler
;; (define-public (hmac-auth-handler request secret-key)
;;   (let ((message (assoc-ref request "message"))
;;         (received-hmac (assoc-ref request "hmac")))
;;     (Show! "Msg: " message "\nHMAC: " received-hmac)
;;     (if (and message received-hmac)
;;         (if (verify-hmac message received-hmac secret-key)
;;             "HMAC verification successful"
;;             "HMAC verification failed")
;;         "Missing message or HMAC")))
;; ;;
;; (define-public (http2alist body)
;;   ;;per prima cosa distinguo in base a &
;;   (let ((data (string-split (uri-decode body) #\&)))
;;     (Show! "Data:" data)
;;     (map (lambda (v)
;; 	   (let ((kv (string-split v #\=)))
;; 	     (cons (first kv) (second kv))))
;; 	 data)))
;; ;;

;;Una define-multiple
(define-syntax-public define-multiple
  (lambda (x)
    (syntax-case x ()
      ((_ (name value) rest ...)
       #'(begin
	   (define name value)
	   (define-multiple rest ...)))
      ((_ ((name value) expr ...) ((n1 v1) expr1 ... ) ...)
       #'(begin
	   (define (name value) expr ...)
	   (define-multiple ((n1 v1) expr1 ...) ...)
	   ))
      ((_) #t))))
;;
;;La struttura dat di indicizzazione delle API
;;Usiamo una mtfa-fs3-make, anche un hash andrebbe bene
(define-public getFunction (mtfa-fs3-make))
;;
(define-public (JSON-ANSWER::error msg)
  (eis::GiveHTTPJSONAnswer `((success . #f)(message . ,msg))))
;;
(define-public (JSON-ANSWER::success msg data)
  (eis::GiveHTTPJSONAnswer `((success . #t)(message . ,msg) ,data)))
;;
;;La verifica del JWT
(define-public (CheckJwtToken pbuf secret command-requested)
  (values #t "default-user")
)
  ; (mtfa-noerr
  ;  (values "default-user" "default-password")
  ;  (let ((auth (mtfa-eis-get-value-current-headers pbuf "authorization")))
  ;    (if (or (null? auth) (not (string? auth)) (string-null? auth) (< (string-length auth) 10))
	;  (values #f #f)
	;  (begin
	;    (set! auth (substring auth 7)) ;;toglie il Bearer!!!
	;    (set! auth (jwt-decode auth secret))
	;    ;; (Show! "CheckJwtToken: " auth)
	;    (if auth
	;        (begin ;;e qui verifico le credenziali
		 
	; 	 (let ((user-in (assoc-ref auth "funuf::username"))
	; 	       (password-in (mtfa-hash-hs 2 (assoc-ref auth "funuf::password"))))
	; 	   (Show! "(TO BE DONE) Checking if user " user-in " is authorized for the command " command-requested)
	; 	   (values (string=? password-in
	; 			     (assoc-ref
	; 			      (assoc-ref
	; 			       (json-string->scm
	; 				(assoc-ref
	; 				 (json-string->scm
	; 				  (mtfa-bc::GetKV cfg::requester cfg::BCCLASS::credenziali user-in))
	; 				 "value"))
	; 			       "value")
	; 			      "password"))
	; 		   user-in)))
	;        (values #f #f)))))))
;;
(define-syntax-rule (ifnot expr stat-yes stat-not)
  (if expr stat-no stat-yes))
