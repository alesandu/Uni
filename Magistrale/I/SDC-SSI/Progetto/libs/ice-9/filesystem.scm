;; Copyright (C) 2020, 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 filesystem)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-71)
  #:autoload (web uri) (uri?
                        uri-scheme uri-path
                        build-uri uri-decode
                        encode-and-join-uri-path)
  #:export (file-name-stem
            file-name-extension

            file-basename
            file-dirname
            file-parent
            file-child
            file-ancestors
            file-name-split
            file-name-join
            file-name-join*
            file-name-absolute?
            normalize-file-name
            canonicalize-file-name
            expand-file-name
            relative-file-name
            rightmost-absolute-file-name
            file-name-equivalent?

            make-directories
            delete-file-recursively
            mkftemp!
            mkdtemp!

            file-is-regular?
            file-is-symlink?
            file-is-other?
            file-is-block-device?
            file-is-character-device?
            file-is-fifo?

            mkfifo

            file-name->uri
            uri->file-name)

  #:replace (file-is-directory?))

(define (%file-name-parts+root file-name)
  (let ((idx (string-index file-name file-name-separator?)))
    (values (let loop ((parts '())
                       (idx (string-index file-name file-name-separator?
                                          (if (absolute-file-name? file-name)
                                              (1+ idx)
                                              0))))
              (if idx
                  (loop (cons idx parts)
                        (string-index file-name file-name-separator? (1+ idx)))
                  (cons (string-length file-name) parts)))
            (if (absolute-file-name? file-name) idx #f))))

(define (%file-name-root file-name)
  (and (absolute-file-name? file-name)
       (substring file-name 0 (1+ (string-index file-name file-name-separator?)))))

(define (file-name-stem file-name)
  "Return the basename of @var{file-name} sans file extensions."
  (let ((basename (basename file-name)))
    (match basename
      ("." ".")
      (".." "..")
      (_ (substring basename 0 (or (string-index basename #\. 1)
                                   (string-length basename)))))))

(define (file-name-extension file-name)
  "Return the file extensions of @var{file-name}."
  (let ((basename (basename file-name)))
    (match basename
      ("." "")
      (".." "")
      (_ (substring basename (or (string-index basename #\. 1)
                                 (string-length basename)))))))

(define* (file-basename file-name #:optional (n 0))
  "Return the basename of @var{file-name} or optionally its @var{n}th parent."
  (cond
   ((< n 0) (scm-error 'out-of-range "file-basename"
                       "expecting positive integer, got ~A."
                       (list n) n))
   ((= n 0) (basename file-name))
   (else
    (let ((parts root (%file-name-parts+root file-name)))
      (cond
       ((> n (length parts)) (scm-error 'out-of-range "file-basename"
                                             "not enough parent directories."
                                             '() n))
       ((= n (length parts))
        (if root (substring file-name 0 (1+ root)) "."))
       (else
        (let ((tail (list-tail parts n)))
          (if (pair? (cdr tail))
              (substring file-name (1+ (cadr tail)) (car tail))
              (substring file-name (if root (1+ root) 0) (car tail))))))))))

(define* (file-dirname file-name #:optional (n 0))
  (let ((parts root (%file-name-parts+root file-name)))
    (cond
     ((> (1+ n) (length parts))
      (scm-error 'out-of-range "file-dirname"
                 "not enough parent directories."
                 '() n))
     ((= (1+ n) (length parts))
      (if root (substring file-name 0 (1+ root)) "."))
     (else
      (substring file-name 0 (list-ref parts (1+ n)))))))

(define (file-parent file-name)
  "Return the parent directory of @var{file-name}.
This is equivalent to @code{(file-dirname file-name 0)}."
  (file-dirname file-name 0))

(define (file-child directory child)
  "Return the child named @var{child} of @var{directory}."
  (when (string-index child file-name-separator?)
    (scm-error 'out-of-range "file-child"  "expected a basename, got ~s"
               (list child) child))

  (cond
   ((string-null? directory)
    (string-append "." file-name-separator-string child))
   ((equal? (1- (string-length directory))
            (string-rindex directory file-name-separator?))
    (string-append directory child))
   (else
    (string-append directory file-name-separator-string child))))

(define (%file-ancestors file-name)
  (let ((parts root (%file-name-parts+root file-name)))
    (let loop ((index (car parts))
               (rest (cdr parts))
               (ancestors '()))
      (if (pair? rest)
          (loop (car rest)
                (cdr rest)
                (cons (substring file-name 0 index) ancestors))
          (values (cons (substring file-name 0 index) ancestors)
                  root)))))

(define (file-ancestors file-name)
  "Return the ancestors of @var{file-name}, i.e. @var{file-name}, its parent,
its parent's parent, etc. up until the start of the string, in reverse order.
To get meaningful results, it is a good idea to first form an absolute file
name without @code{.} and @code{..} parts, e.g. using
@code{canonicalize-file-name}."
  (let ((ancestors root (%file-ancestors file-name)))
    (if (and root (> (string-length file-name) (1+ root)))
        (cons (substring file-name 0 (1+ root)) ancestors)
        ancestors)))

(define (%file-name-split file-name)
  (let ((parts root (%file-name-parts+root file-name)))
    (let loop ((index (car parts))
               (rest (cdr parts))
               (split '()))
      (if (pair? rest)
          (loop (car rest)
                (cdr rest)
                (cons (substring file-name (1+ (car rest)) index) split))
          (values (cons (substring file-name (if root (1+ root) 0) index) split)
                  root)))))

(define (file-name-split file-name)
  "Split @var{file-name} into its parts separated by
@var{file-name-separator?}."
  (let ((split root (%file-name-split file-name)))
    (if root (cons (substring file-name 0 root) split) split)))

(define (file-name-join parts)
  (string-join parts file-name-separator-string))

(define (file-name-join* . parts)
  "Like @code{file-name-join}, but taking @var{parts} as one string per
argument."
  (file-name-join parts))

(define file-name-absolute? absolute-file-name?)

(define (%normalize-file-name parts root)
  (let loop ((result (if root '() '(".")))
             (leading-dots 0)
             (parts parts))
    (match (cons* result parts)
      ;; we have a result
      ((()) (list root ""))
      ((("."))
       (map (const "..") (iota leading-dots)))
      ((result)
       (append (if root (list root) '())
               (map (const "..") (iota leading-dots))
               (reverse result)))
      ;; handling empty dirnames and "."
      ((result "" . rest)
       (loop result leading-dots rest))
      ((result "." . rest)
       (loop result leading-dots rest))
      ;; handling ".."
      ((() ".." . rest)
       (loop result 0 rest))
      (((".") ".." . rest)
       (loop result (1+ leading-dots) rest))
      ((result ".." . rest)
       (loop (cond ((pair? (cdr result)) (cdr result))
                   (root '())
                   (else '(".")))
             leading-dots rest))
      ;; handling directories
      (((".") dir . rest)
       (loop (list dir) leading-dots rest))
      ((result dir . rest)
       (loop (cons dir result) leading-dots rest)))))

(define (normalize-file-name file-name)
  "\"Normalize\" @var{file-name}.
A normalized file name has no @code{.} parts and no repeated path
separators, but it may have a number of leading @code{..} parts and it
need not be an absolute file name.  Use @code{canonicalize-file-name} instead,
if you wish to get rid of all @code{..} parts and obtain an absolute path."
  (file-name-join
   (let ((parts root (%file-name-split file-name)))
     (%normalize-file-name parts
                           (and root (substring file-name 0 root))))))

(define* (canonicalize-file-name file-name #:optional weakly?)
  "Canonicalize @var{file-name} just like @code{canonicalize-path} and return
it as file name.
If @var{weakly?} is true, weakly canonicalize it, that is only resolve symlinks
as far as ancestors exist."
  (cond
   ((not weakly?) (canonicalize-path file-name))
   ((file-exists? file-name) (canonicalize-path file-name))
   (else
    (let ((file-name (expand-file-name file-name #f #t)))
      (let loop ((path (%file-name-root file-name))
                 (ancestors (file-ancestors file-name))
                 (parts (file-name-split file-name)))
        (match ancestors
          (((? file-exists? a) . as)
           (loop a as (cdr parts)))
          (else
           (file-name-join
            (cons (canonicalize-path path) parts)))))))))

(define* (expand-file-name file-name #:optional directory
                           (allow-path-traversal? #f))
  "Expand @var{file-name} in @var{directory}.
If @var{directory} is @code{#f}, use the current directory as returned by
@code{getcwd}.
If @var{allow-path-traversal?} is @code{#f} (the default), @var{file-name} must
be a file name relative to @var{directory}.  Otherwise, absolute file names and
file names with leading dots are also allowed."
  (if (file-name-absolute? file-name)
      (if allow-path-traversal? (canonicalize-file-name file-name)
          (error "No path traversal allowed."))
      (let ((normalized (%normalize-file-name (file-name-split file-name) #f)))
        (and (not allow-path-traversal?)
             (string=? ".." (car normalized))
             (error "No path traversal allowed."))
        (let* ((directory (or directory (getcwd)))
               (directory-parts root (%file-name-split directory)))
         (file-name-join
          (%normalize-file-name
           (append directory-parts normalized)
           (and root (substring directory 0 root))))))))

(define* (relative-file-name file-name #:optional (base (getcwd)))
  "Return @var{file-name}'s relative path w.r.t. @var{base}."
  (let* ((base-parts base-root (%file-name-split base))
         (file-parts file-root (%file-name-split file-name))
         (base-root (and base-root (substring base 0 base-root)))
         (file-root (and file-root (substring file-name 0 file-root))))
    (match (list base-root file-root)
      ((root root) *unspecified*)
      ((#f #f) *unspecified*))
    (let loop ((b (%normalize-file-name base-parts base-root))
               (f (%normalize-file-name file-parts file-root)))
      (match (list b f)
        ((() ()) ".")
        ((() rest) (file-name-join rest))
        ((rest ()) (file-name-join (map (const "..") rest)))
        (((dir . _) (dir . _)) (loop (cdr b) (cdr f)))
        ((dir1 dir2)
         (file-name-join
          (append (map (const "..") dir1) dir2)))))))

(define (rightmost-absolute-file-name file-name)
  "Return the rightmost substring in @var{file-name} that qualifies as an
absolute name, or @code{#f} if no such substring exists."
  (let ((parts root (%file-name-parts+root file-name)))
    (let loop ((parts (cdr parts)))
      (match parts
        (() (and root file-name))
        ((idx . rest)
         (if (absolute-file-name? (substring file-name (1+ idx)))
             (substring file-name (1+ idx))
             (loop rest)))))))

(define* (file-name-equivalent? a b #:optional (stat stat))
  "Return @code{#t} if @var{a} and @var{b} resolve to the same file as
determined by @var{stat}."
  (let ((a (stat a))
        (b (stat b)))
    (and (= (stat:dev a) (stat:dev b))
         (= (stat:ino a) (stat:ino b)))))

(define (make-directories directory)
  "Make @var{directory} and all of its parents."
  (for-each
   (lambda (dir)
     (catch 'system-error
       (lambda ()
         (mkdir dir)
         #t)
       (lambda args
         (if (= EEXIST (system-error-errno args))
             #t
             (apply throw args)))))
   (file-ancestors directory)))

(define (delete-file-recursively file)
  "Delete @var{file} and all of its children."
  (file-system-fold
   (const #t)
   (lambda (leaf stat res) (delete-file leaf))
   (const *unspecified*)
   (lambda (up stat res) (rmdir up))
   (const *unspecified*)
   (const *unspecified*)
   *unspecified*
   file))

(define %random-chars
  (char-set->string
   (char-set-intersection char-set:ascii char-set:letter+digit)))

(define (%try-tempname tmpl proc)
  (let ((suffix (substring/shared tmpl (- (string-length tmpl) 6)))
        (random-bits (logxor (random #xffffffffffffffff) (getpid))))
    (unless (string=? suffix "XXXXXX")
      (error "template must end in XXXXXX"))
    (let loop ((attempts 0)
               (v random-bits))
      (define (v->letter! string i)
        (let ((q r (floor/ v (string-length %random-chars))))
          (set! v q)
          (string-set! string i (string-ref %random-chars r))))
      (when (>= attempts (* 62 62 62))
        (scm-error 'system-error #f "exhausted tempnames" '() EEXIST))
      (v->letter! suffix 0)
      (v->letter! suffix 1)
      (v->letter! suffix 2)
      (v->letter! suffix 3)
      (v->letter! suffix 4)
      (v->letter! suffix 5)
      (catch 'system-error
        (lambda ()
          (proc tmpl))
        (lambda args
          (if (= EEXIST (system-error-errno args))
              (loop (1+ attempts) (+ v 7777))
              (apply throw args)))))))

(define* (mkftemp! tmpl #:optional (mode "w+"))
  "Pure Guile implementation of @code{mkstemp!}.
Since it depends on Guile's internal randomness, you should set up
@code{*random-bits*} before calling this function."
  (%try-tempname tmpl (lambda (f) (open-file f mode))))

(define* (mkdtemp! tmpl #:optional (mode #o700))
  "Create a new unique directory in the file system.

@var{tmpl} is a string specifying where the file should be created: it
must end with @code{XXXXXX} and those @code{X}s will be changed in the string
to return the name of the directory (the name will also be returned).

Since it depends on Guile's internal randomness, you should set up
@code{*random-bits*} before calling this function."
  (%try-tempname tmpl (lambda (d) (mkdir d mode) d)))

(define (stat:type=? st type)
  (eq? (stat:type st) type))

(define* (file-is-regular? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a regular file."
  (stat:type=? (stat file) 'regular))

(define* (file-is-directory? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a directory."
  (stat:type=? (stat file) 'directory))

(define* (file-is-symlink? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a symlink."
  (stat:type=? (stat file) 'symlink))

(define* (file-is-other? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is not a regular file, directory, or symlink."
  (not (member (stat:type (stat file)) '(regular directory symlink))))

(define* (file-is-block-device? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a character device."
  (stat:type=? (stat file) 'block-special))

(define* (file-is-character-device? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a character device."
  (stat:type=? (stat file) 'char-special))

(define* (file-is-fifo? file #:optional (stat lstat))
  "Return @code{#t} if @var{file} is a character device."
  (stat:type=? (stat file) 'fifo))

(define* (mkfifo fifo #:optional (mode (logxor #o666 (umask))))
  "Create a new fifo named @var{fifo}.  If @var{mode} is omitted, then the
permissions of the fifo are set to @code{#o666} masked with the current umask.
Otherwise they are set to the value specified with @var{mode}.  The return
value is unspecified."
  (mknod fifo 'fifo mode 0))

(define (file-name->uri file-name)
  (unless (absolute-file-name? file-name)
    (scm-error 'out-of-range "file-name->uri"
               "expected absolute file name, got ~a."
               (list file-name) file-name))
  (build-uri 'file #:path (encode-and-join-uri-path
                           (file-name-split file-name))))

(define (uri->file-name uri)
  (unless (and uri (equal? 'file (uri-scheme uri)))
    (scm-error 'out-of-range "uri->file-name"
               "expected file URI, got ~a."
               (list uri) uri))
  (normalize-file-name
   (string-join (map uri-decode (string-split (uri-path uri)
                                              file-name-separator?))
                file-name-separator-string)))
