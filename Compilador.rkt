#lang nanopass

(require racket/pretty)

;Archivo con Passes 1 - 13 (Front-end, Middle-end, Back-end)
(require "Passes/Passes.rkt")

;Ruta del archivo de ejemplo
(define path "ejemplos/ejemplo3.mt")


; ----------------------------- Escritura de archivos ----------------------

;; Función que lee un archivo de extensión que contiene
;; expresiones de LF (la extensión debe ser ".mt")
;; directo de la ruta especificada.
(define (read-file path)
  (call-with-input-file path
    (lambda (in) (read in))
    #:mode 'text))

;Funcion que escribe en un archivo lo que contiene la codigo indicado en el path. 
(define (write-file codigo path)
  (with-output-to-file path
    (lambda () (printf "~a" codigo))
    #:mode 'text #:exists 'replace))

;Funcion que crea el archivo correspondiente a alguna de las fases de compilacion
;crea el archivo en la ruta path cuyo contenido sera la variable codigo.
(define (write-file-int unparser codigo path)
  (write-file (pretty-format (unparser codigo)) path))

; Lee el archivo indicado en el path y aplica los procesos definidos en la lista passes.
(define (compilar path)
  (define codigo-LF (read-file path))
  (define codigo-post-front-end (apply-procs passes-front-end codigo-LF))
  (write-file-int unparse-L4 codigo-post-front-end (string-append path ".fe"))
  (define codigo-post-middle-end (apply-procs passes-middle-end (apply-procs passes-front-end lf)))
  (write-file-int unparse-L8 codigo-post-middle-end (string-append path ".me")))

; --------------------------- Pre-procesamiento ----------------------------

;Lista con los pases correspondientes a front-end.
(define passes-front-end (list remove-armed-if remove-string curry-let
                     identify-assignments un-anonymous verify-arity
                     verify-vars))

;Lista con los pases correspondientes a Middle-end.
(define passes-middle-end (list curry quote-const type-const type-infer
                     uncurry))

;Lista con los pases correspondientes a back-end.
;(define passes-back-end (list list-to-array c))
(define passes-back-end (list list-to-array))

;lf contiene el programa parseado contenido en el archivo de la ruta.
(define lf (parser-LF (read-file path)))

;Funcion para aplicar los pases de la lista proc al programa expr.
(define (apply-procs procs expr)
  (if (empty? procs)
    expr
    (apply-procs (rest procs) ((first procs) expr))))


; ----------------------------- Presentacion -------------------------------
(println "Original:")
(println lf)
;(println "vars:")
;(println (vars (apply-procs passes lf)))
(println "Con passes:")
(println (apply-procs passes-back-end (apply-procs passes-middle-end (apply-procs passes-front-end lf))))
