#lang nanopass


(require racket/pretty)

;Archivo con Passes 1 - 13 (Front-end, Middle-end, Back-end)
(require "front-end/passes.rkt")

;Ruta del archivo de ejemplo
(define path "ejemplos/ejemplo1.mt")

; --------------------------- Pre-procesamiento ----------------------------

;Lista con los pases correspondientes a Middle-end.
(define passes (list remove-armed-if remove-string curry-let
                     identify-assignments un-anonymous verify-arity
                     verify-arity))

;lf contiene el programa parseado contenido en el archivo de la ruta.
(define lf (parser-LF (leer-codigo-LF path)))

;Funcion para aplicar los pases de la lista proc al programa expr.
(define (apply-procs procs expr)
  (if (empty? procs)
    expr
    (apply-procs (rest procs) ((first procs) expr))))

; ----------------------------- Escritura de archivos ----------------------

;Funcion que escribe en un archivo lo que contiene la codigo indicado en el path. 
(define (escribir-codigo codigo path)
  (with-output-to-file path
    (lambda () (printf "~a" codigo))
    #:mode 'text #:exists 'replace))

;Funcion que crea el archivo correspondiente a alguna de las fases de compilacion
;crea el archivo en la ruta path cuyo contenido sera la variable codigo.
(define (escribir-codigo-int unparser codigo path)
  (escribir-codigo (pretty-format (unparser codigo)) path))

; Lee el archivo indicado en el path 
(define (compilar path)
  (define codigo-LF (leer-codigo-LF path))
  (define codigo-post-front-end (apply-procs passes codigo-LF))
  (escribir-codigo-int unparse-L8 codigo-post-front-end (string-append path ".fe")))

; ----------------------------- Presentacion -------------------------------
(println "Original:")
(println lf)
;(println "vars:")
;(println (vars (apply-procs passes lf)))
(println "Con passes:")
(println (apply-procs passes lf))


