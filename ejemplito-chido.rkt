#lang nanopass

(require "front-end/passes.rkt")

(define path "ejemplos/ejemplo1.mt")

(define passes (list remove-armed-if remove-string curry-let
                     identify-assignments un-anonymous verify-arity))
(define lf (parser-LF (leer-codigo-LF path)))

(define (apply-procs procs expr)
  (if (empty? procs)
    expr
    (apply-procs (rest procs) ((first procs) expr))))

(println "Original:")
(println lf)
;(println "vars:")
;(println (vars (apply-procs passes lf)))
(println "Con passes:")
(println (apply-procs passes lf))
