#lang racket
(require "TDA-pcar.rkt")


;TDA train, abstraccion de un tren del metro conformado por vagones (TDAs pcar)
;Representacion: lista con elementos (id del tren - fabricante del tren - tipo de vias compatibles con el tren - rapidez del tren
;              - tiempo de tren por estacion - carros del tren/TDAs pcar* (sin aridad de secciones, pueden ser varias)



;CONSTRUCTOR TDA train

#|DOM: id (entero) X fabricante (string) X tipo de vias (strig) X rapidez del tren (real positivo)
       X tiempo del tren por estacion (real no negativo) X carros* (TDAs pcar*)
REC: tren del metro (train)
funcion que crea un elemento del TDA tren, procurando que este sea valida, es decir que en los extremos esten los carros terminales y al centro los centrales|#
(define train (lambda (id maker rail-type speed station-stay-time . carros)
               (if (and (integer? id) (string? maker) (string? rail-type) (positive? speed) (or (positive? station-stay-time) (zero? station-stay-time)))
                        ;no se verifica carros porque puede que estos se agreges despues en otra funcion
                   (list id maker rail-type speed station-stay-time carros)
                   null)
               )
  )
#|
;funcion booleana
(define train-with-valid-pcars (lambda (carros)
                                 (if (and (eqv? (list-ref 0 carros)      ACA DEBE IR UN GET DEL TIPO )
                                          )    ;primera condicion de que los extremos sean terminales
                                     )
                                 )
  )|#