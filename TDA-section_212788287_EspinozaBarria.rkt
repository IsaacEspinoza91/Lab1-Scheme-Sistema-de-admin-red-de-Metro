#lang racket
(require "TDA-station_212788287_EspinozaBarria.rkt")
(provide section section? get-distance-section get-cost-section get-station1-section get-station2-section)

;TDA section, abstraccion de una seccion/enlace/tramo entre dos estaciones de metro como una lista de elementos
;Representacion: lista con elementos (estacion1 - estacion2 - distancia entre estaciones (real positivo) - costo monetario (real no negativo))



;CONSTRUCTOR
#|DOM: estacion 1 (station) X estacion 2 (station) X distancia entre estaciones metros (numero positivo) X costo (real no negativo)
REC: (section) U {null}
funcion que crea un elemento del TDA section, en el caso de algun argumento invalido devuelve null |#
(define section (lambda (station1 station2 distance cost)
                  (if (and (station? station1) (station? station2) (positive? distance) (or (positive? cost) (zero? cost)))
                      (list station1 station2 distance cost)                                 ;OJOOO FALTA CONDICION DE QUE SI LAS ESTACIONES ESTAN EN LA MISMA LINEA O SON ESTACIONES CONBINACION/TERMINAL
                      null)
                  )
  )




;FUNCIONES DE PERTENECIA

#|DOM: seccion entre estaciones (section)
REC: bool
funcion que verifica si un elemento es del tipo section |#
(define section? (lambda (seccion)
                   (if (and (not (number? seccion)) (not (string? seccion)) (not (boolean? seccion)) ;se agregaron estas condiciones al inicio evitar errores en el TDA line al verificar secciones
                            (station? (car seccion)) (station? (cadr seccion))
                            (distance-section? (caddr seccion)) (cost-section? (cadddr seccion))
                            )
                       #t
                       #f)
                   )
  )

#|DOM: distancia entre estaciones (real positivo)
 REC: bool
funcion que verifica si un dato puede ser distancia de un TDA section|#
(define distance-section? (lambda (distancia) (if (positive? distancia) #t #f)))

#|DOM: costo (real no negativo)
 REC: bool
funcion que verifica si un dato puede ser costo de un TDA section|#
(define cost-section? (lambda (costo) (if (or (positive? costo) (zero? costo)) #t #f)))




;SELECTORES

#|DOM: seccion entre estaciones (section)
REC: estacion1 (station) U {null}
funcion que obtiene el elemento estacion1 de un TDA section |#
(define get-station1-section (lambda (seccion)
                               (if (section? seccion)
                                   (car seccion)
                                   null)
                               )
  )

#|DOM: seccion entre estaciones (section)
REC: estacion2 (station) U {null}
funcion que obtiene el elemento estacion2 de un TDA section |#
(define get-station2-section (lambda (seccion)
                               (if (section? seccion)
                                   (cadr seccion)
                                   null)
                               )
  )

#|DOM: seccion entre estaciones (section)
REC: distancia (real positivo) U {null}
funcion que obtiene el elemento distancia de un TDA section |#
(define get-distance-section (lambda (seccion)
                                (if (section? seccion)
                                    (caddr seccion)
                                    null)
                               )
  )

#|DOM: seccion entre estaciones (section)
REC: costo (real no negativo) U {null}
funcion que obtiene el elemento costo de un TDA section |#
(define get-cost-section (lambda (seccion)
                           (if (section? seccion)
                               (cadddr seccion)
                               null)
                           )
  )




;MODIFICADORES

#|DOM: seccion entre estaciones (section) X estacion1 (station)
REC: (section) U {null}
funcion que crea un elemento TDA section a partir del elemento section de argumento, pero con un nueva estacion1 |#
(define set-station1-section (lambda (seccion new-station1)
                               (if (and (section? seccion) (station? new-station1))
                                   (section new-station1 (get-station2-section seccion)
                                            (get-distance-section seccion) (get-cost-section seccion))
                                   null)
                               )
  )

#|DOM: seccion entre estaciones (section) X estacion2 (station)
REC: (section) U {null}
funcion que crea un elemento TDA section a partir del elemento section de argumento, pero con un nueva estacion2 |#
(define set-station2-section (lambda (seccion new-station2)
                               (if (and (section? seccion) (station? new-station2))
                                   (section (get-station1-section seccion) new-station2
                                            (get-distance-section seccion) (get-cost-section seccion))
                                   null)
                               )
  )

#|DOM: seccion entre estaciones (section) X distancia (real positivo)
REC: (section) U {null}
funcion que crea un elemento TDA section a partir del elemento section de argumento, pero con un nueva distancia entre estaciones |#
(define set-distance-section (lambda (seccion new-distance)
                               (if (and (section? seccion) (distance-section? new-distance))
                                   (section (get-station1-section seccion) (get-station2-section seccion)
                                            new-distance (get-cost-section seccion))
                                   null)
                               )
  )

#|DOM: seccion entre estaciones (section) X costo (real no negativo)
REC: (section) U {null}
funcion que crea un elemento TDA section a partir del elemento section de argumento, pero con un nuevo costo |#
(define set-cost-section (lambda (seccion new-cost)
                           (if (and (section? seccion) (cost-section? new-cost))
                               (section (get-station1-section seccion) (get-station2-section seccion)
                                        (get-distance-section seccion) new-cost)
                               null)
                           )
  )