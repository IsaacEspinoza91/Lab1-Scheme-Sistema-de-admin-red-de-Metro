#lang racket
(require "TDA-section.rkt")
(require "TDA-station.rkt")


;TDA line, abstraccion de una linea de metro como una lista de elementos
;Representacion: lista con elementos (id de estacion - nombre de estacion
;              - tipo de vias de tren - secciones entre estaciones* (sin aridad, pueden ser varias)




;CONSTRUCTOR TDA line

#|DOM: id (entero) X nombre (string) X tipo de vias (strig) X secciones* (section*)
REC: linea de metro (line)
funcion que crea un elemento del TDA line |#
(define line (lambda (id name rain-type . sections)
               (if (and (number? id) (string? name) (string? rain-type) (sections-line? sections)) ; la funcion sections-line? usa recursion (ver documentacion mas abajo)
                   (list id name rain-type sections)
                   null)
               )
  )                                    ;SE DEBE MEJORAR PARA CUMPLIR REQUISISTOS DE IMPLEMENTACION




;FUNCIONES DE PERTENECIA

#|DOM: linea de metro (line)
REC: bool
funcion que verifica si un elemento es del tipo TDA line |#
(define line? (lambda (linea)
                (if (and (id-line? (car linea)) (name-line? (cadr linea))
                         (rain-type-line? (caddr linea)) (sections-line? (cadddr linea)))
                    #t
                    #f)
                )
  )

#|DOM: id linea de metro(entero)
 REC: bool
funcion que verifica si un dato puede ser id de un TDA line |#
(define id-line? (lambda (id) (if (number? id) #t #f)))

#|DOM: nombre linea de meotr (string)
 REC: bool
funcion que verifica si un dato puede ser nombre de un TDA line|#
(define name-line? (lambda (name) (if (string? name)#t #f)))

#|DOM: tipo de vias de una linea de metro(string)
 REC: bool
funcion que verifica si un dato puede ser tipo de vias de un TDA line |#
(define rain-type-line? (lambda (tipo-via) (if (string? tipo-via) #t #f)))

#|DOM: secciones de estaciones de metro de una linea (lista de TDAs section)
 REC: bool
funcion que verifica si un dato puede ser lista de sections de un TDA line
Recursion de cola
nota: esta funcion utiliza la recursion de cola para ir aplicando la funcion section? del TDA section a cada elemento
      de la lista como argumento y asi verificar que si los elementos sean del tipo section|#
(define sections-line? (lambda (secciones)
                         (if (and (section? (car secciones)) (not (empty? secciones)))
                             (if (empty? (cdr secciones))
                                 #t
                                 (sections-line? (cdr secciones)))
                             #f)
                         )
  )





;SELECTORES

(define get-id-line (lambda (linea)
                       (if (line? linea)
                           (car linea)
                           null)
                       )
  )

(define get-name-line (lambda (linea)
                        (if (line? linea)
                            (cadr linea)
                            null)
                        )
  )

(define get-rain-type-line (lambda (linea)
                             (if (line? linea)
                                 (caddr linea)
                                 null)
                             )
  )

(define get-sections-line (lambda (linea)
                            (if (line? linea)
                                (cadddr linea)
                                null)
                            )
  )














;OTRAS FUNCIONES
#|DOM: linea de metro (line)
REC: distancia (real positivo)
Función que permite determinar el largo total en distacia de una línea
Resolucion declarativa|#
(define line-length (lambda (linea)
                      (apply + (map get-distance-section (get-sections-line linea)))
                      )
  )                                     ;OJO, ANALIZAR MODO PARA LINEAS CIRCULARES, AUNQUE CREO QUE YA ESTAN ABARCADAS