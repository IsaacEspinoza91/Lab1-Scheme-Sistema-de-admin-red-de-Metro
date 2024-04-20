#lang racket
(require "TDA-section_212788287_EspinozaBarria.rkt")
(require "TDA-station_212788287_EspinozaBarria.rkt")
(require "TDA-type-station_212788287_EspinozaBarria.rkt")
(provide line line? line-length line-section-length line-cost line-section-cost line-add-section
         get-id-line get-name-line get-rail-type-line get-sections-line)


;TDA line, abstraccion de una linea de metro como una lista de elementos
;Representacion: lista con elementos (id de la linea - nombre de linea
;              - tipo de vias de tren - lista de secciones entre estaciones* (sin aridad de secciones, pueden ser varias)
;Como observacion, la lista de secciones del TDA line, tiene la disposicion de secciones determinado por el orden de ingreso de estas,
;    por lo que, en el caso de linea normal, la primera y ultima seccion en sus estaciones extremas deben ser terminales para que se considere una linea correctamente construida




;CONSTRUCTOR TDA line

#|DOM: id (entero) X nombre (string) X tipo de vias (strig) X secciones* (section*)
REC: linea de metro (line)
funcion que crea un elemento del TDA line |#
(define line (lambda (id name rail-type . sections)
               (if (and (id-line? id) (name-line? name) (rail-type-line? rail-type)
                        ;se usa otro condicional para el caso de no ingresar secciones para hacerlo despues,
                        ;y si que se ingresaron secciones que estas efectivamente sean del TDA secciones
                        ;(if (empty? sections) #f (sections-line? sections))                                                                                        ojo esta parte, ver condicion
                        ); la funcion sections-line? usa recursion (ver documentacion mas abajo)
                   (list id name rail-type sections)
                   null)
               )
  )                                                                                                            ;SE DEBE MEJORAR PARA CUMPLIR REQUISISTOS DE IMPLEMENTACION




;FUNCIONES DE PERTENECIA

#|DOM: linea de metro (line)
REC: bool
Recursion de cola. La funcion sections-line? utiliza la recursion de cola para verificar si los elementos pertenecen cada uno al TDA section,
    y la funcion all-station-conected usa recursion de cola para verificar que todas las estaciones esten conectadas
Funcion que verifica si un elemento es del tipo TDA line, cumpliendo con varias condiciones de formato|#
(define line? (lambda (linea)
                (if (and (id-line? (car linea)) (name-line? (cadr linea))
                         (rail-type-line? (caddr linea)) (sections-line? (cadddr linea))
                         (or (normal-line? (cadddr linea))  (circular-line? (cadddr linea)))
                         (all-station-conected? (cadddr linea))
                         )
                    #t
                    #f)
                )
  )


#|DOM: secciones (lista de TDAs section)
REC: bool
Funcion booleana que determina si las seciones de una linea cumplen para ser ser lina normal, esto quiere decir que sus estaciones terminales
   son tipo t y que se puede recorrer toda la linea por las secciones
Se usa en line?|#
(define (normal-line? secciones)
  (and (eqv? (get-type-station-station (get-station1-section (first secciones))) t);primera estacion de la primera seccion de la linea es tipo t
       (eqv? (get-type-station-station (get-station2-section (last secciones))) t);segunda estacion de la ultima seccion de la linea es tipo t
      )
  )


#|DOM:secciones (lista de TDAs section)
REC: bool
Funcion booleana que determina si las secciones de una linea cumplen con el criterio para ser linea circular, es decir, la primera y ultima estacion son la misma.
Se usa en line?|#
(define (circular-line? secciones)
  (eqv? (get-name-station (get-station1-section (first secciones)));nombre de la estacion 1 de la primera seccion de la linea 
        (get-name-station (get-station2-section (last  secciones)));nombre de la estacion 2 de la ultima seccion de la linea
      )
  )


#|DOM: secciones (lista de TDAs section)
REC: bool
Recursion de cola
Funcion que verifica que de una estaciones se puede llegar a todas las demas estaciones de una lista de secciones, recordar que la lista de
   secciones tiene las secciones ordenadas en un sentido de recorrido
Se usa en line?|#
(define (all-station-conected? secciones)
  (if (empty? (cdr secciones))
      #t
      (if (eqv? (get-name-station (get-station2-section (car secciones)));comparacion entre nombre de la seccion anterior y la actual (station2 y station1)
                (get-name-station (get-station1-section (cadr secciones)))
                )
          (all-station-conected? (cdr secciones))
          #f
          )
      )
  )


#|DOM: linea de metro (line)
REC: bool
Declarativo.
funcion que verifica si un elemento es del tipo TDA line sin verificar si tiene la lista de sections, esto con el
     motivo de no perjudicar una linea cuando se quiere agregar sections posteriormente|#
(define line-without-check-sections? (lambda (linea)
                (if (and (id-line? (car linea)) (name-line? (cadr linea)) (rail-type-line? (caddr linea)))
                    #t
                    #f)
                )
  )

#|DOM: id linea de metro(entero)
 REC: bool
funcion que verifica si un dato puede ser id de un TDA line |#
(define id-line? (lambda (id) (if (number? id) #t #f)))

#|DOM: nombre linea de metro (string)
 REC: bool
funcion que verifica si un dato puede ser nombre de un TDA line|#
(define name-line? (lambda (name) (if (string? name)#t #f)))

#|DOM: tipo de vias de una linea de metro(string)
 REC: bool
funcion que verifica si un dato puede ser tipo de vias de un TDA line |#
(define rail-type-line? (lambda (tipo-via) (if (string? tipo-via) #t #f)))

#|DOM: lista secciones de estaciones de metro de una linea (lista de TDAs section)
 REC: bool
funcion que verifica si un dato puede ser lista de sections de un TDA line
Recursion de cola
nota: esta funcion utiliza la recursion de cola para ir aplicando la funcion section? del TDA section a cada elemento
      de la lista como argumento y asi verificar que si los elementos sean del tipo section.
     En el caso de lista vacia, situacion en que todavia no se ingresa lista de sections se retorna #f    |#
(define sections-line? (lambda (secciones)
                         (if (empty? secciones)
                             #f
                             (if (section? (car secciones))
                                (if (empty? (cdr secciones))
                                    #t
                                    (sections-line? (cdr secciones)))
                                #f)
                             )
                         )
  )





;SELECTORES

#|DOM: linea de metro (line)
REC: id linea de metro(entero) U {null}
funcion que obtiene el elemento id de un TDA line |#
(define get-id-line (lambda (linea)
                       (if (line-without-check-sections? linea)
                           (car linea)
                           null)
                       )
  )

#|DOM: linea de metro (line)
REC: nombre linea de meotr (string) U {null}
funcion que obtiene el elemento nombre de un TDA line |#
(define get-name-line (lambda (linea)
                        (if (line-without-check-sections? linea)
                            (cadr linea)
                            null)
                        )
  )

#|DOM: linea de metro (line)
REC: tipo de vias de una linea de metro(string) U {null}
funcion que obtiene el elemento rain-type de un TDA line |#
(define get-rail-type-line (lambda (linea)
                             (if (line-without-check-sections? linea)
                                 (caddr linea)
                                 null)
                             )
  )

#|DOM: linea de metro (line)
REC: secciones de estaciones de metro de una linea (lista de TDAs section) U {null}
funcion que obtiene el elemento TDA secciones de un TDA line |#
(define get-sections-line (lambda (linea)
                            (if (line-without-check-sections? linea)
                                (cadddr linea)
                                null)
                            )
  )




;MODIFICADORES

#|DOM: linea de metro (line) X id linea de metro (entero)
REC: linea de metro (line) U {null}
Funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nueva id |#
(define set-id-line (lambda (linea new-id)
                      (if (and (line-without-check-sections? linea) (id-line? new-id))
                          (line new-id (get-name-line linea)
                                (get-rail-type-line linea) (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X nombre linea de metro (string)
REC: linea de metro (line) U {null}
Funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nuevo nombre de linea |#
(define set-name-line (lambda (linea new-name)
                      (if (and (line-without-check-sections? linea) (name-line? new-name))
                          (line (get-id-line linea) new-name
                                (get-rail-type-line linea) (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X tipo de vias de una linea de metro(string)
REC: linea de metro (line) U {null}
Funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nuevo tipo de vias (rain-type) |#
(define set-rain-type-line (lambda (linea new-rain-type)
                      (if (and (line-without-check-sections? linea) (rail-type-line? new-rain-type))
                          (line (get-id-line linea) (get-name-line linea)
                                new-rain-type (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X lista secciones de estaciones de metro de una linea (lista de TDAs section)
REC: linea de metro (line) U {null}
Funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nueva lista de TDA section |#
(define set-sections-line (lambda (linea new-sections)
                      (if (and (line-without-check-sections? linea) );(sections-line? new-sections))
                          (line (get-id-line linea) (get-name-line linea)
                                (get-rail-type-line linea) new-sections)
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
  )                                                                                           ;OJO, ANALIZAR MODO PARA LINEAS CIRCULARES, AUNQUE CREO QUE YA ESTAN ABARCADAS


#|
idea, recorro la lista de sections preguntando por el primer nombre de estacion
y si el st1 o st2 son este, lo tomo como pivote y voy a la siguiente y paro la recursion                    OJO VER EL TIPO DE RECURSION, EN EL DOC SALE TACHADO Y TODAVIA NO CONTESTAS DE QUE MODO SE HACE, RECURSIVO O DECLARATIVO

DOM: linea de metro (line) X nombre de estacion 1 (station) X nombre de estacion 2 (station)
REC: numero (real no negativo)
Recursion Natural
funion que entrega la distania en metros entre dos estaciones de una linea, dado el nombre exacto de estas estaciones
|#
(define line-section-length (lambda (linea name-station1 name-station2)
                              (define (funcion-aux secciones st1 st2)
                                      (if (empty? secciones)   ;condicion de borde fin de recursion, no se encontraron los nombres
                                          0
                                          (cond
                                            [(eqv? st1 (get-name-station (get-station1-section (car secciones))))  (funcion-aux2 secciones st2)]
                                            [(eqv? st2 (get-name-station (get-station1-section (car secciones))))  (funcion-aux2 secciones st1)]
                                            [else (funcion-aux (cdr secciones) st1 st2)]
                                            )
                                          )
                                )
                              (define (funcion-aux2 secciones-aux estacion-llegada)
                                      (if (eqv? estacion-llegada (get-name-station (get-station2-section (car secciones-aux))))
                                          (get-distance-section (car secciones-aux))
                                          (+ (get-distance-section (car secciones-aux)) (funcion-aux2 (cdr secciones-aux) estacion-llegada))
                                          )
                                )
                              (funcion-aux (get-sections-line linea) name-station1 name-station2)
                              )
  )








#|
DOM: linea de metro (line)
REC: costo (real positivo)
Recursion natural
Función que permite determinar el costo total en distacia de una línea |#
(define line-cost (lambda (linea)
                    (define fn-aux (lambda (secciones)
                                     (if (empty? secciones)
                                         0
                                         (+ (get-cost-section (car secciones)) (fn-aux (cdr secciones))))
                                     )
                      )
                    (fn-aux (get-sections-line linea))
                    )
  )


#|DOM: linea de metro (line) X nombre de estacion 1 (station) X nombre de estacion 2 (station)
REC: costo monetario (numero real no negativo)
Recursion de cola
Funcion que retorna el costo monetario del trayecto entre dos estaciones|#
(define line-section-cost (lambda (linea name-station1 name-station2)
                              (define (fn-aux secciones st1 st2)
                                      (if (empty? secciones)   ;condicion de borde fin de recursion, no se encontraron los nombres
                                          null
                                          (cond
                                            [(eqv? st1 (get-name-station (get-station1-section (car secciones))))  (fn-aux2 secciones st2 0)]
                                            [(eqv? st2 (get-name-station (get-station1-section (car secciones))))  (fn-aux2 secciones st1 0)]
                                            [else (fn-aux (cdr secciones) st1 st2)];notas recursion de cola, no deja estados pendientes
                                            )
                                          )
                                )
                              (define (fn-aux2 secciones-aux estacion-llegada resultado)
                                      (if (eqv? estacion-llegada (get-name-station (get-station2-section (car secciones-aux))))
                                          (+ (get-cost-section (car secciones-aux)) resultado)
                                          (fn-aux2 (cdr secciones-aux) estacion-llegada (+ (get-cost-section (car secciones-aux)) resultado))
                                          );recursion de cola, el costo de va acumulando en resultado
                                )
                              (fn-aux (get-sections-line linea) name-station1 name-station2)
                              )
  )




#|DOM: linea de metro (TDA line) X elemento seccion entre estaciones(TDA section)
 REC: (TDA line)
Recursividad natural; se usa recursion natural para crear la lista de TDAs sections, ademas se hace
     uso de la funcion externa is-section-in-sections-line? que utiliza recursion natural para verificar que no existan secciones repetidas en la lista
Funcion que agrega una seccion (TDA section) entre estaciones a un TDA line|#
(define line-add-section (lambda (linea nueva-seccion)
                           (define fn-aux22 (lambda (secciones new-section);notar recursion de stack
                                            (if (empty? secciones)
                                                (list new-section)
                                                (append (list (car secciones))  (fn-aux22 (cdr secciones) new-section))
                                                )
                                            )
                             )
                           (cond
                             [(not (section? nueva-seccion)) linea];caso que la nueva-seccion no pertenesca al TDA section
                             [(empty? (get-sections-line linea)) (set-sections-line linea nueva-seccion)]
                             [(is-section-in-sections-line? (get-sections-line linea) nueva-seccion) linea];caso de que el nombre de la estacion ya este repetido                                             ojo, se deber[ia impleentar que tambien verifique la id de las secciones y que no este repetida
                             [else (list (get-id-line linea) (get-name-line linea) (get-rail-type-line linea)
                                    (fn-aux22 (get-sections-line linea) nueva-seccion))]; caso de que no hay secciones repetidas y se agrega normalmente
                                   ; se opta por usar la funcion list en vez del constructor de lista para evitar problemas con la encapsulacion de la lista de sections,
                                   ; sin embargo, con las condiciones previas efectivamente se retorna un elemento del TDA line
                             )
                           )
  )



#|DOM: lista de TDAs sections (lista sections) X elemento section buscado (TDA section)
REC: bool
Recursion de natural
Funcion booleana que indica si un TDA section ya esta dentro de una lista de TDAs sections
Metodo: comparacion de nombres
Se utiliza en la funcion line-add-section|#
(define (is-section-in-sections-line? secciones seccion-buscada) 
        (if (empty? secciones)
            #f; caso no esta repetida la seccion-buscada en secciones
            (if (eqv? (get-name-station (get-station1-section (car secciones)))
                      (get-name-station (get-station1-section seccion-buscada))
                      )
                #t
                (or #f (is-section-in-sections-line? (cdr secciones) seccion-buscada))
                ;es un poco innesario la operacion logica, pero cumple la recursion natural al dejar valores pendientes por computar
                )
            )
  )





