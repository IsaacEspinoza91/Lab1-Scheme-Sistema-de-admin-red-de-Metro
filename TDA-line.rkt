#lang racket
(require "TDA-section.rkt")
(require "TDA-station.rkt")


;TDA line, abstraccion de una linea de metro como una lista de elementos
;Representacion: lista con elementos (id de la linea - nombre de linea
;              - tipo de vias de tren - secciones entre estaciones* (sin aridad de secciones, pueden ser varias)




;CONSTRUCTOR TDA line

#|DOM: id (entero) X nombre (string) X tipo de vias (strig) X secciones* (section*)
REC: linea de metro (line)
funcion que crea un elemento del TDA line |#
(define line (lambda (id name rain-type . sections)
               (if (and (id-line? id) (name-line? name) (rain-type-line? rain-type)
                        ;se usa otro condicional para el caso de no ingresar secciones para hacerlo despues,
                        ;y si que se ingresaron secciones que estas efectivamente sean del TDA secciones
                        ;(if (empty? sections) #t (sections-line? sections))                                                                                        ojo esta parte, ver condicion
                        ); la funcion sections-line? usa recursion (ver documentacion mas abajo)
                   (list id name rain-type sections)
                   null)
               )
  )                                                                                                            ;SE DEBE MEJORAR PARA CUMPLIR REQUISISTOS DE IMPLEMENTACION




;FUNCIONES DE PERTENECIA

#|DOM: linea de metro (line)
REC: bool
Recursion de cola. La funcion sections-line? utiliza la recursion de cola para verificar si los elementos pertenecen cada uno al TDA section
funcion que verifica si un elemento es del tipo TDA line |#
(define line? (lambda (linea)
                (if (and (id-line? (car linea)) (name-line? (cadr linea))
                         (rain-type-line? (caddr linea))           );(if (empty? (cadddr linea)) #t (sections-line? (cadddr linea))))
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
(define rain-type-line? (lambda (tipo-via) (if (string? tipo-via) #t #f)))

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
                       (if (line? linea)
                           (car linea)
                           null)
                       )
  )

#|DOM: linea de metro (line)
REC: nombre linea de meotr (string) U {null}
funcion que obtiene el elemento nombre de un TDA line |#
(define get-name-line (lambda (linea)
                        (if (line? linea)
                            (cadr linea)
                            null)
                        )
  )

#|DOM: linea de metro (line)
REC: tipo de vias de una linea de metro(string) U {null}
funcion que obtiene el elemento rain-type de un TDA line |#
(define get-rain-type-line (lambda (linea)
                             (if (line? linea)
                                 (caddr linea)
                                 null)
                             )
  )

#|DOM: linea de metro (line)
REC: secciones de estaciones de metro de una linea (lista de TDAs section) U {null}
funcion que obtiene el elemento TDA secciones de un TDA line |#
(define get-sections-line (lambda (linea)
                            (if (line? linea)
                                (cadddr linea)
                                null)
                            )
  )




;MODIFICADORES

#|DOM: linea de metro (line) X id linea de metro (entero)
REC: linea de metro (line) U {null}
funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nueva id |#
(define set-id-line (lambda (linea new-id)
                      (if (and (line? linea) (id-line? new-id))
                          (line new-id (get-name-line linea)
                                (get-rain-type-line linea) (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X nombre linea de metro (string)
REC: linea de metro (line) U {null}
funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nuevo nombre de linea |#
(define set-name-line (lambda (linea new-name)
                      (if (and (line? linea) (name-line? new-name))
                          (line (get-id-line linea) new-name
                                (get-rain-type-line linea) (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X tipo de vias de una linea de metro(string)
REC: linea de metro (line) U {null}
funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nuevo tipo de vias (rain-type) |#
(define set-rain-type-line (lambda (linea new-rain-type)
                      (if (and (line? linea) (rain-type-line? new-rain-type))
                          (line (get-id-line linea) (get-name-line linea)
                                new-rain-type (get-sections-line linea))
                          null)
                      )
  )

#|DOM: linea de metro (line) X lista secciones de estaciones de metro de una linea (lista de TDAs section)
REC: linea de metro (line) U {null}
funcion que crea un elemento TDA line a partir del elemento linea de los argumentos, pero con un nueva lista de TDA section |#
(define set-sections-type-line (lambda (linea new-sections)
                      (if (and (line? linea) (sections-line? new-sections))
                          (line (get-id-line linea) (get-name-line linea)
                                (get-rain-type-line linea) new-sections)
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








#|                                      DEBE SER RECURSIVA SEGUN EL ENUNCIADO
DOM: linea de metro (line)
REC: costo (real positivo)
Función que permite determinar el costo total en distacia de una línea
Resolucion declarativa
(define line-cost (lambda (linea)
                    (apply + (map get-cost-section (get-sections-line linea)))
                    )
  )
|#


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
funcion que retorna el costo monetario del trayecto entre dos estaciones|#
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





;MALA, NO FUNCIONA
;me dio sueno manana veo que onda
;IDEA hacer funcion bolleana aparte que solo verifique la seccion no este repetida
(define line-add-section (lambda (linea nueva-seccion)
                           (define (fun-aux secciones new-seccion)
                                   (if (section? new-seccion)
                                       (if (empty? secciones)
                                           new-seccion
                                           (if ;(eqv? (get-name-station (get-station1-section (car secciones)))
                                                     ;(get-name-station (get-station1-section new-seccion)))
                                            #f
                                               null  ;caso de que la seccion ya estaba, se devuelve null para que no se agregen seciones al line del parametro inicial
                                               (cons (car secciones) (list (fun-aux (cdr secciones) new-seccion)))
                                               )
                                           )
                                       null)
                             )

                           (line (get-id-line linea) (get-name-line linea) (get-rain-type-line linea) (fun-aux (get-sections-line linea) nueva-seccion))

                           )
  )

