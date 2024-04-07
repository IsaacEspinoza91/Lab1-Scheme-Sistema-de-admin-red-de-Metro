#lang racket

;TDA station, abstraccion de una estacion de metro como una lista
;Representacion: lista con elementos (id - nombre - tipo de estacion - tiempo de parada en estacion)

;DOM
;REC
;Recursion tipo
;Algotimo/estrategia (ej fuerza bruta, backtraking etx



;CONSTRUCTOR station
;DOM: id (entero) X nombre (string) X tipo de estacion (type-station) STRING POR AHORAAAAAAAAAAAAAAA X tiempo de parada en segundos (entero positivo)
;REC: station (lista de elementos), en caso de algun argumento invalido retorna lista vacia
;Declarativo
;funcion que crea un elemento del TDA station, 
(define station (lambda (id name type stop-time)
                  (if (and (number? id) (string? name) (string? type) (integer? stop-time))
                      ;condiciones para verificar la validez de los tipos de datos de los argumentos 
                      (list id name type stop-time);creacion de la lista con los datos
                      null;caso de que algun dato no sea valido
                      )
                  )
  )



;FUNCIONES DE PERTENENCIA
#|DOM: estacion (station)
REC: bool
funcion que verifica si un elemento es del tipo station |#
(define station? (lambda (station)
                   (if (and (number? (car station)) (string? (cadr station)) (string? (caddr station)) (integer? (cadddr station)))
                       #t
                       #f
                       )
                   )
  )

#|DOM: id (entero)
 REC: bool
funcion que verifica si un dato puede ser id de un TDA estation|#
(define id-station? (lambda (id) (if (number? id) #t #f)))

#|DOM: nombre (string)
 REC: bool
funcion que verifica si un dato puede ser nombre de un TDA estation|#
(define name-station? (lambda (name) (if (string? name) #t #f)))

#|DOM: tipo de estacion (string)
 REC: bool
funcion que verifica si un dato puede ser tipo de estacion en un TDA estation|#
(define type-station? (lambda (type) (if (string? type) #t #f)))                     ;OJOOOOOOO

#|DOM: tiempo de parada (entero positivo)
 REC: bool
funcion que verifica si un dato puede ser tiempo de parada en un TDA estation|#
(define time-stop-station? (lambda (time) (if (integer? time) #t #f)))






;SELECTORES

#|DOM: estacion (station)
REC: id (entero) U {null}
funcion que obtiene el elemento id de un TDA station |#
(define get-id-station (lambda (station)
                        (if (station? station)
                            (car station)
                            null
                            )
                        )
  )

#|DOM: estacion (station)
REC: nombre (string) U {null}
funcion que obtiene el elemento nombre de un TDA station|#
(define get-name-station (lambda (station)
                           (if (station? station)
                               (cadr station)
                               null)
                           )
  )

#|DOM: estacion (station)
REC: tipo de estacion (string) U {null}                                  OJO QUE HAY QUE HACER EL TDA TYPE-STATION
funcion que obtiene el elemento tipo de estacion de un TDA station|#
(define get-type-station (lambda (station)
                           (if (station? station)
                               (caddr station)
                               null)
                           )
  )

#|DOM: estacion (station)
REC: tiempo de parada de estacion (entero positivo) U {null}
funcion que obtiene el elemento tiempo de parada en una estacion de un TDA station |#
(define get-time-stop-station (lambda (station)
                           (if (station? station)
                               (cadddr station)
                               null)
                           )
  )




;MODIFICADORES

#|DOM: estacion (station) X id (entero)
REC: (station) U {null}
funcion que crea un elemento TDA station a partir del elemento station de argumento, pero con una nueva id |#
(define set-id-station (lambda (estacion new-id)
                        (if (and (station? estacion) (id-station? new-id))
                            (station new-id (get-name-station estacion)
                                     (get-type-station estacion) (get-time-stop-station estacion))
                            null
                            )
                        )
  )

#|DOM: estacion (station) X nombre (string)
REC: (station) U {null}
funcion que crea un elemento TDA station a partir del elemento station de argumento, pero con un nuevo nombre |#
(define set-name-station (lambda (estacion new-name)
                          (if (and (station? estacion)(name-station? new-name))
                              (station (get-id-station estacion) new-name
                                       (get-type-station estacion) (get-time-stop-station estacion))
                              null
                              )
                          )
  )

#|DOM: estacion (station) X tipo de estacion (string)                                             NSEWWWWWWWWWWWWWWWWWWWWWWWWW
REC: (station) U {null}
funcion que crea un elemento TDA station a partir del elemento station de argumento, pero con un nuevo tipo de estacion |#
(define set-type-station (lambda (estacion new-type)
                          (if (and (station? estacion)(type-station? new-type))
                              (station (get-id-station estacion) (get-name-station estacion)
                                       new-type (get-time-stop-station estacion))
                              null
                              )
                          )
  )

#|DOM: estacion (station) X tiempo de parada (entero positivo)
REC: (station) U {null}
funcion que crea un elemento TDA station a partir del elemento station de argumento, pero con un nuevo tiempo de parada |#
(define set-time-stop-station (lambda (estacion new-time)
                               (if (and (station? estacion)(time-stop-station? new-time))
                                   (station (get-id-station estacion) (get-name-station estacion)
                                            (get-type-station estacion) new-time)
                                   null
                                   )
                               )
  )