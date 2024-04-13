#lang racket
(provide type-station type-station? get-type-station set-type-station r m c t)

#|TDA type-station, utilizado para abtraer los tipos de estaciones de la red de metro y la funcion de estas.
REPRESENTACION: string de un caracter en minusculas donde se indique la funcion de la estacion. Los caracteres posibles para las funciones son:
    "r" (regular): estacion para abordar y dejar pasajeros
    "m" (mantencion): estacion de mantenimiento para trenes
    "c" (combinacion): estacion regular pero que ademas permite hacer transbordo entre otras lineas de metro
    "t" (terminal): estacion final de una linea de metro, tambien puede funcionar para hacer combinaciones entre lineas
|#

(define r "r")
(define m "m")
(define c "c")
(define t "t")


;CONSTRUCTOR type-station
#|DOM: {"r","m","c","t"} (string de un elemento)
REC: type-station U {null}
funcion que crea un elemento del TDA type-station, en caso de un parametro invalido devuleve null |#
(define type-station (lambda (tipo)
                       (if (or (equal? "r" tipo) (equal? "m" tipo) (equal? "c" tipo) (equal? "t" tipo))
                           tipo
                           null)
                       )
  )



;FUNCIONES DE PERTENECIA

#|DOM: tipo de estacion (type-station)
REC: bool
funcion que verifica si un elemento es del tipo type-station |#
(define type-station? (lambda (tipo)
                        (if (or (equal? "r" tipo) (equal? "m" tipo) (equal? "c" tipo) (equal? "t" tipo))
                            #t
                            #f)
                        )
  )



;SELECTORES
#|DOM: tipo de estacion (type-station)
REC: tipo de estacion (type-station) U {null}
funcion que obtiene el type-station de un elemento TDA type-station |#
(define get-type-station (lambda (tipo) (if (type-station? tipo) tipo null)))



;MODIFICADORES
#|DOM: tipo de estacion (type-station)
REC: tipo de estacion (type-station) U {null}
funcion que crea un elemento TDA type-station, pero con un nuevo tipo de estacion|#
(define set-type-station (lambda (tipo new-type)
                           (if (and (type-station? tipo) (type-station? new-type))
                               (type-station new-type)
                               null)
                           )
  )


;OTRAS FUNCIONES
#|DOM: tipo de estacion (type-station)
REC:
funcion que imprime en pantalla el tipo de estacion del argumento TDA type-station|#
(define nombra-type-station (lambda (tipo)
                              (if (type-station? tipo)
                                  (cond
                                    [(equal? "r" tipo) (display "estacion regular")]
                                    [(equal? "m" tipo) (display "estacion de mantencion")]
                                    [(equal? "c" tipo) (display "estacion de combinacion")]
                                    [(equal? "t" tipo) (display "estacion terminal")]
                                    )
                                  null)
                              )
  )