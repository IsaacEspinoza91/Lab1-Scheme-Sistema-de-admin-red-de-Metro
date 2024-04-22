#lang racket
(require "TDA-pcar_212788287_EspinozaBarria.rkt")
(require "TDA-section_212788287_EspinozaBarria.rkt")
(require "TDA-station_212788287_EspinozaBarria.rkt")
(require "TDA-type-station_212788287_EspinozaBarria.rkt")
(require "TDA-line_212788287_EspinozaBarria.rkt")
(require "TDA-train_212788287_EspinozaBarria.rkt")
(require "TDA-driver_212788287_EspinozaBarria.rkt")
(provide subway subway-add-train subway-add-line subway-add-driver subway->string subway-rise-section-cost
         subway-set-station-stoptime subway-assign-train-to-line subway-assign-driver-to-train)



;TDA subway, abstraccion de sistema de metro
;Representacion: lista con elementos
;             (id metro - nombre metro - lista de trenes del metro - lista de lineas del metro -
;              lista de conductores - lista de elementos del tipo par donde el primer elemento
;              es la id de la linea y el segundo es una lista de trenes asociados a esa linea  -
;              lista de elementos con (id conductor - id tren - hora de partida - estacion de
;              partida - estacion de llegada)  para determinar el recorrido de trenes y conductores )




;CONSTRUCTOR
#|DOM: id (entero) X nombre metro (string)
REC: metro (subway)
Funcion que crea un elemento del TDA subway, el que es una lista de 7 elementos, en un inicio, el TDA
     solo tendra 2 elementos (id y nombre), los demas seran agregamos posteriormente en otras funciones|#
;Nivel de implementacion: 1
(define subway (lambda (id nombre)(list id nombre null null null null null)))




;FUNCION DE PERTENECIA
#|DOM: metro (subway)
REC: bool
Funcion que determina si los elementos de un TDA subway son compatibles solo en tipo de dato, no verifica si efectivamente son ej drivers, lines, etc|#
(define subway-without-all-elements? (lambda (metro)
                                    (and (integer? (car metro)) (string? (cadr metro)) (list? (caddr metro))
                                         (list? (cadddr metro)) (list? (cadddr (cdr metro)))
                                         (list? (cadddr (cddr metro))) (list? (cadddr (cdddr metro))))
                                    )
  )


#|DOM: metro (TDA subway)
REC: bool
Funcion que determina si un elemento pertenece al TDA suwbay|#
(define (subway? metro)
  (and (integer? (get-id-subway metro)) (string? (get-name-subway metro))
       (null? (filter (lambda (q) (not (train? q))) (get-trains-subway metro)));en caso de que no sean trenes, se filtra y se agrega a la lista que ya no seria null y se rompe la condicion
       (null? (filter (lambda (z) (not (line? z)))  (get-lines-subway metro)))
       (null? (filter (lambda (m) (not (driver? m)))(get-drivers-subway metro)))
       ;no se verifican line-trains porque cuando son agregadas se verifica su validez antes
       (null? (filter (lambda (v) (not (and (integer? (car v)) (integer? (cadr v)) (string? (caddr v))
                                            (string? (cadddr v)) (string? (last v))))   ) (get-routes-subway metro)))
       )
  )





;SELECTORES
#|DOM: metro (subway)
REC: id (entero)
Funcion que retorna el elemento id de un TDA subway|#
(define get-id-subway (lambda (metro)(if (subway-without-all-elements? metro) (car metro) null)))

#|DOM:metro (subway)
REC: nombre metro (string)
Funcion que retorna el elemento nombre de un TDA subway|#
(define get-name-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadr metro) null)))

#|DOM:metro (subway)
REC: lista de trenes (list TDAs train)
Funcion que retorna el elemento trenes de un TDA subway|#
(define get-trains-subway (lambda (metro)(if (subway-without-all-elements? metro) (caddr metro) null)))

#|DOM:metro (subway)
REC: lista de lineas (list TDAs line)
Funcion que retorna el elemento lineas de un TDA subway|#
(define get-lines-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr metro) null)))

#|DOM:metro (subway)
REC: lista de conductores (list TDAs driver)
Funcion que retorna el elemento conductores de un TDA subway|#
(define get-drivers-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cdr metro)) null)))

#|DOM:metro (subway)
REC: lista de pares id-line y lista trains
Funcion que retorna el elemento line-trains de un TDA subway|#
(define get-line-trains-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cddr metro)) null)))

#|DOM:metro (subway)
REC:lista de elementos de recorrido (id conductor - id tren - hora de partida - estacion de partida
   - estacion de llegada)
Funcion que retorna el elemento rutas de un TDA subway|#
(define get-routes-subway (lambda (metro)(if (subway-without-all-elements? metro) (cadddr (cdddr metro)) null)))





;MODIFICADORES
#|DOM: metro (subway) X id (entero)
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento id|#
(define (set-id-subway metro new-id)
  (if (subway-without-all-elements? metro)
      (list new-id (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro)
           (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X nombre (string)
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento nombre|#
(define (set-name-subway metro new-name)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) new-name (get-trains-subway metro) (get-lines-subway metro)
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X trenes (list TDAs train) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento trains|#
(define (set-trains-subway metro list-new-trains)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) list-new-trains (get-lines-subway metro)
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X lineas (lista TDAs lines) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento lines|#
(define (set-lines-subway metro list-new-lines)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) list-new-lines 
            (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X conductores (lista TDAs driver) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento drivers|#
(define (set-drivers-subway metro list-new-drivers)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            list-new-drivers (get-line-trains-subway metro) (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X line-trains (lista de pares (id linea - lista de TDAs train)) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento line-trains|#
(define (set-line-trains-subway metro list-new-line-trains)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            (get-drivers-subway metro) list-new-line-trains (get-routes-subway metro))
      null)
  )

#|DOM: metro (subway) X rutas (lista de elementos de ruta) | null
REC: metro (subway) U {null}
Funicion que crea un TDA subway pero con una nuevo elemento routes|#
(define (set-routes-subway metro list-new-routes)
  (if (subway-without-all-elements? metro)
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro) (get-lines-subway metro) 
            (get-drivers-subway metro) (get-line-trains-subway metro) list-new-routes)
      null)
  )











;OTRAS FUNCIONES

;DOM: (list) X (list)               REC: (list)
;Funcion que compara l2 con l1 y retorna l2 sin las ocurrencias de l1 en el, util para no agregar elemento repetidos
;Se usa en las tres funciones siguientes
(define (aux-no-repe l1 l2)
  (filter (lambda (act) (boolean? (member act l1)))   l2)
  )




#|DOM: metro (subway) X trenes+ (TDAs train+ ) (puede ser 1 o mas)
REC: metro (subway)
Recursion natural, por su facilidad. La funcion fn-aux emplea recursividad, para cumplir el requisisto de implementacion
Funcion que agrega los trenes del argumento al parametro de trenes dentro del subway de modo que este tenga o no otros trenes en un inicio.
    Ademas no considera los elementos repetidos.|#
;Nivel de implementacion: 1
(define subway-add-train (lambda (metro . trenes)
                           ;DOM: (lista) X (lista)         REC: (lista)        Recursion natural
                           ;funcion que usa la recursion natural para retornar la union de dos listas de parametros
                           (define (fn-aux lista1 lista2)
                             (cond
                               [(empty? lista1) lista2]
                               [else (cons (car lista1) (fn-aux (cdr lista1) lista2))])
                             )
                           
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (fn-aux (get-trains-subway metro)        (aux-no-repe (get-trains-subway metro)  (filter (lambda (d) (train? d)) trenes)  ));se realiza filter para verificar que solo se agregen trenes validos
                                     (get-lines-subway metro)
                                     (get-drivers-subway metro) (get-line-trains-subway metro)
                                     (get-routes-subway metro))
                               null)
                           )
  )


#|DOM: metro (subway) X lineas+ (TDAs line+) (puede ser 1 o mas)
REC: metro (subway)
NO RECURSION a diferencia de subway-add-train
Funcion que agrega las lineas del argumento al parametro de lineas del subway, sin considerar los elementos repetidos|#
;Nivel de implementacion: 1
(define subway-add-line (lambda (metro . lineas)
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (get-trains-subway metro) (append (get-lines-subway metro)   (aux-no-repe  (get-lines-subway metro) (filter (lambda (act) (line? act)) lineas) ));se realiza filtro para verificar que se agregen efectivamente lineas
                                     (get-drivers-subway metro) (get-line-trains-subway metro)
                                     (get-routes-subway metro))
                               null)
                           )
  )



#|DOM: metro (subway) X conductores+ (TDAs driver+) (puede ser 1 o mas)
REC: metro (subway)
SIN RECURSION
Funcion que agrega los conductores del argumento al parametro de drivers del subway, sin elementos repetidos|#
;Nivel de implementacion: 1
(define subway-add-driver (lambda (metro . conductores)
                           (if (subway-without-all-elements? metro)
                               (list (get-id-subway metro) (get-name-subway metro)
                                     (get-trains-subway metro) (get-lines-subway metro)
                                     (append (get-drivers-subway metro)        (aux-no-repe (get-drivers-subway metro)     (filter (lambda (a) (driver? a)) conductores)))
                                     (get-line-trains-subway metro)(get-routes-subway metro))
                               null
                               )
                           )
  )









#|DOM: metro (subway)
REC: null
Recursion natural
Funcion que retorna un string con todos los elementos de un metro TDA subway. Utiliza varias funciones
   auxiliares que estan encapsuladas dentro de la misma|#
;Nivel de implementacion: 1
(define (subway->string metro)

  ;Dom lista de trenes (list TDAs train)   REC: string
  (define (string-trenes list-trenes)
    ;DOM: tren (TDA train)     REC: string
    (define (string-tren tren)
      (string-append
         "\n   Id tren: "
         (number->string (get-id-train tren))
         "\n   Fabricante: "
         (get-maker-train tren)
         "\n   Tipo de riel: "
         (get-rail-type-train tren)
         "\n   Rapidez: "
         (number->string (get-speed-train tren))
         (string-append "\n   Tiempo de espera por estacion: " (number->string (get-station-stay-time-train tren))  " minutos\n")        
         )
      )

    (if (empty? list-trenes)
        "\n"
        (string-append  (string-tren (car list-trenes))  (string-trenes (cdr list-trenes)) )
        )
    )



  ;DOM: lineas (lista de TDAs line)   REC: string  Recursion natural
  (define (string-lineas list-lineas)

    
    ;DOM: linea (TDA line)    REC: string    Recursion natural
    (define (string-linea linea)

      ;DOM: seccion (TDA section)     REC:string
      (define (string-seccion seccion)
        (string-append
          "\n      Estacion 1: "
          (string-append (get-name-station (get-station1-section seccion))
                           "  Tipo: " (get-type-station-station (get-station1-section seccion)))
          "\n      Estacion 2: "
          (string-append (get-name-station (get-station2-section seccion))
                           "  Tipo: " (get-type-station-station (get-station2-section seccion)))
          "\n      Distancia entre estaciones: "
          (number->string (get-distance-section seccion))
          "km\n      Costo monetario: "
          (number->string (get-cost-section seccion))  "\n"
          )
        )
      ;DOM: (lista de secciones)    REC: string     Recursion natural
      (define (aux-string-line secciones)
        (if (empty? secciones) "\n"
          (string-append
            "\n   Id linea: "
            (number->string (get-id-line linea))
            "\n   Nombre: " (get-name-line linea)
            "\n   Tipo de riel: " (get-rail-type-line linea)
            "\n   Secciones:"
            (if (empty? secciones) "\n"    (string-append (string-seccion (car secciones))   (aux-string-line (cdr secciones)))) 
            )
          )
        )
      (aux-string-line (get-sections-line linea))
      )
    
    (if (empty? list-lineas)
        "\n"
        (string-append  (string-linea (car list-lineas)) "\n\n\n\n\n" (string-lineas (cdr list-lineas)) )
        )

    )




   ;DOM: conductores (lista de TDAs driver)      REC: string
  (define (string-drivers drivers)
    ;DOM: conductor (TDA driver)     REC: string
    (define (string-conductor conduc)
      (string-append "\n   Id conductor: "
        (number->string (get-id-driver conduc))
        "\n   Nombre: "
        (get-name-driver conduc)
        "\n   Fabricante de trenes que conduce: "
        (get-maker-train-driver conduc) "\n"
        )
      )
    ;(map (lambda (s) (string-conductor s)) drivers)
    (if (empty? drivers) "\n"  (string-append (string-conductor (car drivers))  (string-drivers (cdr drivers))))
    )

    
  
  (string-append
     "\n\nDatos del sistema de metro\n ID: " (number->string (get-id-subway metro))
     "\n Nombre: " (get-name-subway metro)  "\n\n"
      "Trenes:" (string-trenes (get-trains-subway metro))
      "\nLineas:" (string-lineas (get-lines-subway metro))
      "\nConductores:" (string-drivers (get-drivers-subway metro))
     )
  )





#|DOM: metro (TDA subway) X funcion de costo currificada (f(x))
REC:metro (TDA subway)
Sin recursion
Funcion que modifica los costos de todas las secciones de de todas las lineas de un TDA subway, y lo retorna|#
;Nivel de implementacion: 1
(define (subway-rise-section-cost metro funct)
  ;DOM: secciones (lista de TDAs section) X funcion de costo currificada
  ;REC: secciones (lista TDAs section)
  ;Funcion que mediante una funcion de entrada modifica los costos de todos los elementos de una lista de secciones
  (define (mod-cost-lista-secciones secciones fun)
    (map (lambda (a)(section (get-station1-section a) (get-station2-section a)
                             (get-distance-section a) (fun (get-cost-section a)))) secciones)
    )

  ;DOM: lineas (lista de TDAs line) X funcion de costo currificada
  ;REC: lineas (lista de TDAs line)
  ;Funcion que retorna una lista de lineas dado que se modificaron los costos de todas las secciones de cada linea
  (define (mod-cost-secciones-en-line lineas fun)
    (map (lambda (c) (list (get-id-line c) (get-name-line c) (get-rail-type-line c); se usa list en vez del constructor line para evitar errores de que el elemento sea una lista dentro de otra lista,evitamos ese error
                           (mod-cost-lista-secciones (get-sections-line c) fun))) lineas)
    )

  (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro)
        (mod-cost-secciones-en-line (get-lines-subway metro) funct)
        (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
  )






#|DOM: metro (TDA subway) X nombre de la estacion (string) X tiempo en segundos (numero)
REC: metro (subway)
Sin recursion, y ademas utiliza funciones propias auxiliares
Funcion que modifica el tiempo de parada de una estacion|#
;Nivel de implementacion: 1
(define (subway-set-station-stoptime metro name-st time)

  #|DOM: secciones (lista de TDAs section) X nombre estacion (string) X nuevo tiempo en segundos (numero)
  REC: secciones (lista de TDAs section)
  No recursion
  Funcion que modifica el tiempo de parada de un estacion especifica buscada por su nombre en una lista de secciones|#
  (define (mod-time-en-secciones secciones name-st new-time)
    (map (lambda (s-act) (cond
                           [(eqv? name-st (get-name-station (get-station1-section s-act)))
                                (list (station (get-id-station (get-station1-section s-act)) name-st
                                               (get-type-station-station (get-station1-section s-act)) new-time
                                               );con esto construimos un nuevo elemento station1 con el nuevo tiempo
                                      (get-station2-section s-act) (get-distance-section s-act)
                                      (get-cost-section s-act))];se crea un elemento section actualizado
                         
                           [(eqv? name-st (get-name-station (get-station2-section s-act)))
                                (list (get-station1-section s-act)
                                      (station (get-id-station (get-station2-section s-act)) name-st
                                               (get-type-station-station (get-station2-section s-act)) new-time
                                               );con esto construimos un nuevo elemento station2
                                      (get-distance-section s-act) (get-cost-section s-act))];elemento section actualizado
                           [else s-act];caso en que al comparar nombres sean distintos, se agrega el mismo section
                          )
           ) secciones)
    )

  #|DOM: lineas (lista TDAs line) X nombre estacion (string) X nuevo tiempo en segundos (numero)
  REC: lineas (lista de TDAs line)
  Funcion que modifica el tiempo de parada de un estacion especifica dentro de una lista de lineas|#
  (define (mod-time-en-lineas lineas name-st new-time)
    (map (lambda (l-act)(list (get-id-line l-act) (get-name-line l-act) (get-rail-type-line l-act)
                              (mod-time-en-secciones (get-sections-line l-act) name-st new-time)))
         lineas)
    )

  (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro)
        (mod-time-en-lineas (get-lines-subway metro) name-st time)
        (get-drivers-subway metro) (get-line-trains-subway metro) (get-routes-subway metro))
  )







#|DOM: metro (subway) X id tren (entero) X id linea (entero)
REC: metro (subway)
Sin recursividad
Funcion que asigna un tren a una linea dentro del subway. Si ya existen metros asociados a una linea, se agregan junto a los previos.
     En el caso de que los trenes y/o lineas ingresadas no esten en el subway, se retorna el mismo subway sin cambios|#
;Nivel de implementacion: 1
(define (subway-assign-train-to-line metro id-tren id-linea)

  ;DOM:lista de line-trains (lista line-trains de TDA subway) X id linea (entero) X id tren (entero)
  ;Funcion que agrega trenes a una linea existente en el apartado de line-trains del TDA subway
  (define (agregar-trenes-a-linea-existente list-line-trains id-line id-train)
    (map (lambda (act) (if (eqv? id-line (car act))  (cons id-line (list (append (cadr act) (list id-train))))     act  ))   list-line-trains)
    )

  ;DOM: metro (subway) X id tren (entero) X id linea (entero)            REC: bool
  ;Funcion que determina la compatibilidad entre un tren y una linea, mediante si son iguales los tipos de riel
  (define (compatible-train-line sub id-train id-line)
    (eqv? (get-rail-type-train (filter  (lambda (a) (eqv? (get-id-train a) id-train))  (get-trains-subway sub)));obtenemos el tipo de riel del tren
          (get-rail-type-line  (filter  (lambda (e) (eqv? (get-id-line  e) id-line ))  (get-lines-subway sub))));obtenemos el tipo de riel de la linea
    )
  
  ;        condicion que la linea ya exista en el metro y que el tren ya exista en el metro
  (if (and (not (boolean? (member id-linea  (map (lambda (s) (get-id-line s))   (get-lines-subway metro))   )))
           (not (boolean? (member id-tren   (map (lambda (g) (get-id-train g))  (get-trains-subway metro))  )))
           (compatible-train-line metro id-tren id-linea))

      (if (boolean? (member id-linea (map (lambda (a)(car a))(get-line-trains-subway metro)))  )
                     ;condicion de la linea no tiene trenes asociados, ergo de que no este en el antepenultimo elemtno

          ;condicion de que la linea no tenia trenes asignados, de que no estaba en la lista N6
          (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro)
                (get-lines-subway metro) (get-drivers-subway metro)
                (append (get-line-trains-subway metro) (list (cons id-linea (list (list id-tren)))));utilizamos doble list porque al usar solo una lo toma como el cdr del cons y se pierde el formato deseado
                (get-routes-subway metro))

          ;caso en que la linea si tiene trenes
          (if ;condicion tren ya esta asociado a la linea
              (not (boolean?  (member id-tren  (cadr (car (filter (lambda (l-tr) (eqv? id-linea  (car l-tr))) (get-line-trains-subway metro))))    )))
              metro;se devuleve el mismo metro

             ;caso de que el tren no estaba en la linea, se hace append del en el id del tren anterior
             (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro)
                  (get-lines-subway metro) (get-drivers-subway metro)
                  (agregar-trenes-a-linea-existente (get-line-trains-subway metro) id-linea id-tren)
                  (get-routes-subway metro))
              )
          )
      
      metro;en caso de que la linea y/o tren no existan, se retorna el metro tal cual
      )
  )






#|DOM: metro (subway) X id conductor (entero) X id tren (entero) X hora de partida (string en formato HH:MM:SS de 24 hrs)
      X nombre de estacion de inicio (string) X nombre estacion de llegada (string)
REC: metro (subway)
Funcion que asigna un conductor a un tren en un horario de salida considerando estacion de partida y llegada|#
;Nivel de implementacion: 0,5
(define (subway-assign-driver-to-train metro id-driver id-tren time-i st-inicial st-final)

  ;    condicion de que el driver y el tren esten en el subway, agregar condicion compatibilidad de maker con el de la linea
  (if (and (not (boolean? (member id-driver  (map (lambda (s) (get-id-driver s))   (get-drivers-subway metro))  )))
           (not (boolean? (member id-tren    (map (lambda (g) (get-id-train g))    (get-trains-subway  metro))  )))
           )

      
      ;usamos alguna funcion que agrega un nuevo elemento a la lista, sin importanr que ya existan previos o
      ; que sea nula o que esten repetidos, despues ver c[omo implementar esta condicion
      (list (get-id-subway metro) (get-name-subway metro) (get-trains-subway metro)
            (get-lines-subway metro) (get-drivers-subway metro)
            (get-line-trains-subway metro)
            (append (get-routes-subway metro) (list (list id-driver id-tren time-i st-inicial st-final) ))
            )
      
      metro;caso de que no este el driver y/o el tren, se devuelve el subway sin cambios
      )
  )




;Nivel de implementacion: 0
;(define where-is-train )


;Nivel de implementacion: 0
;(define subway-train-path)



