#lang racket
(require "TDA-pcar_212788287_EspinozaBarria.rkt")
(require "TDA-section_212788287_EspinozaBarria.rkt")
(require "TDA-station_212788287_EspinozaBarria.rkt")
(require "TDA-type-station_212788287_EspinozaBarria.rkt")
(require "TDA-line_212788287_EspinozaBarria.rkt")
(require "TDA-train_212788287_EspinozaBarria.rkt")
(require "TDA-driver_212788287_EspinozaBarria.rkt")
(require "TDA-subway_212788287_EspinozaBarria.rkt")



;SCRIPT DE PRUEBAS ISAAC ESPINOZA 212788287
;3 o más ejemplos por cada funcionalidad implementada


;Estaciones L1 simplificada metro santiago
(define e0 (station 0 "San Pablo" t 90))
(define e1 (station 1 "Neptuno" r 45))
(define e2 (station 2 "Pajaritos" c 45))
(define e3 (station 3 "Las Rejas" r 45))
(define e4 (station 4 "Ecuador" r 60))
(define e5 (station 5 "San Alberto Hurtado" r 40))
(define e6 (station 6 "Universidad de Santiago de Chile" c 40))
(define e7 (station 7 "Estación Central" c 45))
(define e8 (station 8 "Unión Latinoamericana" r 30))
(define e9 (station 9 "República" r 40))
(define e10 (station 10 "Los Héroes" c 60))
(define e11 (station 11 "La Moneda" r 40))
(define e12 (station 12 "Universidad de Chile" c 90))
(define e13 (station 13 "Santa Lucía" r 40))
(define e14 (station 14 "Universidad Católica" c 60))
(define e15 (station 15 "Baquedano" r 40))
(define e16 (station 16 "Los Dominicos" t 90))
(define e17 (station 17 "Cochera Neptuno" m 3600))

;Estaciones L2 simplificada metro santiago, para una versión circular
(define e18 (station 18 "El Llano" r 60))
(define e19 (station 19 "Franklin" r 50))
(define e20 (station 20 "Rondizzoni" r 55))
(define e21 (station 21 "Parque O'Higgins" r 65))
(define e22 (station 22 "Toesca" r 65))
(define e23 (station 23 "Santa Ana" c 65))
(define e24 (station 24 "Puente Cal y Canto" c 65))

;Estaciones L3 simplificada metro santiago
(define e25 (station 25 "Plaza Quilizura" t 90))
(define e26 (station 26 "Lo Cruzat" r 45))
(define e27 (station 27 "Ferrocarril" r 45))
(define e28 (station 28 "Los Libertadores" c 60))
(define e29 (station 29 "Cardenal Caro" r 45))
(define e30 (station 30 "Vivaceta" r 45))
(define e31 (station 31 "Conchalí" r 45))
(define e32 (station 32 "Plaza Chacabuco" r 45))
(define e33 (station 33 "Hospitales" r 45))
(define e34 (station 34 "Puente Cal y Canto" c 65))
(define e35 (station 35 "Plaza de Armas" c 60))
(define e36 (station 36 "Universidad de Chile" c 90))
(define e37 (station 37 "Parque Almagro" r 45))
(define e38 (station 38 "Matta" t 90))

;Tramos Línea 1
(define s0 (section e0 e1 4 15))
(define s1 (section e1 e2 3 14))
(define s2 (section e2 e3 2.5 10))
(define s3 (section e3 e4 4.5 17))
(define s4 (section e4 e5 4.7 18))
(define s5 (section e5 e6 4.3 17))
(define s6 (section e6 e7 3.8 12))
(define s7 (section e7 e8 2.5 10))
(define s8 (section e8 e9 4.5 17))
(define s9 (section e9 e10 4.7 18))
(define s10 (section e10 e11 4.3 17))
(define s11 (section  e11 e12 3.8 12))
(define s12 (section e12 e13 4.5 17))
(define s13 (section e13 e14 4.7 18))
(define s14 (section e14 e15 4.3 17))
(define s15 (section e15 e16 4.2 17))
;enlace cochera
(define s16 (section e1 e17 3.8 12))

;Tramos Línea 2, línea circular
(define s17 (section e18 e19 4 15))
(define s18 (section e19 e20 3 12))
(define s19 (section e20 e21 5 18))
(define s20 (section e21 e22 4.5 16))
(define s21 (section e22 e10 4.2 16))
(define s22 (section e10 e23 4.2 16))
(define s23 (section e23 e24 4.2 16))
(define s24 (section e24 e18 28 90))

;Tramos Linea 3
(define s25 (section e25 e26 5 20))
(define s26 (section e26 e27 7 25))
(define s27 (section e27 e28 4 12))
(define s28 (section e28 e29 6 23))
(define s29 (section e29 e30 10 14))
(define s30 (section e30 e31 4 33))
(define s31 (section e31 e32 23 13))
(define s32 (section e32 e33 32 30))
(define s33 (section e33 e34 2 30))
(define s34 (section e34 e35 4 15))
(define s35 (section e35 e36 8 20))
(define s36 (section e36 e37 10 12))
(define s37 (section e37 e38 20 22))

;Creación de Línea 1 con todos los tramos
(define l1 (line 1 "Línea 1" "UIC 60 ASCE" s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15))
;Creación de Línea 2 sin incluir tramos
(define l2 (line 2 "Línea 2" "100 R.E."))
;Creación de Línea 3 con todos los tramos
(define l3 (line 3 "Línea 3" "UIC 60 ASCE" s25 s26 s27 s28 s29 s30 s31 s32 s33 s34 s35 s36 s37))

(line-length l1) ;resultado es 64,3 considerando tramos hacia estaciones de mantenimiento
(line-length l2) ;resultado es 0
(line-length l3) ;resultado es 135

(line-section-length l1 "San Pablo" "Las Rejas")   ;respuesta es 9.5
(line-section-length l3 "Hospitales" "Plaza de Armas") ;respuesta es 6
(line-section-length l3 "Plaza de Armas" "Hospitales") ;respuesta es 6, notamos que la funcion retorna la distancia de una seccion sin importar el orden de entrada de las estaciones

(line-cost l1) ;resultado es 246 considerando tramos hacia estaciones de mantenimiento
(line-cost l2) ;resultado es 0
(line-cost l3) ;resultado es 269

(line-section-cost l1 "San Pablo" "Las Rejas") ;resultado es 39
(line-section-cost l1 "Universidad de Santiago de Chile" "Baquedano") ;resultado es 138
(line-section-cost l3 "Lo Cruzat" "Parque Almagro") ;resultado es 227

;añadiendo tramos a l2
(define l2a (line-add-section l2 s17))
(define l2b (line-add-section l2a s18))
(define l2c (line-add-section l2b s19))
(define l2d (line-add-section l2c s20))
(define l2e (line-add-section l2d s21))
(define l2f (line-add-section l2e s22))
(define l2g (line-add-section l2f s23))
(define l2h (line-add-section l2g s24))
(define l2i (line-add-section l2h s19))  ;no se añade la estacion duplicada y se de vuelve la linea de entrada sin cambios l2h 
(define l3-invalida (line-add-section l3 s20))

(line? l1)  ;devuelve true
(line? l2)  ;devuelve false
(line? l2e) ;devuelve false
(line? l2h) ;devuelve true
(line? l3)  ;devuleve true
(line? l3-invalida) ;devulve false
(display "\n") ; se imprime salto de linea para visualizar resultados en la consola

;creando carros
(define pc0 (pcar 0 100 "NS-74" tr))
(define pc1 (pcar 1 100 "NS-74" ct))
(define pc2 (pcar 2 150 "NS-74" ct))
(define pc3 (pcar 3 100 "NS-74" ct))
(define pc4 (pcar 4 100 "NS-74" tr))
(define pc5 (pcar 5 100 "AS-2014" tr))
(define pc6 (pcar 6 100 "AS-2014" ct))
(define pc7 (pcar 7 100 "AS-2014" ct))
(define pc8 (pcar 8 100 "AS-2014" ct))
(define pc9 (pcar 9 100 "AS-2014" tr))
(define pc10 (pcar 10 100 "AS-2014" tr))
(define pc11a (pcar 11 100 "AS-2016" tr))
(define pc11 (pcar 12 100 "AS-2016" ct))
(define pc12 (pcar 13 100 "AS-2016" ct))
(define pc13 (pcar 14 150 "AS-2016" ct))
(define pc14 (pcar 15 100 "AS-2016" ct))
(define pc15 (pcar 16 100 "AS-2016" ct))
(define pc16 (pcar 17 100 "AS-2016" ct))
(define pc17 (pcar 18 100 "AS-2016" tr))

;creando trenes
(define t0 (train 0 "CAF" "UIC 60 ASCE" 60 1.5)) ;tren sin carros definidos
(define t1 (train 1 "CAF" "UIC 60 ASCE" 70  2 pc0 pc1 pc2 pc3 pc4)) ;tren válido
(define t2 (train 2 "CAF" "100 R.E." 70  2 pc5 pc6 pc7 pc8 pc9)) ;tren válido
(define t3 (train 3 "CAF" "100 R.E." 70  2 pc11a pc11 pc12 pc13 pc14 pc15 pc16 pc17)) ;tren válido
(define t4 (train 4 "CAF" "100 R.E." 70  2 pc1 pc2 pc3)) ;tren inválido sin terminales en extremos
(define t5 (train 5 "CAF" "100 R.E." 70  2 pc0 pc5 pc9 pc12 pc17))  ;tren inválido por incompatibilidad de carros
(define t6 (train 6 "CAF" "UIC 60 ASCE" 60 2 pc1 pc2 pc6 pc4)); tren invalido sin un extremo terminal y un incompatible de modelo
  
(define t0a (train-add-car t0 pc5 0))
(define t0b (train-add-car t0a pc6 1))
(define t0c (train-add-car t0b pc7 2))
(define t0d (train-add-car t0c pc8 3))
(define t0e (train-add-car t0d pc9 4)) ;tren válido

(define t1a (train-remove-car t1 0)) ;tren invalido
(define t1b (train-remove-car t1 2)) ;tren valido
(define t1c (train-remove-car t1b 3)) ;tren invalido

;verificación de válidez en la conformación de trenes
(train? t0)  ;arroja #f
(train? t1)  ;arroja #t
(train? t2)  ;arroja #t
(train? t3)  ;arroja #t
(train? t4)  ;arroja #f
(train? t0a) ;arroja #f
(train? t0b) ;arroja #f
(train? t0c) ;arroja #f
(train? t0d) ;arroja #f
(train? t0e) ;arroja #t
(train? t1a) ;arroja #f
(train? t1b) ;arroja #t
(train? t1c) ;arroja #f
(display "\n") ; se imprime salto de linea para visualizar resultados en la consola

;determinar capacidad del tren
(train-capacity t0) ;arroja 0
(train-capacity t1) ;arroja 550
(train-capacity t1b);arroja 400


;Creando drivers
(define d0 (driver 0 "Juan" "CAF"))
(define d1 (driver 1 "Alejandro" "Alsthom"))
(define d2 (driver 2 "Diego" "Alsthom"))
(define d3 (driver 3 "Pedro" "CAF"))
(define d4 (driver 4 "Isaac" "CAF"))

;Creando Metros
(define sw0 (subway 0 "Metro de Santiago"))
(define sw1 (subway 1 "Subte"))
(define sw2 (subway 2 "Metro de Valparaíso"))

;Agregando trenes
(define sw0a (subway-add-train sw0 t1 t2 t0e))
(define sw0b (subway-add-train sw0a t3))
(define sw0a1 (subway-add-train sw0 t1 t2 t0e t3 t1b))

;Agregando lineas
(define sw0c (subway-add-line sw0b l1 l2h))
(define sw0d (subway-add-line sw0c l3))
(define sw0d1 (subway-add-line sw0b l1))

;Agregando drivers
(define sw0e (subway-add-driver sw0d d0 d1 d2))
(define sw0f (subway-add-driver sw0e d3 d4))
(define sw0f1 (subway-add-driver sw0e d2));notar d2 repetido entonces retorna el subway sin cambios, sw0e

;Expresado subway como string
;(subway->string sw0e)
;(subway->string sw0f)
;(subway->string sw0f1)

;Aumentando los costos de las estaciones en un 30%
(define sw0g (subway-rise-section-cost sw0f (lambda (c) (* c 1.3))));aumento 30 porciento
(define sw0h (subway-rise-section-cost sw0g (lambda (c) (* c 1.05)))); aumento 5 porciento
(define sw0i (subway-rise-section-cost sw0f (lambda (c) (* c 0.9)))) ;disminucion del 10 porciento

;Cambiando el tiempo de parada de algunas estaciones
(define sw0j (subway-set-station-stoptime sw0g "Los Héroes" 180))
(define sw0j1 (subway-set-station-stoptime sw0e "San Pablo" 50))
(define sw0j2 (subway-set-station-stoptime sw0e "Universidad de Santiago de Chile" 300))

;Asignando trenes a líneas
(define sw0k (subway-assign-train-to-line sw0j 0 1))
(define sw0l (subway-assign-train-to-line sw0k 2 2))
(define sw0m (subway-assign-train-to-line sw0l 1 1))

;Asignando conductores a trenes
(define sw0n (subway-assign-driver-to-train sw0m 0 0 "11:00:00" "San Pablo" "Los Héroes"))
(define sw0o (subway-assign-driver-to-train sw0n 2 2 "12:00:00" "El Llano" "Toesca"))
(define sw0p (subway-assign-driver-to-train sw0o 4 1 "08:00:00" "Matta" "Lo Cruzat"))

;preguntando dónde está el tren
;(where-is-train sw0j 0 "11:12:00") 

;produciendo la ruta que sigue el tren
;(subway-train-path sw0j 0 “11:30:00”)

