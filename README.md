# point-3d-vectors
Implementation of basics of [point](https://github.com/JMC-design/point) protocol for Shinmera's 3d-vectors. 

Just some of the basics to mess around with.

````
(progn (setf *p3d* (point:random 1000000 1000.0 1000.0 1000.0)) (values))
(progn (setf *psv* (loop :repeat 1000000 :collect (3d-vectors:vec3-random 0 1000.0 ))) (values))

;; naive point:3d implementation
LIFE> (time-nogc (point:bounds *p3d*))
Evaluation took:
  0.064 seconds of real time
  0.063988 seconds of total run time (0.063988 user, 0.000000 system)
  100.00% CPU
  114,912,284 processor cycles
  16 bytes consed
  
P (1.1920929e-4,0.0019073486,1.1920929e-4)
P (999.99725,999.99835,999.9989)

;;basic implementation of 3d-vectors
LIFE> (time-nogc (point:bounds *psv*)) ;no min max

Evaluation took:
  0.148 seconds of real time
  0.146597 seconds of total run time (0.146597 user, 0.000000 system)
  99.32% CPU
  263,577,196 processor cycles
  49,136 bytes consed
(VEC3 1.1920929e-4 3.5762787e-4 64.375046)
(VEC3 999.999 999.99976 64.375046)

;; current implemntation with added nmin and nmax, specific methods are important
LIFE> (time-nogc (point:bounds *psv*))
Evaluation took:
  0.020 seconds of real time
  0.022471 seconds of total run time (0.022182 user, 0.000289 system)
  110.00% CPU
  40,185,932 processor cycles
  16 bytes consed
  
(VEC3 1.1920929e-4 3.5762787e-4 4.7683716e-4)
(VEC3 999.999 999.99976 999.99976)
;; using the implementation package showing gf taking up ~40%
LIFE> (time-nogc (point-3d-vectors%::bounds *psv*))
Evaluation took:
  0.012 seconds of real time
  0.011719 seconds of total run time (0.011719 user, 0.000000 system)
  100.00% CPU
  20,819,193 processor cycles
  0 bytes consed
  
(VEC3 1.1920929e-4 3.5762787e-4 4.7683716e-4)
(VEC3 999.999 999.99976 999.99976)
````
