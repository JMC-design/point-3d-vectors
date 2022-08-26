(defpackage #:point-3d-vectors%
  (:use :cl)
  (:shadow cl:+
           cl:-
           cl:*
           cl:/
           cl:random
           cl:min
           cl:max)
  (:local-nicknames (#:s #:org.shirakumo.flare.vector))
  (:export #:dimensions
           #:copy
           #:element-type 

           #:==
           #:x
	   #:y
	   #:z
           #:w
           
           #:->list
	   #:->vector
	   #:->values

           ;; Protocol parts that would be nice to have for new implementations

           #:+
           #:-
           #:*
           #:/

           #:magnitude
           #:dot
           #:cross
           #:normalize

           #:min
           #:max
           #:nmin
           #:nmax

          
           #:distance
           ;; #:euclidean-sqrt 
           ;; #:euclidean
           ;; #:manhattan
           ;; #:minkowski      
           ;; #:of-division
           ;; #:1norm
           ;; #:2norm
           ;; #:pnorm
           ;; #:angle
                   
           #:angle
          
           ))
(in-package :point-3d-vectors%)
(declaim (inline * / + - x y z w
                 (setf x) (setf y) (setf z) (setf w)
                 min max nmin nmax angle))

(defun new (type x y &optional z w)
  (declare (ignore type))
  (cond
    (w (s:vec x y z w))
    (z (s:vec x y z))
    (t (s:vec x y))))

(defun copy (p)
  (s:vcopy p))

(defun dimensions (p)
  (typecase p
    (s:vec2 2)
    (s:vec3 3)
    (s:vec4 4)))

(defun x (p)
  (s:vx p))
(defun y (p)
  (s:vy p))
(defun z (p)
  (s:vz p))
(defun w (p)
  (s:vw p))

(defun * (p1 p2)
  (s:v* p1 p2))
(defun / (p1 p2)
  (s:v/ p1 p2))
(defun + (p1 p2)
  (s:v+ p1 p2))
(defun - (p1 p2)
  (s:v- p1 p2))

(defun (setf x) (value p)
  (setf (s:vx p) value))
(defun (setf y) (value p)
  (setf (s:vy p) value))
(defun (setf z) (value p)
  (setf (s:vz p) value))
(defun (setf w) (value p)
  (setf (s:vw p) value))
(defun set-all (p x y &optional z w)
  (typecase p
    (s:vec2 (setf (s:vx p) x (s:vy p) y) p)
    (s:vec3 (setf (s:vx p) x (s:vy p) y (s:vz p) z) p)
    (s:vec4 (setf (s:vx p) x (s:vy p) y (s:vz p) z (s:vw p) w) p)))
(defun element-type (p)
  (declare (ignore p))
  s::*float-type*)

(defun ->list (p)
  (typecase p
    (s:vec2 (list (x p) (y p)))
    (s:vec3 (list (x p) (y p) (z p)))
    (s:vec4 (list (x p) (y p) (z p) (w p)))))

(defun ->vector (p)
  (typecase p
    (s:vec2 (vector (x p) (y p)))
    (s:vec3 (vector (x p) (y p) (z p)))
    (s:vec4 (vector (x p) (y p) (z p) (w p)))))

(defun ->values (p)
  (typecase p
    (s:vec2 (values (x p) (y p)))
    (s:vec3 (values (x p) (y p) (z p)))
    (s:vec4 (values (x p) (y p) (z p) (w p)))))

(defun magnitude (p)
  (s::length p))
(defun dot (p1 p2)
  (s:v. p1 p2))
(defun cross (p1 p2)
  "Will error if points are not vec3s."
  (s:vc p1 p2))
(defun normalize (p)
  (s:vunit p))

(defun 1norm (p)
  (s:v1norm p))
(defun 2norm (p)
  (s:v2norm p))
(defun pnorm (p h)
  (s:vpnorm p h))

(defun min (p1 p2)
  (s:vmin p1 p2))
(defun max (p1 p2)
  (s:vmax p1 p2))

(defun nmin (p1 p2)
  (typecase p1
    (s:vec2 (setf (s:vx2 p1) (cl:min (s:vx2 p1) (s:vx2 p2))
                  (s:vy2 p1) (cl:min (s:vy2 p1) (s:vy2 p2))))
    (s:vec3 (setf (s:vx3 p1) (cl:min (s:vx3 p1) (s:vx3 p2))
                  (s:vy3 p1) (cl:min (s:vy3 p1) (s:vy3 p2))
                  (s:vz3 p1) (cl:min (s:vz3 p1) (s:vz3 p2))))
    (s:vec4 (setf (s:vx4 p1) (cl:min (s:vx4 p1) (s:vx4 p2))
                  (s:vy4 p1) (cl:min (s:vy4 p1) (s:vy4 p2))
                  (s:vz4 p1) (cl:min (s:vz4 p1) (s:vz4 p2))
                  (s:vw4 p1) (cl:min (s:vw4 p1) (s:vw4 p2))))))

(defun nmax (p1 p2)
  (typecase p1
    (s:vec2 (setf (s:vx2 p1) (cl:max (s:vx2 p1) (s:vx2 p2))
                  (s:vy2 p1) (cl:max (s:vy2 p1) (s:vy2 p2))))
    (s:vec3 (setf (s:vx3 p1) (cl:max (s:vx3 p1) (s:vx3 p2))
                  (s:vy3 p1) (cl:max (s:vy3 p1) (s:vy3 p2))
                  (s:vz3 p1) (cl:max (s:vz3 p1) (s:vz3 p2))))
    (s:vec4 (setf (s:vx4 p1) (cl:max (s:vx4 p1) (s:vx4 p2))
                  (s:vy4 p1) (cl:max (s:vy4 p1) (s:vy4 p2))
                  (s:vz4 p1) (cl:max (s:vz4 p1) (s:vz4 p2))
                  (s:vw4 p1) (cl:max (s:vw4 p1) (s:vw4 p2))))))

(defun bounds (points)
  (loop :with low := (copy (car points))
        :with high := (copy (car points))
        :for p :in points
        :do (nmin low p) (nmax high p)
        :finally (return (values low high))))

(defun angle (p1 &optional (p2 p1))
  (s:vangle p1 p2))
