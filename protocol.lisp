(in-package #:point-3d-vectors)

;(defmethod  ((p s:vec)))
(defmethod point:new ((type (eql :svec2)) x y &optional z w)
  (declare (ignore type z w))
  (s:vec x y))
(defmethod point:new ((type (eql :svec3)) x y &optional z w)
  (declare (ignore type w))
  (s:vec x y z))
(defmethod point:new ((type (eql :svec4)) x y &optional z w)
  (declare (ignore type))
  (s:vec x y z w))

(defmethod point:copy ((p s:vec2)) (s:vcopy2 p))
(defmethod point:copy ((p s:vec3)) (s:vcopy3 p))
(defmethod point:copy ((p s:vec4)) (s:vcopy4 p))

(defmethod point:dimensions ((p s:vec2)) 2)
(defmethod point:dimensions ((p s:vec3)) 3)
(defmethod point:dimensions ((p s:vec4)) 4)

(defmethod point:element-type ((p s:vec2)) s::*float-type*)
(defmethod point:element-type ((p s:vec3)) s::*float-type*)
(defmethod point:element-type ((p s:vec4)) s::*float-type*)

(defmethod point:x ((p s:vec2)) (s:vx2 p))
(defmethod point:x ((p s:vec3)) (s:vx3 p))
(defmethod point:x ((p s:vec4)) (s:vx4 p))

(defmethod point:y ((p s:vec2)) (s:vy2 p))
(defmethod point:y ((p s:vec3)) (s:vy3 p))
(defmethod point:y ((p s:vec4)) (s:vy4 p))

(defmethod point:z ((p s:vec3)) (s:vz3 p))
(defmethod point:z ((p s:vec4)) (s:vz4 p))

(defmethod point:w ((p s:vec4)) (s:vw4 p))

(defmethod (setf point:x) (value (p s:vec2)) (setf (s:vx2 p) value))
(defmethod (setf point:x) (value (p s:vec3)) (setf (s:vx3 p) value))
(defmethod (setf point:x) (value (p s:vec4)) (setf (s:vx4 p) value))

(defmethod (setf point:y) (value (p s:vec2)) (setf (s:vy2 p) value))
(defmethod (setf point:y) (value (p s:vec3)) (setf (s:vy3 p) value))
(defmethod (setf point:y) (value (p s:vec4)) (setf (s:vy4 p) value))

(defmethod (setf point:z) (value (p s:vec3)) (setf (s:vz3 p) value))
(defmethod (setf point:z) (value (p s:vec4)) (setf (s:vz4 p) value))
(defmethod (setf point:w) (value (p s:vec4)) (setf (s:vw4 p) value))

(defmethod point:* ((p1 s:vec2) (p2 s:vec2)) (point-%3d-vectors:* p1 p2))
(defmethod point:* ((p1 s:vec3) (p2 s:vec3)) (point-%3d-vectors:* p1 p2))
(defmethod point:* ((p1 s:vec4) (p2 s:vec4)) (point-%3d-vectors:* p1 p2))

(defmethod point:/ ((p1 s:vec2) (p2 s:vec2)) (point-%3d-vectors:/ p1 p2))
(defmethod point:/ ((p1 s:vec3) (p2 s:vec3)) (point-%3d-vectors:/ p1 p2))
(defmethod point:/ ((p1 s:vec4) (p2 s:vec4)) (point-%3d-vectors:/ p1 p2))


(defmethod point:+ ((p1 s:vec2) (p2 s:vec2)) (point-%3d-vectors:+ p1 p2))
(defmethod point:+ ((p1 s:vec3) (p2 s:vec3)) (point-%3d-vectors:+ p1 p2))
(defmethod point:+ ((p1 s:vec4) (p2 s:vec4)) (point-%3d-vectors:+ p1 p2))

(defmethod point:- ((p1 s:vec2) (p2 s:vec2)) (point-%3d-vectors:- p1 p2))
(defmethod point:- ((p1 s:vec3) (p2 s:vec3)) (point-%3d-vectors:- p1 p2))
(defmethod point:- ((p1 s:vec4) (p2 s:vec4)) (point-%3d-vectors:- p1 p2))

(defmethod point:->list ((p s:vec2)) (list (s:vx2 p)(s:vy2 p)))
(defmethod point:->list ((p s:vec3)) (list (s:vx3 p)(s:vy3 p)(s:vz3 p)))
(defmethod point:->list ((p s:vec4)) (list (s:vx4 p)(s:vy4 p)(s:vz4 p)(s:vw4 p)))

(defmethod point:->values ((p s:vec2)) (values (s:vx2 p)(s:vy2 p)))
(defmethod point:->values ((p s:vec3)) (values (s:vx3 p)(s:vy3 p)(s:vz3 p)))
(defmethod point:->values ((p s:vec4)) (values (s:vx4 p)(s:vy4 p)(s:vz4 p)(s:vw4 p)))

(defmethod point:->vector ((p s:vec2)) (vector (s:vx2 p)(s:vy2 p)))
(defmethod point:->vector ((p s:vec3)) (vector (s:vx3 p)(s:vy3 p)(s:vz3 p)))
(defmethod point:->vector ((p s:vec4)) (vector (s:vx4 p)(s:vy4 p)(s:vz4 p)(s:vw4 p)))

(defmethod point:min ((p1 s:vec2)(p2 s:vec2)) (s:vmin p1 p2))
(defmethod point:min ((p1 s:vec3)(p2 s:vec3)) (s:vmin p1 p2))
(defmethod point:min ((p1 s:vec4)(p2 s:vec4)) (s:vmin p1 p2))

(defmethod point:max ((p1 s:vec2)(p2 s:vec2)) (s:vmax p1 p2))
(defmethod point:max ((p1 s:vec3)(p2 s:vec3)) (s:vmax p1 p2))
(defmethod point:max ((p1 s:vec4)(p2 s:vec4)) (s:vmax p1 p2))


(defmethod point:nmin ((p1 s:vec2)(p2 s:vec2)) (setf (s:vx2 p1) (min (s:vx2 p1) (s:vx2 p2))
                                                     (s:vy2 p1) (min (s:vy2 p1) (s:vy2 p2))))
(defmethod point:nmin ((p1 s:vec3)(p2 s:vec3)) (setf (s:vx3 p1) (min (s:vx3 p1) (s:vx3 p2))
                                                     (s:vy3 p1) (min (s:vy3 p1) (s:vy3 p2))
                                                     (s:vz3 p1) (min (s:vz3 p1) (s:vz3 p2))))
(defmethod point:nmin ((p1 s:vec4)(p2 s:vec4)) (setf (s:vx4 p1) (min (s:vx4 p1) (s:vx4 p2))
                                                     (s:vy4 p1) (min (s:vy4 p1) (s:vy4 p2))
                                                     (s:vz4 p1) (min (s:vz4 p1) (s:vz4 p2))
                                                     (s:vw4 p1) (min (s:vw4 p1) (s:vw4 p2))))
(defmethod point:nmax ((p1 s:vec2)(p2 s:vec2)) (setf (s:vx2 p1) (max (s:vx2 p1) (s:vx2 p2))
                                                     (s:vy2 p1) (max (s:vy2 p1) (s:vy2 p2))))
(defmethod point:nmax ((p1 s:vec3)(p2 s:vec3)) (setf (s:vx3 p1) (max (s:vx3 p1) (s:vx3 p2))
                                                     (s:vy3 p1) (max (s:vy3 p1) (s:vy3 p2))
                                                     (s:vz3 p1) (max (s:vz3 p1) (s:vz3 p2))))
(defmethod point:nmax ((p1 s:vec4)(p2 s:vec4)) (setf (s:vx4 p1) (max (s:vx4 p1) (s:vx4 p2))
                                                     (s:vy4 p1) (max (s:vy4 p1) (s:vy4 p2))
                                                     (s:vz4 p1) (max (s:vz4 p1) (s:vz4 p2))
                                                     (s:vw4 p1) (max (s:vw4 p1) (s:vw4 p2))))

;welp, that was pretty dumb, isn't this what macros are for?

