(in-package :asdf-user)
(defsystem "point-3d-vectors"
  :description "Point protocol implementation for Shinmera's 3d-vector library."
  :version "0.0.1"
  :licence "LGPL"
  :author "Johannes Martinez Calzada"
  :depends-on ("point" "3d-vectors")
  :components ((:file "package")
               (:file "point")
               (:file "protocol")))
