;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-alarm - Test/example usage code. Note this is public domain.
;;
;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>

(in-package :cl-user)

(defparameter *join-tree*
  (bn:load-dne (merge-pathnames "git/cl-bayesnet/nets/alarm.dne"
				(user-homedir-pathname))))

(defparameter *interpreted*
  (bn:load-xmlbif (merge-pathnames "git/cl-bayesnet/nets/alarm.xml"
				(user-homedir-pathname))))

(defparameter *compiled-c*
  (bn:load-xmlbif (merge-pathnames "git/cl-bayesnet/nets/alarm.xml"
				(user-homedir-pathname))))

(time
 (defparameter *jt* (bn:use-join-tree *join-tree*)))
;; 0.033 seconds of real time

(time
 (defparameter *instructions* (bn:use-interpreted *interpreted*)))
;; 27.236 seconds of real time

(time
 (bn:use-compiled-c *compiled-c*))
;; 28.207 seconds of real time

(bn:node-order *join-tree*) ;; dne
#(:HYPOVOLEMIA :LVFAILURE :LVEDVOLUME :STROKEVOLUME :CVP :PCWP :INSUFFANESTH
  :PULMEMBOLUS :INTUBATION :SHUNT :KINKEDTUBE :MINVOLSET :VENTMACH :DISCONNECT
  :VENTTUBE :VENTLUNG :VENTALV :FIO2 :PVSAT :SAO2 :ANAPHYLAXIS :TPR :ARTCO2
  :CATECHOL :HR :CO :HISTORY :BP :ERRCAUTER :HREKG :HRSAT :ERRLOWOUTPUT :HRBP
  :EXPCO2 :PAP :PRESS :MINVOL)

(bn:node-order *interpreted*) ;; bif
#(:HYPOVOLEMIA :STROKEVOLUME :LVFAILURE :LVEDVOLUME :PCWP :CVP :HISTORY
  :MINVOLSET :VENTMACH :DISCONNECT :VENTTUBE :KINKEDTUBE :PRESS :ERRLOWOUTPUT
  :HRBP :ERRCAUTER :HREKG :HRSAT :BP :CO :HR :TPR :ANAPHYLAXIS :INSUFFANESTH
  :PAP :PULMEMBOLUS :FIO2 :CATECHOL :SAO2 :SHUNT :PVSAT :MINVOL :EXPCO2 :ARTCO2
  :VENTALV :VENTLUNG :INTUBATION)

(bn:states (bn:node :hypovolemia *compiled-c*))
#(:TRUE :FALSE)

(bn:states (bn:node :bp *compiled-c*))
#(:LOW :NORMAL :HIGH)

;; For queries you can use the join-tree object same as the net object.
;; I use *jt* to demonstrate
(bn:query *jt* '(:bp :low :history :false))
0.41846273520929067d0

(bn:query *interpreted* '(:bp :low :history :false))
0.41846274444015086d0

(bn:query *compiled-c* '(:bp :low :history :false))
0.41846274444015086d0

(bn:query (bn:node :hrbp *jt*))
#(0.07128078734685757d0 0.4167348123134152d0 0.5119844003397273d0)

(bn:query (bn:node :hrbp *interpreted*))
#(0.07128078957655479d0 0.4167348170680417d0 0.5119843933554036d0)

(bn:query (bn:node :hrbp *compiled-c*))
#(0.07128078957655479d0 0.4167348170680417d0 0.5119843933554036d0)

(bn:query (bn:node :hrbp *jt*) '(:ventlung :zero))
#(0.037591916440952444d0 0.13858216958572717d0 0.8238259139733204d0)

(bn:query (bn:node :hrbp *interpreted*) '(:ventlung :zero))
#(0.03759191698211968d0 0.13858217452537563d0 0.8238259084925047d0)

(bn:query (bn:node :hrbp *compiled-c*) '(:ventlung :zero))
#(0.03759191698211968d0 0.13858217452537563d0 0.8238259084925047d0)

;; some speed tests.
(progn
  (time 
   (length (bn::test-net-random *compiled-c* 100000))))
;; 0.636 seconds of real time

(progn
  (time 
   (length (bn::test-net-random *interpreted* 100000))))
;; 12.010 seconds of real time

;; Here we use only 1000 because join tree is much slower.
(progn
  (time 
   (length (bn::test-net-random *join-tree* 1000))))
;; 8.281 seconds of real time

;; Evidence
(bn:with-evidence (*compiled-c* :ventlung 0 :pcwp 0)
  (bn:query *compiled-c*))
0.029107331259445833d0

(bn:states (bn:node :hrbp *compiled-c*))
#(:LOW :NORMAL :HIGH)

(bn:with-evidence (*compiled-c* :ventlung :high :pcwp :high)
  (bn:query (bn:node :hrbp *compiled-c*)))
#(0.09471917513150513d0 0.6102541714395288d0 0.2950266534289663d0)
