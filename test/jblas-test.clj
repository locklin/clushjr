(require 'idx)

(def mx1 (idx/rand-normal 3000 3000))

(def mx2 (idx/rand-normal 3000 3000))

(time (def mx3 (idx/mul mx1 mx2))) ;; 3.8 seconds

(def mx4 (idx/rand-normal 500 500))
(def mx5 (idx/rand-normal 1500 1500))

(time (def ev (idx/eigenvalues mx4))) ;; 0.5 secs for 500x500,
(time (def ev (idx/eigenvalues mx5))) ;;  14 sec for 1500^2

(time (def mx6 (idx/mul mx3 (idx/mul mx1 mx2)))) ;;7.2 secs

(time (def ev (idx/eigenvalues mx3))) ;;


;; incanter stuff
(use '[incanter core stats])

(defn rand-matrix [n]
  (matrix (sample-normal (* n n)) n))


(def imx1 (rand-matrix 3000))
(def imx2 (rand-matrix 3000))


(time (def imx3 (mmult imx1 imx2))) ;;37 secs 
(time (def imx4 (mmult imx3 (mmult imx1 imx2)))) ;; 74 seconds 

(def imx5 (rand-matrix 1500))
(def imx6 (rand-matrix 500))
(time (def ev (decomp-eigenvalue imx5))) ;;195 secs
(time (def ev (decomp-eigenvalue imx6))) ;; 2.4

;; more tests for jblas
(def mx1 (idx/rand-normal 1000 500))
(def mx2 (idx/rand-normal 1000 500))
(idx/concat-horiz mx1 mx2)